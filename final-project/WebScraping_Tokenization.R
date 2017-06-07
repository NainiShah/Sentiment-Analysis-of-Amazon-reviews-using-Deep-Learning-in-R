
#WebScrapping of Girl on the Train
library(rvest)
library(curl)

#Read the URL
url <- "https://www.amazon.com/Girl-Train-Novel-Paula-Hawkins/product-reviews/0735219753/ref=cm_cr_dp_d_acr_sr?ie=UTF8&reviewerType=avp_only_reviews"
data <- read_html(paste(url,1,sep = ""))
review <- data %>% html_nodes(".review-text") %>%  html_text()

for(level in c(2:500)){
  data <- read_html(paste(url,level,sep = ""))
  review <- c(review,data %>% html_nodes(".review-text") %>% html_text())
}

review <- as.data.frame(review)
View(review)
write.csv(review, "Amazon_Comments.csv")


library(tm)
library(RTextTools)
library(plyr)
library('stringr')
library('dplyr')
library(tidytext)

review <- read.csv("C:\\Users\\Neola\\Documents\\Amazon_Comments.csv")


Tokenize_Review <- function(string){
  # Change all the words to lowercase
  t_re <- tolower(string)
  # Remove everything that is not a number or letter 
  t_re <- stringr::str_replace_all(t_re,"[^a-zA-Z\\s]", " ")
  # Shrink down to just one white space
  t_re <- stringr::str_replace_all(t_re,"[\\s]+", " ")
  # Split the data
  t_re <- stringr::str_split(t_re, " ")[[1]]
  # Get rid of trailing "" if necessary
  i <- which(t_re == "")
  if(length(i) > 0){
    t_re <- t_re[-i]
  } 
  print(t_re)
  return(t_re)
}


tokenized_data = Corpus(DataframeSource(review))

cleanData <- tm_map(tokenized_data,Tokenize_Review)
cleanData
plaindata = tm_map(cleanData, PlainTextDocument)
plaindata

tokenized_data = tm_map(plaindata, removeWords, c("the","and","you","for","that","this","are","was","your","has","there","had","they","our","them","were","hey","of","is","to", stopwords("english")))

StemDocs <- tm_map(tokenized_data, stemDocument)
StemDocs
dtm = DocumentTermMatrix(StemDocs)
dtm
ncol(dtm)
nrow(dtm)

# Check for sparsity
findFreqTerms(dtm, lowfreq=1)
# Remove sparse terms
sparse = removeSparseTerms(dtm, 0.99)
# Convert to a data frame
dataSparse = as.data.frame(as.matrix(sparse), row.names = F)
View(dataSparse)


#Sentiment Analisis
library(NLP)
library(Rstem)
require(quanteda)

positive = read.table("C:\\Neola\\ADS\\Project2\\positive-words.txt", sep="\t")
negative = read.table("C:\\Neola\\ADS\\Project2\\negative-words.txt", sep="\t")

stem <- function(list){
  for(i in c(1:nrow(list))){
    list[i,1] <- wordStem(String(list[i,1]))
  }
  list <- unique(list[,1] )
}


positive <- stem(positive)
negative <- stem(negative)

positive


for(level in c(1:nrow(review))){
  str <- String(review$review[level])
  tokens <- str[wordpunct_tokenizer(str)]
  stem <- wordStem(tokens)
  positiveCount <- length(intersect(stem,positive))
  negativeCount <- length(intersect(stem,negative))
  review$PositiveWordsCount[level] <- positiveCount
  review$NegativeWordsCount[level] <- negativeCount
  if((positiveCount + negativeCount) == 0){
    positiveCount <- 1 
    negativeCount <- 1
  }
  review$Sentiment_Positive[level] <- positiveCount / (positiveCount + negativeCount) 
  review$Sentiment_Negative[level] <- negativeCount / (positiveCount + negativeCount)
  if(positiveCount >= negativeCount){
    review$result[level] <- "Positive"
  } else {
    review$result[level] <- "Negative"
  }
}


write.csv(review, file = "Amazon_Reviews.csv")

review <- read.csv("C:\\Neola\\ADS\\Project2\\Amazon_Reviews.csv")
result1 <- as.numeric(review$result)-1

dataSparse <- cbind(dataSparse,result1)
write.csv(dataSparse,file = "SparseRepresentation.csv")
getwd()


#extra
library(tm)
review_corpus<-Corpus(VectorSource(review[1]))
review_body=Corpus(VectorSource(review))

review_body=tm_map(review_body,content_transformer(tolower))
inspect(review_body)

review = gsub("[[:punct:]]", "", review)
review_corpus = gsub("[[:digit:]]", "", review_corpus)
review_corpus = gsub("http\\w+", "", review_corpus)
review_corpus = gsub("[ \t]{2,}", "", review_corpus)
review_corpus = gsub("^\\s+|\\s+$", "", review_corpus)
review_corpus <- gsub('[[:punct:]]', '', review_corpus)
review_corpus <- gsub('[[:cntrl:]]', '', review_corpus)
review_corpus <- gsub('\\d+', '', review_corpus)

try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

review_corpus = sapply(review_corpus, try.error)

class(review_corpus)
review_corpus = review_corpus[!is.na(review_corpus)]
names(review_corpus) = NULL

termDocumentMatrix1 <- TermDocumentMatrix(review_body, control=list(stemDocument=TRUE))
as.matrix(termDocumentMatrix1)
termDocumentMatrix2 <- TermDocumentMatrix(review_body, control=list(stemming=TRUE))
as.matrix(termDocumentMatrix2)
getwd()




