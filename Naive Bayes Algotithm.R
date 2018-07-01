sms_raw <- read.csv("C:/Users/Admin/Desktop/Machine Learning/sms_spam.csv", stringsAsFactors = FALSE)
str(sms_raw)
table(sms_raw$type)
prop.table(table(sms_raw$type))
round(prop.table(table(sms_raw$type))*100, digits = 2)
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

#cleaning data
library(tm)
#Cleaning - 1. creating corpus - collection of documents
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
#inspect
inspect(sms_corpus[1:2])
#to view actual text message
as.character(sms_corpus[[1]])
#to view multiple documents
lapply(sms_corpus[[1:2]], as.character)
#Cleaning - 2. convertng to lowercase
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
as.character(sms_corpus_clean[[1]])
#Cleaning - 3. Remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
#Cleaning - 4. Remove stopwords
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
#Cleaning - 5. Remove puncuations
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
#Cleaning - 6. Stemming
library(SnowballC)
wordStem(c("work", "worked", "working", "works"))
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
#Cleaning - 7. Removing Whitespace
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

#Data Preparation
#Data Preparation- 1. splittng text documents into words - 
#sparse matrix - most cells are zeros
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(tolower = TRUE,
                                                          removeNumbers = TRUE,
                                                          stopwords = TRUE,
                                                          removePunctuation = TRUE,
                                                          stemming = TRUE
))
                                                          
#Data Preparation- 2. Creating training and Test datasets
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type
round(prop.table(table(sms_train_labels))*100, digits = 2)                                                          
round(prop.table(table(sms_test_labels))*100, digits = 2)

#Data Preparation- 3.Visualizing Text data - Creating wordclouds
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

#Data Preparation- 3. Creating indicator features for frequent words
findFreqTerms(sms_dtm_train, 5)
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

convert_counts <- function(x) {
x <- ifelse(x > 0, "Yes", "No")
}
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

#Training a model
install.packages("e1071")
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

#Evaluating Model Performance
sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))

#improving model perpformance
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
dnn = c('predicted', 'actual'))

