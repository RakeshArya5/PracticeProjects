#Modi in Devos Speech Analysis

#Loading required packages
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
#install.packages("readtext")
library("readtext")
#input file
speech_text <- readtext("ModiInDevos.txt") #Importing Modi's Speech at Devos

#speech_text <- readline(speech_raw)
str(speech_text)
speech_docs <- Corpus(VectorSource(speech_text))  
inspect(speech_docs)

#text transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
#the tm_map() function is used to remove unnecessary white space, to convert the text to lower case, to remove common stopwords like 'the', "we".
speech_docs <- tm_map(speech_docs, toSpace, "/")
speech_docs <- tm_map(speech_docs, toSpace, "@")
speech_docs <- tm_map(speech_docs, toSpace, "\\|")

# Convert the text to lower case
speech_docs <- tm_map(speech_docs, content_transformer(tolower))
# Remove numbers
speech_docs <- tm_map(speech_docs, removeNumbers)
# Remove english common stopwords
speech_docs <- tm_map(speech_docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
speech_docs <- tm_map(speech_docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
speech_docs <- tm_map(speech_docs, removePunctuation)
# Eliminate extra white spaces
speech_docs <- tm_map(speech_docs, stripWhitespace)


#Build a term-document matrix
dtm <- TermDocumentMatrix(speech_docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# How twitter reacted on Modi's speech at devos


#step 1: loading libraries

library(twitteR)
library(RCurl)
library(ROAuth)
#step 2: API connection keys
# go to https://apps.twitter.com/app/
# under Key and Access token -> get following information

myconsumerkey <- "ei81JvaJJ03ERmFjnLelQvA8L" #Consumer Key (API Key) from app.twitter.com
reqURL <- "https://api.twitter.com/oauth/request_token"
myaccessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
myconsumersecret <- "eWg7Cn0zfjAWxB4MJSJurNbiJyCbPu3eXvBrJmBUyVbQeaflaa" #Consumer secret Key (API Key) from app.twitter.com
accesstoken <- "605572385-hEorbbNFTHB4ySE3Vwh9PAZbSUdJAHlxkiQS7lPC"
accesstokensecret <- "6ojxVnF9NCFUb32KETgnOP6VEejLWP5jofXOQF6aRtTOT"

#step 3: authorization from twitter

setup_twitter_oauth(myconsumerkey,myconsumersecret,accesstoken,accesstokensecret)

#Search Tweets
speech_tweets_1 <- searchTwitter("#ModiAtDavos", n=2000, lang = "en")
class(speech_tweets_1)

#Converting Tweets to dataframes
speech_tweets_df <- twListToDF(speech_tweets_1)
#write.csv(speech_tweets_df,"sppechDF.csv")


#Cleaning Data

speech_tweets_df$text <- sapply(speech_tweets_df$text, function(row) iconv(row, "latin1","ASCII",sub="")) #remove emoticons
speech_tweets_df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", speech_tweets_df$text) #remove url
tail(speech_tweets_df$text)

#importing positive and negaive words
pos.words = scan("C:/Users/Admin/Desktop/Sentiment Analysis/positive_words_for_SA.txt", what='Character', comment.char = ';')
neg.words = scan("C:/Users/Admin/Desktop/Sentiment Analysis/Negative_words_for_SA.txt", what='Character', comment.char = ';')
pos.words = c(pos.words,'congrats','prize','thanks','thx')
neg.words = c(neg.words,'fight','fighting','wtf')

#Lexical Analysis - takes words from the twwets and analysis the sentiments of the tweets
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)  #removes decimal number
    sentence = gsub('\n','',sentence)    #removes new lines
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)  #changes a list to character vector
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp = sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1 = c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new = lapply(list, `[[`, 1)
  pp1 = lapply(list, `[[`, 2)
  nn1 = lapply(list, `[[`, 3)
  
  scores.df = data.frame(score = score_new, text=sentences)
  positive.df = data.frame(Positive = pp1, text=sentences)
  negative.df = data.frame(Negative = nn1, text=sentences)
  
  list_df = list(scores.df, positive.df, negative.df)
  return(list_df)
}


#Calling function - this fuction cleans the tweet return merged dataframe
result = score.sentiment(speech_tweets_df$text,pos.words, neg.words)
library(reshape)
test1=result[[1]]
test2=result[[2]]
test3=result[[3]]
#Creating three different data frames for Score, Positive and Negative
#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL
#Storing the first row(Containing the sentiment scores) in variable q
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
qq1=melt(q1, ,var='Score')
qq2=melt(q2, ,var='Positive')
qq3=melt(q3, ,var='Negative') 
qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL
#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)

table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)
#percentage
#Positive Percentage
#Renaming

posSc=table_final$Positive
negSc=table_final$Negative

#Adding column
table_final$PosPercent = posSc/ (posSc+negSc)
#Replacing Nan with zero
pp = table_final$PosPercent
pp[is.nan(pp)] <- 0
table_final$PosPercent = pp
#Negative Percentage
#Adding column
table_final$NegPercent = negSc/ (posSc+negSc)
#Replacing Nan with zero
nn = table_final$NegPercent
nn[is.nan(nn)] <- 0
table_final$NegPercent = nn

#graphs
#Histogram
hist(table_final$Positive, col=rainbow(10))
hist(table_final$Negative, col=rainbow(10))
hist(table_final$Score, col=rainbow(10))

#Pie
slices <- c(sum(table_final$Positive), sum(table_final$Negative))
labels <- c("Positive", "Negative")
#install.packages("plotrix")
library(plotrix)
pie(slices, labels = labels, col=rainbow(length(labels)), main="Modi#Devos")
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Reaction on #ModiAtDavos Hashtag")


#WordCloud
speech_text = sapply(speech_tweets_1, function(x) x$getText()) #sapply returns a vector 
df <- do.call("rbind", lapply(speech_tweets_1, as.data.frame)) #lapply returns a list
modi_text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
str(modi_text) #gives the summary/internal structure of an R object

library(tm) #tm: text mining
modi_corpus <- Corpus(VectorSource(modi_text)) #corpus is a collection of text documents
modi_corpus
inspect(modi_corpus[1])
#clean text
library(wordcloud)
modi_clean <- tm_map(modi_corpus, removePunctuation)
modi_clean <- tm_map(modi_clean, removeWords, stopwords("english"))
modi_clean <- tm_map(modi_clean, removeNumbers)
modi_clean <- tm_map(modi_clean, stripWhitespace)
wordcloud(modi_clean, random.order=F,max.words=80, col=rainbow(50), scale=c(3.5,1))


#Rahul Gandhi Tweets
#Searching tweets for more combinations using + operator
#rahul_tweet <- searchTwitter("@OfficeOfRG + #modiatdavos", resultType = "popular", n=50, lang = "en", since = '2018-01-24', until = '2018-01-25')
rahul_tweet <- searchTwitter("@OfficeOfRG + #modiatdavos", n=50, lang = "en", since = '2018-01-24', until = '2018-01-25')

#searching tweets with OR operator
#rahul_tweet <- searchTwitter("@OfficeOfRG OR davos", n=50, lang = "en", since = '2018-01-24', until = '2018-01-25')


rahul_tweet_df <- twListToDF(rahul_tweet)

#rahul_tweet_df$text[[3]]

#write.csv(rahul_tweet_df,"rahul.csv" )
result = score.sentiment(rahul_tweet_df$text,pos.words, neg.words)
class(result)
summary(result$score)


library(reshape)
test1=result[[1]]
test2=result[[2]]
test3=result[[3]]
#Creating three different data frames for Score, Positive and Negative
#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL
#Storing the first row(Containing the sentiment scores) in variable q
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
qq1=melt(q1, ,var='Score')
qq2=melt(q2, ,var='Positive')
qq3=melt(q3, ,var='Negative') 
qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL
#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)

table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)
#percentage
#Positive Percentage
#Renaming

posSc=table_final$Positive
negSc=table_final$Negative

#Adding column
table_final$PosPercent = posSc/ (posSc+negSc)
#Replacing Nan with zero
pp = table_final$PosPercent
pp[is.nan(pp)] <- 0
table_final$PosPercent = pp
#Negative Percentage
#Adding column
table_final$NegPercent = negSc/ (posSc+negSc)
#Replacing Nan with zero
nn = table_final$NegPercent
nn[is.nan(nn)] <- 0
table_final$NegPercent = nn

#graphs
#Histogram
hist(table_final$Positive, col=rainbow(10))
hist(table_final$Negative, col=rainbow(10))
hist(table_final$Score, col=rainbow(10))

#Pie
slices <- c(sum(table_final$Positive), sum(table_final$Negative))
labels <- c("Favors Rahul", "Againt Rahul")
#install.packages("plotrix")
library(plotrix)
pie(slices, labels = labels,  main="Rahul_On_Modi@Davos")
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Rahul_On_Modi@Davos")



#WordCloud
rahul_text = sapply(rahul_tweet, function(x) x$getText()) #sapply returns a vector 
df <- do.call("rbind", lapply(rahul_tweet, as.data.frame)) #lapply returns a list

library(tm) #tm: text mining
rahul_corpus <- Corpus(VectorSource(rahul_text)) #corpus is a collection of text documents


rahul_text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

library(wordcloud)
rahul_clean <- tm_map(rahul_corpus, removePunctuation)
rahul_clean <- tm_map(rahul_clean, removeWords, stopwords("english"))
rahul_clean <- tm_map(rahul_clean, removeNumbers)
rahul_clean <- tm_map(rahul_clean, stripWhitespace)
wordcloud(rahul_clean, random.order=F,max.words=80, col=rainbow(50), scale=c(3.5,1))
