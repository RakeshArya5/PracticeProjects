#Step-by-step Sentiment Analysis on Twitter

#step 1: loading libraries

require(twitteR)
require(RCurl)

#step 2: API connection keys
myconsumerkey <- "ei81JvaJJ03ERmFjnLelQvA8L"
#reqURL <- "https://api.twitter.com/oauth/request_token"
#myaccessURL <- "https://api.twitter.com/oauth/access_token"
#authURL <- "https://api.twitter.com/oauth/authorize"
myconsumersecret <- "eWg7Cn0zfjAWxB4MJSJurNbiJyCbPu3eXvBrJmBUyVbQeaflaa"
accesstoken <- "605572385-hEorbbNFTHB4ySE3Vwh9PAZbSUdJAHlxkiQS7lPC"
accesstokensecret <- "6ojxVnF9NCFUb32KETgnOP6VEejLWP5jofXOQF6aRtTOT"

#step 3: authorization from twitter
setup_twitter_oauth(myconsumerkey,myconsumersecret,accesstoken,accesstokensecret)

#step 4: Search tweets
crackers <- searchTwitter("sangeet som", n=200, lang = "en")
#class(crackers)
#step 5: converting tweets to dataframes
crackers_df <- twListToDF(crackers)
#crackers_df
head(crackers_df$text)

#step 5: Cleaning data
#to remove the emoticons

crackers_df$text <- sapply(crackers_df$text, function(row) iconv(row, "latin1","ASCII",sub="")) #remove emoticons
crackers_df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", crackers_df$text) #remove url
head(crackers_df$text)

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
result = score.sentiment(crackers_df$text,pos.words, neg.words)
result
#write.csv(result, file='C:/Users/Admin/Desktop/Sentiment Analysis/result.csv')
#install.packages("reshape")
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

#Merging three data frames into one
table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)
#write.csv(table_final, file='C:/Users/Admin/Desktop/Sentiment Analysis/table_final.csv')

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
pie(slices, labels = labels, col=rainbow(length(labels)), main="Response to Chetan Bhagat on #Crackers")
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Reaction on #SangeetSom Tweet")

#WordCloud
trump_text = sapply(crackers, function(x) x$getText()) #sapply returns a vector 
df <- do.call("rbind", lapply(crackers, as.data.frame)) #lapply returns a list
trump_text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
str(trump_text) #gives the summary/internal structure of an R object


library(tm) #tm: text mining
trump_corpus <- Corpus(VectorSource(trump_text)) #corpus is a collection of text documents
trump_corpus
inspect(trump_corpus[1])
#clean text
library(wordcloud)
trump_clean <- tm_map(trump_corpus, removePunctuation)
trump_clean <- tm_map(trump_clean, removeWords, stopwords("english"))
trump_clean <- tm_map(trump_clean, removeNumbers)
trump_clean <- tm_map(trump_clean, stripWhitespace)
wordcloud(trump_clean, random.order=F,max.words=80, col=rainbow(50), scale=c(3.5,1))

#trends from any location - this portion of the code is nor working properly
#assuming input = Ottawa
a_trends = availableTrendLocations()
woeid = a_trends[which(a_trends$name=="Ottawa"),3]
#woeid
canada_trend = getTrends(woeid)
trends = canada_trend[1:2]

#To clean data and remove Non English words: 
dat <- cbind(trends$name)
dat2 <- unlist(strsplit(dat, split=", "))
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
dat4 <- dat2[-dat3]
dat4

#top 10 hashtags of any particular user

library(twitteR)
tw = userTimeline("rakesharya5arya", n = 2000)
tw = twListToDF(tw)
vec1 = tw$text

#Extract the hashtags:
hash.pattern = "#[[:alpha:]]+"
have.hash = grep(x = vec1, pattern = hash.pattern) #stores the indices of the tweets which have hashes
hash.matches = gregexpr(pattern = hash.pattern,
                        text = vec1[have.hash])
extracted.hash = regmatches(x = vec1[have.hash], m = hash.matches) #the actual hashtags are stored here
df = data.frame(table(tolower(unlist(extracted.hash)))) #dataframe formed with var1(hashtag), freq of hashtag
colnames(df) = c("tag","freq")
df = df[order(df$freq,decreasing = TRUE),]
dat = head(df,50)
dat2 = transform(dat,tag = reorder(tag,freq)) #reorder it so that highest freq is at the top

library(ggplot2)
p = ggplot(dat2, aes(x = tag, y = freq)) + geom_bar(stat="identity", fill = "blue")
p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of the Obama team (@rakesharya5arya)")


