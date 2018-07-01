#Sentiment Analysis using Twiter
# El Classico Sentiment Analysis

# Libraries to use
#install.packages("rtweet")
#library(rtweet)

#install.packages("devtools")
library(devtools)
devtools::install_version("httr", version="0.6.0", repos="http://cran.us.r-project.org")

library(twitteR)
library(purrr)
library(dplyr)
require('ROAuth')
require('RCurl')
library(plyr)
library(stringr)
library(base64enc)
#install.packages("httr", dependencies = TRUE)
library(httr)


#importing positive and negative works in the memory
pos.words = scan("C:/Users/Admin/Desktop/Sentiment Analysis/positive_words_for_SA.txt", what='Character', comment.char = ';')

neg.words = scan("C:/Users/Admin/Desktop/Sentiment Analysis/Negative_words_for_SA.txt", what='Character', comment.char = ';')


#Sentiment Analysis
#Function for Sentiment Analysis

##Function start
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences,function(sentence, pos.words, neg.words ){
    sentence <- gsub('[[:punct:]]',"",sentence)
    sentence <- gsub('[[:cntrl:]]',"",sentence)
    sentence <- gsub('\\d+',"",sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
    }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame (score=scores, text=sentences)
  return(scores.df)
}
##Function End  
## Connection to Twitter

myconsumerkey <- "ei81JvaJJ03ERmFjnLelQvA8L"
reqURL <- "https://api.twitter.com/oauth/request_token"
myaccessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
myconsumersecret <- "eWg7Cn0zfjAWxB4MJSJurNbiJyCbPu3eXvBrJmBUyVbQeaflaa"
accesstoken <- "605572385-hEorbbNFTHB4ySE3Vwh9PAZbSUdJAHlxkiQS7lPC"
accesstokensecret <- "6ojxVnF9NCFUb32KETgnOP6VEejLWP5jofXOQF6aRtTOT"

twitCred <- OAuthFactory$new(consumerKey = myconsumerkey, 
                             consumerSecret=myconsumersecret,
                             requestURL=reqURL,
                             accessURL=myaccessURL,
                             authURL=authURL)
twitCred$handshake()

setup_twitter_oauth(myconsumerkey, myconsumersecret, accesstoken, accesstokensecret)
tweet1 <- userTimeline("@iHritik", n=100)
tweet2 <- userTimeline("@narendramodi", n=100)

tweet_df <- tbl_df(map_df(tweet1, as.data.frame))
tweet2_df <- tbl_df(map_df(tweet2, as.data.frame))

bscore <- score.sentiment(tweet_df$text, pos.words, neg.words, .progress = 'text')
rscore <- score.sentiment(tweet2_df$text, pos.words, neg.words, .progress = 'text')
hist(rscore$score)
hist(bscore$score)














