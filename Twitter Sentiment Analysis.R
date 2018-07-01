# Twitter Mining
# two packages required twitteR and RCurl
require(twitteR)
require(RCurl)

#Twitter Authorization code
myconsumerkey <- "ei81JvaJJ03ERmFjnLelQvA8L"
reqURL <- "https://api.twitter.com/oauth/request_token"
myaccessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
myconsumersecret <- "eWg7Cn0zfjAWxB4MJSJurNbiJyCbPu3eXvBrJmBUyVbQeaflaa"
accesstoken <- "605572385-hEorbbNFTHB4ySE3Vwh9PAZbSUdJAHlxkiQS7lPC"
accesstokensecret <- "6ojxVnF9NCFUb32KETgnOP6VEejLWP5jofXOQF6aRtTOT"
setup_twitter_oauth(myconsumerkey,myconsumersecret,accesstoken,accesstokensecret)

ds <- searchTwitter("Data Science", n=50, lang = "en")
ds
ds[1:2]

#Creating WordClould 
require(tm)
install.packages('tm')
#above installation is not working
install.packages("http://cran.r-project.org/bin/windows/contrib/3.0/tm_0.5-10.zip",repos=NULL) 
library(tm)
install.packages("wordcloud")
require(wordcloud)

himearth <- searchTwitter('hritik+kangana', lang = 'en', n=500, resultType = "recent")
class(himearth)
#convert above to a character vector because we want to create a corpus
himearth_text <- sapply (himearth, function(x) x$getText())
himearth_text
str(himearth_text)

#now we use tm package to convert tweets to a word corpus
#Corpus is a collection of text documents
#creeate corppus from text of tweets
require(tm)
him_corpus <- Corpus(VectorSource(himearth_text))



