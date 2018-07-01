#Creating word cloud
#http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

# Step 1: Create a text file
#Step 2 : Install and load the required packages
#Loading required packages
install.packages("tm")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

#Step 3: Text Mining
# Read the text file from internet
#filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
#filePath <- file("Mod2017Speech.txt", open="r") #Importing Modi's Speech 2017

filePath <- file("RAGA_UC_Barkley_speech.txt", open="r") #Importing RAGA's Speech 2017 
#mylines <- readline(textfile)

str(filePath)
text <- readLines(filePath)
str(text)

# Load the data as a corpus
docs <- Corpus(VectorSource(text))  
inspect(docs)

#text transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
#the tm_map() function is used to remove unnecessary white space, to convert the text to lower case, to remove common stopwords like 'the', "we".
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

#Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#Generate the Word Cloud

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#words : the words to be plotted
#freq : their frequencies
#min.freq : words with frequency below min.freq will not be plotted
#max.words : maximum number of words to be plotted
#random.order : plot words in random order. If false, they will be plotted in decreasing frequency
#rot.per : proportion words with 90 degree rotation (vertical text)
#colors : color words from least to most frequent. Use, for example, colors ="black" for single color.
