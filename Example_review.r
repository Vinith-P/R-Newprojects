# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud2") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("syuzhet") # for sentiment analysis (I will print the graph)
install.packages('sentimentr') # sentiment analysis (I print sentiment with respect to every sentence)
install.packages("ggplot2") # for plotting graphs

# Load
library("tm")
library("SnowballC")
library("wordcloud2")
library("RColorBrewer")
library("syuzhet")
library(sentimentr)
library("ggplot2")
library('dplyr')


# Read the text file from local machine, choose file interactively
library(readxl)
text <- read.csv('Example data.csv')
class(text)
# Load the data as a corpus
TextDoc <- Corpus(VectorSource(text$Review.Text))
class(TextDoc)

stopwords()
default_stopwords <- stopwords()# it will contain all 174 stopwords
words_to_exclude <- c("not", "doesn't")
# Create a custom stopwords list by removing words to exclude
custom_stopwords <- setdiff(default_stopwords, words_to_exclude)
custom_stopwords
# Update the TextDoc by setting custom stopwords
TextDoc <- tm_map(TextDoc, removeWords, custom_stopwords)
#####################################################################################

#Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
TextDoc <- tm_map(TextDoc, toSpace, "!")

# Convert the text to lower case
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
stopwords()
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
stopwords()
# Remove your own stop word
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("service", "dogs", "food", "dress", "like", "fabric", "wear")) #### some example
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)


##########Building the term document matrix#######
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)

dtm_m <- as.matrix(TextDoc_dtm)
class(dtm_m)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(words = names(dtm_v),freq = dtm_v)
# Display the top 10 most frequent words
head(dtm_d, 20)# helps in printing the top 20 words


library(dplyr)


##################################################################################################################
#Plotting the top 5 most frequent words using a bar chart is a good basic way to visualize this word frequent data. 
#In your R script, add the following code and run it to generate a bar chart, which will display in the Plots sections of RStudio.
#################################################################################################################
# Plot the most frequent words
?barplot ## will give you help 
barplot(dtm_d[1:20,]$freq, las = 2, names.arg = dtm_d[1:20,]$words,
        col ="green", main ="Top 20 most frequent words",
        ylab = "WORD FREQUENCIES")### las shows legacy axis = 2 vertical
###################### INTERPRETATION
#The most frequently occurring word is "LIKE". 
#Also notice that negative words  feature in the bar chart, which indicates there are no negative prefixes to change the context or meaning of the word "LIKE" 
#(In short, this indicates most responses don't mention negative phrases like "hate").
#"TASTE", "FLAVOUR" and "GOOD" are the next three most frequently occurring words, which indicate that most people feel good about their TASTE and THE FLAVOUR.
#Finally, the word "product"  is also on the chart, and you need further analysis to infer that the product is good, tasty and having good flavour.
###########################GENDRATE THE WORD CLOUD
#generate word cloudlibrary(wordcloud2)
?wordcloud2
wordcloud2(dtm_d,
           size = 0.5,
           shape = 'rectangle',
           rotateRatio = 0.99,
           minSize = 1)
