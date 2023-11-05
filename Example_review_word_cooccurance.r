### HERE WE ARE CREATING CO-OCCURANCE NETWORK

# Install
install.packages("tm")  # for text mining
install.packages("tidytext") # place the text in tidy text format
install.packages("dplyr") # for doing basic data manipulations
install.packages("igraph") # identify and graphing relationships
install.packages("tidyr") # tidy text data
install.packages('ggraph')# create network graph

# Load
library("tm")
library("tidytext")
library("igraph")
library("tidyr")
library("tidyr")
library("ggraph")
library('dplyr')


# Read the text file from local machine, choose file interactively
library(readxl)
text <- read.csv('example data.csv')
# Load the data as a corpus
TextDoc <- Corpus(VectorSource(text$Review.Text))

#####Cleaning up Text Data#######################################################
#Cleaning the text data starts with making transformations like removing special characters from the text.
#This is done using the tm_map() function to replace special characters like /, @ and | with a space. 
#The next step is to remove the unnecessary whitespace and convert the text to lower case.
#Then remove the stopwords. They are the most commonly occurring words in a language and have very little value in terms of gaining useful information. They should be removed before performing further analysis.
#Examples of stopwords in English are "the, is, at, on".
#There is no single universal list of stop words used by all NLP tools. 
#stopwords in the tm_map() function supports several languages like English, French, German, Italian, and Spanish.
#Please note the language names are case sensitive. 
#I will also demonstrate how to add your own list of stopwords, which is useful in this Team Health example for removing non-default stop words like "team", "company", "health". Next, remove numbers and punctuation.
#The last step is text stemming. It is the process of reducing the word to its root form. 
#The stemming process simplifies the word to its common origin.
#For example, the stemming process reduces the words"fishing", "fished" and "fisher" to its stem "fish".
#Please note stemming uses the SnowballC package. 
#(You may want to skip the text stemming step if your users indicate a preference to see the original "unstemmed" words in the word cloud plot)

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
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
stopwords()
# Remove your own stop word
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c( "true", "size", "<NA>", "null", "NA")) #### some example
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)

## convert the corpus into a data frame with the words inside
review_data <- data.frame(text = sapply(TextDoc,as.character),
                          stringsAsFactors = F)
## you have corpus "TextDoc"...now for further processing I need to convert
#it into dataframe format using data.frame. In this the column name is 'text'
#here we use sapply...it is a conversion techique, it will convert a corupus documnet
#called as "TextDoc" as dataframe. Here I don't want to convert this character vector
#as factor.


## creating bi-grams using the dataframe we created earlier
review_trigram <- review_data %>% 
  unnest_tokens(trigram, text, token = 'ngrams',n = 3)

review_trigram

### count the frequency of bigrams
review_trigram %>% 
  count(trigram, sort = T)


### separate the bigrams, so that it can print the relationships

trigram_sep <- review_trigram %>% 
  separate (trigram, c("word1","word2","word3"), sep = " ")

trigram_sep

#### filter the bigrams and further remove the stopwords
trigram_fil <- trigram_sep %>% 
  filter(!word1 %in% stop_words$word)%>% 
  filter(!word2 %in% stop_words$word)
?stop_words

####### not required########################
trigram_fil <- trigram_sep %>% 
  filter(!(word1 %in% stop_words$word & word1 != "not")) %>% 
  filter(!(word2 %in% stop_words$word & word2 != "not")) %>%
  filter(!(word2 %in% stop_words$word & word3 != "not"))

##################################################

### create new bigram count
trigram_count <- trigram_fil%>% 
  count(word1, word2, word3, sort = T)
trigram_count


#### create a bigram graph
trigram_graph <- trigram_count %>% 
  filter(n > 10) %>% 
  graph_from_data_frame() ## it create a dataframe for analysing in bigrram graph
?graph_from_data_frame


### create a graph based on the network or relationships
set.seed (1)
ggraph(trigram_graph, layout = 'fr') +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

