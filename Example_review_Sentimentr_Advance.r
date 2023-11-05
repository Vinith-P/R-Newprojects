library(NLP)
library(tm)
library(RColorBrewer)
library(SnowballC)
library(stringr)
library(syuzhet)

reviews <- read.csv("Example data.csv")

abc <- as.matrix(reviews$Review.Text)
head(abc)
tail((abc))
abc <- str_remove_all(abc,"–")
abc <- str_remove_all(abc,"’")
abc <- str_remove_all(abc,"—")
abc <- str_remove_all(abc,"“")
abc <- str_remove_all(abc,"”")

abc<-removeNumbers(abc)
abc<-removePunctuation(abc)
abc<-tolower(abc)
abc<-removeWords(abc,c("now", "one", "will", "may", "says", "said", 
                       "also", "figure", "etc", "re", "can"))
stopwords<-c("the", "and", stopwords("en"))
abc<-removeWords(abc, stopwords("en"))
abc<-stripWhitespace(abc)
abc<-wordStem(abc)  

review_text<-abc
head(review_text)
tail(review_text)

syuzhet_score <- get_sentiment(review_text, method="syuzhet")
head(syuzhet_score)
summary(syuzhet_score)

bing_score <- get_sentiment(review_text, method="bing")
head(bing_score)
summary(bing_score)


afinn_score <- get_sentiment(review_text, method="afinn")
head(afinn_score)
summary(afinn_score)

nrc_score <- get_sentiment(review_text, method="nrc")
head(nrc_score)
summary(nrc_score)

comb_score <- cbind(syuzhet_score, bing_score, afinn_score, nrc_score)
dimnames(comb_score) <- list(1:nrow(comb_score), c("s1", "s2", "s3", "s4"))
df <- as.data.frame(comb_score)
head(df,20)

min(df$s1)
minLoc1 <- which(df$s1==min(df$s1))
minLoc1
review_text[minLoc1]


max(df$s1)
maxLoc1 <- which(df$s1==max(df$s1))
maxLoc1
review_text[maxLoc1]


syuz_neg <- which(syuzhet_score<=(-5))
txt<-review_text[syuz_neg]
result<-cbind(syuz_neg,txt)
result


syuz_posit <- which(syuzhet_score>=4.5)
txt<-review_text[syuz_posit]
result1<-cbind(syuz_posit,txt)
result1

norm_score <- cbind(
  sign(syuzhet_score), 
  sign(bing_score), 
  sign(afinn_score),
  sign(nrc_score))

dimnames(norm_score)<-list(1:nrow(norm_score), c("x1", "x2", "x3", "x4"))
head(norm_score)

z<-as.data.frame(norm_score)
head(z,20)

round(prop.table(table(z$x1)),2) 


nrc_sentiment <- get_nrc_sentiment(review_text)
head(nrc_sentiment,20)
tail(nrc_sentiment,20)

nrc_average <- apply(nrc_sentiment,2,mean)
nrc_average

sentisum <- colSums(nrc_sentiment)
sentisum

barplot(sentisum[1:10],col=rainbow(10), cex.names=0.8, cex.axis=0.8, 
        las=2, main="Emotions and Sentiments")

Lb <- paste(names(sentisum), ",", sentisum)
pie(sentisum[1:10],col=brewer.pal(8,'Dark2'), labels=Lb, 
    main="Emotions and Sentiment nrc Scores", cex=0.8, cex.main=2)









