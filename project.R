library(Rfacebook)
library(httpuv)

install_github("pablobarbera/Rfacebook/Rfacebook")

fb_oauth <- fbOAuth(app_id="Select_UR_Author_id", app_secret="upload_UR_API_KEY",extended_permissions = TRUE)
library("stringr")
library("reshape2")
library(devtools)
getpagedata<-getPage("JimmyAnderson",token=fb_oauth,n=20)
getpagedata[which.max(getpagedata$likes_count), ]
format.facebook.date <- function(datestring) {
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}
aggregate.metric <- function(metric) {
  m <- aggregate(getpagedata[[paste0(metric, "_count")]], list(month = getpagedata$month), 
                 mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}

getpagedata$datetime <- format.facebook.date(getpagedata$created_time)
getpagedata$month <- format(getpagedata$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)
library(ggplot2)
library(scales)
ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric)) + 
  scale_x_date(date_breaks = "years", labels = date_format("%Y")) + scale_y_log10("Average count per post", 
                                                                                  breaks = c(10, 100,1000,10000,50000)) + theme_bw() + theme(axis.title.x = element_blank())
#word cloud

library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

data<-data.frame(getpagedata)
data
some_txt<-toString(data$message)
some_txt
clean.text <- function(some_txt)
{
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

clean_text = clean.text(some_txt)
tweet_corpus = Corpus(VectorSource(clean_text))

tdm = TermDocumentMatrix(tweet_corpus, control = list(removePunctuation = TRUE,stopwords = stopwords("english"), removeNumbers = TRUE, tolower = TRUE))



library(wordcloud)
m = as.matrix(tdm) #we define tdm as matrix
word_freqs = sort(rowSums(m), decreasing=TRUE) #now we get the word orders in decreasing order
dm = data.frame(word=names(word_freqs), freq=word_freqs) #we create our data set
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2")) #and we visualize our data

#sentiment analysis 

library(sentiment)
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]

emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(some_txt, algorithm="bayes")
polarity = class_pol[,4]
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) + scale_fill_brewer(palette="Dark2")

ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) + scale_fill_brewer(palette="RdGy")

emos = levels(factor(sent_df$emotion))

nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = some_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 random.order = FALSE, title.size = 1.5)


