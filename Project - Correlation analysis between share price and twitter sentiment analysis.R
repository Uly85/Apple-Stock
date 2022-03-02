# CLEAR MEMORY rm(list=ls())  ----
rm(list=ls()) 
RNGversion(vstr = 3.5)

# Project - Correlation analysis between share price and twitter sentiment analysis

# We applied two types of classifiers:
#   
#   -Naive Bayes Bernoulli 
#   -Support Vector Machine
# 
# Load required libraries

library(utils)
library(GGally)
library(devtools)
library(twitteR)
library(tm)
library(wordcloud2)
library(plyr)
library(stringr)
library(data.table)
library(dplyr)
library(ggthemes)
# install.packages('plotly')
library(plotly)
library(plotrix)
library(httr)
library(ROAuth)
library(dplyr)
library(NLP)
library(wordcloud2)
library(base64enc)
library(wordcloud)
library(plotrix)
# install.packages('ggplot2')
library(ggplot2)
library(lattice)
library(devtools)
library(rjson)
library(bit64)
library(mice)
library(ISLR)
library(caret)
library(caTools)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(tm); library(SnowballC); library(magrittr)
library(ggthemes)


# from the twitter package we use the setup_twitter_oauth function You need first to register an app to twitter development, and then create the keys. I hide my keys.

# require(ROAuth)
# require(twitteR)
# 
# setup_twitter_oauth
# 
# mykey = "XXXXXXXXXX"
# mysecret = "XXXXXXXXXX"
# mytoken = "XXXXXXXXXX"
# mysecrettoken = "XXXXXXXXXX"
# 
# # it should be on that order
# setup_twitter_oauth(mykey, mysecret, mytoken, mysecrettoken)

# from the twitter package we use the searchTwitter function to extract
# tweets ..  Tweeter free api allows only the tweets of the last 10 days
# resultType you can also change it to popular. With AND, OR you can add
# more search keywords

company_tweets_id = read.csv('../data twitter/Company_Tweet.csv',stringsAsFactors = F)
str(company_tweets_id)

tweets = read.csv('../data twitter/Tweet.csv',stringsAsFactors = F)
str(tweets)

tweets = merge(company_tweets_id, tweets, by = 'tweet_id')
str(tweets)

# Filter for AAPL
df = tweets %>%
  filter(ticker_symbol == 'AAPL') %>%
  select(c(tweet_id, post_date, body))

df$post_date = as_datetime(df$post_date)

str(df)

# Data Cleaning
corpus = Corpus(VectorSource(df$body))
corpus = 
  corpus%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*', replacement = ' ',x = x)))%>%
  tm_map(content_transformer(function(x)gsub(pattern = '@\\w+',replacement = ' ',x = x)))%>%
  tm_map(content_transformer(function(x)gsub(pattern = '<.*>',replacement = ' ',x = x))) %>%
  tm_map(content_transformer(function(x)iconv(x = x,from = "latin1", to = "ASCII", sub="")))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, stopwords('english'))


dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(df$body))),lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))

corpus =
  corpus %>%
  tm_map(stemDocument)%>%
  tm_map(stripWhitespace)

data = unlist(corpus)
data = as.data.frame(data)
data <- data[1:(nrow(data)-1), ]
data = as.data.frame(data)
cleaned_data = cbind(data, df)
head(cleaned_data)

colnames(cleaned_data)[1] = 'cleaned_tweets'
colnames(cleaned_data)[4] = 'original_tweets'
str(cleaned_data)

# collected Apple price data for one year, from 1 Jan 2018 to 31 December 2018
data = cleaned_data %>%
  filter(post_date >= as.Date('2018-01-01') & post_date <= as.Date('2018-12-31'))

write.csv(data, 'Cleaned_data.csv')



tweets_appl_clean = read.csv('../data twitter/Cleaned_data.csv',stringsAsFactors = F)
str(tweets_appl_clean)


#Just get the data without weekend
library('chron')
# data_no_weekend <- tweets_appl_clean$post_date
# table(is.weekend(dts))
# table (is.holiday(dts))

# collected Apple price data without weekend
data_no_weekend = tweets_appl_clean %>%
  filter(is.weekend(tweets_appl_clean$post_date) == FALSE)

# head(data_no_weekend$post_date, 100)

write.csv(data_no_weekend, 'Cleaned_data_Weekdays.csv')

tweets_appl_clean = read.csv('../data twitter/Cleaned_data_Weekdays.csv',stringsAsFactors = F)

tweets_appl <- distinct(tweets_appl_clean, cleaned_tweets, .keep_all = TRUE)
tweets_appl$cleaned_tweets <- gsub("… ", "", tweets_appl$cleaned_tweets)
# We change the feature created to Date
# tweets_appl <- plyr::rename(tweets_appl, c(created = "Date"))
tweets_appl$post_date <- as.Date(tweets_appl$post_date)

# We transform from datetime to date format
x = function(x) x$getText()
is.recursive(x)
tweets_text <- lapply(tweets_appl, x)

#check here, still error
tweets_text <- lapply(tweets_appl, function(x) x$getText())
# We delete the (retweets)
tweets_unique <- unique(tweets_appl)



# We remove the emoticons
tweets_appl <- sapply(tweets_appl, function(row) iconv(row, "latin1", "ASCII", 
                                                       sub = "byte"))

require(stringr)
functionalText = str_replace_all(tweets_appl, "[^[:graph:]]", " ")
# We create the tweets collection with the appl tweets to use into the
# sentiment analysis
require(tm)

tweets_collections <- Corpus(VectorSource(tweets_unique))
# Distinct the words from the sentence to use them on word cloud plot Ignore
# the warnings into the following scripts

functionalText = str_replace_all(tweets_collections, "[^[:graph:]]", " ")

tweets_collections <- tm_map(tweets_collections, content_transformer(function(x) iconv(x, 
                                                                                       to = "latin1", "ASCII", sub = "")))

# We change all words from capital to lower case
tweets_collections <- tm_map(tweets_collections, content_transformer(tolower))

# We delete all punctuations
tweets_collections <- tm_map(tweets_collections, removePunctuation)

# From tm package we use the below functions to return various kinds of
# stopwords with support for different languages.
tweets_collections <- tm_map(tweets_collections, function(x) removeWords(x, 
                                                                         stopwords()))
tweets_collections <- tm_map(tweets_collections, removeWords, stopwords("english"))
tweets_collections <- tm_map(tweets_collections, removeNumbers)
tweets_collections <- tm_map(tweets_collections, stripWhitespace)

# From tm package we use the below functions to construct or coerce to a
# term-document matrix or a document-term matrix
term_matrix <- TermDocumentMatrix(tweets_collections, control = list(wordLengths = c(1, 
                                                                                     Inf)))

# view the terms
term_matrix

# From tm package we find frequencyuent terms in a document-term or
# term-document matrix.
library(tm)
(frequency.terms <- findFreqTerms(term_matrix, lowfreq = 20))
# Transform it to matrix
term_matrix <- as.matrix(term_matrix)

# We compute the frequency of the distinct words from the tweets and we sort
# it by frequency
distinctWordsfrequency <- sort(rowSums(term_matrix), decreasing = T)

# Transform it to df
distinctWordsDF <- data.frame(names(distinctWordsfrequency), distinctWordsfrequency)
colnames(distinctWordsDF) <- c("word", "frequency")


# `Display the 5 most frequent words
# 
# We plot an Interactive colorfoul word cloud with the most frequent words

require(wordcloud2)
wordcloud2(distinctWordsDF)


# We used the famous lexicon of posive and negative words that was created from : Liu, Bing, Minqing Hu, and Junsheng Cheng. “Opinion observer: analyzing and comparing opinions on the web.” 
# In Proceedings of the 14th international conference on World Wide Web (WWW-2005), pp. 342-351. ACM, May 10-14, 2005. Thanks to Liu and Hu we will add more than 6500 positive phrases, idioms and quotes
# 
# We will add some extra words, that were observed inside the review of tweets We merge the above two positive lexicons
# Binary Sentiment Lexicons
# We will begin by examining lexicons that classify tokens into two categories based on valence, usually as positive or negative.

library('tidytext')
lexicons <-  as.data.frame(get_sentiments('bing'))
pos.words <- lexicons[lexicons$sentiment == 'positive',]$word
neg.words <- lexicons[lexicons$sentiment == 'negative',]$word

# We add the extra words we noticed
pos.words <- c(pos.words, "thanx", "awesome", "fantastic", "super", "prima", 
               "toll", "cool", "geil", "profit", "profits", "earnings", "congrats", "prizes", 
               "prize", "thanks", "thnx", "Grt", "gr8", "plz", "trending", "recovering", 
               "brainstorm", "leader")

neg.words <- c(neg.words, "avoid", "lose", "loses", "scandal", "dieselgate", 
               "sucks", "awful", "disgusting", "negative", "wait", "waiting", "hold", "onhold", 
               "on hold", "cancel", "spam", "spams", "cancel", "wth", "Fight", "fighting", 
               "wtf", "arrest", "no", "not")

# pos.words[pos.words == 'gr8']
# neg.words[neg.words == 'scandal']


# 04.Score Sentiment Function
# 
# We create the score sentiment function in order to run it afterwards in our aapl tweets 
# Transform the vector with the sentences to simple array of scores with plyr package Then we clean the sentences with gsub() function 
# Then we convert the letters from capital to lower case The below function was inspired by (https://medium.com/@rohitnair_94843/analysis-of-twitter-data-using-r-part-3-sentiment-analysis-53d0e5359cb8)

require(plyr)
require(stringr)
score.sentiment = function(sentences, pos.words, neg.words, .progress = "none") {
scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    sentence = gsub("[[:punct:]]", "", sentence)
    sentence = gsub("[[:cntrl:]]", "", sentence)
    sentence = gsub("\\d+", "", sentence)
    
    sentence = tolower(sentence)
    # With stringr package we distinct it into words
    word.list = str_split(sentence, "\\s+")
    # We unlist the vector
    words = unlist(word.list)
    # We compare the words from appl tweets, with the above positive and
    # negative words of the dictionaries
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # The match function returned the position of the matched word otherwise NA
    # Remove NAs
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # We tranform the matches into 1 or 0 with the sum fiunction
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress = .progress)
  scores.df = data.frame(score = scores, text = sentences)
  return(scores.df)
  
}

# We apply the above sentiment function on our tweets

aapl.scores <- score.sentiment(tweets_unique, pos.words, neg.words, .progress = "none")

# 05.Sentiment Classification using Distant Supervision

# We apply the “Sentiment Classification using Distant Supervision” to create the Sentiments 
# this method was first applied by:Huang, Bhayani and Go Based on Huang, Bhayani and Go 
# Twitter “Sentiment Classification using Distant Supervision” research paper for Stanford university published on Dec 18,2017 
# (https://www-cs.stanford.edu/people/alecmgo/papers/TwitterDistantSupervision09.pdf) 
# They presented the results of machine learning algorithms for classifying the sentiment of Twitter messages using distant supervision. 
# Their training data consists of Twitter messages with emoticons, which are used as noisy labels. 
# This type of training data is abundantly available and can be obtained through automated means. 
# We show that machine learning algorithms (Naive Bayes, Maximum Entropy, and SVM) have accuracy above 80% when trained with emoticon data.

# install.packages('sentiment')
require(devtools)
# install_url('http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz')
# install_url('http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz')
#install_url('https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz')
# 
require(sentiment)
ls("package:sentiment")
# > ls("package:sentiment")
# [1] "classify_emotion"  "classify_polarity" "create_matrix"   

# CLASSIFY EMOTIONS
classify_emotion(tweets_unique,algorithm="bayes",verbose=TRUE)

require(sentiment)
sentiments <- sentiment(tweets_unique)
table(sentiments$polarity)
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1

library(lexicon)


require(data.table)
sentiments$date <- as.IDate(tweets_daimler.nodups.df$Date)
result <- aggregate(score ~ date, data = sentiments, mean)

# We create an interactive plot with the  scoring results mean per date

require(ggthemes)
require(ggplot2)
require(plotly)

plotresults <- ggplot(result, aes(x = date, y = score)) + xlab("Date") + ylab("Sentiment Scoring Mean") + 
  ggtitle("Sentiment Scoring Mean by Date") + theme_solarized(light = FALSE) + 
  geom_path(color = "yellow", size = 1) + geom_point(color = "red", size = 3)

ggplotly(plotresults)



# 06.Distant Supervised Learning Classifier

daimler.scores[1] = daimler.scores[1] + sentiments[4]

# display the 6 first scores
head(daimler.scores[1])

# Now we create a new df,which is the combine of all the above results with the original appl tweets df
tweets_aapl$text = tweets_unique
aapl.score.merge <- merge(appl, tweets_aapl, by = "text")

# We plot an interactive histogram of sentiment for all tweets

plothist <- ggplot(aapl.scores, aes(x = aapl.scores$score, fill = "score", 
)) + xlab("Date") + ylab("Sentiment Scoring") + ggtitle("Sentiment of tweets that mention Apple by Date") + 
  theme_solarized(light = FALSE) + geom_histogram()

ggplotly(plothist)


# Plot interactive scatter plot with tweet date vs sentiment score

plotscatter <- ggplot(NULL, aes(tweets_aapl$Date, aapl.scores$score)) + 
  geom_point(data = tweets_aapl.nodups.df) + geom_point(data = aapl.scores, 
                                                           color = "red") + xlab("Date") + ylab("Sentiment Scoring") + ggtitle("Sentiment of tweets that mention Apple by Date") + 
  theme_solarized(light = FALSE)

ggplotly(plotscatter)


# We create the percentages of the 7 different sentiments of our tweets

Perc = aapl.scores$score

# The output will be FALSE or TRUE ,Sentiment good
good <- sapply(Perc, function(Perc) Perc <= 3 && Perc >= 1)

# We convert it to actual value
Perc[good]
list_good = Perc[good]
value_good = length(list_good)

# Sentiment Very good
verygood <- sapply(Perc, function(Perc) Perc > 3 && Perc < 6)
# We convert it to actual value
Perc[verygood]
list_verygood = Perc[verygood]
value_verygood = length(list_verygood)

# Sentiment Outstanding
outstanding <- sapply(Perc, function(Perc) Perc >= 6)
# We convert it to actual value
Perc[outstanding]
list_outstanding = Perc[outstanding]
value_outstanding = length(list_outstanding)

# Sentiment Bad : Unsatisfactory Output of following is FALSE or TRUE
bad <- sapply(Perc, function(Perc) Perc >= -3 && Perc <= -1)
# We convert it to actual value
Perc[bad]
list_bad = Perc[bad]
value_bad = length(list_bad)

# Sentiment Very bad : Poor Output of following is FALSE or TRUE
verybad <- sapply(Perc, function(Perc) Perc < -3 && Perc > -6)
# We convert it to actual value
Perc[verybad]
list_verybad = Perc[verybad]
value_verybad = length(list_verybad)

# Sentiment extremely bad
extremelybad <- sapply(Perc, function(Perc) Perc <= -6)
# We convert it to actual value
Perc[extremelybad]
list_extremelybad = Perc[extremelybad]
value_extremelybad = length(list_extremelybad)

# Sentiment Neutral
neutral <- sapply(Perc, function(Perc) Perc > -1 && Perc < 1)
list_neutral = Perc[neutral]
value_neutral = length(list_neutral)

slices <- c(value_good, value_extremelybad, value_bad, value_verygood, value_verybad, 
            value_neutral, value_outstanding)
lbls <- c("Good", "Extremely Bad", "Bad", "Great", "Poor", "Neutral", "Outstanding")

# Check for 0's
slices
# We see that we have 0 extremely bad tweets so we remove it
slices <- c(value_good, value_bad, value_verygood, value_verybad, value_neutral, 
            value_outstanding)
lbls <- c("Good", "Bad", "Great", "Poor", "Neutral", "Outstanding")
pct <- round(slices/sum(slices1) * 100)  #We add percentage to use it in the pie
lbls <- paste(lbls, pct)  # add percentage to the labels 
lbls <- paste(lbls, "%", sep = "")  # we add the symbol % to labels 

# We create a 3D Pie chart with percentages of positive, negstive, neutral

# We create 3 main sentiment categories : positive - negative - neutral

sentimCategories <- aapl.scores$score

require(plyr)
sentimCategories <- plyr::mutate(aapl.scores, tweet = ifelse(aapl.scores$score > 
                                                                  0, "positive", ifelse(daimler.scores$score < 0, "negative", "neutral")))
# Ignore warnings
require(dplyr)
require(utils)
lapply(paste("package:", names(sessionInfo()$otherPkgs), sep = ""), detach, 
       character.only = TRUE, unload = TRUE)

byTweet <- dplyr::group_by(sentimCategories, tweet, tweets_aapl$Date)

byTweet <- dplyr::summarise(byTweet, number = n())

# We plot the sentiment categories (positive, negative and neutral) per day

require(ggplot2)
require(ggthemes)

ggplot(byTweet, aes(byTweet$`tweets_aapl$Date`, byTweet$number)) + 
  xlab("Date") + ylab("Number Of Tweets") + ggtitle("Daimler Tweets Sentiments per Date") + 
  geom_line(aes(group = tweet, color = tweet), size = 2) + geom_point(aes(group = tweet, 
                                                                          color = tweet), size = 4) + theme(text = element_text(size = 18), axis.text.x = element_text(angle = 90, 
                                                                                                                                                                       vjust = 1)) + theme_solarized(light = FALSE)


# 07.Apple share price calculation


# Now is the part that i will calculate the stock price of Apple
# I downloaded the csv from yahoo finance and used the same 10 days that i collected the tweets Import the daimler stock index from my github

aapl_stock_prices <- read.csv("https://raw.githubusercontent.com/papacosmas/daimler_sentiment_stock_analysis/master/DDAIF.csv", 
                                 header = TRUE)

# We format the date feature in order R know that this is a date feature

aapl_stock_prices$Date <- as.Date(daimler_stock_prices$Date, format = "%Y-%m-%d")

aapl_stock_prices$Date <- as.Date(strptime(aapl_stock_prices$Date, format = "%Y-%m-%d"))

# Now we create a new df with the above sentiment analysis merged df plus
# the stock prices index Be carefull to have the same dates to both
require(dplyr)
tweets_plus_stock <- left_join(aapl.score.merge, aapl_stock_prices, by = "Date")

# create a new df in order to remove the rows that we dont have adjusted
# closing entry. (adj closing is the closing price of the share that
# corresponding date)
weekday_tweets_plus_stock <- tweets_plus_stock
weekday_tweets_plus_stock <- subset(tweets_plus_stock, !is.na(Adj.Close))

# We add new features fields (as 0,1 factor) to mark tweets as positive or
# negative or neutral
weekday_tweets_plus_stock$positive <- as.numeric(weekday_tweets_plus_stock$score > 
                                                   0)

weekday_tweets_plus_stock$negative <- as.numeric(weekday_tweets_plus_stock$score < 
                                                   0)

weekday_tweets_plus_stock$neutral <- as.numeric(weekday_tweets_plus_stock$score == 
                                                  0)

# We create a new df with sums. From one row per tweet - to one row per day.
# Showing the sum positives, negatives and newtral tweets per day
require(plyr)
tweets_plus_stock_df <- ddply(weekday_tweets_plus_stock, c("Date", "High", "Low", 
                                                           "Adj.Close"), plyr::summarise, pos.count = sum(positive), neg.count = sum(negative), 
                              neu.count = sum(neutral))

# We add a new feature with the sum tweets of the 3 sentiment categories
tweets_plus_stock_df$sum.count <- tweets_plus_stock_df$pos.count + tweets_plus_stock_df$neg.count + 
  tweets_plus_stock_df$neu.count

# We calculate the percentage of negative tweets for each day, and add it as
# a new feature
tweets_plus_stock_df$percentage.negatives <- round((tweets_plus_stock_df$neg.count/tweets_plus_stock_df$sum.count) * 
                                                     100)
# Simple correlation

require(GGally)
ggpairs(tweets_plus_stock_df, columns = c(9, 4), ggplot2::aes(color = "red"), 
        title = "Correlation: Closing Stock Price VS Tweets Sentiment", upper = list(continuous = wrap("cor", 
                                                                                                       size = 10)), lower = list(continuous = "smooth"))

# 08.Correlation analysis

# From the scatterplot we can identify a negative relationship between (percentage of negative tweets vs stock price) 
# That means as the percentage of negative tweets increase, the value of the stock decreases.
# 
# Into the Pearson correlation coefficient, we see that there is a medium negative correlation (-0.473). 
# That means as the percentage of negative tweets increase, the value of the stock decreases.
# 
# Into the kernel density plot we can observe strong negative skew with high bandwidth correlation coefficients 
# Density plots for every numeric continuous variable help us to identify skewness, kurtosis and distribution information. 
# Q-Q normal plots can also be useful at diagnosing departures from normality by comparing the data quantiles to those of a standard normal distribution. 
# Substantial deviations from linearity, indicate departures from normality. Quantiles are a regular spacing of points throughout an ordered data set.

library(stats)
cor(tweets_plus_stock_df$percentage.negative, tweets_plus_stock_df$Adj.Close, 
    use = "complete")


# 09.Fitting Generalized Linear Models from stats package
glm_model <- glm(tweets_plus_stock_df$Adj.Close ~ tweets_plus_stock_df$percentage.negative)
summary(glm_model)


# Plot of % negative tweets vs daily change in stock price with linear regression line overlaid
# fit a loss line

require(lattice)
xyplot(tweets_plus_stock_df$Adj.Close ~ tweets_plus_stock_df$percentage.negatives, 
       grid = TRUE, type = c("p", "r"), col.line = "red", lwd = 3, ylab = "Daily Change of Daimler Share Price in $", 
       xlab = "% of Negative Tweets", main = "% of negative Tweets vs Daily
       Daimler Share Price")

# Add new feature by calculating the percentage of positive tweets per day

tweets_plus_stock_df$percentage.positives <- round((tweets_plus_stock_df$pos.count/tweets_plus_stock_df$sum.count) * 
                                                     100)
# Simple correlation

library(stats)
cor(tweets_plus_stock_df$percentage.positives, tweets_plus_stock_df$Adj.Close, 
    use = "complete")


glm_model <- glm(tweets_plus_stock_df$Adj.Close ~ tweets_plus_stock_df$percentage.positives)
summary(glm_model)


# Plot of % positive tweets vs daily change in stock price with linear regression line overlaid

require(lattice)
xyplot(tweets_plus_stock_df$Adj.Close ~ tweets_plus_stock_df$percentage.positives, 
       grid = TRUE, type = c("p", "r"), col.line = "blue", lwd = 3, ylab = "Daily Change of Daimler Share Price in $", 
       xlab = "% of Positive Tweets", main = "% of positive Tweets vs Daily
       Daimler Share Price")

# 10.Conclusion
# 
# The predictions obtained with the aid of sentiments analysis presented significantly good potential for share market forecasting. 
# At least on a short-term basis.
# 
# I would propose to create a model that would
# analyze and combine the results of:
#   
# -Social media sentiment
# -Economic News
# -Market Time Series Analysis


