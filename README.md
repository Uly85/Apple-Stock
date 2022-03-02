# Apple-Stock

## Project Proposal - Apple Stock: The impact of news sentiments on a daily closing price of shares 

Group 5 (Jiajun Du, Nate Lim, Victoriya Murga, Romauli Butarbutar)

### **Executive Summary / Abstract**

Due to the highly volatile nature of stocks which depends on diverse political and economic factors, political factors, investor sentiment, and many other factors, predicting stock market prices is hard and encourages interest among both analysts and researchers for a long time. Recent studies in sentiment analysis have shown that there is a strong correlation between the movement of stock prices and the publication of news articles. Investors’ opinions towards financial markets have been affected by the vast amount of online information in the public domain, such as Wikipedia usage patterns, news stories from the mainstream media, and social media. As such, there is a need to analyze the effects of news sentiments on the stock price. Our project proposal would use non-quantifiable data, from social media news, namely tweets about the Apple stock, to predict its future stock trend with sentiment analysis. We assume that Tweets impact the stock market, we would study the relationship between tweets and stock trends. We will retrieve, extract, and analyze the effects of news sentiments on the Apple stock price in the market, developing a dictionary-based sentiment analysis model, and evaluating the model for gauging the effects of news sentiments on the Apple stock.

#### **Part 1: Research Proposal**

**Statement of the Problem**
Apple Inc. (AAPL) is the world’s largest multinational technology company specializing in electronics and software. AAPL first went public in 1980 through Nasdaq and in March 2015. In August 2018, Apple became the first publicly traded U.S. company valued at over $1 trillion and two years later, the first $2 trillion valued company. The company’s stock price is evident in its success and conveys an immense pool of fascinating data. AAPL’s ticker symbol represents the most-watched stock globally and is based on market capitalization - one of the largest companies in the world.

Predicting the AAPL stock price and its trends are highly volatile. Hence, researchers are constantly studying to capture the volatility and predict its next moves. Investors and market analysts study the market behavior and plan their buy or sell strategies accordingly.

Market sentiments witness different types of patterns with time, namely bullish, bearish and symmetric patterns. While a bullish or a bearish run may be an imperative consequence of an economic or geo-political vital event and therefore may be of short duration, a symmetric pattern is perceived to be the rule of vibrant economic activity, investor sentiment.

There are two main methods for forecasting market trends through sentiment analysis. One is technical analysis, and the other is fundamental analysis. Technical analysis considers past prices and volume to predict the future trend, while fundamental analysis involves analyzing its financial data to predict insights. Our research pursues the fundamental analysis technique to discover the future trend of AAPL stock by considering social media news such as tweets as the primary information and classifying it into positive and negative sentiments. If the tweet sentiment is positive, it is more likely that the stock price will increase, and if the news sentiment is negative, then the stock price may likely decrease. We aim to build a model that predicts tweet polarity, affecting stock trends through supervised machine learning as classification and other text mining techniques to examine news polarity. We have taken the past two years of tweets and stock prices from Apple as data for our analysis.

**Research Question and Hypotheses**

**Research Question**

Before outlining our hypotheses and research process, we want to explain the question we are targeting.

From $42.31 at the end of 2017 to $124.28 currently, which is a dramatic increase of 193.8%. Apple’s stock price is sensitive to investor sentiment indicator changes.
Contingent on the aforementioned reasons, the primary research question that we would like to address is:

**Does a positive news sentiments increase the daily closing price of Apple stock?**

**Hypotheses**

Null Hypothesis With Mathematical Notation
In 2020 the increment of the Apple (NASDAQ: AAPL) stock was a little more than 60% (Forbes, 2020). Of course, Apple’s Services business is a big driver of its value, but it would be interesting to examine whether news sentiments will have a significant effect on the price of its stock.

Therefore, we propose the following null hypothesis:

**News sentiment indicators do not have any incremental effect over the average price of Apple stock.**
