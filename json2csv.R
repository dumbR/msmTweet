library('jsonlite')
library('plyr')
library('reshape2')

data = fromJSON("C:\\Users\\luca.minello\\Downloads\\sampleTweetMSM\\msm_tweet_Oct2015.json")

tweetData <- ldply( data, json2DataFrame)
rm(data)

# dedup by tweet id
tweetData <- tweetData[!duplicated(tweetData$id_str),]

write.csv(tweetData,"msm_tweet_Oct2015.csv",)