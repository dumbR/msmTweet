library('jsonlite')
library('dplyr')
library('reshape2')


#dev

# Start Time       Gap(hours)
# 29/07/2015 09:00	2
# 30/07/2015 00:00	3
# 05/08/2015 19:00	38 (1day 14hours)
# 12/08/2015 03:00	137(5days 17hours)
# 19/08/2015 20:00	13

sepTweet = read.csv("msm_tweet_Sep2015.csv",colClasses="character")
augTweet = read.csv("msm_tweet_Aug2015.csv",colClasses="character")
octTweet = read.csv("msm_tweet_Oct2015.csv",colClasses="character")

msmTweet = merge(augTweet,sepTweet,all=TRUE)
msmTweet = merge(msmTweet,octTweet,all=TRUE)

rm(sepTweet)
rm(augTweet)
rm(octTweet)

# fix date format
tmp = strptime(msmTweet$created_at, "%a %b %e %H:%M:%S %z %Y")
msmTweet$CreatedDate <- strftime(tmp, "%d/%m/%Y %H:00" )

rm(tmp)

# extract relevant tweets and get summary
# Rules
# Inclusion Hashtags keywords
# Exclude some user


tweetBuilder <- filter(msmTweet,
                       (
                         grepl("colin",hashtags) | 
                         grepl("builder",hashtags) | 
                         grepl("epicstrut",hashtags) | 
                         grepl("moneysupermarket",hashtags) |
                         grepl("moneysupermarket",text) |
                         grepl("epicbuilder",text) |
                         grepl("MoneySuper",text)                         
                       ) &
                         !(
                           grepl("straightouttacompton",hashtags)  |
                           user.id_str == 93495836 |
                             user.id_str ==1666083234
                           )                       
                       ) 

listWords = as.data.frame(table(scan(text=tolower(str_replace_all(tweetBuilder$text, "[^[:alnum:]]", " ")),what="character")))



listWords$Var1 = as.character(listWords$Var1)
topWords = listWords[listWords$Freq < 600 & listWords$Freq > 5 & nchar(listWords$Var1)>3,]
colors = c("blue", "red", "orange", "green")

png("out.png",height = 1000, width = 1000)
wordcloud(words=topWords$Var1, freq = topWords$Freq,colors=colors,random.color = FALSE,scale = c(6,2))
dev.off()

plot(topWords$Freq,xlab = topWords$Var1)

topUsers <- group_by(tweetBuilder, user.id_str,user.screen_name) %>% summarise(count = n()) %>% ungroup() %>% arrange(desc(count))


topUsersTweets <- filter(tweetBuilder, user.id_str %in% unlist(topUsers[topUsers$count > 10,1]))

%>% group_by(CreatedDate) %>% summarise(count = n() )


tweetBuilder <- group_by(msmTweet, CreatedDate) %>% 
  summarise(count = n() )


werido <- filter(msmTweet,(
  grepl("colin",hashtags) | 
    grepl("builder",hashtags) | 
    grepl("epicstrut",hashtags) | 
    grepl("moneysupermarket",hashtags) |
    grepl("moneysupermarket",text) |
    grepl("epicbuilder",text)
) &
  !(
    grepl("straightouttacompton",hashtags) |
      user.id_str == 93495836
  ) & CreatedDate == '31/07/2015 14:00')
View(werido)

write.csv(tweetBuilder,"tweetData2.csv")

a = hashFrequency(tweetBuilder$hashtags)
png("out.png")
wordcloud(a$Var1, a$Freq, scale = c(5,1), rot.per=0.2)
dev.off()