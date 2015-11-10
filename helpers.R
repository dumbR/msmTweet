
json2DataFrame <- function(x){  
  
  cols = c(
    'coordinates',
    'coordinates.coordinates1',
    'coordinates.coordinates2',
    'coordinates.type',
    'created_at',
    'geo',
    'geo.coordinates1',
    'geo.coordinates2',
    'geo.type',
    'id_str',
    'retweet_count',
    'retweeted',
    'text',
    'user.created_at',
    'user.followers_count',
    'user.friends_count',
    'user.geo_enabled',
    'user.id_str',
    'user.lang',
    'user.location',
    'user.name',
    'user.screen_name'
  );
  
  hashtags = c(
    'entities.hashtags.text',
    'entities.hashtags.text1',
    'entities.hashtags.text10',
    'entities.hashtags.text11',
    'entities.hashtags.text12',
    'entities.hashtags.text13',
    'entities.hashtags.text14',
    'entities.hashtags.text15',
    'entities.hashtags.text16',
    'entities.hashtags.text17',
    'entities.hashtags.text2',
    'entities.hashtags.text3',
    'entities.hashtags.text4',
    'entities.hashtags.text5',
    'entities.hashtags.text6',
    'entities.hashtags.text7',
    'entities.hashtags.text8',
    'entities.hashtags.text9')
  
  colsName = c(
    'coordinates',
    'coordinates.coordinates1',
    'coordinates.coordinates2',
    'coordinates.type',
    'created_at',
    'geo',
    'geo.coordinates1',
    'geo.coordinates2',
    'geo.type',
    'id_str',
    'retweet_count',
    'retweeted',
    'text',
    'user.created_at',
    'user.followers_count',
    'user.friends_count',
    'user.geo_enabled',
    'user.id_str',
    'user.lang',
    'user.location',
    'user.name',
    'user.screen_name',
    'hashtags'
  );
  
  b = unlist(x)
  
  y1=b[cols]
  #  names(y1) = cols
  
  y2 = b[hashtags]
  y2 = y2[!is.na(y2)]  
  if(length(y2)==2)
    y2=c("")
  
  setNames( c(y1,tolower(paste(y2,collapse=","))), colsName)
  
}


hashFrequency <- function(tweetHashTags, split_chr = ","){
  
  as.data.frame(table(unlist(strsplit(tweetHashTags, split_chr))))
  
}

paste(scan(url("http://textuploader.com/1k0g/raw"), 
           what="character"), collapse=" ")
