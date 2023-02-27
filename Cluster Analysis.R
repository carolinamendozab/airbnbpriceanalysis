#Read Dataset
london = read.csv(file = 'C:/Users/Carolina Mendoza/Documents/Applied Analytics/2.Fall2021/5205-FRAMEWORKS & METHDS II/Project/London_cleansed.csv', stringsAsFactors = F)
reviews = read.csv(file = 'C:/Users/Carolina Mendoza/Documents/Applied Analytics/2.Fall2021/5205-FRAMEWORKS & METHDS II/Project/reviews.csv', stringsAsFactors = F)

#Libraries
library(dplyr)
library(recommenderlab)
library(tidyverse)
library(tidytext)
library(magrittr)

#Set up memory size
memory.size()
memory.limit(size= 1000000)

#Data evaluation 
glimpse(london)
summary(london)

#SENTIMENT ANALYSIS

#Use listing id to create a new data base for Sentiment Analysis.Listing id will help us to identify which reviews below to which listing
count(reviews,"id")
reviews$id<-as.integer(reviews$id)
table(is.na(reviews$id))

data_reviews=as.data.frame(london[,c("id","review_scores_rating")])
data_reviews$review_scores_rating<-as.numeric(round(data_reviews$review_scores_rating))

data_reviews<-merge(data_reviews,reviews,by.x=c("id"),by.y=c("listing_id"))
remove(reviews)

#Use bing dictionary to obtain sentiment 
data_reviews%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = comments)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()


#left_join(data_reviews,hash_sentiment_senticnet, by = c('word'='x'))

#Count top words used in comments
data_reviews%>%
  unnest_tokens(input = comments, output = word)%>%
    select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)

#Remove Stop words
data_reviews%>%
  unnest_tokens(input = comments, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)

#Sentiment Analysis Lexicon 
library(lexicon)

#left_join(data_reviews,hash_sentiment_senticnet, by = c('word'='x'))

#create database sentiment_analysis
sentiment_analysis<-data_reviews %>% 
  select(id,comments)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=comments)%>%
  inner_join(hash_sentiment_senticnet, by = c('word'='x'))%>%
  summarize(reviewSentiment = mean(y))%>%
  ungroup()

table(is.na(data_reviews$id))

remove(data_reviews)

sentiment_analysis<-merge(london,sentiment_analysis,by.x=c("id"),by.y=c("id"))
london$id<-as.numeric(london$id)

#Obtain statistical data for Lexicon sentiment index analysis
sentiment_analysis %>% 
  group_by(neighbourhood_cleansed)%>%
  summarize(min=min(reviewSentiment),max=max(reviewSentiment),median=median(reviewSentiment),mean=mean(reviewSentiment))

#Review Ratings Statistical data 
sentiment_analysis %>% 
  summarize(min=min(review_scores_rating),max=max(review_scores_rating),median=median(review_scores_rating),mean=mean(review_scores_rating))

sentiment_analysis %>% 
  summarize(min=min(review_scores_accuracy),max=max(review_scores_accuracy),median=median(review_scores_accuracy),mean=mean(review_scores_accuracy))

sentiment_analysis %>% 
  summarize(min=min(review_scores_cleanliness),max=max(review_scores_cleanliness),median=median(review_scores_cleanliness),mean=mean(review_scores_cleanliness))

sentiment_analysis %>% 
  summarize(min=min(review_scores_communication),max=max(review_scores_communication),median=median(review_scores_communication),mean=mean(review_scores_communication))

sentiment_analysis %>% 
  summarize(min=min(review_scores_location),max=max(review_scores_location),median=median(review_scores_location),mean=mean(review_scores_location))

sentiment_analysis %>% 
  summarize(min=min(review_scores_value),max=max(review_scores_value),median=median(review_scores_value),mean=mean(review_scores_value))

sentiment_analysis %>% 
  summarize(min=min(reviewSentiment),max=max(reviewSentiment),median=median(reviewSentiment),mean=mean(reviewSentiment))

#Number of listings by borough
bouroughs_listings<-sentiment_analysis %>% 
  group_by(neighbourhood_cleansed)%>%
  summarize(count=n())%>%
  arrange(desc(count))
write.csv(bouroughs_listings,"C:/Users/Carolina Mendoza/Documents/Applied Analytics/2.Fall2021/5205-FRAMEWORKS & METHDS II/Project/bourough_listings.csv", row.names = FALSE)

#Mean sentiment score by borough
sentiment_location<-sentiment_analysis %>%
  group_by(neighbourhood_cleansed) %>%
  summarise_at(vars(reviewSentiment), funs(mean(., na.rm=TRUE)))

sentiment_location<-sentiment_location[order(sentiment_location$reviewSentiment),]

write.csv(sentiment_location,"C:/Users/Carolina Mendoza/Documents/Applied Analytics/2.Fall2021/5205-FRAMEWORKS & METHDS II/Project/sentiment_location.csv", row.names = FALSE)

#Barchart sentiment by borough
library(ggplot2)
ggplot(sentiment_location,aes(x=reviewSentiment,y=reorder(neighbourhood_cleansed,reviewSentiment)))+
  geom_bar(stat="identity")+xlab("Review Sentiment") + ylab("Neighbourhood")

#Mean sentiment across boroughs
sentiment_location %>% 
  summarize(min=min(reviewSentiment),max=max(reviewSentiment),median=median(reviewSentiment),mean=mean(reviewSentiment))


##CLUSTER ANALYSIS
library(mice)

#Subset reviews ratings

data_cluster=sentiment_analysis[,c(1,26:32)] #review evaluation

#Review and data preparation data_cluster
#summary(data_cluster)
#class(data_cluster$review_scores_rating)

data_cluster$review_scores_rating<-as.numeric(data_cluster$review_scores_rating)
data_cluster$review_scores_rating<-round(data_cluster$review_scores_rating)
data_cluster$review_scores_rating<-as.integer(data_cluster$review_scores_rating)

data_cluster$review_scores_accuracy<-as.numeric(data_cluster$review_scores_accuracy)
data_cluster$review_scores_accuracy<-round(data_cluster$review_scores_accuracy)
data_cluster$review_scores_accuracy<-as.integer(data_cluster$review_scores_accuracy)

data_cluster$review_scores_cleanliness<-as.numeric(data_cluster$review_scores_cleanliness)
data_cluster$review_scores_cleanliness<-round(data_cluster$review_scores_cleanliness)
data_cluster$review_scores_cleanliness<-as.integer(data_cluster$review_scores_cleanliness)

data_cluster$review_scores_checkin<-as.numeric(data_cluster$review_scores_checkin)
data_cluster$review_scores_checkin<-round(data_cluster$review_scores_checkin)
data_cluster$review_scores_checkin<-as.integer(data_cluster$review_scores_checkin)

data_cluster$review_scores_communication<-as.numeric(data_cluster$review_scores_communication)
data_cluster$review_scores_communication<-round(data_cluster$review_scores_communication)
data_cluster$review_scores_communication<-as.integer(data_cluster$review_scores_communication)

data_cluster$review_scores_location<-as.numeric(data_cluster$review_scores_location)
data_cluster$review_scores_location<-round(data_cluster$review_scores_location)
data_cluster$review_scores_location<-as.integer(data_cluster$review_scores_location)

data_cluster$review_scores_value<-as.numeric(data_cluster$review_scores_value)
data_cluster$review_scores_value<-round(data_cluster$review_scores_value)
data_cluster$review_scores_value<-as.integer(data_cluster$review_scores_value)

#Complete data

library(mice)
set.seed(1706)
data_cluster = complete(mice(data_cluster, use.matcher=T))

#table(is.na(data_cluster))
#glimpse(data_cluster)
#summary(data_cluster)
data_cluster<-round(data_cluster)

#Calculate Eucledian Distance 
distance = dist(data_cluster,method = "euclidean")
table(is.na(distance))

#Calculate Clusters
clust = hclust(distance,method = "ward.D2")
plot(clust)
rect.hclust(tree=clust,k = 5,border='tomato')

#Goodness of fit
cor(cophenetic(clust),distances)

#Calculate clusters size
h_segments = cutree(tree = clust,k=5)
table(h_segments)

#Profile Clusters-Binding data to clusters
sentiment_analysis=cbind(sentiment_analysis,h_segments)#aca mirar 

#Profile by location 
prop.table(table(sentiment_analysis$h_segments,sentiment_analysis[,8]),1)

library(ggplot2)
tab = prop.table(table(sentiment_analysis$h_segments,sentiment_analysis[,8]),1)
tab2 = data.frame(round(tab,2))


library(RColorBrewer)

#Heatmap Review rating vs. location

#unique(sentiment_location$neighbourhood_cleansed)

lapply(8,function(x) {
  dat = round(prop.table(table(sentiment_analysis$h_segments,sentiment_analysis[,x]),1),2)*100
  dat = data.frame(dat)
  ggplot(data=dat,aes(x=Var2,y=Var1,fill=Freq))+
    geom_tile()+
    geom_text(aes(label=Freq),size=2)+
    xlab(label = '')+
    ylab(label = '')+
    scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))+
    theme(axis.text.x = element_text(size = 5,angle = 90))
})



#Profile by property type
prop.table(table(sentiment_analysis$h_segments,sentiment_analysis[,9]),1)

library(ggplot2)
tab_property_type = prop.table(table(sentiment_analysis$h_segments,sentiment_analysis[,9]),1)
tab2_property_type = data.frame(round(tab_property_type,2))

#Table property type %
table(sentiment_analysis$property_type)
sentiment_analysis$property_type<-as.factor(sentiment_analysis$property_type)


property_type<-sentiment_analysis %>% 
  group_by(property_type) %>% 
  summarize(count=n())%>%
  arrange(desc(count))

write.csv(property_type,"C:/Users/Carolina Mendoza/Documents/Applied Analytics/2.Fall2021/5205-FRAMEWORKS & METHDS II/Project/property_type.csv", row.names = FALSE)

#Heatmap Review rating vs. apartment type 
library(ggplot2)
lapply(9,function(x) {
  dat = round(prop.table(table(sentiment_analysis$h_segments,sentiment_analysis[,x]),1),2)*100
  dat = data.frame(dat)
  ggplot(data=dat,aes(x=Var2,y=Var1,fill=Freq))+
    geom_tile()+
    geom_text(aes(label=Freq),size=2)+
    xlab(label = '')+
    ylab(label = '')+
    scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))+
    theme(axis.text.x = element_text(size = 5,angle = 90))
})

#Table room type
room_type=sentiment_analysis %>% 
  group_by(room_type) %>% 
  summarize(count=n())%>%
  arrange(desc(count))

write.csv(room_type,"C:/Users/Carolina Mendoza/Documents/Applied Analytics/2.Fall2021/5205-FRAMEWORKS & METHDS II/Project/room_type.csv", row.names = FALSE)

#Heatmap Review rating vs. room type 
library(ggplot2)
lapply(10,function(x) {
  dat = round(prop.table(table(sentiment_analysis$h_segments,sentiment_analysis[,x]),1),2)*100
  dat = data.frame(dat)
  ggplot(data=dat,aes(x=Var2,y=Var1,fill=Freq))+
    geom_tile()+
    geom_text(aes(label=Freq),size=2)+
    xlab(label = '')+
    ylab(label = '')+
    scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))
})

#Price Range 
summary(sentiment_analysis$price)

quantile(sentiment_analysis$price)


sentiment_analysis$pricerange = cut(sentiment_analysis$price,c(0,45,76,120,7175))
summary(sentiment_analysis$pricerange)
levels(sentiment_analysis$pricerange) = c("0-45","46-76","77-120",">120")

#Heatmap Review rating vs. price range
library(ggplot2)
lapply(39,function(x) {
  dat = round(prop.table(table(sentiment_analysis$h_segments,sentiment_analysis[,x]),1),2)*100
  dat = data.frame(dat)
  ggplot(data=dat,aes(x=Var2,y=Var1,fill=Freq))+
    geom_tile()+
    geom_text(aes(label=Freq),size=2)+
    xlab(label = '')+
    ylab(label = '')+
    scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))
})



