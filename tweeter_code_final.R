#Way to access Twitter 
library(twitteR)
consumer_key = "WuPz2tajntfSbqd******YKN"
consumer_secret = "mVn9kfN**********mgtvb1qfHEqTB0PaZ87d0KaAvGeEW"
access_token="2892448993-y4BaX4***********vvpZ8uk6XzshzCRfwU7CXhZ"
access_secret="jYHUoCZtssVKw344v24****VebQT7QTIXD52j9"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
setup_twitter_oauth

#################################
trump <- searchTwitter('realdonaldtrump', n = 500, since = '2016-01-01',retryonratelimit = TRUE)
trump_df = twListToDF(trump)
View(trump)
trump_fan<-table(trump_df$screenName)
trump_fan_sort <- sort(trump_fan, decreasing = TRUE)
View(trump_fan)
head(trump_fan_sort, 10)
hist(trump_fan_sort)
#####################################
#Import required libraries
install.packages("devtools")
devtools::install_github("mkearney/rtweet")
library(rtweet)
library(twitteR)
library(ggplot2)
library(dplyr)
library(tidytext)
##############################################

#set up Authentication & access data
auth_setup_default()

################################################
#Search tweet for a user
tw <- search_tweets("donaldtrump")
df <- apply(tw,2,as.character) 
##to resolve Error : unimplemented type 'list' in 'EncodeElement'##
class(tw)
class(df)
write.csv(df, file = "C:/Users/MESSI/Documents/tw.csv", fileEncoding = 'UTF-8')
View(tw)
##################################################
#Live Tweet data
tweets30s <- stream_tweets("") ##default value is 30s
tweets120s <- stream_tweets("", timeout = 120) ##timeout value can be chaged as per requirement
dim(tweets30s)
class(tweets30s)
tweets30secs = as.character(tweets30s)
tweets30secs
df1 <- apply(tweets30s,1,as.character)
dimnames(tweets30s)
write.csv(tweets30s, file = "C:/Users/MESSI/Documents/live.csv", fileEncoding = 'UTF-8')
######################################################

#Extract tweet posted on the user
get_cris <- get_timeline("@Cristiano", n = 3200)

# View output for the first 5 columns and 10 rows
head(get_cris[,1:5], 10)
View(get_cris)
#######################################################

#extract number of followers for using screen name 

interest_soccer<-lookup_users(c("ChelseaFC","Arsenal","LCFC","CPFC","SouthamptonFC","Everton","AVFCOfficial","Wolves","SpursOfficial","ManCity","ManUtd","FCBarcelona","realmadrid","FCBayern","BVB"),token = NULL,parse = FALSE)
View(interest_soccer)
class(interest)
interest_symbi<-as.data.frame(interest_soccer)

#Fetching screen name & followers count from the main dataset
df<-interest_symbi[,c("screen_name","followers_count")]

#Potting the extracted data
barplot(height=df$followers_count/1000000,
        col=rainbow(15),
        names.arg=df$screen_name, 
        las=2, 
        ylab = "Followers in millions", 
        main= "Football Clubs and their followers" )+
  mtext("Football Clubs", side=1, line=8)
##########################################################
#Searching tweet based on #rstat
users_data1 <- search_users("#rstats",
                            n = 500)
View(tail(users_data1))

#ploting horizontal bar graph using unique locations from where user tweeted.

users_data1 %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = location, y = n)) +
  
  geom_col() +
  coord_flip() +
  labs(x = "Number of users",
       y = "Country",
       title = "Twitter users count from unique locations ")
################################################################
