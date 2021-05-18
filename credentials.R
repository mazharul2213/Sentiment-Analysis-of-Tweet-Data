#setwd("G:\\Twitter-sentiment-analysis-using-RShiny-master")
rm(list=ls())
library(twitteR)

library(base64enc)
#I am using the direct authentication method here , so incase if there is an error with the oauth please run this again and
#choose "1" for YES in case of direct authentication 
library(streamR)
library(RCurl)
library(rjson)
library(streamR)
library(stringr)
library(ROAuth)
library(httr)
library(httk)
library(httpuv)

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumer_key<-"fbnfZ4t0cS1Y1IW9h7kDQdNcw"
consumer_secret<-"Q39Ogb27Ce3FkismYTdIC5XwlqZdh2sptoEXTUTch7aBWgV084"
access_token<-"279395507-Sv4U3NMqV4eJfZhy8t94kfSs6NeTgzu1zts5URib"
access_secret<-"iEgJXoUx4nvZBrvGMOqK3UU8nz8IA3BwBuNBCFwnWRSCk"

my_oauth<-setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#specifiy the proper path to store the credentials 

save(my_oauth ,file="credentials1.RData")


