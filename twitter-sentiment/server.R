rm(list = ls())
setwd("G:\\r-shiny\\twitter-sentiment")

# For the first time, uncomment the following code to install required packages.
#install.packages('devtools')
#install.packages("MASS")
#install.packages("Rstem")
#install.packages("RTextTools")
#install.packages("MASS")
# install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
#install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
#install.packages("e1071")
#install.packages("RTextTools")
#install.packages("Rgraphviz")
# install.packages("SnowballC")
# install.packages("Rstem")

# library('devtools')
# library(tm) # Framework for text mining.
# library(SnowballC) # Provides wordStem() for stemming.
# library(ggplot2) # Plot word frequencies.
# library(scales) # Include commas in numbers.
# #library(Rgraphviz) # Correlation plots
# library(wordcloud) # routines to create word cloud
# library(rpart)  # cart models
# library(ROCR)   # model performance routines
# library(pROC)   # model performance routines
# library('MASS')
# library('devtools')
# library(sentiment) # sentiments
# library(twitteR) # twitter
# library(plyr) #plyr for text mining 
# library(RColorBrewer) # color plots
# library(e1071) # naive bayes
# library(RTextTools) # accuracy measure
# library(Rstem)
# library(ROAuth) #Roauth for crdential authentication 
# library(shiny) #shiny app

#Loading packages
libs <- c("devtools","tm","SnowballC","ggplot2","scales","rpart","wordcloud","rpart","ROCR","MASS","devtools","sentiment","sentiment","twitteR","plyr","RColorBrewer","e1071","RTextTools","Rstem","ROAuth","shiny")
lapply(libs, require, character.only = T)


# load("credentials1.RData") #loading my credentials which is present in the crdentials.Rdata 
#setup_twitter_oauth(cred)

#includind the source that had score , emotion and polarity functions
source("G:\\r-shiny\\twitter-sentiment\\totalcleanupneeded.R")

#including the  clean up function 
# source("cleanuptweets.R")

source("G:\\r-shiny\\twitter-sentiment\\cleanuptweets.R")
#including the source that has word cloud function 

source("G:\\r-shiny\\twitter-sentiment\\wordcloud.R")

#Reading required positive and negative words file
pos.words <- scan('positive-words.txt', what='character', comment.char=';')
neg.words <- scan('negative-words.txt', what='character', comment.char=';')
pos.words <- c(pos.words, 'upgrade')

#Setting Up twitter connection
consumer.key <- "BCbRsDux4h5jK5VpWk3xTkp2t"
consumer.secret <- "ddzSkOXsA2nY4yDDV9RKzDqGFkBrlBwDxt35hrBNDVAgKJLqcN"
access.token <- "783936660933255169-5ko7hwrzOB9orfPpDgLDg5xVQZRvlKb"
access.token.secret <- "iU1HyDNTqSkZJotBO5enjyQV7stlHOJ4wUpqhkkJpdoq0"
setup_twitter_oauth(consumer_key=consumer.key, consumer_secret=consumer.secret, access_token=access.token, access_secret=access.token.secret)



shinyServer(function(input, output) {
  
  #connect to the api and seacrh and retrive term that is passed from the UI , 
  #1000 tweets are being request , language english 
  
  tweets <- reactive ({ searchTwitter(input$searchTerm,n=input$numTweet, lang="en") }) 
  
  #retriving only the gettext field in the tweets 
  txtTweets.1 <- reactive ({ sapply(tweets(),function(x) x$getText()) })
  
  #clean up is called 
  txtTweets.2 <- reactive ({ cleanTweets (txtTweets.1()) }) 
  txtTweets   <- reactive ({ cleanTweetsAndRemoveNAs (txtTweets.2()) }) 
  
  output$plot_emotion <- renderPlot({
    #txttweets is passed to the emotin function to clasify the words 
    #and their emotions based on it 
    #bayes algorithm is being used here 
    
    emotion <- emotionSentimentAnalysis(txtTweets())
    
    #input txttweets is passed to the polarity function and the polarity of the words are clasified (Positive , Negative or Nuetral)
    
    polarity <- polaritySentimentAnalysis(txtTweets())
    
    #creating a dataframe that holds the text , emotion (joy) , polarity (Positive)
    
    final_result.df <- reactive ({ data.frame(text = txtTweets(), emotion = emotion , polarity = polarity) })
    
    #this below piece of code plots the output based on the choice from the user ,like emotion or polarity 
    
    if (input$plot_opt == "emotion"){
      ggplot(final_result.df()) + 
        geom_bar(aes(x=emotion, y=..count.., fill=emotion)) + 
        ggtitle(paste('Tweet Sentiment Analysis for "', input$searchTerm, '"', sep='')) +       
        xlab("Emotion Class") + ylab("No of Tweets") + 
        scale_fill_brewer(palette="Set1") + 
        theme_bw() + 
        theme(axis.text.y = element_text(colour="black", size=18, face='plain')) + 
        theme(axis.title.y = element_text(colour="black", size=18, face='plain', vjust=2)) +  
        theme(axis.text.x = element_text(colour="black", size=18, face='plain', angle=90, hjust=1)) + 
        theme(axis.title.x = element_text(colour="black", size=18, face='plain')) +  
        theme(plot.title = element_text(colour="black", size=20, face='plain', vjust=2.5)) + 
        theme(legend.text = element_text(colour="black", size=16, face='plain')) + 
        theme(legend.title = element_text(colour="black", size=18, face='plain')) + 
        guides(fill = guide_legend(keywidth = 2, keyheight = 2)) 
      
    } 
    else {
      ggplot(final_result.df(), aes()) + 
        geom_bar(aes(x=polarity, y=..count.., fill=polarity), width=0.6) + 
        ggtitle(paste('Tweet Sentiment Analysis for "', input$searchTerm, '"', sep='')) + 
        xlab("Polarity Class") + ylab("No of Tweets") +    
        scale_fill_brewer(palette="Set1") + 
        theme_bw() + 
        theme(axis.text.y = element_text(colour="black", size=18, face='plain')) + 
        theme(axis.title.y = element_text(colour="black", size=18, face='plain', vjust=2)) +  
        theme(axis.text.x = element_text(colour="black", size=18, face='plain', angle=90, hjust=1)) + 
        theme(axis.title.x = element_text(colour="black", size=18, face='plain')) +  
        theme(plot.title = element_text(colour="black", size=20, face='plain', vjust=2.5)) + 
        theme(legend.text = element_text(colour="black", size=16, face='plain')) + 
        theme(legend.title = element_text(colour="black", size=18, face='plain')) + 
        guides(fill = guide_legend(keywidth = 2, keyheight = 2)) 
      
    }
    
    
  })
  
  
  #plotting the wordcloud for the tweets that was fetched , and finding the most frquently used words associated with the search term 
  output$wordcloudform <- renderPlot({
    emotion <- emotionSentimentAnalysis(txtTweets())
    
    polarity <- polaritySentimentAnalysis(txtTweets())
    
    final_result.df <- reactive ({ data.frame(text = txtTweets(), emotion = emotion , polarity = polarity) })
    
    return(getWordCloud(final_result.df(),txtTweets(),emotion))
    
  })
  
  #  histplot1 <- reactive(function(){
  #  scores.df$text <- as.factor(scores.df$text)
  # sentiment.scores <- score.sentiment(scores.df$text, pos.words, neg.words, .progress=.progress)
  # sentiment.scores
  
  
  # hist.plot <- ggplot() + geom_histogram(data=sentiment.scores, aes(x=score))
  # hist.plot = hist.plot + ylab("Number of Tweets")
  # hist.plot = hist.plot + xlab("Tweets Having Sentiment score")
  # hist.plot = hist.plot + ggtitle("Sentiment Analysis of Tweet Data")
  # 
  # hist.plot + theme_economist() + scale_colour_economist()
  # hplot <- hist.plot + theme_solarized_2(light = FALSE) + scale_color_solarized("blue")
  
  #  png(filename = "scores.png")
  
  # dev.off()
  #  })
  output$histplot <- renderPlot({
    sentiment.scores <- score.sentiment(txtTweets(), pos.words, neg.words)
    hist.plot <- ggplot() + geom_histogram(data=sentiment.scores, aes(x=score))
    hist.plot = hist.plot + ylab("Number of Tweets")
    hist.plot = hist.plot + xlab("Tweets Having Sentiment score")
    hist.plot = hist.plot + ggtitle("Sentiment Analysis of Tweet Data")
    
    #hist.plot + theme_economist() + scale_colour_economist()
    #hplot <- hist.plot + theme_solarized_2(light = FALSE) + scale_color_solarized("blue")
    
    plot(hist.plot)})
})  

