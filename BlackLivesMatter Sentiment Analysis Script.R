# Twitter Sentiment Analysis for Black Lives Matter Movement, Relation to Presidential Election

library(twitteR)
library(ROAuth)
library(ggplot2)
library (plyr)
library (stringr)

# Call Twitter API

api_key <- "wj6rgMYJeWvYDXJbOMfgYQWgb"
api_secret <- "qnst0OiEdSQOqdzgdICA0raRhCzutSyzuAqC2DvkBGWLEhtStL"
access_token <- "472551758-hHOvpATyf35ANynUxYCuCHsGxBUqYZ535ZXXiyzZ"
access_token_secret <- "RjeG221t5zIDSxaS848k8x8dAaWtGkIjiRirWmzLDQCDZ"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# Input tweets for Black Lives Matter, All Lives Matter, Clinton, Trump

BLM.list <- searchTwitter('Black Lives Matter', n=50000)  
BLMNoRetweets.list <- strip_retweets(BLM.list)
BLM.df = twListToDF(BLMNoRetweets.list) 

ALM.list <- searchTwitter('All Lives Matter', n=50000)  
ALMNoRetweets.list <- strip_retweets(ALM.list)
ALM.df = twListToDF(ALMNoRetweets.list)    

head(ALM.list)
Clinton.list <- searchTwitter('Hillary Clinton', n=50000)  
ClintonNoRetweets.list <- strip_retweets(Clinton.list)
Clinton.df = twListToDF(ClintonNoRetweets.list)  

Trump.list <- searchTwitter('Donald Trump', n=50000)  
TrumpNoRetweets.list <- strip_retweets(Trump.list)
Trump.df = twListToDF(TrumpNoRetweets.list) 


score.sentiment = function(sentences, pos.words, neg.words,.progress='none')  
{  
  require(plyr)  
  require(stringr)       
  
  # with a vector of sentences, plyr will handle a list or a vector as an "l"
  # get a simple array ("a") of scores back using "l" + "a" + "ply" = "laply":  
  
  good.smiley <- c(":)")
  bad.smiley <- c(":(") 
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {  
    
    # clean up sentences with R's regex-driven global substitute, gsub():  
    
    sentence = gsub(":)", 'support', sentence)
    sentence = gsub('[[:punct:]]', '', sentence)  
    sentence = gsub('[[:cntrl:]]', '', sentence)  
    sentence = gsub('\\d+', '', sentence)  
    
    # convert to lower case:  
    
    sentence = tolower(sentence)  
    
    # split into words
    
    word.list = str_split(sentence, '\\s+')  
    
    words = unlist(word.list)  
    
    # compare words to the dictionaries of positive & negative terms  
    
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)  
     
    # just want a TRUE/FALSE response:  
    
    pos.matches = !is.na(pos.matches)  
    neg.matches = !is.na(neg.matches)  
    
    score = sum(pos.matches) - sum(neg.matches)  
    
    return(score)  
    
  }, pos.words, neg.words, .progress=.progress )  
  scores.df = data.frame(score=scores, text=sentences)  
  return(scores.df)  
} 



#Load sentiment word lists
hu.liu.pos = scan('/Users/munirpathak/Documents/positive-words.rtf', what='character', comment.char=';')
hu.liu.neg = scan('/Users/munirpathak/Documents/negative-words.rtf', what='character', comment.char=';')

#Add words to list
pos.words = c(hu.liu.pos, 'support', 'stand', 'love')
neg.words = c(hu.liu.neg, 'racist', 'racism', 'against', 'crooked', 'hate')

#convert text to factor
BLM.df$text<-as.factor(BLM.df$text)
ALM.df$text<-as.factor(ALM.df$text)
Clinton.df$text<-as.factor(Clinton.df$text)
Trump.df$text<-as.factor(Trump.df$text)

#calculate all the scores
BLM.scores = score.sentiment(BLM.df$text, pos.words,neg.words, .progress='text')
ALM.scores = score.sentiment(ALM.df$text, pos.words,neg.words, .progress='text')
Clinton.scores = score.sentiment(Clinton.df$text,pos.words,neg.words, .progress='text')
Trump.scores = score.sentiment(Trump.df$text,pos.words,neg.words, .progress='text')

BLM.scores$Label = 'BlackLivesMatter'
ALM.scores$Label = 'AllLivesMatter'
Clinton.scores$Label = 'Clinton'
Trump.scores$Label = 'Trump'

#Check the negative tweets. What made them negative
BLM.scores.2 = subset(BLM.scores,BLM.scores$score < 0)
head(BLM.scores.2)


# Final outputs
hist(BLM.scores$score)
hist(ALM.scores$score)
hist(Clinton.scores$score)
hist(Trump.scores$score)
table(BLM.scores$score)
table(ALM.scores$score)
table(Clinton.scores$score)
table(Trump.scores$score)

all.scores = rbind(BLM.scores, ALM.scores, Clinton.scores, Trump.scores)
head(all.scores)

movement.scores = rbind(BLM.scores, ALM.scores)
head(movement.scores)

candidate.scores = rbind(Clinton.scores, Trump.scores)
head(candidate.scores)

table(all.scores$score,all.scores$Label)
table(movement.scores$score,movement.scores$Label)
table(candidate.scores$score,candidate.scores$Label)

ggplot(data=movement.scores) + 
  geom_bar(mapping=aes(x=score, fill=Label), binwidth=1) +
  facet_grid(Label~.) + # make a separate plot for each label
  theme_bw() + scale_fill_brewer()
  
ggplot(data=candidate.scores) + 
  geom_bar(mapping=aes(x=score, fill=Label), binwidth=1) +
  facet_grid(Label~.) + # make a separate plot for each label
  theme_bw() + scale_fill_brewer()

ggplot(data=all.scores) + 
  geom_bar(mapping=aes(x=score, fill=Label), binwidth=1) +
  facet_grid(Label~.) + # make a separate plot for each label
  theme_bw() + scale_fill_brewer()
