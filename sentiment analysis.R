library(syuzhet)

##Sentiment Analysis
comments <- iconv(youtube_comment$Comment, to='utf-8')

s <- get_nrc_sentiment(comments)
head(s)
s$neutral <- ifelse(s$negative+s$positive==0,1,0)
head(s)
#check what is in 3rd comment
comment[3]

#Bar plot
barplot(100*colSums(s)/sum(s),
        las=2,
        col=rainbow(10),
        ylab="Percentge",
        main="Sentiment scores for YouTube comments")