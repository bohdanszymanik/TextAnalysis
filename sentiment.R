# download tweets using twitter search api, combine text and count +ve -ve words to get sentiment
# taken from http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/
library(twitteR)

kb.tweets = searchTwitter('@kiwibanknz', n = 1500)
bnz.tweets = searchTwitter('@bnzbank', n = 1500)
asb.tweets = searchTwitter('@asbbank', n = 1500)

class(kb.tweets)

length(kb.tweets)
length(bnz.tweets)
length(asb.tweets)

library(plyr)
kb.text = laply(kb.tweets, function(t) t$getText() )
bnz.text = laply(bnz.tweets, function(t) t$getText() )
asb.text = laply(asb.tweets, function(t) t$getText() )

head(kb.text)

hu.liu.pos.words = scan('c:\\wd\\positive-words.txt', what='character', comment.char=';')
hu.liu.neg.words = scan('c:\\wd\\negative-words.txt', what='character', comment.char=';')

# add in emoticons
pos.words = c(hu.liu.pos.words, 'smiley')
neg.words = c(hu.liu.neg.words, 'unsmiley')

substitute.emoticons = function(text) {
  smiled <- gsub(':\\)', 'smiley', text)
  return(gsub(':\\(', 'unsmiley', smiled))
}

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')    
{  
  require(plyr)  
  require(stringr)  
  
  # we got a vector of sentences. plyr will handle a list  
  # or a vector as an "l" for us  
  # we want a simple array ("a") of scores back, so we use   
  # "l" + "a" + "ply" = "laply":  
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {  
    
    # clean up sentences with R's regex-driven global substitute, gsub():  
    sentence = gsub('[[:punct:]]', '', sentence)  
    sentence = gsub('[[:cntrl:]]', '', sentence)  
    sentence = gsub('\\d+', '', sentence)  
    
    # and convert to lower case:  
    sentence = tolower(sentence)  
    
    # split into words. str_split is in the stringr package  
    word.list = str_split(sentence, '\\s+')  
    
    # sometimes a list() is one level of hierarchy too much  
    words = unlist(word.list)  
    
    # compare our words to the dictionaries of positive & negative terms  
    pos.matches = match(words, pos.words)  
    neg.matches = match(words, neg.words)  
    
    # match() returns the position of the matched term or NA  
    # we just want a TRUE/FALSE:  
    pos.matches = !is.na(pos.matches)  
    neg.matches = !is.na(neg.matches)  
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():  
    score = sum(pos.matches) - sum(neg.matches)  
    
    return(score)  
  }, pos.words, neg.words, .progress=.progress )  
  
  scores.df = data.frame(score=scores, text=sentences)  
  return(scores.df)  
} 

test = c("you're awesome and I love you :)", "you're awful and I hate you. This is terrible service.", "I'm both impressed and amazed")
result = score.sentiment(substitute.emoticons(test), pos.words, neg.words)

# hmmm, I've had to clean the punctuation characters out - there were some errors from bnz tweets
# they didn't have to do this in the original...
kb.scores = score.sentiment(substitute.emoticons(kb.text), pos.words, neg.words, .progress='text')
bnz.scores = score.sentiment(substitute.emoticons(gsub('[[:punct:]]', '', bnz.text)), pos.words, neg.words, .progress='text')
asb.scores = score.sentiment(substitute.emoticons(gsub('[[:punct:]]', '', asb.text)), pos.words, neg.words, .progress='text')

library(ggplot2)
qplot(kb.scores$score)
qplot(bnz.scores$score)
qplot(asb.scores$score)

# add identifying variable for each bank
kb.scores$bank = 'Kiwibank'
bnz.scores$bank = "BNZ"
asb.scores$bank = 'ASB'

# combine scores and plot stacked
all.scores = rbind( kb.scores, bnz.scores, asb.scores)
ggplot(data = all.scores) +
  geom_bar(mapping = aes(x=score, fill=bank), binwidth=1 ) +
  facet_grid(bank~.) +
  theme_bw() +
  scale_fill_brewer()


# let's explore word associations using tm: http://rdatamining.wordpress.com/2011/11/09/using-text-mining-to-find-out-what-rdatamining-tweets-are-about/
library(tm)

# build corpus
cp <- Corpus(VectorSource(all.scores$text))

# clean
cp <- tm_map(cp, tolower)
cp <- tm_map(cp, removePunctuation)
cp <- tm_map(cp, removeNumbers)

# tm includes stopwords lists for languages but it misses some important common examples
customStopwords <- c(stopwords('english'), "available", "via")

# feel free to view stopwords for some other languages eg dutch, french, italian, german
cp <- tm_map(cp, removeWords, customStopwords)

# stem if necessary but you get some oddities eg mining stems to mine
# requires packages Snowball, RWeka, rJava, RWekajars
# example of what it can do for better or worse: stemDocument(c("mining", "miners"))
#cp <- tm_map(cp, stemDocument)

# build the term document matrix
td <- TermDocumentMatrix(cp, control = list(minWordLength=1))

# we have a LOT of sparse terms... let's get rid of them
tds <- removeSparseTerms(td, 0.99)

# what's getting mentioned most?
findFreqTerms(tds, lowfreq = 20)

# let's uncover some associations
findAssocs(tds, 'awesome', 0.1)

library(wordcloud)
m <- as.matrix(tds)
v <- sort(rowSums(m), decreasing=TRUE)
cloudNames <- names(v)
d <- data.frame(word=cloudNames, freq=v)
wordcloud(d$word, d$freq, min.freq=10)

inspect(tds[1:10,1:10])

# let's examine term adjacency and plot a graph of connectivity: http://rdatamining.wordpress.com/2012/05/17/an-example-of-social-network-analysis-with-r-using-package-igraph/
# transform sparse termDocumentMatrix to an actual matrix, make boolean, then transform to adjacency matrix
tdsm <- as.matrix(tds)
tdsm[tdsm>=1] <- 1
termMatrix <- tdsm %*% t(tdsm)

library(igraph)
g <- graph.adjacency(termMatrix, weighted=TRUE, mode="undirected")

# remove loops
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)

set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)
plot(g, layout=layout.kamada.kawai)
tkplot(g, layout=layout.kamada.kawai)

V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$color <- rgb(.5, .5, 0, egam)
E(g)$width <- egam
plot(g, layout=layout1)
tkplot(g, layout=layout1)
