#--------------------------
# Archel Aguilar - 98081767
# Assignment 1A - Analsyis of unstructured data
#
# OBJECTIVE
# Provide insights into the contents and themes of the documents in the directory.
#
#
# PROCESS
# 1. Load packages
# 2. Create corpus - collection of docs
# 3. pre-processing (Stemming) - remove punctuation, stop words, whitespace and digits
# 4. Create DTM - Document term matrix 
# 5. Compute frequencies & associations - Computed directly from DTM

#---------------------------

#clear environment variables
rm(list=ls())

#1. LOAD PACKAGES
setwd("C:/Users/arche/Documents/UTS/36106 Data Algorithms and Meaning/Assignment 1")
getwd()


install.packages("tm") #text mining package
library(tm)


#-------------------------------------
#2. CREATE CORPUS
#-------------------------------------
docs = Corpus(DirSource("./docs"))
docs #get summary


#inspect a sample set of documents
writeLines(as.character(docs[c(1,5,10,15,30,34)]))


#-------------------------------------
#3. PRE-PROCESSING
#-------------------------------------
getTransformations() #view options
#[1] "removeNumbers"     "removePunctuation" "removeWords"       "stemDocument"      "stripWhitespace"  

#Before running these transformation tools you need to deal with things like "string:string" turning into "stringstring"
#e.g replace all instances of a character by space using gsub()
#create the toSpace content transformer

toSpace = content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})


#tm_map() to apply transformation to corpus
docs = tm_map(docs, toSpace, "-")
docs = tm_map(docs, toSpace, " -")
docs = tm_map(docs, toSpace, "- ")
docs = tm_map(docs, toSpace, ":")
docs = tm_map(docs, toSpace, "([\\])")
docs = tm_map(docs, toSpace, "/")
docs = tm_map(docs, toSpace, "\\(")
docs = tm_map(docs, toSpace, "\\)")
docs = tm_map(docs, toSpace, "%")
#check encoding 
Encoding(as.character(docs[1])) #UTF-8

#clean special characters
#http://www.i18nqa.com/debug/utf8-debug.html

install.packages("corpus")
library(corpus)
library(utf8)

docs = tm_map(docs, toSpace, as_utf8("â€“"))
docs = tm_map(docs, toSpace, as_utf8("â€"))
docs = tm_map(docs, toSpace, as_utf8("â€˜"))
docs = tm_map(docs, toSpace, as_utf8("â€š"))
docs = tm_map(docs, toSpace, as_utf8("â€ž"))
docs = tm_map(docs, toSpace, as_utf8("â€¦"))
docs = tm_map(docs, toSpace, as_utf8("â€™"))
docs = tm_map(docs, toSpace, as_utf8("â€œ"))
docs = tm_map(docs, toSpace, as_utf8("â€"))
docs = tm_map(docs, toSpace, as_utf8("Â¦"))


#view doc
vd = function (pg) { 
  showdoc = docs[pg]  
  fix(showdoc)
}

#remove punctuation marks
docs = tm_map(docs, removePunctuation)

#convert corpus to lowercase (since R is case-sensitive) - leave capitalised due to IT terms
#docs = tm_map(docs, content_transformer(tolower))

#remove digits 
docs = tm_map(docs, removeNumbers)

#remove common words  from the text
docs = tm_map(docs, removeWords, stopwords("english"))

#Strip whitespace
docs = tm_map(docs, stripWhitespace)
docsraw = docs

#-------------------------------------
#3. STEMMING
#-------------------------------------
install.packages("SnowballC") #for stemming
library(SnowballC)

#Typically a large corpus will contain  many words that have a common root - 
docs = tm_map(docs, stemDocument)


#standardize spelling
docs = tm_map(docs, content_transformer(gsub), pattern = "organiz", replacement = "organ")
docs = tm_map(docs, content_transformer(gsub), pattern = "organis", replacement = "organ")
docs = tm_map(docs, content_transformer(gsub), pattern = "andgovern", replacement = "govern")
docs = tm_map(docs, content_transformer(gsub), pattern = "inenterpris", replacement = "enterpris")
docs = tm_map(docs, content_transformer(gsub), pattern = "team-", replacement = "team")

#-------------------------------------
#4. DTM (Document Term Matrix)
#-------------------------------------
# lists all the occurance of words (column) in the corpus by document (row)
dtm = DocumentTermMatrix(docs)
#<<DocumentTermMatrix (documents: 34, terms: 3968)>>
#Non-/sparse entries: 16518/118394
#Sparsity           : 88%
#Maximal term length: 52
#Weighting          : term frequency (tf)


#to inspect the data 
dtmx = as.matrix(dtm)
write.csv(dtmx, "dtm.csv")

#check words sorted alphabetically
dtmx = dtmx[,sort(colnames(dtmx))]
dterms = colnames(dtmx)
#head(dterms, 500)
dterms[1:500]
write.csv(dterms, "terms.csv")

#check frequency
freq = colSums(dtmx)
length(freq)

#create sort order (descending)
ord = order(freq, decreasing=TRUE)

#inspect the frequently occuring top 20
freq[head(ord, 20)]
#the  project     risk    manag      can    organ      use      one document     work    point     this      map   exampl    figur      doc  problem 
#530      521      504      499      398      380      326      297      245      233      230      214      186      176      175      170      168 
#knowledg  process  discuss 
#165      165      164 

#inspect the least frequently occuring
freq[tail(ord, 20)]
#earnest     empathi    empathis      exceed   inclusion      indulg     intutit        jump      jurgen       plate       probe     request       sixth 
#1           1           1           1           1           1           1           1           1           1           1           1           1 
#strive     suffici    timefram       uncov unproblemat       utter       wener 
#1           1           1           1           1           1           1 


# to remove useless 3 letter words and lower frequency words 
# This keeps words with 4 to 20 characters long and occur in 3 to 30 documents
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 23), bounds = list(global = c(3,30))))
#<<DocumentTermMatrix (documents: 34, terms: 1460)>>
#  Non-/sparse entries: 12165/37475
#Sparsity           : 75%
#Maximal term length: 21
#Weighting          : term frequency (tf)


#to inspect the data 
dtmrx = as.matrix(dtmr)
write.csv(dtmrx, "dtmrx.csv")

#check words
dtmrx = dtmrx[,sort(colnames(dtmrx))]
dtermsr = colnames(dtmrx)
#head(dterms, 500)
#dtermsr[1:500]
#dtermsr[500:1000]
write.csv(dtermsr, "termsr.csv")

#check frequency
freqr = colSums(as.matrix(dtmr))
length(freqr)

#create sort order (descending)
ordr = order(freqr, decreasing=TRUE)

#inspect the frequently occuring top 20
freqr[head(ordr, 20)]
# words makes sense - project risk management

#project     risk    manag    organ document     work    figur  problem knowledg  process  practic     will     ibis     issu     word question   system 
#521      504      499      380      245      233      175      168      166      165      160      145      141      139      138      135      135 
#term    model     time 
#133      129      115


# ------------ PLOT THE RESULTS
#The first line creates a data frame - a list of columns of equal length. A data frame also contains the name of the 
#columns - in this case these are term and occurrence respectively.  
#We then invoke ggplot(), telling it to consider plot only those terms that occur more than 100 times.  
#The aes option in ggplot describes plot aesthetics - in this case, we use it to specify the x and y axis labels. 
#The stat="identity" option in geom_bar () ensures  that the height of each bar is proportional to the data value 
#that is mapped to the y-axis  (i.e occurrences). The last line specifies that the x-axis labels should be at a 45 degree 
#angle and should be horizontally justified (see what happens if you leave this out). 

install.packages("ggplot2")
library(ggplot2)


wf=data.frame(term=names(freqr), occurances=freqr)
library(ggplot2)
p <- ggplot(subset(wf, freqr>100), aes(reorder(term, occurances), occurances))
p <- p + geom_bar(stat="identity", fill=alpha("skyblue"), colour = "black", size = 0.2)
p <- p + ggtitle("Word Frequency")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p


#  p1 = ggplot(subset(wf, freqr>100), aes(reorder(term, occurances), occurances))  
#  p1 = p1 + geom_bar(width = 0.75, stat = "identity", fill=alpha("skyblue"), colour = "black", size = 0.2) 
#  #p1 = p1 + xlab("") 
#  #p1 = p1 + ylab("")
#  p1 = p1 + ylim(-100,520) 
#  p1 = p1 + ggtitle("Word Frequency")  
# #p1 = p1 + theme(legend.position = "none")  
#  p1 = p1 + labs(x = NULL, y = NULL)  
#  p1 = p1 + theme(axis.text.x = element_text(angle = 0, hjust=1))  
#  p1 = p1 + coord_polar(start = 90) 
#  p1 = p1 + geom_text(data=wf$term, aes(x=id, y=value+10, label="hello", hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle=0, inherit.aes = FALSE )
#  p1
    



#Get a list of terms that occur at least 200 times in the entire corpus. 
findFreqTerms(dtmr, lowfreq=200)
#"manag"    "organ"    "risk"     "work"     "document" "project" 

#Check for correlations between some of these and other terms that occur in the corpus.  
#Search for correlations between word and other terms 60% or more
findAssocs(dtmr, "project", 0.6)
findAssocs(dtmr, "risk", 0.6)
findAssocs(dtmr, "manag", 0.6)
findAssocs(dtmr, "organ", 0.6)
findAssocs(dtmr, "work", 0.6)
findAssocs(dtmr, "document", 0.6)


#wordcloud
install.packages("wordcloud")
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freqr),freqr, min.freq=70)

#.add color
wordcloud(names(freqr),freqr,min.freq=70,colors=brewer.pal(6,"Dark2"))



#Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_161\\bin")

#install.packages("RWeka")
#library(RWeka)
#install.packages("rJava")
#library(rJava)
##create bigram tokenizer
#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#create DTM
#dtmbi <- DocumentTermMatrix(docs, control = list(tokenize = BigramTokenizer))
#Continue as above....
#dtmbix = as.matrix(dtmbi)
#write.csv(dtmrx, "dtmbix.csv")

#----------------------------------
#CHAPTER 2 - CLUSTER ANALYSIS
#https://eight2late.wordpress.com/2015/07/22/a-gentle-introduction-to-cluster-analysis-using-r/
#---------------------------------
#shorten rownames for display purposes
rownames(dtmr) <- paste(substring(rownames(dtmr),1,3),rep("..",nrow(dtmr)),
                     substring(rownames(dtmr),
                               nchar(rownames(dtmr))-12,nchar(rownames(dtmr))-4))


#run hierarchical clustering using cosine distance
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs = cosineSim(dtmrx)
cd <- 1-cs 


groups <- hclust(cd,method="ward.D")
#plot, use hang to ensure that labels fall below tree
plot(groups, hang=-1)

#identify 4 groups
rect.hclust(groups,5)



#-------------------
#kmeans clustering
#-------------------
library(cluster)

#Using cosine distance
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs = cosineSim(dtmrx)
cd <- 1-cs

#number of clusters
K = 5

kfit <- kmeans(cd, K, nstart=100)

clusplot(as.matrix(cd), kfit$cluster, color=T, shade=T, labels=2, lines=0)
#print contents of kfit
print(kfit)
#print cluster sizes
kfit$size
#print clusters (members)
kfit$cluster

#sum of squared distance between cluster centers 
kfit$betweenss
#sum of squared distance within a cluster (this are the quantities that the algorithm
#attempts to minimise)
kfit$withinss

#kmeans - how to determine optimal number of clusters?
#One approach: look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
#from K to (1- doc count)
wss <- 5:33
for (i in 5:33) wss[i] <- sum(kmeans(cd,centers=i,nstart=25)$withinss)
plot(5:33, wss[5:33], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 


#----------------------------------
# NETWORK GRAPHS
#----------------------------------
install.packages("igraph")
library(igraph)

setwd("./docs")
filenames <- list.files(getwd(),pattern="*.txt")
#read files into a character vector
files <- lapply(filenames,readLines)
#create corpus from vector
docsv <- Corpus(VectorSource(files))
#Check number of docs loaded
print(docsv)
setwd("../")

#Map filenames to matrix row numbers
#these numbers will be used to reference files in the network graph
filekey <- cbind(rownames(dtmr),filenames)
write.csv(filekey,"filekey.csv")


#compute cosine similarity between document vectors
#converting to distance matrix sets diagonal elements to 0
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(dtmrx)

c=as.matrix(cs)
write.csv(c,"distancecoeff.csv")

#adjacency matrix: set entries below a certain threshold to 0.
#set cells that are less than half to 0%
cs[cs < max(cs)/2] <- 0
cs <- round(cs,3)
#write to disk
write.csv(as.matrix(cs),file="AdjacencyMatrix.csv")


# build a graph from the above matrix
#mode is undirected because similarity is a bidirectional relationship
g <- graph.adjacency(as.matrix(cs), weighted=T, mode = "undirected")

#Plot a Graph
# set seed to make the layout reproducible
set.seed(42)
#one of many possible layouts, see igraph docs
layout1 <- layout.fruchterman.reingold(g)
#basic plot with no weighting - fruchtermann reingold weighting
plot(g, layout=layout1)
#another layout
#plot(g, layout=layout.kamada.kawai)

#lets weight the nodes and edges
#V=vertex, E=edge
V(g)$label <- V(g)$name
#Vertex size proportional to number of connections
V(g)$size <- degree(g)*1
#Vertex label size proportional to number of connections
V(g)$label.cex <-  degree(g) / max(degree(g))+ 1
#label colour default black
V(g)$label.color <- "black"
#Vertex color organe
V(g)$color <- "lightblue"
#edge color grey
E(g)$color <- "grey"
#edge width proportional to similarity (weight)
E(g)$width <- E(g)$weight*6
# plot the graph in layout1 (fruchtermann reingold)
plot(g, layout=layout1)




#----------------------------------
#CHAPTER 3 - TOPIC MODELLING
#https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
#---------------------------------

install.packages("topicmodels")
library(topicmodels)

#Run LDA using Gibbs Sampling
burnin <- 1000
# and perform 2000 iterations (after burn-in)...
iter <- 2000
#..taking every 500th one for further use. This "thinning" is done to ensure that
# samples are not correlated.
thin <- 500
#We'll use 5 different, randomly chosen starting points
nstart <- 5
#using random integers as seed. 
seed <- list(2003,5,63,100001,765)
#...and take the best run (the one with the highest probability) as the result
best <- TRUE

#Number of topics (4 looks best)
k <- 4

ldaOut <- LDA(dtmr,k, method="Gibbs", control=
                list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
topics(ldaOut)
ldaOut.topics <-as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

terms(ldaOut,10)
#Topic 1   Topic 2    Topic 3    Topic 4   
#[1,] "organ"   "knowledg" "document" "project" 
#[2,] "work"    "ibis"     "word"     "risk"    
#[3,] "practic" "question" "cluster"  "manag"   
#[4,] "manag"   "issu"     "topic"    "process" 
#[5,] "best"    "idea"     "term"     "author"  
#[6,] "chang"   "said"     "data"     "model"   
#[7,] "system"  "figur"    "corpus"   "techniqu"
#[8,] "design"  "argument" "figur"    "social"  
#[9,] "team"    "develop"  "function" "problem" 
#[10,] "process" "share"    "result"   "respons" 

ldaOut.terms <- as.matrix(terms(ldaOut,10))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#Find probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma) 
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))

#How good (i.e. distinct) are the topic assignments?
#Find relative importance of top 2 topic assignments
topic1ToTopic2 <- lapply(1:nrow(dtmr),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

#Find relative importance of second and third ranked topic assignments
topic2ToTopic3 <- lapply(1:nrow(dtmr),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])

#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))

#----------
#find words based on stemmed topics

library(stringr)


getTopicWords = function (topicword, mydocs) {
  topiclist = list()
  mywords = ""
  for (i in 1:length(mydocs)) {
    mywords = c(mywords, str_extract_all(mydocs[[i]]$content, paste(topicword, "\\w+", sep="")))
    topiclist[[i]] = unique(mywords[[i]])
  }
  return(topiclist) 
}
wordtopics = getTopicWords("manag", docsraw)



#Get first and last line per document
docsOrig = Corpus(DirSource("./docs"))

getFirstLastLines = function (mydocs) {
  
  linedf = data.frame()
  allLines = list()
  for (i in 1:length(mydocs)) {
    
    allLines = str_extract_all(docsOrig[[i]]$content, boundary("sentence"))
    lineCount = length(allLines[[1]])
    linedf = rbind(linedf, data.frame(page=i, firstLine=allLines[[1]][1], lastLine=allLines[[1]][lineCount], stringsAsFactors=FALSE))
  }
  return(linedf)
}
mydf = getFirstLastLines(docsOrig)
write.csv(file="FirstLastLines.csv", mydf)

countWords = function (mydocs) {
  
  worddf = data.frame()
  allWords = list()
  for (i in 1:length(mydocs)) {
    
    allWords = str_extract_all(docsOrig[[i]]$content, boundary("word"))
    wordCount = length(allWords[[1]])
    worddf = rbind(worddf, data.frame(page=i, wCount=wordCount))
  }
  return(worddf)
}
mywdf = countWords(docsOrig)
mean(mywdf$wCount)
