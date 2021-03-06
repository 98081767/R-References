#Some general tips before you proceed:
# 1. Install the following packages before you proceed: tm, SnowballC, ggplot2, 
# wordcloud
# 2. Download documentation for tm from CRAN: 
# https://cran.r-project.org/web/packages/tm/tm.pdf
# 3. Create a folder called "textmining" in your workarea for this subject (or "Documents folder)
# 4. Unzip the files in docs.zip into a subdirectory called "docs"
# 5. Run the code for part 1 line by line and examine the results.
# 6. Understand what each step does. Check out the environment panel (on the right
#    in RStudio) to see more about the variables created in each step.
# 7. Check the tm documentation for tm specific functions: tm_map, DocumentTermMatrix, 
#    findFreqTerms and FindAssocs
# 8. Do you see anything odd in the results? Think about how you can fix it.
# 9. Experiment! For example, try with and without stemming; change plot parameters etc.
# 10. Look at part 2 only after you're comfortable with part 1.
# 11. Check out the following vignette on CRAN for more on tm: https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# 12. ...and finally, don't forget to modify the directory path in the setwd() 
#    command before starting!


#set working directory
setwd("C:\\Users\\arche\\Documents\\UTS\\36106 Data Algorithms and Meaning\\Week 4\\Workshop\\in-class-exercises-dam-aut18-bs1\\in-class-exercises-dam-aut18-bs1") #change path as needed
getwd() #check that you're where you should be
library(tm) 
library(SnowballC)
#Let's go. Load corpus... 
docs <- Corpus(DirSource("./docs"))
#Mac users only!!
#docs <- tm_map(docs, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))
#Check details
#of docs
print(docs)
#contents
#Patience, this might take a while....
inspect(docs[1])

#Remove punctuation - replace punctuation marks with " "
docs <- tm_map(docs, removePunctuation)
#Transform to lower case
docs <- tm_map(docs,content_transformer(tolower))
#Strip digits
docs <- tm_map(docs, removeNumbers)
#Remove stopwords from standard stopword list 
docs <- tm_map(docs, removeWords, stopwords("english"))
#Strip whitespace (cosmetic?)
docs <- tm_map(docs, stripWhitespace)
#inspect output
writeLines(as.character(docs[[30]]))
#Stem document
##########################
##WARNING!!! - Mac users may want to avoid running stemDocument as it can give an
##error and corrupt the corpus.
docs <- tm_map(docs,stemDocument)
#########################
#some clean up
##NOTE: you will need to change the replacements appropriately if you do not stem!!
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "organiz", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "organis", replacement = "organ")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "andgovern", replacement = "govern")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "inenterpris", replacement = "enterpris")
docs <- tm_map(docs, content_transformer(gsub),
               pattern = "team-", replacement = "team")
#end clean up
#inspect
writeLines(as.character(docs[[30]]))
#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
#summary
dtm
#inspect segment of document term matrix
inspect(dtm[1:2,1000:1005])

#collapse matrix by summing over columns - this gets total counts (over all docs) for each term
freq <- colSums(as.matrix(dtm))
#length should be total number of terms
length(freq)

freqtable = data.frame(freq)

#create sort order (asc)
ord <- order(freq,decreasing=TRUE)
#inspect most frequently occurring terms
freq[head(ord)]

#one  organ    can  manag   work system 
#323    278    244    230    210    194

#write to disk and inspect file
write.csv(file="freq.csv",freq[ord])

#inspect least frequently occurring terms
freq[tail(ord)]

#tempor  therebi uncommit unionist  willing  workday 
#1        1        1        1        1        1 


#list most frequent terms. Lower bound specified as second argument
findFreqTerms(dtm,lowfreq=80)
#correlations
findAssocs(dtm,"project",0.6)
#inher     manag    occurr     handl       bok experienc 
#0.80      0.67      0.66      0.66      0.62      0.60 
#themes: project management, project inheritance, bok (pm bok), project experience


findAssocs(dtm,"enterpris",0.7)
#agil    compli     imper    nobodi  scrutini   upfront   realist increment 
#0.80      0.78      0.78      0.78      0.78      0.77      0.76      0.74 
#themes: agile, incremental, realistic


findAssocs(dtm,"system",0.7)
#lock  design  incent   adopt  subset    user rollout  involv specifi 
#0.81    0.79    0.79    0.77    0.77    0.75    0.74    0.71    0.71 
#themes: system design, system lock (author on PM)


#histogram
library(ggplot2)
wf=data.frame(term=names(freq),occurrences=freq)
p <- ggplot(subset(wf, occurrences>100), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
#order by frequency
p <- ggplot(subset(wf, occurrences>100), aes(reorder(term,occurrences), occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freq),freq, min.freq=90)
#...add color
wordcloud(names(freq),freq,min.freq=90,colors=brewer.pal(6,"Dark2"))


#------------------------------
##Part 2
#------------------------------
#define and eliminate all custom stopwords
#NOTE: change stopwords appopriately if you do not stem
myStopwords <- c("can", "say","one","way","use",
                  "also","howev","tell","will",
                  "much","need","take","tend","even",
                  "like","particular","rather","said",
                  "get","well","make","ask","come","end",
                  "first","two","help","often","may",
                  "might","see","someth","thing","point",
                  "post","look","right","now","think","'ve ",
                  "'re ","anoth","put","set","new","good",
                  "want","sure","kind","larg","yes,","day","etc",
                  "quit","sinc","attempt","lack","seen","awar",
                  "littl","ever","moreov","though","found","abl",
                  "enough","far","earli","away","achiev","draw",
                  "last","never","brief","bit","entir","brief",
                  "great","lot")
docs <- tm_map(docs, removeWords, myStopwords)


#remove very frequent and very rare words - since common words may not give you much info
#get words that are over 4 to 20 characters long and in more than 3 - 27 docs.
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20),
                                             bounds = list(global = c(3,27))))
freqr <- colSums(as.matrix(dtmr))
#length should be total number of terms
length(freqr)

#show freq
myfreqtable = data.frame(freqr)
fix(myfreqtable)


#create sort order (asc)
ordr <- order(freqr,decreasing=TRUE)
#inspect most frequently occurring terms
freqr[head(ordr)]
#  organ   manag    work  system project problem 
#    278     230     210     194     188     175 
# themes: project management, problem solving

#write to disk and inspect file
write.csv(file="freqr.csv",freqr[ordr])
#inspect least frequently occurring terms
freqr[tail(ordr)]
#resembl  struck  multin   lower  pseudo   gloss 
#3       3       3       3       3       3 
# themes: 


#list most frequent terms. Lower bound specified as second argument
findFreqTerms(dtmr,lowfreq=60)

#correlations
findAssocs(dtmr,"project",0.6)
#       agil     upfront     realist   increment   technolog        solv       adapt     neither    movement architectur   architect 
#       0.80        0.77        0.76        0.74        0.70        0.67        0.67        0.67        0.65        0.65        0.64 
#chanc        fine      featur 
#0.63        0.63        0.61 
#themes: agile project managemnt, technology, 

findAssocs(dtmr,"enterpris",0.6)
#themes
#agil     upfront     realist   increment   technolog        solv       adapt     neither    movement architectur   architect 
#0.80        0.77        0.76        0.74        0.70        0.67        0.67        0.67        0.65        0.65        0.64 
#chanc        fine      featur 
#0.63        0.63        0.61 
#themes: agile, incremnetal process, enterprise architecture


findAssocs(dtmr,"system",0.6)
#design    adopt   subset     user   involv  specifi function   intend     step  softwar   specif   intent   compos   depart    phone 
#0.79     0.77     0.77     0.75     0.71     0.71     0.70     0.67     0.67     0.67     0.66     0.66     0.66     0.65     0.63 
#frequent  pattern 
#0.63     0.61 
#themes: system design, user adoption, system software


#histogram
wf=data.frame(term=names(freqr),occurrences=freqr)
library(ggplot2)
p <- ggplot(subset(wf, freqr>100), aes(reorder(term,occurrences), occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
#wordcloud
library(wordcloud)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freqr),freqr, min.freq=70)
#...add color
wordcloud(names(freqr),freqr,min.freq=70,colors=brewer.pal(6,"Dark2"))

##Optional extra
##So far we've looked at unigrams (single words). What if you want to look at phrases?
##Can do n-gram analysis using RWeka package. 
#######################
##NOTE: RWeka requires Java, so you will first need to install Java and the Java
##Development Kit if you do not have them already. These can be obtained from:
##https://java.com/en/download/mac_download.jsp and
##http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html
##You may also need to set the JAVA_HOME environment variable manually. This is 
##essentially the path to the Java binaries (Java home directory) on your computer.
#######################
install.packages("Rweka")
library(RWeka)
##create bigram tokenizer
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#create DTM
dtmbi <- DocumentTermMatrix(docs, control = list(tokenize = BigramTokenizer))
#Continue as above....
