library(tm)
library(xml2)
library(stringr)
library(dplyr)
library(katadasaR)
library(NLP)
library(tau)
library(parallel)
library(caret)
library(devtools)
library(ggplot2)
library(car)
library(plotly)
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(plyr)
library(reshape2)
library(plotly)


setwd('C:/Users/Asus/Documents/R/Skripsi_Adita')

# LOAD DATA
unggahan.berita =
  read.csv('C:/Users/Asus/Documents/R/Skripsi_Adita/Dataset/Report-Portal-Berita.csv',
           header = TRUE)
View(unggahan.berita)

#Data Cleansing

#Make Corpus Function

messageB.corpus = Corpus(VectorSource(unggahan.berita$Message))

inspect(messageB.corpus[[1]])

# TRANSFORM TO LOWER CASE
messageB.corpus = tm_map(messageB.corpus,
                        content_transformer(tolower))

inspect(messageB.corpus[[1]])

#Menghilangkan URL
removeURL <- function(x){
  gsub("http[^[:space:]]*", "", x)
}

messageB.corpus = tm_map(messageB.corpus,
                        content_transformer(removeURL))

inspect(messageB.corpus[[1]])

#MENGHILANGKAN HASTAG
remove.hashtag <- function(x){
  gsub("#\\S+", "", x)
}

messageB.corpus = tm_map(messageB.corpus,
                        remove.hashtag)

inspect(messageB.corpus[[1]])

#CLEANING NUMBER
messageB.corpus <- tm_map(messageB.corpus, 
                         content_transformer(removeNumbers))

inspect(messageB.corpus[[1]])

#PUNCTUATION (MENGHILANGKAN TANDA BACA)
messageB.corpus <- tm_map(messageB.corpus, 
                         content_transformer(removePunctuation))

inspect(messageB.corpus[[1]])

#MENGHILANGKAN KATA TIDAK PENTING

Stopword <- readLines("stopwords.csv")

messageB.corpus <- tm_map(messageB.corpus,
                         removeWords,
                         Stopword)
inspect(messageB.corpus[[1]])

#MENGHILANGKAN SPASI BERLEBIHAN

messageB.corpus <- tm_map(messageB.corpus, 
                         stripWhitespace)

inspect(messageB.corpus[[1]])

# Stemming Words
stem_text<-function(text,mc.cores=1)
{
  stem_string<-function(str)
  {
    str<-tokenize(x=str)
    str<-sapply(str,katadasaR)
    str<-paste(str,collapse = "")
    return(str)
  }
  x<-mclapply(X=text,FUN=stem_string,mc.cores=mc.cores)
  return(unlist(x))
}

messageB.corpus <- tm_map(messageB.corpus, 
                         stem_text)

inspect(messageB.corpus[[1]])


#SCORING

setwd('C:/Users/Asus/Documents/R/Skripsi_Adita/Helpers/Data Helper')


kata.positif = readLines('s-pos.txt')   
kata.negatif = readLines('s-neg.txt')    
kalimat2 = data.frame(text = sapply(messageB.corpus,       
                                    as.character), stringsAsFactors = FALSE)
score.sentiment = function(kalimat2, kata.positif,
                           kata.negatif, .progress='none')
  
{
  require(plyr)
  require(stringr)
  scores = laply(kalimat2, function(kalimat2,
                                    kata.positif, kata.negatif) {
    kalimat = gsub('[[:punct:]]', '', kalimat2)
    kalimat = gsub('[[:cntrl:]]', '', kalimat2)
    kalimat = gsub('\\d+', '', kalimat2)
    kalimat = tolower(kalimat2)
    
    list.kata = str_split(kalimat2, '\\s+')
    kata2 = unlist(list.kata)
    positif.matches = match(kata2, kata.positif)
    73
    negatif.matches = match(kata2, kata.negatif)
    positif.matches = !is.na(positif.matches)
    negatif.matches = !is.na(negatif.matches)
    score = sum(positif.matches) -
      (sum(negatif.matches))
    return(score)
  }, kata.positif, kata.negatif, .progress=.progress )
  scores.df = data.frame(score=scores, text=kalimat2)
  return(scores.df)
}
hasilB = score.sentiment(kalimat2$text, kata.positif,
                        kata.negatif)
View(hasilB)
View(dataP)


#CONVERT SCORE TO SENTIMENT
hasilB$konten<- ifelse(hasilB$score<0,
                           "Negatif",ifelse(hasilB$score==0,"Netral","Positif"))
hasilB$konten
View(hasilB)
dim(hasilB$konten)

#EXCHANGE ROW SEQUENCE
data <- hasilB[c(3,1,2)]
View(data)
write.csv(data, file =
            "C:/Users/Asus/Documents/R/Skripsi_Adita/sentimenBerita.csv")

#menyatukan variable lain dengan hasil konten dan menambahkan variable reaksi

sentimenBerita <- cbind(unggahan.berita, hasilB)
View(sentimenBerita)


sentimenBerita2 <- dplyr::select(sentimenBerita, User.Name, Likes.at.Posting, Post.Created.Date, Total.Interactions:Care, Message, Overperforming.Score,text, konten )

sentimenBerita2$reaksi <- sentimenBerita2$Love - sentimenBerita2$Angry



View(sentimenBerita2)

#uji ANOVA pada akun pemerintah versi 1 (netral ga masuk)

anovaBerita2 <- dplyr::select(sentimenBerita2, konten,reaksi)

View(anovaBerita2)

anovaBerita2$konten <- as.factor(anovaBerita2$konten)


anovaBeritaHasil2 = aov(reaksi~konten , data = anovaBerita2)

anovaBeritaHasil2

summary(anovaBeritaHasil2)

plot(reaksi~konten , data = anovaBerita2)

kruskal.test(reaksi~konten, data = anovaBerita1)


#membuat wordcloud

beritaNegatif <- dplyr::filter(sentimenBerita, klasifikasi %in% c("Negatif"))
beritaPositif <- dplyr::filter(sentimenBerita, klasifikasi %in% c("Positif"))
View(beritaNegatif)

beritaPositif <- dplyr::select(beritaPositif, Message)
beritaNegatif <- dplyr::select(beritaNegatif, Message)

WCB_positif <- Corpus(VectorSource(beritaPositif$Message))
wcB_negatif <- Corpus(VectorSource(beritaNegatif$Message))

#MENGHILANGKAN KATA TIDAK PENTING

WCB_positif <- tm_map(WCB_positif,
                     content_transformer(tolower))
wcB_negatif <- tm_map(wcB_negatif,
                     content_transformer(tolower))


Stopword <- readLines("stopwords.csv")

WCB_positif <- tm_map(WCB_positif,
                     removeWords,
                     Stopword)

wcB_negatif <-tm_map(wcB_negatif,
                    removeWords,
                    Stopword)

WCB_positif <- tm_map(WCB_positif,
                     content_transformer(removePunctuation))

wcB_negatif <- tm_map(wcB_negatif,
                     content_transformer(removePunctuation))

WCB_positif <- tm_map(WCB_positif,
                     content_transformer(removeNumbers))

wcB_negatif <- tm_map(wcB_negatif,
                     content_transformer(removeNumbers))

Stopword_manual <- readLines("stopwords_manual.csv")

WCB_positif <- tm_map(WCB_positif,
                     removeWords,
                     Stopword_manual)
inspect(WCB_positif[[1]])

wcB_negatif <- tm_map(wcB_negatif,
                     removeWords,
                     Stopword_manual)

#MENGHILANGKAN SPASI BERLEBIHAN

WCB_positif <- tm_map(WCB_positif, 
                     stripWhitespace)

wcB_negatif <- tm_map(wcB_negatif,
                     stripWhitespace)

#membangun term-document matrix
dtmB <- TermDocumentMatrix(WCB_positif)
mB <- as.matrix(dtmB)
vB <- sort(rowSums(mB),decreasing=TRUE)
dB <- data.frame(word = names (vB), freq=vB)
head(dB,50) #menampilkan 10 kata paling banyak muncul

#tampilkan kata sebagai wordcloud
set.seed(1234)
wordcloud(words = dB$word, freq = dB$freq, min.freq=20, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

#menemukan kata yang berasosiasi (keluar bersamaan) corlimit-->minimal derajat korelasi
findAssocs(dtmB, terms = "user", corlimit = 0.2)


#membangun term-document matrix
dtm_nB <- TermDocumentMatrix(wcB_negatif)
m_nB <- as.matrix(dtm_nB)
v_nB <- sort(rowSums(m_nB),decreasing=TRUE)
d_nB <- data.frame(word = names (v_nB), freq=v_nB)
head(d_nB,50) #menampilkan 10 kata paling banyak muncul

#tampilkan kata sebagai wordcloud
set.seed(1234)
wordcloud(words = d_nB$word, freq = d_nB$freq, min.freq=10, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
warnings()

wordcloud(WCB_positif,
          scale=c(2,0.6),
          max.words = 50,
          min.freq = 1,
          random.order = FALSE,
          rot.per = 0.35,
          random.color = FALSE,
          colors = brewer.pal(6, "Dark2"))
wordcloud(wcB_negatif,
          scale=c(2,0.2),
          max.words = 50,
          min.freq = 1,
          random.order = FALSE,
          rot.per = 0.35,
          random.color = FALSE,
          colors = brewer.pal(6, "Dark2"))
