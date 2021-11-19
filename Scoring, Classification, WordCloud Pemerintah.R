
install_github("nurandi/katadasaR")

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

unggahan.pemerintah =
  read.csv('C:/Users/Asus/Documents/R/Skripsi_Adita/Dataset/Report-Pemerintah.csv',
           header = TRUE)

View(unggahan.pemerintah)

#Data Cleansing & Make Corpus Function

message.corpus = Corpus(VectorSource(unggahan.pemerintah$Message))

inspect(message.corpus[[1]])

# TRANSFORM TO LOWER CASE
message.corpus = tm_map(message.corpus,
                        content_transformer(tolower))

inspect(message.corpus[[1]])

#Menghilangkan URL
removeURL <- function(x){
  gsub("http[^[:space:]]*", "", x)
  }

message.corpus = tm_map(message.corpus,
                      content_transformer(removeURL))

inspect(message.corpus[[1]])

#MENGHILANGKAN HASTAG
remove.hashtag <- function(x){
  gsub("#\\S+", "", x)
}

message.corpus = tm_map(message.corpus,
                        remove.hashtag)

inspect(message.corpus[[1]])

#CLEANING NUMBER
message.corpus <- tm_map(message.corpus, 
                         content_transformer(removeNumbers))

inspect(message.corpus[[1]])

#PUNCTUATION (MENGHILANGKAN TANDA BACA)
message.corpus <- tm_map(message.corpus, 
                         content_transformer(removePunctuation))

inspect(message.corpus[[1]])

#MENGHILANGKAN KATA TIDAK PENTING

Stopword <- readLines("stopwords.csv")

message.corpus <- tm_map(message.corpus,
                         removeWords,
                         Stopword)
inspect(message.corpus[[1]])

#MENGHILANGKAN SPASI BERLEBIHAN

message.corpus <- tm_map(message.corpus, 
                         stripWhitespace)

inspect(message.corpus[[1]])

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

message.corpus <- tm_map(message.corpus, 
                         stem_text)

inspect(message.corpus[[1]])


#SCORING

setwd('C:/Users/Asus/Documents/R/Skripsi_Adita/Helpers/Data Helper')

kata.positif = readLines('s-pos.txt')   
kata.negatif = readLines('s-neg.txt')    
kalimat2 = data.frame(text = sapply(message.corpus,       
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
hasil.pemerintah = score.sentiment(kalimat2$text, kata.positif,
                        kata.negatif)
View(hasil.pemerintah)
dataP <- hasil.pemerintah[c(3,1,2)]
View(dataP)
write.csv(dataP, file =
            "C:/Users/Asus/Documents/R/Skripsi_Adita/sentimenPemerintahpart2.csv")

#CONVERT SCORE TO SENTIMENT
hasil.pemerintah$content<- ifelse(hasil.pemerintah$score > 0,
                           "Positive",ifelse(hasil.pemerintah$score==0,"Neutral","Negative"))

View(hasil.pemerintah)

#WORDCLOUD


Pemerintah_wordcloud <- cbind(unggahan.pemerintah, hasil.pemerintah)

View(Pemerintah_wordcloud)

PemerintahPositif <- dplyr::filter(Pemerintah_wordcloud, content %in% c("Positive"))
PemerintahNegatif <- dplyr::filter(Pemerintah_wordcloud, content %in% c("Negative"))

View(PemerintahPositif)
View(PemerintahNegatif)

WC_positif <- Corpus(VectorSource(PemerintahPositif$Message))
wc_negatif <- Corpus(VectorSource(PemerintahNegatif$Message))

#MENGHILANGKAN KATA TIDAK PENTING

WC_positif <- tm_map(WC_positif,
                     content_transformer(tolower))
wc_negatif <- tm_map(wc_negatif,
                     content_transformer(tolower))


Stopword <- readLines("stopwords.csv")

WC_positif <- tm_map(WC_positif,
                         removeWords,
                         Stopword)

wc_negatif <-tm_map(wc_negatif,
                    removeWords,
                    Stopword)


WC_positif <- tm_map(WC_positif,
                     content_transformer(removePunctuation))

wc_negatif <- tm_map(wc_negatif,
                     content_transformer(removePunctuation))

inspect(WC_positif[[1]])
Stopword_manual <- readLines("stopwords_manual.csv")

WC_positif <- tm_map(WC_positif,
                         removeWords,
                         Stopword_manual)


wc_negatif <- tm_map(wc_negatif,
                     removeWords,
                     Stopword_manual)


WC_positif <- tm_map(WC_positif,
                     content_transformer(removeNumbers))

wc_negatif <- tm_map(wc_negatif,
                     content_transformer(removeNumbers))

#MENGHILANGKAN SPASI BERLEBIHAN

WC_positif <- tm_map(WC_positif, 
                         stripWhitespace)

wc_negatif <- tm_map(wc_negatif,
                     stripWhitespace)

#membangun term-document matrix & tampilkan kata sebagai wordcloud Positive
dtm <- TermDocumentMatrix(WC_positif)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names (v), freq=v)
head(d,50) #menampilkan 10 kata paling banyak muncul


#membangun term-document matrix & tampilkan kata sebagai wordcloud negative
dtm_n <- TermDocumentMatrix(wc_negatif)
m_n <- as.matrix(dtm_n)
v_n <- sort(rowSums(m_n),decreasing=TRUE)
d_n <- data.frame(word = names (v_n), freq=v_n)
head(d_n,50) #menampilkan 10 kata paling banyak muncul

wordcloud(WC_positif,
          scale=c(2,0.2),
          max.words = 50,
          min.freq = 1,
          random.order = FALSE,
          rot.per = 0.35,
          random.color = FALSE,
          colors = brewer.pal(6, "Dark2"))

wordcloud(wc_negatif,
          scale=c(2,0.2),
          max.words = 50,
          min.freq = 1,
          random.order = FALSE,
          rot.per = 0.35,
          random.color = FALSE,
          colors = brewer.pal(6, "Dark2"))

