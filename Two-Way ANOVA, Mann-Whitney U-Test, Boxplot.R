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
# pemberian kolom jenis akun

anovaPemerintah2$jenis <- "pemerintah"
View(anovaPemerintah2)

anovaBerita2$jenis <- "berita"
View(anovaBerita2)

#penyatuan baris untuk berita dan pemerintah

gabungan <- rbind(anovaPemerintah2, anovaBerita2)
View(gabungan)

gabungan$konten <- as.factor(gabungan$konten)

gabungan$jenis <- as.factor(gabungan$jenis)
#two way anova

TWA_hasil = aov(reaksi~konten*jenis, data = gabungan)
TWA_hasil
summary(TWA_hasil)


#man whitney

MWUhasil = wilcox.test(reaksi~jenis, paired = F ,mu= 0, alt= "two.sided" ,correct= T, data = gabungan)
MWUhasil = wilcox.test(reaksi~jenis, paired = F ,mu= 0, alt= "two.sided" ,correct= T, data = gabungan)
MWUhasil
summary(MWUhasil)




# AOV PART 2

anovaPemerintah1$jenis <- "pemerintah"

anovaBerita1$jenis <- "berita"

#penyatuan baris untuk berita dan pemerintah

gabungan2 <- rbind(anovaPemerintah2, anovaBerita2)

gabungan2$konten <- as.factor(gabungan2$konten)

gabungan2$jenis <- as.factor(gabungan2$jenis)
#two way anova

TWA_hasil2 = aov(reaksi~konten*jenis, data = gabungan2)
TWA_hasil2
summary(TWA_hasil2)


#man whitney

MWUhasil2 = wilcox.test(reaksi~jenis, paired = F ,mu= 0, alt= "two.sided" ,correct= T, data = gabungan2)
MWUhasil2 = wilcox.test(reaksi~jenis, paired = F ,mu= 0, alt= "two.sided" ,correct= T, data = gabungan2)
MWUhasil2
summary(MWUhasil2)


#boxplot

hasilboxplotp$Content<- ifelse(hasil$score<0,
                               "Negative",ifelse(hasil$score==0,"Neutral","Positive"))
hasilboxplotb$Content<- ifelse(hasilB$score<0,
                               "Negative",ifelse(hasilB$score==0,"Neutral","Positive"))

hasilboxplotp <-hasil 
hasilboxplotp$Type <- "Government"
hasilboxplotb <- hasilB
hasilboxplotb$Type <- "News portal"
View(hasilboxplotb)
View(hasilboxplotp)
boxplotgabungan <- rbind(hasilboxplotp, hasilboxplotb)
 boxplot(score~jenis*konten, data = boxplotgabungan,
         col = c("red", "blue"), aes(x=content, y=score, fill=cond))
 

 View(hasilboxplotb)
 boxplotgabungan <- dplyr::filter(boxplotgabungan, klasifikasi %in% c("Negatif") | klasifikasi %in% c("Positif"))

 p <- ggplot(boxplotgabungan, aes(x=factor(jenis), y=score ,fill=konten))+
   geom_boxplot()+
   facet_grid(.~konten)+
   labs(x="Type")+
   theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))
 
  b <- ggplot(boxplotgabungan, aes(x=factor(Type), y=score ,fill=Content))+
    geom_boxplot()+
    facet_grid(.~Content)+
    labs(x="Type")+
    theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))
ggplotly(b)
ggplotly(p) 

sd(hasilboxplotp$score)
sd(hasilboxplotb$score)
mean(hasilboxplotp$score)
mean(hasilboxplotb$score)


#PORTAL BERITA
MEAN = 0.2522965
SD = 1.430173
MIN = -19
max = 24

#PEMERINTAH
MEAN = 4.156293
SD = 6.762834
MIN = -15
MAX = 40
Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
   0.0000   0.0000   0.2523   1.0000  24.0000
