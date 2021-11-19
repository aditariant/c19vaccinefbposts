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
#PEMERINTAH

# SENTIMEN METODE PERHITUNGAN (LOVE, CARE & ANGRY, SAD)

sentimenPemerintah <- cbind(unggahan.pemerintah, hasil.pemerintah)

sentimenPemerintah$type <- "Government"

sentimenPemerintah$positive <- sentimenPemerintah$Love + sentimenPemerintah$Care

sentimenPemerintah$negative <- sentimenPemerintah$Sad + sentimenPemerintah$Angry 

sentimenPemerintah$reaction <- sentimenPemerintah$positive - sentimenPemerintah$negative

View(sentimenPemerintah)

#uji ANOVA pada METODE PERHITUNGAN (LOVE, CARE & ANGRY, SAD)

anovaPemerintah <- dplyr::select(sentimenPemerintah, content, reaction)

View(anovaPemerintah)

anovaPemerintah$content <- as.factor(anovaPemerintah$content)

anovaPemerintahHasil = aov(reaction~content , data = anovaPemerintah)

anovaPemerintahHasil

summary(anovaPemerintahHasil)

plot(reaction~content , data = anovaPemerintah)

#menghitung reaksi masyarakat terhadap unggahan

#PEMERINTAH

ReactPemerintahNegatif <- dplyr::filter(sentimenPemerintah, content %in% c("Negative"))

ReactPemerintahPositif <- dplyr::filter(sentimenPemerintah, content %in% c("Positive"))

ReactPemerintahNetral <- dplyr::filter(sentimenPemerintah, content %in% c("Neutral"))


#PEMERINTAH 

Hitung_react_P_Negatif <-  c(sum(ReactPemerintahNegatif$Love) + sum(ReactPemerintahNegatif$Care) , sum(ReactPemerintahNegatif$Angry) + sum(ReactPemerintahNegatif$Sad),
                             (sum(rpneg$Love) + sum(rpneg$Care) + sum(rpneg$Angry) + sum(rpneg$Sad)))

Hitung_react_P_Negatif

Hitung_react_P_Positif <- c(sum(ReactPemerintahPositif$Love) + sum(ReactPemerintahPositif$Care), sum(ReactPemerintahPositif$Angry)+sum(ReactPemerintahPositif$Sad),
                            (sum(rpp$Love) + sum(rpp$Care) + sum(rpp$Angry) + sum(rpp$Sad)))

Hitung_react_P_Positif

Hitung_react_P_Netral <- c(sum(ReactPemerintahNetral$Love)+sum(ReactPemerintahNetral$Care),sum(ReactPemerintahNetral$Angry) + sum(ReactPemerintahNetral$Sad),
                           (sum(rpnet$Love) + sum(rpnet$Care) + sum(rpnet$Angry) + sum(rpnet$Sad)))

Hitung_react_P_Netral

rpp=ReactPemerintahPositif
rpnet=ReactPemerintahNetral
rpneg=ReactPemerintahNegatif

persenrpp <- (sum(rpp$Love) + sum(rpp$Care))/((sum(rpp$Love) + sum(rpp$Care)) + (sum(rpp$Angry) + sum(rpp$Sad)))
persenrpneg <- (sum(rpneg$Love) + sum(rpneg$Care))/((sum(rpneg$Love) + sum(rpneg$Care)) + (sum(rpneg$Angry) + sum(rpneg$Sad)))
persenrpnet <- (sum(rpnet$Love) + sum(rpnet$Care))/((sum(rpnet$Love) + sum(rpnet$Care)) + (sum(rpnet$Angry) + sum(rpnet$Sad)))

persenrpp
persenrpneg
persenrpnet

#BERITA

React.B.Negatif <- dplyr::filter(sentimenBerita, content %in% c("Negative"))

React.B.Positif <- dplyr::filter(sentimenBerita, content %in% c("Positive"))

React.B.Netral <- dplyr::filter(sentimenBerita, content %in% c("Neutral"))


#BERITA

# SENTIMEN METODE PERHITUNGAN (LOVE, CARE & ANGRY, SAD)

sentimenBerita <- cbind(unggahan.berita, hasil.Berita)
View(sentimenBerita)

sentimenBerita$type <- "News Portal"

sentimenBerita$positive <- sentimenBerita$Love + sentimenBerita$Care

sentimenBerita$negative <- sentimenBerita$Sad + sentimenBerita$Angry 

sentimenBerita$reaction <- sentimenBerita$positive - sentimenBerita$negative

View(sentimenBerita)

#uji ANOVA 

anovaBerita <- dplyr::select(sentimenBerita, content,reaction)

View(anovaBerita)

anovaBerita$content <- as.factor(anovaBerita$content)

anovaBeritaHasil = aov(reaction~content , data = anovaBerita)

anovaBeritaHasil

summary(anovaBeritaHasil)

plot(reaction~content , data = anovaBerita)


#menghitung reaksi masyarakat terhadap unggahan
#BERITA 


hasil.react.b.positif <- c(sum(React.B.Positif$Love + React.B.Positif$Care), sum(React.B.Positif$Angry + React.B.Positif$Sad),
                           (sum(React.B.Positif$Love + React.B.Positif$Care)+ sum(React.B.Positif$Angry + React.B.Positif$Sad)))

hasil.react.b.negatif <- c(sum(React.B.Negatif$Love + React.B.Negatif$Care), sum(React.B.Negatif$Angry + React.B.Negatif$Sad), 
                           (sum(React.B.Negatif$Love + React.B.Negatif$Care)+ sum(React.B.Negatif$Angry + React.B.Negatif$Sad)))

hasil.react.b.netral <- c(sum(React.B.Netral$Love + React.B.Netral$Care), sum(React.B.Netral$Angry + React.B.Netral$Sad),
                          (sum(React.B.Netral$Love + React.B.Netral$Care)+ sum(React.B.Netral$Angry + React.B.Netral$Sad)))

hasil.react.b.positif

hasil.react.b.negatif

hasil.react.b.netral

rbp = React.B.Positif
rbneg = React.B.Negatif
rbnet = React.B.Netral

persennegatif <- sum(React.B.Negatif$Love + React.B.Negatif$Care)/(sum(React.B.Negatif$Love + React.B.Negatif$Care)+ sum(React.B.Negatif$Angry + React.B.Negatif$Sad))
persenpositif <- sum(React.B.Positif$Love + React.B.Positif$Care)/(sum(React.B.Positif$Love + React.B.Positif$Care)+ sum(React.B.Positif$Angry + React.B.Positif$Sad))
persennetral <- sum(React.B.Netral$Love + React.B.Netral$Care)/(sum(React.B.Netral$Love + React.B.Netral$Care)+ sum(React.B.Netral$Angry + React.B.Netral$Sad))

persennegatif
persenpositif
persennetral



