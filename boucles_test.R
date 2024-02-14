library(dplyr)
library(readxl)

payss <- read_excel("pays__.xlsx")
head(payss)

saison<- "été"
enf<-0
duree<-8
budg<-250
typays<- "pays chaud"
typvac<- "culturel"


if (saison == "été"){PP<- filter(payss, Saison == "été")} else if (saison == "printemps") {PP<- filter(payss, Saison == "printemps")} else if (saison == "hiver") {PP<- filter(payss, Saison == "hiver")} else if (saison == "automne") {PP<- filter(payss, Saison == "automne")}
head(PP)
if (enf >= 1) {PP <- filter(PP, Enfant == "Oui")} else {PP<- filter(PP, Enfant == "Non")}
head(PP)
if (duree > 15) {PP<- filter(PP, Duree == "Long")} else if(duree < 7) {PP<- filter(PP, Duree == "Court")} else if(duree>7 & duree<14){PP<-filter(PP, Duree == "Moyen")}
head(PP)
if (budg > 700) {PP<- filter(PP, Budget == "Fort")} else if(budg < 350) {PP<- filter(PP, Budget == "Faible")} else if (budg>350 & budg<700) {PP<-filter(PP, Budget == "Moyen")}
head(PP)
if (typays == "pays chaud"){PP<- filter(PP, Climat == "Chaud")} else if(typays == "pays froid") {PP<- filter(PP, Climat == "Froid")} else if (typays == "pays tempéré") {PP<- filter(PP, Climat == "Tempéré")}
head(PP)
if (typvac == "festif") {PP<- filter(PP, Festives == "Oui")} else if (typvac == "sportif"){PP<- filter(PP, Sportives == "Oui")} else if (typvac == "culturel") {PP<- filter(PP, Culturelles == "Oui")} else if (typvac == "détendu") {PP<- filter(PP, Détentes == "Oui")}
head(PP)
PPP<- select(PP, Pays)

if (is.null(PPP) == TRUE) {paste("Nous n'avons trouvé aucune destination qui corresponde à votre demande")} else{destid<-PPP} #j'arrive pas à faire un tirage au sort parmis les données de PPP


