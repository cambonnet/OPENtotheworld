library(readxl)
library(dplyr)



# Charger les données sur les pays
pays <- read_excel("pays.xlsx")

typays = "pays chaud"
enfant = 0
typvac = "culturel"
budget = 500
duree = 5


# Fonction pour trouver la destination en fonction des critères
trouver_destination <- function(criteres) {
  PP <- filter(pays, Saison == criteres$saison)
  
  if (criteres$enfant > 0) {
    PP <- filter(PP, 'Enfant(s)' == "Oui")
  } else {
    PP <- filter(PP, 'Enfant(s)' == "Non")
  }
  
  if (criteres$duree > 15) {
    PP <- filter(PP, Durée == "Long")
  } else if (criteres$duree < 7) {
    PP <- filter(PP, Durée == "Court")
  } else {
    PP <- filter(PP, Durée == "Moyen")
  }
  
  if (criteres$typvac == "culturel") {
    PP <- filter(PP, Culturelles == "Oui")
  } else if (criteres$typvac == "festif") {
    PP <- filter(PP, Festives == "Oui")
  } else if ((criteres$typvac == "sportif")) {
    PP <- filter(PP, Sportives == "Oui")
  } else if ((criteres$typvac == "détendu")) {
    PP <- filter(PP, Détentes == "Oui")
  } else {
    PP <- filter(PP, Familiales == "Oui")
  }
  
  if (criteres$budget > 700) {
    PP <- filter(PP, Budget == "Fort")
  } else if (criteres$budget < 350) {
    PP <- filter(PP, Budget == "Faible")
  } else {
    PP <- filter(PP, Budget == "Moyen")
  }
  
  if (criteres$typays == "pays chaud") {
    PP <- filter(PP, Climat == "Chaud")
  } else if (criteres$typays == "pays froid") {
    PP <- filter(PP, Climat == "Froid")
  } else {
    PP <- filter(PP, Climat == "Tempéré")
  }
  
  result <- PP$Pays
  
  print(result)
}

