library(shiny)
library(dplyr)


payss <- read.csv("pays__act.csv", sep = ";", encoding = "UTF-8")
head(payss)


function(input, output) {
  
  validate_click <- eventReactive(input$validate, {
    list(age = input$age, nom = input$nom)
  })
  output$message <- renderText({
    req(validate_click())
    if (validate_click()$age < 16 )  {
      return("Vous êtes trop jeune pour utiliser notre application seul, 
             il est conseillé de faire appel à un adulte pour continuer.")
    } else {
      return("Vous avez l'âge parfait pour partir à la découverte de nouveaux horizons !")
    }    })
  
  output$message2 <- renderText({
    req(validate_click())
    paste("Récapitulons ! Vous êtes", input$nom, "et vous avez", input$age, "ans.
          Vos vacances se dérouleront en ", input$saison, "pour une durée de ", input$duree,
          "jour(s). Vous partirez dans un ", input$typays, "et emmènerez avec vous ", input$adulte ,
          "adulte(s) et ", input$enfant,
          "enfant(s) en profitant d'agréables moments dans un cadre", 
          input$typvac, " avec un budget de ", input$budget, "€/personne/jour." )
  })
  output$message3 <- renderText({
    req(validate_click())
    
    #Les boucles marchent !!!!! (je suis trop contente j'ai enfin compris)
    
    if (input$saison == "été"){PP<- filter(payss, Saison == "été")} else if (input$saison == "printemps") {PP<- filter(payss, Saison == "printemps")} else if (input$saison == "hiver") {PP<- filter(payss, Saison == "hiver")} else if (input$saison == "automne") {PP<- filter(payss, Saison == "automne")}
    
    if (input$enfant >= 1) {PP <- filter(PP, Enfant == "Oui")} else {PP<- PP}
    
    if (input$duree > 15) {PP<- PP} else if(input$duree < 7) {PP<- filter(PP, Duree == "Court")} else if(input$duree>7 & input$duree<14){PP<-filter(PP, Duree == "Moyen")}
    
    if (input$budget > 700) {PP<- PP} else if(input$budget < 350) {PP<- filter(PP, Budget == "Faible")} else if (input$budget>350 & input$budget<700) {PP<-filter(PP, Budget == "Moyen")}
    
    if (input$typays == "pays chaud"){PP<- filter(PP, Climat == "Chaud")} else if(input$typays == "pays froid") {PP<- filter(PP, Climat == "Froid")} else if (input$typays == "pays tempéré") {PP<- filter(PP, Climat == "Tempéré")}
    
    if (input$typvac == "festif") {PP<- filter(PP, Festives == "Oui")} else if (input$typvac == "sportif"){PP<- filter(PP, Sportives == "Oui")} else if (input$typvac == "culturel") {PP<- filter(PP, Culturelles == "Oui")} else if (input$typvac == "détendu") {PP<- filter(PP, Détentes == "Oui")}
    
    #enfant <- ifelse(input$enfant>=1, "Oui", "Non")
    
    #payss %>% 
    # filter(Saison == input$saison) %>% 
    #filter(Enfant == enfant) -> PP##
    
    if (length(PP$Pays)==0) {destid<-"Nous sommes désolé, nous n'avons trouvé aucune destination qui corresponde à votre demande"} else {destid<-sample(x=PP$Pays, size=1)}
    
    destid
    
  })
  
  output$message4 <- renderText({
    req(validate_click())
    
    AA<-filter(payss, Pays == destid)
    AA<-filter(AA, Saison == input$saison)
    activ<-AA$Activité
    
    activ
    
    
  })
}
