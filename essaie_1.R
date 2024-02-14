library(shiny)
library(dplyr)
library(readxl)

#ça marche bien avec ce document là que je vais mettre sur git, je l'ai légerement modifié genre des accents et des majuscules mais sinon ça marche pas

payss <- read_excel("pays__.xlsx")
head(payss)

ui <- fluidPage(
  navbarPage("Venez explorer le monde",
             tabPanel("Trouvez votre destination")
  ),
  titlePanel("OPEN to the World"),
  tags$head(
    tags$style(
      HTML("
      body {
        font-family: 'Arial', sans-serif;
<<<<<<< HEAD
        background-color: #22ADCC;
=======
        background-color: #2986cc;
>>>>>>> af06e45fece4d0a2e1d7000503bb4a689d2f2392
        color: #ffffff;
      }
      .title {
        font-size: 30px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 20px;
        color: #22ADCC;
      }
      .intro {
        font-size: 18px;
        font-style: italic;
        text-align: center;
        margin-bottom: 10px;
        color: #ffffff;

      }
      .sidebar {
        font-size: 18px;
        background-color: #FFA0AC;
        padding: 10px;
        border-radius: 40px;
        box-shadow: 0px 0px 20px #000000;
      }
        .sidebar2 {
        font-size: 18px;
        background-color: #2CBDAF;
        padding: 10px;
        border-radius: 40px;
        box-shadow: 0px 0px 20px #000000;
      }
      .main {
        background-color: #FFA0AC;
        padding: 5px;
        border-radius: 10px;
        text-align: center;
        font-style: bold;
        font-size: 20px;
        color: #7B886F;
        box-shadow: 0px 0px 10px #000000;
      }
      ")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      textInput(inputId = "nom", label = "Votre prénom et nom", placeholder = "Prénom Nom"),
      numericInput("age", "Votre age:", value = 0, max = 99),
      titlePanel("Avec qui souhaitez vous partir ?"),
      numericInput("adulte", "Nombre d'adulte(s) (+ 16 ans):", value = 0),
      numericInput("enfant", "Nombre d'enfant(s) (0-16 ans):", value = 0),
      
      
      selectInput("saison", "A quelle saison souhaitez vous partir ?:",
                  choices = c("été", "automne","hiver", "printemps")),
      sliderInput("duree",
                  "Durée de vos vacances (en jour(s)):",
                  min = 1,
                  max = 60,
                  value = 10),
      sliderInput(inputId = "budget",
                  "Budget par personne pour réussir vos vacances (en €)):",
                  min = 1,
                  max = 5000,
                  value = 500),
      radioButtons(inputId = "typays", label = "Type de votre destination de rêves :", inline = TRUE,
                   choices = c("pays chaud", "pays froid", "pays tempéré")),
      radioButtons(inputId = "typvac", label = "En vacances, quel est le type d'activité que vous souhaitez réalisé ?", inline = TRUE,
                   choices = c("festif", "sportif", "culturel", "détendu")),
      
      actionButton("validate", "Lancez les recherches ...")
    ),
    mainPanel(class="main", 
              tabsetPanel(
                tabPanel("", tags$p(class = "intro", "Vous êtes en manque d'inspiration pour vos prochaine vacances ? 
             OPEN to the world est là pour vous aider à passer les meilleures vacances de votre vie !"),
                                                 textOutput("message"))),
              tabPanel(class="intro", title = "", textOutput("message2")),
              tabPanel("resultat",textOutput("message3")),
              tabPanel("resultat",textOutput("message4"))
  )
  )
)


server <- function(input, output) {

  validate_click <- eventReactive(input$validate, {
    list(age = input$age, nom = input$nom)
  })
  output$message <- renderText({
    req(validate_click())
    if (validate_click()$age < 16 )  {
      return("Vous êtes trop jeune pour utiliser notre application seul, il est conseillé de faire appel à un adulte pour continuer.")
    } else {
      return("Vous avez l'âge parfait pour partir à la découverte de nouveaux horizons !")
    }    })
  
  output$message2 <- renderText({
    req(validate_click())
    paste("Récapitulons ! Vous êtes", input$nom, "et vous avez", input$age, "ans.
          Vos vacances se dérouleront en ", input$saison, "pour une durée de ", input$duree,
          "jour(s). Vous partirez dans un ", input$typays, "et emmènerez avec vous ", input$adulte , "adulte(s) et ", input$enfant,
          "enfant(s) profiteront d'agréables moments dans un cadre", 
          input$typvac, " avec un budget de ", input$budget, "€/personne." )
  })
  output$message3 <- renderText({
    req(validate_click())
    
    #Les boucles marchent !!!!! (je suis trop contente j'ai enfin compris)
    
    if (input$saison == "été"){PP<- filter(payss, Saison == "été")} 
    else if (input$saison == "printemps") {PP<- filter(payss, Saison == "printemps")} 
    else if (input$saison == "hiver") {PP<- filter(payss, Saison == "hiver")} 
    else if (input$saison == "automne") {PP<- filter(payss, Saison == "automne")}
    
    if (input$enfant >= 1) {PP <- filter(PP, Enfant == "Oui")} 
    else {PP<- filter(PP, Enfant == "Non")}
    
    if (input$duree > 15) {PP<- filter(PP, Duree == "Long")} 
    else if(input$duree < 7) {PP<- filter(PP, Duree == "Court")} 
    else if(input$duree>7 & duree<14){PP<-filter(PP, Duree == "Moyen")}
    
    if (input$budget > 700) {PP<- filter(PP, Budget == "Fort")} 
    else if(input$budget < 350) {PP<- filter(PP, Budget == "Faible")} 
    else if (input$budget>350 & input$budget<700) {PP<-filter(PP, Budget == "Moyen")}
    
    if (input$typays == "pays chaud"){PP<- filter(PP, Climat == "Chaud")} 
    else if(input$typays == "pays froid") {PP<- filter(PP, Climat == "Froid")} 
    else if (input$typays == "pays tempéré") {PP<- filter(PP, Climat == "Tempéré")}
    
    if (input$typvac == "festif") {PP<- filter(PP, Festives == "Oui")} 
    else if (input$typvac == "sportif"){PP<- filter(PP, Sportives == "Oui")} 
    else if (input$typvac == "culturel") {PP<- filter(PP, Culturelles == "Oui")} 
    else if (input$typvac == "détendu") {PP<- filter(PP, Détentes == "Oui")}
    
    renderTable({      req(validate_click())
    PPP<- select(PP, Pays)
    PPP})
    
    #la ça marche plus j'arrive pas à faire un tirage au sort parmis les données de PPP et je sais comment l'afficher sur Shiny
    
    #if (is.null(PPP)== TRUE) 
    #return({paste("Nous n'avons trouvé aucune destination qui corresponde à votre demande")} )
    #else{destid<-PPP} 
    
    #aussi il faudrait essayer de mettre le message3 sur l'autre onglet
    
})
}

# Exécution de l'application
shinyApp(ui = ui, server = server)