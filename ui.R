library(shiny)
library(dplyr)


payss <- read.csv("pays__.csv", sep = ";", encoding = "UTF-8")
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
        background-color: #22ADCC;
        background-color: #2986cc;
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
      numericInput("age", "Votre age:", value = 0, max = 99, min = 0),
      titlePanel("Avec qui souhaitez vous partir ?"),
      numericInput("adulte", "Nombre d'adulte(s) (+ 16 ans):", value = 0, min = 0),
      numericInput("enfant", "Nombre d'enfant(s) (0-16 ans):", value = 0, min = 0),
      
      
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
      radioButtons(inputId = "typvac", label = "En vacances, 
                   quel est le type d'activité que vous souhaitez réalisé ?", inline = TRUE,
                   choices = c("festif", "sportif", "culturel", "détendu")),
      
      actionButton("validate", "Lancez les recherches ...")
    ),
    mainPanel(class = "main",
              tabsetPanel(
                tabPanel("Descriptif",
                         tags$p(class = "intro",
                                "Vous êtes en manque d'inspiration pour vos prochaine vacances ?
             OPEN to the world est là pour vous aider à passer les meilleures vacances de votre vie !"
                         ),
                         textOutput("message")
                )),
              tabsetPanel(
                tabPanel(class = "intro", title = "Récapitulatif", textOutput("message2")),
                tabPanel(class = "intro", title = "Votre future destination...", textOutput("message3")),
                tabPanel(class = "intro", title = "Activités proposées", 
                         textOutput("message4"),"Grâce à  nos partenaires exclusifs, 
                         nous vous proposons diverses activités qui vous séduiront à coup sûr !")
              ))
  )
)

