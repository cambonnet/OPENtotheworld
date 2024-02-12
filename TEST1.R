library(shiny)

ui <- fluidPage(
  navbarPage("Venez explorer le monde",
             tabPanel("Trouvez votre destination"),
  ),
  titlePanel("OPEN to the World"),
  tags$head(
    tags$style(
      HTML("
      body {
        font-family: 'Arial', sans-serif;
        background-color: #B4DC7F;
        color: #ffffff;
      }
      .title {
        font-size: 30px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 20px;
        color: #FAFB6;
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
      titlePanel("Avec qui souhaitez vous partir ?"),
      numericInput("age", "Votre age:", value = 0),
      numericInput("senior", "Nombre de sénior(s) (+ 62 ans):", value = 0),
      numericInput("adulte", "Nombre d'adulte(s) (+ 16 ans):", value = 0),
      numericInput("enfant", "Nombre d'enfant(s) (3-16 ans):", value = 0),
      numericInput("banbin", "Nombre de bambin(s) (-3 ans):", value = 0),
      
            selectInput("mois", "A quel mois souhaitez vous partir ?:",
                  choices = c("Janvier", "Février","Mars", "Avril", "Mai",
                              "Juin", "Juillet", "Août", "Septembre",
                              "Octobre", "Novembre", "Décembre")),
      sliderInput("duree",
                  "Durée de vos vacances (en jour(s)):",
                  min = 1,
                  max = 60,
                  value = 10),
      sliderInput("budget",
                  "Budget par personne pour réussir vos vacances (en €)):",
                  min = 1,
                  max = 5000,
                  value = 500),
      radioButtons(inputId = "typpays", label = "Type de votre destination de rêves :", inline = TRUE,
                   choices = c("Pays chaud", "Pays froid", "Pays tempéré", "Peu m'importe")),
      radioButtons(inputId = "typvac", label = "En vacances, quelle est le type d'activité que vous souhaitez réalisé ?", inline = TRUE,
                   choices = c("Festif", "Sportif", "Culturel", "Detendu", "Peu m'importe")),
      
      actionButton("validate", "Où pourriez vous partir ...")
    ),
    mainPanel(
      class = "main",
      tags$p(class = "intro", "En manque d'inspiration pour vos prochaine vacances ? 
             OPEN to the world est là pour vous aider à passer les meilleures vacances de votre vie !"),
      textOutput("message")
    ))
    ,
navbarPage("D'autres infos",tabPanel("Ressources"),
    tabPanel("Infos par pays")))

server <- function(input, output) {
  
  validate_click <- eventReactive(input$validate, {
    list(age = input$age, mois = input$mois)
  })
  
  output$message <- renderText({
    req(validate_click())
    if (validate_click()$age < 16 )  {
      return("Vous êtes trop jeune pour utiliser notre application.")
    } else {
      return("Vous avez l'âge parfait pour partir à la découverte de nouveaux horizons !")
    }
  })
}

# Exécution de l'application
shinyApp(ui = ui, server = server)