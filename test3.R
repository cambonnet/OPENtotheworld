library(readxl)
library(dplyr)

pays <- read_excel("pays.xlsx")

library(shiny)
library(readxl)

pays <- read_excel("pays.xlsx")

trouver_destination <- function(criteres) {
  results <- pays
  
  if (criteres$saison == "en été") {
    results <- filter(results, Saison == "été")
  } else if (criteres$saison == "en automne") {
      results <- filter(results, Saison == "automne")
  } else if (criteres$saison == "en hiver") {
      results <- filter(results, Saison == "hiver")
  } else {
    results <- filter(results, Saison == "printemps")
  }                 
  
  if (criteres$enfant > 0) {
    results <- filter(results, 'Enfant(s)' == "Oui")
  } else {
    results <- filter(results, 'Enfant(s)' == "Non")
  }
  
  if (criteres$typvac == "culturel") {
    results <- filter(results, Culturelles == "Oui")
  } else if (criteres$typvac == "festif") {
    results <- filter(results, Festives == "Oui")
  } else if (criteres$typvac == "sportif") {
    results <- filter(results, Sportives == "Oui")
  } else if (criteres$typvac == "détendu") {
    results <- filter(results, Détentes == "Oui")
  } else if (criteres$typvac == "familliale") {
    results <- filter(results, Familiales == "Oui")
  } else { 
    results <- filter (results,"Non")
    }
  
  
  if (criteres$budget == "Fort = + 700€") {
    results <- filter(results, Budget == "Fort")
  } else if (criteres$budget == "Faible = 0 - 350€") {
    results <- filter(results, Budget == "Faible")
  } else {
    results <- filter(results, Budget == "Moyen")
  }
  
  if (criteres$duree > 15) {
    results <- filter(results, Durée == "Long")
  }  else if (criteres$duree < 7) {
    results <- filter(results, Durée == "Court")
  } else {
    results <- filter(results, Durée == "Moyen")
  }
  
  if (criteres$typays == "pays chaud") {
    results <- filter(results, Climat == "Chaud")
  } else if (criteres$typays == "pays froid") {
    results <- filter(results, Climat == "Froid")
  } else {
    results <- filter(results, Climat == "Tempéré")
  }
  
  if (nrow(results) > 0) {
    destination_aleatoire <- sample(results$Destination, 1)
    return(data.frame(Destination = destination_aleatoire))
  } else {
    return(data.frame(Destination = character(0)))
  }
  results <- filter(results)
}

ui <- fluidPage(
  navbarPage("Venez explorer le monde",
             tabPanel("Trouvez votre destination",
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
        background-color: #93c47d;
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
        background-color: #93c47d;
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
                          numericInput("adulte", "Nombre d'adulte(s) (+ 16 ans):", value = 0, min = 0, max = 99),
                          numericInput("enfant", "Nombre d'enfant(s) (0-16 ans):", value = 0, min = 0, max = 99),
                          
                          selectInput("saison", "Quand souhaitez vous partir ?:",
                                      choices = c("en été", "en hiver", "en automne", "au printemps")),
                          sliderInput("duree",
                                      "Durée de vos vacances (en jour(s)):",
                                      min = 1,
                                      max = 60,
                                      value = 10),
                          selectInput("budget", "Budget par jour et par personne en €:",
                                      choices = c("Faible = 0 - 350€", "Moyen = 350 - 700 €", "Fort = + 700€")),
                          radioButtons(inputId = "typays", label = "Type de votre destination de rêves :", inline = TRUE,
                                       choices = c("pays chaud", "pays froid", "pays tempéré")),
                          radioButtons(inputId = "typvac", label = "Quel est type d'activité souhaitez-vous réaliser ?", inline = TRUE,
                                       choices = c("festif", "sportif", "culturel", "détendu", "famillilale")),
                          
                          actionButton("validate", "Lancez les recherches ...")
                        ),
                        mainPanel(class="main", tabsetPanel(tabPanel("",
                                                                     tags$p(class = "intro", "Vous êtes en manque d'inspiration pour vos prochaine vacances ? 
             OPEN to the world est là pour vous aider à passer les meilleures vacances de votre vie !"),
                                                                     textOutput("message"))),
                                  tabsetPanel(
                                    tabPanel("Récapitulatif", textOutput("message2")),
                                    tabPanel("Résultats", textOutput("destination_recommandee"))
                                  ))
                      )),
  ),
  
)


server <- function(input, output) {
  
  validate_click <- eventReactive(input$validate, {
    list(
      nom = input$nom,
      age = input$age,
      adulte = input$adulte,
      enfant = input$enfant,
      saison = input$saison,
      duree = input$duree,
      budget = input$budget, # Extraire la valeur numérique du budget
      typays = input$typays,
      typvac = input$typvac
    )
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
          Vos vacances se dérouleront ", input$saison, "pour une durée de ", input$duree,
          "jour(s). Vous partirez dans un ", input$typays, "et emmènerez avec vous ", input$adulte , "adulte(s) et ", input$enfant,
          "enfant(s) qui profiteront d'agréables moments dans un cadre", 
          input$typvac, " avec un budget ", input$budget, "€/jour/personne." )
  })
  output$destination_recommandee <- renderText({
    req(validate_click())
    resultats <- trouver_destination(validate_click())
    if (!is.na(resultats$Destination)) {
      paste("La destination recommandée est :", resultats$Destination)
    } else {
      "Aucune destination recommandée avec les critères sélectionnés."
    }
  })
  
}

# Exécution de l'application
shinyApp(ui = ui, server = server)
