###fin script app pour les résultats desti

tabPanel(class="intro", title = "", textOutput("message3")),
tabPanel(class="intro", title = "", textOutput("message4"))
##
output$message3 <- renderDataTable({    
  req(validate_click())
  if (input$enfant < 1 ){
    return(test1 <-filter(pay ,Enfants== "Oui"))
  } else {
    return(test1 <-filter(pay ,Enfants== "Non"))
  }
}
)
output$message4 <- renderText({    
  req(validate_click())
  paste(test1[1:5,1],test1[1:5,1])})

############################################
##séléction du mois si possible pour remplacer saison
sidebarPanel(selectInput("mois", "A quel mois souhaitez vous partir ?:",
                         choices = c("Janvier", "Février","Mars", "Avril", "Mai",
                                     "Juin", "Juillet", "Août", "Septembre",
                                     "Octobre", "Novembre", "Décembre"))
             
#############################################
###Test filter :

#install.packages(dplyr)
library(readxl)
library(dplyr)
pay <- read_excel("pays.xlsx")
dim(pay)
head(pay)

pay[3,7]

if (enfant<=1)
  test1<- filter(pay, Culturelles == "Oui")
test1
test2 <-filter(test1,Enfants== "Oui")
test2
test3 <-filter(test2,Détentes== "Oui")
test3 
test4 <-filter(test3,Festives == "Oui")
test4

Result <- test3[1:5,1]
Result

###tests if

(if (test1 == "Oui"){
  return("test oui c'est culturelle")}
  else 
  {return("test non c'est pas cultu")})

pay[,7]
test1<- filter(pay[,7])
if (test1 == "Oui"){
  return("test oui c'est culturelle")}
 else {
  return("test non c'est pas cultu")}

x <- 1:100
filter(x, rep(1, 3))
filter(x, rep(1, 3), sides = 1)
filter(x, rep(1, 3), sides = 1, circular = TRUE)

filter(presidents, rep(1, 3))
