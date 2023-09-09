#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

#

library(shiny)
library(ggplot2)
library(tidyverse)
library(babynames)
library(timeDate)
library(shinythemes)
library(DT)
library(flexdashboard)
library(leaflet)
library(shinydashboard)
#library(rbokeh)

# Define UI for application that draws a histogram
ui <- fluidPage(

    
  #theme=shinytheme("united"), 
  shinythemes::themeSelector(), 
  # Application title
  titlePanel("DossierMedical"),
  
  
  #For nom de l'hopital ou se fait la consultation
  
  
  textInput(inputId = "nomhopital",
            label = "NH :", 
            value = " "),
  
  #dataTableOutput(outputId= "nomhopital"),
  
  #For date and Hour
  #Date
  dateRangeInput(inputId = "date", 
                 label = "Date",
                 format = "yy-mm-dd",
                 weekstart = " "),
  #Heure
  
  
  #for name
  textInput(inputId = "name",
            label = "Name :", 
            
            value = " "),
  dataTableOutput(outputId= "name"),
  
  #For sex
  selectInput(inputId = "sexe", 
              label = "Sexe", 
              choices = list(Female = "F", 
                             Male = "M")),
  dataTableOutput(outputId= "sexe"),
  
  #For Age
  numericInput(inputId = "age",
               label= "Age", 
               value = " " ), 
  plotOutput(outputId= "age"),
  
  #For Situation matrimonial
  selectInput(inputId = "sm", 
              label = "Situation matrimoniale", 
              choices = list(Marie.e = "M", 
                             Divorce.e = "D",
                             Celibataire = "C",
                             Celibataireavecenfant = "CavE",
                             Unionlibre = "unionl")),
  plotOutput(outputId= "sm"),
  
  #Profession
  textInput(inputId = "profession",
            label = "Profession :", 
            value = " "),
  plotOutput(outputId = "profession"),
  
  #zone de Provenance
  textInput(inputId = "provenance",
            label = "Provenance :", 
            value = " "),
  plotOutput(outputId = "provenance"),
  
  #Nationality/Pays d'origine
  
  #Ethnie
  
  
  #Numero Piece d'identide ou Numero d'identification Personnelle???
  
  #Code de la personne  : comment generer automatiquement un code 
  numericInput(inputId = "code",
               label= "Code", 
               value = " "),
  
  
  #Mode d'admission
  selectInput(inputId = "ma", 
              label = "Mode d'admission", 
              choices = list("Venu.e de lui-meme" , 
                             "Amene.e par les parents",
                             "Adresse.e par une autre structure",
                             "Amene.e par les sapeurs-pompiers",
                             "Référe.e avec ambulance")),
  plotOutput(outputId = "ma"),
  
  #Principaux symptômes/Principal symptome
  textInput(inputId = "PS", 
            label = "Symptomes", 
            value = " "), 
  
  plotOutput(outputId = "PS"),          
  
  #Préciser les informations sur la …/Diagnostic de la référence
  
  #Avis du medecion/specialiste qui a consulte
  textInput(inputId = "dm", 
            label = "Diagnostic", 
            value = " "), 
  
  textOutput(outputId = "dm"),
  
  #Diagnostic de la reference(de l'hopital qui a refere)
  textInput(inputId = "dmr", 
            label = "DiagnosticReference", 
            value = " "), 
  
  verbatimTextOutput(outputId = "dmr"),
  
  #Anamnèse (histoire de la maladie)
  
  numericInput(inputId = "duree", 
               label = "Duree", 
               value = " ",
               width = 3),
  
  plotOutput(outputId = "duree"),
  
  #Signes saignants de sa maladie
  textInput(inputId = "signessaignants", 
            label = " SS", 
            value = " "), 
  
  verbatimTextOutput(outputId = "signessaignants"),       
  
  #Signespositifs
  textInput(inputId = " signep ", 
            label = " SP ", 
            value = " "), 
  
  verbatimTextOutput(outputId = "signep"), 
  
  #Signesnégatifs
  
  textInput(inputId="signen", 
            label="SN", 
            value=" " ),
  verbatimTextOutput(outputId = "signen"), 
  
  #Déjà parcouru d’autres centres
  textInput(inputId = " centresparcourues", 
            label = " Centres Parcourus", 
            value = " "),
  
  verbatimTextOutput(outputId = "centresparcourues"),
  
  
  #Diagnostic du médecin
  textInput(inputId = "diagnostic", 
            label = " Diagnostic du medecin", 
            value = " "),
  verbatimTextOutput(outputId = "diagnostic"),
  
  #Traitement antérieur
  textInput(inputId = " traitements anterieurs", 
            label = " Traitements anterieurs", 
            value = " "),
  
  verbatimTextOutput(outputId = "traitements anterieurs"),
  
  #les antecedents
  #Antécédents médicaux: Fumer, Hypertendu, Des comportements sexuels a risque, Obésité, Diabétique
  selectInput(inputId = "antecedents", 
              label = "Antecedents", 
              choices = list("Fumeur/Fumeuse" , 
                             "Hypertendu.e",
                             "Comportements sexuels a risque",
                             "Obesite",
                             "Diabetique", 
                             "Autres")),
  
  
  #Antécédent chirurgicaux (année, raison, etc.)
  
  #Antécédents gynéco-obstétriques pour les femmes (réservés à la femme)
  
  ##Date des dernières règles
  wellPanel(
    dateInput("a", ""),
    submitButton( ) 
  ),
  
  ##Contraception en cours
  selectInput(inputId = "contraptionact", 
              label = "Contraception en cours", 
              choices = list(Oui = "1" , 
                             Non ="0")),
  
  
  #si oui a la question precedente, preciser la contraception en cours
  
  ##Nbre de grossesses
  numericInput(inputId = "nbgrosessesse", 
               label = "Nombre de Grossesses", 
               value = " ",),
  
  ##Nbre d’avortements
  numericInput(inputId = "nbavortements", 
               label = "Nombre d'Avortements", 
               value = " ",),
  
  
  ##Nbre d’enfants vivants
  numericInput(inputId = "nbenfanvivant", 
               label = "Nombre d'enfants vivants", 
               value = " ",),
  
  #Enquête sociale (l’enquête sociale rejoint les antécédents médicaux…. Si il fume, s’il boit, etc.)
  
  
  #Examen physique (varie d’un spécialiste a un autre)   
  
  #
  
  
  #Examen physique (varie d’un spécialiste a un autre) 
  textInput(inputId = "examen_physique", 
            label= "EP", 
            value = ""),
  
  
  #Résumé syndromique (Résumer ce que le malade a dit sous forme de syndromes ; regroupements des signes généraux et physiques en des syndromes cliniques.  
  #A partir de ceci, on dégage les hypothèses  )
  textInput(inputId = "resume_syndromique", 
            label= "RS", 
            value = ""),
  
  
  #Hypothèses diagnostic (Les classer du plus probable au moins probable )
  
  #Bilan paracliniques
  
  
  #Diagnostic retenu (Dire la maladie en question et préciser le stade)
  
  
  #Traitements
  ##Moyens médicaux 
  
  ##Moyens physiques
  
  
  ##Moyens médicamenteux :  , préciser les posologies, les voies d’administrations, 
  ##le nbre de jours/durée de traitement
  
  
  ##Moyens chirigucaux en même temps 
  
  
  
  
  #Elements de surveillance
  ##Température
  
  textInput(inputId = "temperature", 
            label= "Temperature", 
            value = ""),
  ##Poids
  textInput(inputId = "poids", 
            label= "Poids", 
            value = ""),
  ##Pulls
  textInput(inputId = "pulls", 
            label= "Pulls", 
            value = ""),
  
  
  ##Battements de cœur
  textInput(inputId = "battements_coeur", 
            label= "BattementsCoeurs", 
            value = ""),
  
  
  ##Eléments de surveillance paracliniques
  
  
  #Mode de sortie
  selectInput(inputId = "mode_sortie", 
              label = "Mode de sorties", 
              choices = list("Exeat (guerison/rentre.e sur ses deux pieds)" , 
                             "Decede.e",
                             "sortie contre avis medical",
                             "Transférer dans un autre service(dans le même hôpital)",
                             "Referer vers un autre hôpital")),
  
  
  #Creer une base de donnees qu'on peut exporter/utiliser,etc
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
