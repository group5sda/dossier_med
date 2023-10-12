#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Define UI for application that draws a histogram
library(shiny)
library(shinyTime)
library(ggplot2)
library(tidyverse)
library(babynames)
library(timeDate)
library(shinythemes)
library(DT)
library(flexdashboard)
library(leaflet)
library(shinydashboard)
library(gsheet)#to call my data set placed in my google sheet
library(rbokeh)
#library(plotly)


########Call data for application###############
################################################

data <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1h4zkDUWoqukezbfZqER2UC-92WhZEPSJcR3oiRGCkS4/edit?usp=sharing")

#############################
##################### Let's data part#################
#content5 for antecedents

ante <- data.frame(data$antecedentsmedicaux, data$hypertensionart, data$diabete, data$hemoglobinopathie, data$cardiopathie, data$fumeur_tabac, data$autresantecends_medical, data$preciser_autresantecends_medical, data$ac_neurochirurgie)

avcdata <- data.frame(data$accident_vasculaire_cerebral, data$territoire_avc, data$traumatisme_cranien, data$resultats_tdm)

tbord <- data.frame(data$accident_vasculaire_cerebral, data$territoire_avc, data$traumatisme_cranien, data$resultats_tdm, data$tumeur_cerebrale, data$neuropaludisme, data$maladie_neurodegenerative, data$nombre_seances_faites,data$prescripteur_pcorthophonique , data$anciennete_trouble_enjours, data$duree_reeducation_ensemaines)


infog <- data.frame(data$age, data$sexe, data$adresse, data$nationalite, data$ethnie, data$autrelanguesparles, data$religion, data$profession, data$prisenecharge, data$niveau_instruction, data$situation_matrimoniale, data$lateralite)

Data<- data.frame(data$resultats, data$age, data$anciennete_trouble_enjours,data$nombre_seances_faites, data$duree_reeducation_ensemaines)
################################################

## Define UI for application 
ui <- fluidPage (
  
  ################################################
  
  shinythemes::themeSelector(), 
  
  ####################################
  navbarPage(
    windowTitle = "MedicalApp",
    fluid = TRUE,
    title = "Mon Dossier Medical"),
  
  ###############################Differentes parties de l'application#####################
  #############################################################
  #First part
  navbarPage( title = "Informations generales",
              tabPanel("Date de la consultation", 
                       dateInput(inputId = "date", 
                                 label = "Date",
                                 format = "yy-mm-dd",
                                 weekstart = " "), ),
              
              tabPanel( "Nom de l'hopital", 
                        textInput(inputId = "identifian",
                                  label = "Nom de l'hopital:", 
                                  value = " "),)
              
              
  ),
  
  #second part
  navbarPage( title = "Identité du/de la patient.e.",
              tabPanel("Nom du Patient.e", 
                       textInput(inputId = "name",
                                 label = "Name :", 
                                 value = " ") 
              ),
              
              
              tabPanel("Age", 
                       
                       numericInput(inputId = "age",
                                    label= "Age", 
                                    value = " " ), 
                       
              ),
              
              tabPanel("Sexe", 
                       
                       selectInput(inputId = "sexe", 
                                   label = "Sexe", 
                                   choices = list(Female = "F", 
                                                  Male = "M")),
              ),
              
              tabPanel("Adresse", 
                       
                       textInput(inputId = "adresse",
                                 label = " Adresse:", 
                                 value = " ") 
              ),
              
              tabPanel( "Nationalite", 
                        
                        textInput(inputId = "nationalite",
                                  label = "Nationalite :", 
                                  value = " "),
              ),
              
              tabPanel( "Ethnie", 
                        
                        selectInput(inputId = "ethnie",
                                    label = "Ethnie", 
                                    choices = list(Fon = "Fon", 
                                                   Mina = "Mina", 
                                                   Adja = "Adja", 
                                                   Idatcha ="Idatcha", 
                                                   Mahi = "Mahi", 
                                                   Yoruba = "Yoruba")),
              ),
              
              tabPanel( "Autres langues ", 
                        
                        textInput(inputId = "autreslanguesparles",
                                  label = "Autres langues :", 
                                  value = " "),
              ),
              
              tabPanel( "Profession", 
                        
                        textInput(inputId = "profession",
                                  label = "Profession :", 
                                  value = " "),
              ),
              
              tabPanel( "Prise en charge", 
                        
                        selectInput(inputId = "priseencharge",
                                    label = "Prise en charge", 
                                    choices = list(Oui = "1",
                                                   Non = "2") ),
              ),
              
              tabPanel( "Niveau d'instruction", 
                        
                        selectInput(inputId = "niveau_instruction",
                                    label = "Niveau d'instruction", 
                                    choices = list(Primaire = "1",
                                                   Secondaire = "2",
                                                   Universitaire = "3",
                                                   NonScolarise = "4" )),
              ),
              
              tabPanel("Situation matrimoniale", 
                       selectInput(inputId = "situation_matrimoniale", 
                                   label = "Situation matrimoniale", 
                                   choices = list(Marie.eouUL = "M", 
                                                  Divorce.e = "D",
                                                  Celibataire = "C",
                                                  Celibataireavecenfant = "CavE",
                                                  Veuf.ve = "Veuf")),
              ),  
              
              tabPanel("Lateralite", 
                       selectInput(inputId = "lateralite", 
                                   label = "Lateralite", 
                                   choices = list(Droite = "Droite", 
                                                  Gauche = "Gauche",
                                                  NonPrecise = "NonPrecise")),
              ),  
              
              
              tabPanel("Data of this section", DT::dataTableOutput("data")),
              
              ##Use NavbarMenu 
              navbarMenu("Menu Options",
                         tabPanel("Menu item A - Summary stats", verbatimTextOutput("summary")),
                         tabPanel("Menu item B - Link to code",))
              
              
              
              
              
  ),
  #####Thrisd part: Motif d'admission
  navbarPage( title = "Motif d'admission", 
              tabPanel("Mode d'admission",
                       selectInput(inputId = "ma", 
                                   label = "Mode d'admission", 
                                   choices = list("Venu.e de lui-meme" , 
                                                  "Amene.e par les parents",
                                                  "Adresse.e par une autre structure",
                                                  "Amene.e par les sapeurs-pompiers",
                                                  "Référe.e avec ambulance")),
              ),
              
              
              tabPanel("Principaux symptômes", 
                       textInput(inputId = "PS", 
                                 label = "Symptomes :", 
                                 value = " "),
              ),
              ######Preciser les informations concernant le Diagnostic de la référence
              tabPanel("Avis du medecion/specialiste qui a consulte",
                       textInput(inputId = "dm", 
                                 label = "Diagnostic :", 
                                 value = " "), 
              ),
              
              tabPanel("Diagnostic de l'hopital de Reference", 
                       textInput(inputId = "dmr", 
                                 label = "DiagnosticReference :", 
                                 value = " "), 
              ),
              
              
  ),
  #####Four part: Anamnèse (histoire de la maladie)
  navbarPage( title = "Anamnese (histoire) de la maladie",
              tabPanel("Duree ecoulee/Combien de jours, etc.?", 
                       numericInput(inputId = "duree", 
                                    label = "Duree", 
                                    value = " ",
                                    width = 3),
              ),
              
              tabPanel("Signes saignants de sa maladie",
                       textInput(inputId = "signessaignants", 
                                 label = " SS :", 
                                 value = " "), 
                       
              ),
              
              tabPanel("Signes positifs", 
                       textInput(inputId = " signep ", 
                                 label = " SP :", 
                                 value = " "), 
                       
              ),
              
              tabPanel("signes negatifs", 
                       textInput(inputId="signen", 
                                 label="SN :", 
                                 value=" " ),
              ),
              
              tabPanel("hopitaux deja parcourus",
                       textInput(inputId = " centresparcourues", 
                                 label = " Centres Parcourus:", 
                                 value = " "),
              ),
              
              tabPanel("Diagnostic du medecin", 
                       textInput(inputId = "diagnostic", 
                                 label = " Diagnostic du medecin:", 
                                 value = " "),
                       
              ),
              
              
  ),
  
  #####Fifth part: Traitements anterieurs
  navbarPage(title = "Traitements anterieurs", 
             tabPanel("Traitements medicaux anterieurs",
                      textInput(inputId = " traitements medicaux anterieurs", 
                                label = " Traitements medicaux anterieurs:", 
                                value = " "),
             ),
             tabPanel("Autres traitements anterieurs",
                      textInput(inputId = " autres traitements anterieurs", 
                                label = " Autres traitements anterieurs:", 
                                value = " "),      
                      
             )
             
             
             
             
  ),
  
  #### sixeth part: les antecedents
  navbarPage(title = "Antecedents", 
             
             ###Les donnees sur les antecedents
             tabPanel("Donnees sur les antecedents", DT::dataTableOutput("Data")),
             
             tabPanel("Antecedents medicaux",
                      selectInput(inputId = "antecedentsmedicaux",
                                  label = "Antecedents medicaux",
                                  choices = list(Oui = "1",
                                                 Non = "2")),
             ),
             tabPanel("Preciser l'antecedent medical",
                      selectInput(inputId = "antecedents", 
                                  label = "Antecedents", 
                                  choices = list("Fumeur_tabac" , 
                                                 "Hypertenionarterielle",
                                                 "Comportements sexuels a risque",
                                                 "Obesite",
                                                 "Diabetie",
                                                 "cardiopathie",
                                                 "hemoglobinopathie",
                                                 "Autres")),
             ),
             tabPanel("Antecedents neurochirulgies",
                      selectInput(inputId = "ac_neurochirulgie",
                                  label = "Antecedents neurochirulgies",
                                  choices = list(Oui = "1",
                                                 Non = "2")),
             ),
             ###Antécédents gynéco-obstétriques pour les femmes (réservés à la femme)
             
             tabsetPanel(
               
               tabPanel("Date des dernieres regles", 
                        wellPanel(
                          dateInput("a", ""),
                          submitButton( ) 
                        ),),
               
               tabPanel("Contraceptif en cours", selectInput(inputId = "contraptionact", 
                                                             label = "Contraception en cours", 
                                                             choices = list(Oui = "1" , 
                                                                            Non ="2")),)
               
             ),
             tabPanel("Nombres de grossesses", numericInput(inputId = "nbgrosessesse", 
                                                            label = "Nombre de Grossesses", 
                                                            value = " ",),),
             
             tabPanel("Nombre d'avortements/fausses couches", numericInput(inputId = "nbavortements", 
                                                                           label = "Nombre d'Avortements", 
                                                                           value = " ",),),
             
             tabPanel("Nombre d'enfants vivants", numericInput(inputId = "nbenfanvivant", 
                                                               label = "Nombre d'enfants vivants", 
                                                               value = " ",),)
             
             
             
             
  ),
  ####Seventh part: Examen physique (varie d’un spécialiste a un autre)   
  navbarPage(title = "Examen physique", ),
  
  #####eight part: Resume syndromique
  
  navbarPage(title = "Resume syndromique", 
             tabPanel("Signes generaux", textAreaInput(inputId = "signes generaux", 
                                                 label = "Signes genraux", 
                                                 value = "", ), ),
             
             tabPanel("signes physiques", textAreaInput(inputId = "signes physiques", 
                                                        label = "Signes physiques", 
                                                        value = "", ), ),
             
             tabPanel("syndromes cliniques",  textAreaInput(inputId = "syndromes cliniques", 
                                                            label = "syndromes cliniques", 
                                                            value = "", ), )
             
             
             
             ),
  
  #####nine part: Hypotheses diagnostic
  
  navbarPage(title = "Hypothese diagnostic", 
             
             
             tabPanel("Hypotheses plus probables",  textAreaInput(inputId = "hypotheses plus probables", 
                                                            label = "Hypotheses plus probables", 
                                                            value = "", ), ),
             
             tabPanel("Hypotheses moins probables",  textAreaInput(inputId = "hypotheses moins probables", 
                                                                  label = "Hypotheses moins probables", 
                                                                  value = "", ), )
             
             ),
  
  #####ten part: Bilan paracliniques
  
  navbarPage(title = "Bilan paraclinique", 
             
             tabPanel("Analyses de Laboratoire", 
                         ),
             
             
             tabPanel("Examens d’imagerie ",  )
             
             
             ),
  
  #####eleven part: Diagnostic retenu
  
  navbarPage(title = "Diagnostic retenu", 
             
              tabPanel("Diagnostic", textAreaInput(inputId = "diagnostic retenu", 
                                                   label = "Diagnostic retenu", 
                                                   value = "", ), )
             
             
             ),
  
  #####twelve part: Traitements
  
  navbarPage(title = "Traitements",
             
             tabPanel("Moyens Physiques",  
                      textAreaInput(inputId = "moyens physiques", 
                                    label = "Moyens Physiques", 
                                    value = "", ), ),
             
             tabPanel("Moyens medicamenteux" ,  textAreaInput(inputId = "moyens medicamenteux", 
                                                              label = "Moyens Medicamenteux", 
                                                              value = "", ), ),
             
             tabPanel("Moyens chirigucaux",  textAreaInput(inputId = "moyens chirugicaux", 
                                                           label = "Moyens Chirigicaux", 
                                                           value = "", ), )
             
             
             ),
  
  #### part: Eléments de surveillance
  navbarPage(title = "Elements de surveillance", 
             
             
             tabPanel("Température",  numericInput(inputId = "temperature",
                                                   label = "Temperature", 
                                                   value = " ", ) 
                      ), 
             
             tabPanel("Poids",  numericInput(inputId = "poids",
                                             label = "Poids", 
                                             value = " ", )
                                             
                                             ),
                      
                      
             
             
             tabPanel("Pulls",  numericInput(inputId = "pulls",
                                             label = "Pulls", 
                                             value = " ", )
                                             ),
                      
                      
         
             tabPanel("Battements de cœur",  numericInput(inputId = "nbrebattementsdecoeur",
                                                          label = "Nombre de battements de coeur", 
                                                          value = " ", )
                                                          ),
             
             
             tabPanel("Eléments de surveillance paracliniques", )
             
             ),
  
  
  #### part: Mode de sortie
  navbarPage(title = "Mode de sortie", 
             tabPanel( "Mode de sortie", 
                       selectInput(inputId = "mode_sortie", 
                                   label = "Mode de sorties", 
                                   choices = list("Exeat (guerison/rentre.e sur ses deux pieds)" , 
                                                  "Decede.e",
                                                  "sortie contre avis medical",
                                                  "Transférer dans un autre service(dans le même hôpital)",
                                                  "Referer vers un autre hôpital") ),
             )
             
             
             
             
             
             
             
  ),
  ###About some analysis
  #### part: Certaines analyses
  ### Tableau de bord
  dashboardPage(
    dashboardHeader(title = "Tableau de Bord"),
    dashboardSidebar(
      sliderInput("resultats", "please choose",
                  min = 0, max = 252, value = 3, step = 1,  
      ),
      sidebarMenu(
        menuItem("fiche", tabName = "fiche"),
        menuItem("age", tabName = "age"),
        menuItem("identifian", tabName = "identifian"),
        menuItem("territoire_avc", tabName = "AVC"),
        menuItem("neuropaludisme", tabName = "neuropaludisme"),
        menuItem("traumatisme_cranien", tabName = "traumatisme_cranien")
        
      )
    ),
    
    dashboardBody(
      tabItems(
        tabItem("age",
                fluidRow(
                  valueBoxOutput("summary"),
                  valueBoxOutput("plot")
                  
                ),
                
                box(
                  width = 4, status = "info",
                  title = "Ages des patients",
                  tableOutput("tbord$age")
                )
        )
      ),
      tabItem("neuropaludisme",
              numericInput("maxrows", "Rows to show", 25),
              verbatimTextOutput("neuropaludisme"),
              downloadButton("downloadCsv", "Download as CSV")    
      )
    )
  )
  
  
  
  #####
)

# Define server logic required 
server <- function(input, output) {
  
  # for display of mtcars dataset in the "Data Page"
  output$data <- DT::renderDataTable ( DT::datatable({
    infog 
  }) 
  )
  
  # for display of histogram in the "Widget & Sidepar page"
  output$plot <- renderPlot({
    hist(infog$age , col ="blue", breaks=input$b )
  })
  
  # for display of mtcars dataset summary statistics in the "Menu item A page"
  output$summary <- renderPrint ({
    summary(infog)
    
  })
  #For display histogram for antecedent data named ante
  output$Data <- DT::renderDataTable ( DT::datatable({
    ante 
  }) 
  )
  ###
  output$summary <- renderPrint ({
    summary(ante)
  })
  ###
  output$plot <- renderDT(DT::datatable({
    ante 
  }) )
  ###########Some analysis on data##########
  ###Tableau de bord
  
  output$count <- renderValueBox({
    valueBox(
      value = count(),
      subtitle = " Total",
      icon = icon("")
    )
  })
  
  
  #####
}
# Run the application 
shinyApp(ui = ui, server = server)
