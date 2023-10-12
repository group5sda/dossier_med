####Call for library
library(readxl)
library(ggplot2)
library(forcats)
library(hrbrthemes)
library(gsheet)#to call my data set placed in my google sheet
####Apprécier les résultats de la rééducation orthophonique

#call dataappreci
datap<-readxl::read_excel("D:/Sciences_de_donnees_Formation/GAPP/datap.xlsx")
View(datap)
head(datap)
summary(datap)

### Barplot 
# 1: uniform color. Color is for the border, fill is for the inside
ggplot(datap, aes(x=as.factor(nombre_seances_faites) )) +
  geom_bar(color="blue", fill=rgb(0.1, 0.4, 0.5, 0.7) )



###Scaterplot
#first try
# A basic scatterplot with color depending on Species
ggplot(datap, aes(x=anciennete_trouble_enjours, y=nombre_seances_faites, color=prescripteur_pcorthophonique)) + 
  geom_point(size=6) + theme_ipsum()


#scond try
# Transparency
ggplot(datap, aes(x=anciennete_trouble_enjours, y=nombre_seances_faites, alpha= prescripteur_pcorthophonique)) + 
  geom_point(size=6, color="#69b3a2") +
  theme_ipsum()

# Shape
ggplot(datap, aes(x=anciennete_trouble_enjours, y=nombre_seances_faites, shape=prescripteur_pcorthophonique)) + 
  geom_point(size=6) +
  theme_ipsum()

# Size
ggplot(datap, aes(x=anciennete_trouble_enjours, y=nombre_seances_faites, size=prescripteur_pcorthophonique)) + 
  geom_point(size=6) +
  theme_ipsum()

# A basic scatterplot with color depending on Species
ggplot(datap, aes(x=anciennete_trouble_enjours, y=nombre_seances_faites, shape=prescripteur_pcorthophonique, alpha=prescripteur_pcorthophonique, size=prescripteur_pcorthophonique, color=prescripteur_pcorthophonique)) + 
  geom_point() +
  theme_ipsum()

#scatterplot with rug
# plot
ggplot(data=datap, aes(x=anciennete_trouble_enjours, nombre_seances_faites)) +
  geom_point() +
  geom_rug(col="steelblue",alpha=0.1, size=1.5)



###Déterminer les fréquences des différents troubles acquis de la communication verbale 
#et leurs étiologies chez ces patients





####summary base de donnees
data <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1h4zkDUWoqukezbfZqER2UC-92WhZEPSJcR3oiRGCkS4/edit?usp=sharing")

summary(data)
head(data$adresse)

