library(shiny)
library(ggplot2)
library(xlsx)
library(gdata)
library(readxl)
library(tidyverse)
library(shinydashboard)
library(shiny)
library(tidyverse)
library(shinyFiles)
library(devtools)
library(Rfacebook)
library(tm)
library(wordcloud)
library(stylo)
library(tidytext)
library(lubridate)
source("./auxFunctions.R")


fluidPage(
   titlePanel("Badogue's Figure download"),
   fileInput('file', 'Escolha o Arquivo EXCEL', accept=c('.xlsx')),
   downloadButton('indicesentimentos',"Download Indice de Sentimento"),
   tags$hr(),
   mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Lista de Palavras", downloadButton('palavras',"Download palavras mais usadas")),
        tabPanel("Lista de Palavras Negativas", downloadButton('palavrasnegativas',"Download palavras mais usadas em posts de polaridade negativa")),
        tabPanel("Wordcloud Negativo", downloadButton('wordcloudnegativo',"Download Wordcloud de palavras usadas em posts de polaridade negativa")),
        tabPanel("Lista de Palavras Positivas", downloadButton('palavraspositivas',"Download palavras mais usadas em posts de polaridade positiva")),
        tabPanel("Wordcloud Positivo", downloadButton('wordcloudpositivo',"Download Wordcloud de palavras usadas em posts de polaridade positiva")),
        tabPanel("Lista de Palavras Neutras", downloadButton('palavrasneutras',"Download palavras mais usadas em posts de polaridade neutra")),
        tabPanel("Wordcloud Neutro", downloadButton('wordcloudneutro',"Download Wordcloud de palavras usadas em posts de polaridade neutra"))
      )
   )
)

#
#
#
#library(shiny)
#
#shinyUI(fluidPage(
#  titlePanel("This is a scatterplot"),
#
#  sidebarLayout(
#    sidebarPanel(
#
#      fileInput('datafile', 'Choose CSV file',
#                accept=c('text/csv', 'text/comma-separated-values,text/plain')),
#
#      uiOutput("varselect1"),
#
#      uiOutput("varselect2"),
#
#      downloadButton('downloadPlot', 'Download Plot')
#
#      ),
#
#    mainPanel(          
#          h4("Here is your scatterplot"),
#          plotOutput("plot1")
#                  )
#      ))
#)
#
#
#fluidPage(
#  titlePanel("Badogue's Figure generator"),
#  sidebarLayout(
#    sidebarPanel(
#      fileInput('file', 'Escolha o Arquivo EXCEL',
#                 accept=c('.xlsx')),
#      downloadButton('downloadData', 'Download')
#    ),
#    mainPanel()
#    )
#  )
#

#fluidPage(
#   titlePanel("Badogue Sentimentos"),
#   
#   # Sidebar with controls to select the random distribution type
#   # and number of observations to generate. Note the use of the
#   # br() element to introduce extra vertical spacing
#   sidebarLayout(
#      sidebarPanel(
#         fileInput('file', 'Escolha o Arquivo EXCEL',
#                   accept=c('.xlsx')),
#         tags$hr(),
#         actionButton("do", "Badogar")
#      ),
#      mainPanel(
#         tabsetPanel(type = "tabs",
#                     tabPanel("Distribuição de Sentimentos", plotOutput("plotSentimentos")),
#                     tabPanel("Lista de Palavras", plotOutput("plotLista")),
#                     tabPanel("Palavras Positivas", plotOutput("plotPalavrasPositivas")),                     
#                     tabPanel("Palavras Negativas", plotOutput("plotPalavrasNegativas")),
#                     tabPanel("Palavras Neutras", plotOutput("plotPalavrasNeutras")),
#                     tabPanel("Nuvem de Palavras", plotOutput("plotNuvem")),
#                     tabPanel("Gênero da Audiência", plotOutput("plotGenero"))
#         )
#      )
#   )
#)
#
#

