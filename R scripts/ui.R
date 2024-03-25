#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
source('data.R')


indikaattorit = Indicator_Ids()
# Define UI for application that draws a histogram
navbarPage(

    # Application title
    "Hyvinvointialueiden vertailu",
    theme =  bslib::bs_theme(bootswatch = 'flatly'),
    tabPanel("Sotkanet", 
             
             sidebarLayout(
                 sidebarPanel(
                     
                     
                     # selectInput("HVA", "Hyvinvointialue", alueet, multiple = T),
                     uiOutput('HVA_valinta'),
                     uiOutput('GROUP_ID'),
                     selectInput("GROUP_ID_2", "Osa-alue", choices = NULL),
                     selectInput("ID", "Indikaattori", choices = NULL),
                     # selectInput("ID_2", "Indikaattori", indikaattorit),
                     sliderInput("VUOSI", "Valitse vuosi", min = 2000, max = year(Sys.Date())-1, step = 1, value = c(2010,2023), sep = ""),
                     radioButtons("LAN", "Language", choices = c("FI" = "title.fi", "SV" = "title.sv", 'EN' = 'title.en'))
                     
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                    
                     plotOutput("newPlot", width = 'auto', height = 600),
                     tableOutput("table"),
                     textOutput("text"),
                     textOutput("text2")
                     
                     
                     
                 )
             )
             
             )
)
