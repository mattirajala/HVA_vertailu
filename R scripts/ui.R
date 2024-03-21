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


alueet = Region_Names_HVA()
indikaattorit = Indicator_Ids()
# Define UI for application that draws a histogram
navbarPage(

    # Application title
    "Hyvinvointialueiden vertailu",
    
    tabPanel("Sotkanet", 
             
             sidebarLayout(
                 sidebarPanel(
                     
                     
                     selectInput("HVA", "Hyvinvointialue", alueet, multiple = T),
                     selectInput("ID", "Indikaattori", indikaattorit),
                     sliderInput("VUOSI", "Valitse vuosi", min = 2000, max = year(Sys.Date())-1, step = 1, value = c(2010,2023), sep = ""),
                     
                     
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     radioButtons("LAN", "Language", choices = c("FI" = "title.fi")),
                     plotOutput("newPlot"),
                     tableOutput("table"),
                     textOutput("text"),
                     textOutput("text2")
                     
                     
                     
                 )
             )
             
             )
)
