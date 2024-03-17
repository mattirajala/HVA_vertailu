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
                     uiOutput('group1'),
                     uiOutput('group2'),
                     uiOutput('indikaattori')
                     
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                     plotOutput("newPlot"),
                     textOutput("text"),
                     textOutput("text2")
                     
                     
                     
                 )
             )
             
             )
)
