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
                     sliderInput("bins",
                                 "Number of bins:",
                                 min = 1,
                                 max = 50,
                                 value = 30),
                     
                     selectInput("HVA", "Hyvinvointialue", alueet),
                     selectInput("IND", "Indikaattori", indikaattorit)
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
