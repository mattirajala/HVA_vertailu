#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
function(input, output, session) {

    
    output$newPlot = renderPlot({
        
        
        data = getIndicatorData(indicator_id = input$IND, year = 2000:2020)
        data = data %>% 
            filter(region == input$HVA) %>% 
            arrange(year)
        
        
        data %>% ggplot(aes(year, value))+geom_line() + theme_classic()
        
    })
    
    output$text = renderText({paste0("Alue: ",input$HVA)})
    output$text2 = renderText({paste0("Indikaattori: ", input$IND)})

}
