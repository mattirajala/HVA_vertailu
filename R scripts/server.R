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

HVA_names = getRegions() %>% 
    select(id, title.fi_HVA) %>% 
    distinct() %>% 
    rename(name = title.fi_HVA)
function(input, output, session) {

    
    output$newPlot = renderPlot({
        
        
        data = getIndicatorData(indicator_id = input$IND, year = 2000:2023)
        data = data %>% 
            filter(region %in% input$HVA) %>% 
            arrange(year) %>% 
            left_join(HVA_names, by=c('region' = 'id'))
        
        
        data %>% ggplot(aes(year, value, col = name))+geom_line(size=1.5) + theme_classic()
        
    })
    
    output$text = renderText({paste0("Alue: ",input$HVA)})
    output$text2 = renderText({paste0("Indikaattori: ", input$IND)})

}
