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

groups = getGroups()

indikaattorit = getIndicators()

function(input, output, session) {
    
    
    output$group1 = renderUI({
        selectInput("Group1","Osa-alue", choices = unlist(groups[[1]][,'fi'], use.names = F))
    })
    
    output$group2 = renderUI({

        req(input$Group1)
        id = groups[[1]][groups[[1]]['fi'] == input$Group1,"id_1"] %>% unlist(use.names = F)
        inds = getGroupIndicators(id)
        
        selectInput("IND","Indikaattori", choices = indikaattorit$title.fi[indikaattorit$id %in% inds])

    })


    
    output$newPlot = renderPlot({
        
        req(input$IND)
        data = getIndicatorData(indicator_id = unique(indikaattorit$id[indikaattorit$title.fi == input$IND]),regions = input$HVA ,years = 2000:2023)
        data = data %>% 
            left_join(HVA_names, by=c('region' = 'id'))
        
        
        data %>% ggplot(aes(year, value, col = name))+geom_line(size=1.5) + theme_classic()
        
    })
    
    output$text = renderText({paste0("Alue: ",input$HVA)})
    output$text2 = renderText({
        req(input$IND)
        paste0("Indikaattori: ", input$IND)})

}
