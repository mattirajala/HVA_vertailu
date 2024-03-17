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
        selectInput("Group1","Osa-alue", choices = groups$fi) 
    })
    
    output$group2 = renderUI({
        
        req(input$Group1)
        inds = getGroupIndicators(groups$id[groups$fi == input$Group1])
        
        selectInput("Group2","Indikaattoriryhmä", choices = inds$inds.title.fi) 
    })
    
    output$indikaattori = renderUI({
        
        req(input$Group1)
        inds = getGroupIndicators(groups$id[groups$fi == input$Group1])
        
        req(input$Group2)
        indikaattorit_choice = indikaattorit%>% 
                                    filter(id %in% inds$inds.indicators_under_group[inds$inds.title.fi == input$Group2]) %>% 
                                    arrange(title.fi)

        selectInput("Indikaattori","Indikaattori", choices = indikaattorit_choice$title.fi)
    })
    
    output$newPlot = renderPlot({
        
        req(input$Indikaattori)
        data = getIndicatorData(indicator_id = unique(indikaattorit$id[indikaattorit$title.fi == input$Indikaattori]), year = 2000:2023)
        data = data %>% 
            filter(region %in% input$HVA) %>% 
            arrange(year) %>% 
            left_join(HVA_names, by=c('region' = 'id'))
        
        
        data %>% ggplot(aes(year, value, col = name))+geom_line(size=1.5) + theme_classic()
        
    })
    
    output$text = renderText({paste0("Alue: ",input$HVA)})
    output$text2 = renderText({
        req(input$Indikaattori)
        paste0("Indikaattori: ", unique(indikaattorit$id[indikaattorit$title.fi == input$Indikaattori]))})

}
