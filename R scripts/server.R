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
    
    data = reactive({
        
        vuosi1 = input$VUOSI[1]
        vuosi2 = input$VUOSI[2]
        getIndicatorData(indicator_id = input$ID, years = vuosi1:vuosi2, regions = input$HVA)
        
    })
    
    output$newPlot = renderPlot({
        


        df = data() %>% 
            left_join(HVA_names, by=c('region' = 'id'))
        
        
        df %>% ggplot(aes(year, value, col = name))+
            geom_line(size=1.5) + 
            geom_point(size=4, fill = 'white', shape = 21) + 
            theme_classic() +
            scale_x_continuous(breaks =  seq(input$VUOSI[1], input$VUOSI[2], by = 1))+
            labs(x = 'Vuosi', y = 'Arvo')+
            theme(legend.text = element_text(size = 15),
                  legend.position = 'bottom',
                  axis.text = element_text(size = 15),
                  axis.title = element_text(size = 20))
        
    })
    
    output$table = renderTable({
        
        data() %>% 
            left_join(HVA_names, by=c('region' = 'id')) %>% 
            select(name, value, gender, year) %>% 
            pivot_wider(names_from = year, values_from = value)
        
        
    })
    
    output$text = renderText({paste0("Alue: ",input$HVA, input$VUOSI)})
    output$text2 = renderText({
        input$VUOSI[1]})

}
