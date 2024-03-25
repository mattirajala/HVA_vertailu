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

groups = getGroups()


function(input, output, session) {
    
    # HVA-nimet
    HVA_names = reactive({Region_Names_HVA(input$LAN, vector = F)})
    
    # Indikaattoridata
    data = reactive({
        
        vuosi1 = input$VUOSI[1]
        vuosi2 = input$VUOSI[2]
        getIndicatorData(indicator_id = indikaattori_ID()[input$ID] , years = vuosi1:vuosi2, regions = input$HVA)
        
    })
    
    # Aihepiirin valinta
    output$GROUP_ID = renderUI({
        
        selectInput("GROUP_ID", "Aihepiiri", Group_Ids(input$LAN)) 
        
    })
    
    # Osa-alueet --> määräytyy ensimmäisen aihepiirin avulla
    sub_groups = reactive({
        req(input$GROUP_ID)
        req(input$LAN)
        getGroupIndicators(input$GROUP_ID, input$LAN)})
    
    observeEvent(input$GROUP_ID, {
                
                updateSelectInput(inputId = "GROUP_ID_2", choices = sub_groups()$title)})
    
    # Indikaattorit --> määräytyy osa-alueen avulla
    indicators = reactive({
        
        req(input$GROUP_ID_2)
        getIndicators() %>% filter(id %in% unique(sub_groups()$inds.indicators_under_group[sub_groups()$title == input$GROUP_ID_2]))
        
    })
    
    observeEvent(input$GROUP_ID_2, {
        
        updateSelectInput(inputId = "ID", choices = indicators()[[input$LAN]])
        
    })
    
    # Indikaattoreiden ID:t --> tarvitaan datan haussa, haetaan valitun indikaattorin avulla
    indikaattori_ID = reactive({Indicator_Ids(input$LAN)})
    
    # Kuvaaja
    output$newPlot = renderPlot({
        


        df = data() %>% 
            left_join(HVA_names(), by=c('region' = 'id'))
        
        
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
            left_join(HVA_names(), by=c('region' = 'id')) %>% 
            select(name, value, gender, year) %>% 
            pivot_wider(names_from = year, values_from = value)
        
        
    }, striped = T)
    
    
    output$text = renderText({paste0("Alue: ",input$HVA, input$VUOSI)})
    output$text2 = renderText({
        input$VUOSI[1]})
    
    output$HVA_valinta = renderUI({
        
        selectInput("HVA", "Hyvinvointialue", Region_Names_HVA(input$LAN), multiple = T) 
        
    })
    
    

}
