library(tidyverse)
library(httr)
library(jsonlite)



getRegions = function(){
  
  url = 'https://sotkanet.fi/rest//1.1/regions'
  
  resp = GET(url)
  resp$content
  d =  fromJSON(rawToChar(resp$content), flatten = T) %>% unnest_longer(memberOf, keep_empty = T)
  
  regions = d %>% 
    filter(category == "HYVINVOINTIALUE") %>% 
    select(id, code, title.fi) %>% 
    left_join(d[d$category == "KUNTA",c("id", "code", "title.fi", "memberOf")], by = c("id" = "memberOf"), suffix = c("_HVA", "_KUNTA"))
  
  return(regions)
}


getIndicators = function(){
  
  
  url = 'https://sotkanet.fi/rest/1.1/indicators'
  resp = GET(url)
  data = fromJSON(rawToChar(resp$content), flatten = T) %>% 
    unnest_longer(ncol(.), keep_empty = T) %>% 
    unnest_longer(ncol(.)-1, keep_empty = T) %>% 
    filter(classifications.region.values == "Kunta")
 
  return(data) 
}


getIndicatorData = function(indicator_id = 127, year = 2009, gender = 'total'){
  
  url = paste0('https://sotkanet.fi/rest/1.1/json?indicator=', indicator_id, paste0("&years=",year,collapse = ""), "&genders=", gender)
  resp = GET(url)
  df = fromJSON(rawToChar(resp$content))
  
  return(df)

}


getGroups = function(){
  
  url = 'https://sotkanet.fi/rest/1.1/groups'
  resp = GET(url)
  d = fromJSON(rawToChar(resp$content))
  f = d %>% 
    do.call(cbind, .) %>% 
    as.data.frame() %>% 
    unnest_wider(groups) %>% 
    unnest_longer(children) %>%
    select(-children_id)%>% 
    unnest_wider(children)
}
