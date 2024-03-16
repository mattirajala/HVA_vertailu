library(tidyverse)
library(httr)
library(jsonlite)



getRegions = function(){
  
  url = 'https://sotkanet.fi/rest//1.1/regions'
  
  resp = GET(url)
  d =  fromJSON(rawToChar(resp$content), flatten = T) %>% unnest_longer(memberOf, keep_empty = T)
  
  regions = d %>% 
    filter(category == "HYVINVOINTIALUE") %>% 
    select(id, code, title.fi) %>% 
    left_join(d[d$category == "KUNTA",c("id", "code", "title.fi", "memberOf")], by = c("id" = "memberOf"), suffix = c("_HVA", "_KUNTA"))
  
  return(regions)
}

Region_Names_HVA = function(){
  
  regions = getRegions()
  regions = regions %>% select(id, title.fi_HVA) %>% distinct()
  hva = regions$id %>% as.vector()
  names(hva) = regions$title.fi_HVA %>% as.vector() 
  hva = hva %>% sort()
  
  return(hva)
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

Indicator_Ids = function(){
  
  inds = getIndicators() %>% 
    select(id, title.fi) %>% 
    distinct() %>% arrange(title.fi)
  
  v = inds$id %>% as.vector()
  names(v) = inds$title.fi %>% as.vector()

  
  return(v)
  
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




