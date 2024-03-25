library(tidyverse)
library(httr)
library(jsonlite)


# Aluetiedot
getRegions = function(lan = 'title.fi'){
  
  url = 'https://sotkanet.fi/rest//1.1/regions'
  
  resp = GET(url)
  d =  fromJSON(rawToChar(resp$content), flatten = T) %>% unnest_longer(memberOf, keep_empty = T)
  
  regions = d %>% 
    filter(category == "HYVINVOINTIALUE") %>% 
    select(id, code, {{lan}}) %>% 
    left_join(d[d$category == "KUNTA",c("id", "code", lan, "memberOf")], by = c("id" = "memberOf"), suffix = c("_HVA", "_KUNTA"))
  
  return(regions)
}

# HVA:den nimet ja koodit
Region_Names_HVA = function(lan = 'title.fi', vector = TRUE){
  
  regions = getRegions(lan)
  regions = regions %>% select(1, 3) %>% 
    distinct() %>% 
    arrange(pick(contains("HVA")))
  
  if (vector) {
    
    hva = regions$id %>% as.vector()
    names(hva) = regions[[2]] %>% as.vector() 
  
    }else{
    
    hva = regions %>% rename(name = 2)
    
  }

  return(hva)
}
 

# Indikaattorilistaus
getIndicators = function(){
  
  
  url = 'https://sotkanet.fi/rest/1.1/indicators'
  resp = GET(url)
  data = fromJSON(rawToChar(resp$content), flatten = T) 
 
  return(data) 
}

# Indikaattoreiden ID:t
Indicator_Ids = function(lan = 'title.fi'){
  
  inds = getIndicators() %>% 
    select(id, {{lan}}) %>% 
    distinct() %>% arrange(.data[[lan]])
  
  v = inds$id %>% as.vector()
  names(v) = inds[[lan]] %>% as.vector()

  
  return(v)
  
}


# Indikaattoridata
getIndicatorData = function(indicator_id = 127, years = 2009, gender = 'total', regions = 658){
  
  url = paste0('https://sotkanet.fi/rest/1.1/json?indicator=', indicator_id, paste0("&years=",years,collapse = ""), "&genders=", gender)
  resp = GET(url)
  
  if (resp$status_code == 200) {
    
    df = fromJSON(rawToChar(resp$content))
    df = df %>% 
      filter(region %in% regions) %>% 
      arrange(year)
    
    return(df)
    
  }else{
    
    return(NULL)
    
  }
  


}


getGroups = function(){
  
  url = 'https://sotkanet.fi/rest/1.1/groups'
  resp = GET(url)
  d = fromJSON(rawToChar(resp$content))
  f = d %>% 
    do.call(cbind, .) %>% 
    as.data.frame() %>% 
    unnest_wider(groups) %>% 
    unnest_wider(title) %>% 
    select(-description) %>% 
    rename(id_1 = id) %>% 
    filter(children != "NULL")
  
  # f_2 = f %>% select(id_1, children) %>% 
  #   unnest_longer(children) %>% 
  #   unnest_wider(children) %>%
  #   unnest_wider(title) %>% 
  #   select(-description, -children_id) %>% 
  #   rename(id_2 = id) %>% 
  #   unnest_longer(indicators, keep_empty = T)
  # 
  # 
  # f_3 = f_2 %>% select(id_2, children) %>% 
  #   unnest_longer(children) %>% 
  #   unnest_wider(children) %>% 
  #   unnest_wider(title) %>% 
  #   select(-description, -children_id) %>% 
  #   rename(id_3 = id) %>% 
  #   unnest_longer(indicators, keep_empty = T)
  # 
  # f_4 = f_3 %>% select(id_3, children) %>% 
  #   unnest_longer(children) %>% 
  #   unnest_wider(children) %>% 
  #   unnest_wider(title) %>% 
  #   select(-description, -children_id) %>% 
  #   rename(id_4 = id) %>% 
  #   unnest_longer(indicators, keep_empty = T)
  # 
  # f_5 = f_4 %>% select(id_4, children) %>% 
  #   unnest_longer(children) %>% 
  #   unnest_wider(children) %>% 
  #   unnest_wider(title) %>% 
  #   select(-description, -children_id) %>% 
  #   rename(id_5 = id) %>% 
  #   unnest_longer(indicators, keep_empty = T)
  # 
  # groups_list = list()
  # groups_list[[1]] = f
  # groups_list[[2]] = f_2
  # groups_list[[3]] = f_3
  # groups_list[[4]] = f_4
  # groups_list[[5]] = f_5
  # 
  return(f)
}

Group_Ids = function(lan = 'title.fi'){
  
  lan = str_split(lan, "\\.", simplify = T)[,2]
  
  df = getGroups() %>% select({{lan}}, id_1) %>% arrange(.data[[lan]])
  
  groups = df$id_1 %>% as.vector()
  names(groups) =  df[[lan]] %>% as.vector()
  
  return(groups)
  
}

getGroupIndicators = function(group_id = 358, lan = 'title.fi'){
  
  url = paste0('https://sotkanet.fi/sotkanet/api/group/', group_id)
  resp = GET(url)
  d = fromJSON(rawToChar(resp$content))
  lan = paste0('inds.', lan)
  df = data.frame(inds = jsonlite::flatten(d$groups_of_group)) %>%
    unnest_longer(inds.indicators_under_group) %>% 
    select(1,2, {{lan}}) %>% 
    rename('title' = 3)
  
  # return(d$indicators_under_group)
  return(df)
}



