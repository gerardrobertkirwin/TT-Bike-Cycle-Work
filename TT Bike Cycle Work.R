#Download Data ------------------------------------------------------------

commute <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")

# Upload packages ---------------------------------------------------------

library(readxl)
library(tidyverse)
library(tidyverse)
library(ggplot2)
library(stringr)
library(ggmap)
library(httr)
library(rjson)
library(leaflet)
library(htmlwidgets)

# Clean up Data -----------------------------------------------------------
clean_commute <- commute %>% mutate(city=str_replace_all(city, " city", "")) %>%  
mutate(city=str_replace_all(city, " town", "")) %>%   
mutate(city=str_replace_all(city, " village", "")) %>%  
mutate(city=str_replace_all(city, " municipality", "")) %>%   
mutate(city=str_replace_all(city, " borough", ""))  %>% 
mutate(city=str_replace_all(city, " \\(balance\\)", "")) %>% 
mutate(city=str_replace_all(city, " urban county", "")) 




# find coords ----------------------------------

clean_commute <- clean_commute %>% 
mutate(address=str_c(city, state_abb, sep =", "))


geo.dsk <- function(addr){ 
  require(httr)
  require(rjson)
  url      <- "http://www.datasciencetoolkit.org/maps/api/geocode/json"
  response <- GET(url,query=list(sensor="FALSE",address=addr))
  json <- fromJSON(content(response,type="text"))
  loc  <- json['results'][[1]][[1]]$geometry$location
  return(c(address=addr,long=loc$lng, lat= loc$lat))
}
map_commute <- do.call(rbind,lapply(as.character(clean_commute$address),geo.dsk))
map_commute <- tbl_df(map_commute)
map_commute
tbl_df(clean_commute)
new_map_commute <- map_commute %>% 
inner_join(clean_commute) %>% 
distinct()

new_map_commute <- new_map_commute %>%  mutate(long = as.numeric(long)) %>% 
  mutate(lat=as.numeric(lat)) %>% 
  mutate(address=str_replace_all(city, "Juneau and", "Juneau")) %>% 
  filter(!is.na(address))

# plot coords ----------------------------------

new_map_commute$mode <- as.factor(new_map_commute$mode)



type <- c("green", "blue")[new_map_commute$mode]
icons <- awesomeIcons(
    icon = 'ios-close',
    library = "fa",
    markerColor = type, 
    iconColor = "black"
  )
legend_type <- c("darkolivegreen4", "deepskyblue2")[new_map_commute$mode]

pal <- colorFactor(palette = legend_type,
                     domain =new_map_commute$mode)


commute_map <- leaflet(new_map_commute) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addAwesomeMarkers(lng = ~long, lat = ~lat,
                    icon=icons,
                    clusterOptions=markerClusterOptions(showCoverageOnHover = FALSE),
                    popup = ~paste0(address, ":<br />", percent, "%"))  %>% 
  addLegend("bottomright", pal=pal, values=~mode, title="Mode of Transportation", opacity=1)

  
savewidget(widget=commute_map,
           file="commute_map.html",
           selfcontained=TRUE)
  
  
