
finalMap <- function(SC_map,Treat_map,control_ID)
{  
            
            Sweden_map <- readRDS("./data/Swedish_Kommun_shp")
            greenLeafIcon <- makeIcon(
                        iconUrl = "./data/leaf-green.png",
                        iconWidth = 19, iconHeight = 48,
                        iconAnchorX = 11, iconAnchorY = 47)
            
            IkeaIcon <- makeIcon(
                        iconUrl = "./data/Logo.png",
                        iconWidth = 28, iconHeight = 30,
                        iconAnchorX = 15, iconAnchorY = 32)
            
            # Visualization
            map <- leaflet(SC_map) %>% addTiles() %>% setView(lng = 15, lat = 62.5, zoom = 5) %>%
                        # SC units
                        addPolygons(stroke = T,weight = 1, fillOpacity = ~ Opacity, smoothFactor = 0.5,
                                    fillColor = "red") %>%
                        
                        addMarkers( lng = ~lng, lat = ~lat, 
                                    popup = ~paste0(as.character(KnNamn),"_",as.character( Weight)), 
                                    icon = greenLeafIcon) %>%
                        # Treat unit
                        addPolygons(data = Treat_map, stroke = T, weight = 1.5, fillOpacity = 1, smoothFactor = 0.1,
                                    fillColor = "yellow",popup = ~as.character(KnNamn)) %>%
                        addMarkers( data = Treat_map, lng = ~lng, lat = ~lat,
                                    popup = ~as.character(KnNamn), icon =IkeaIcon) %>%
                        
                        # pool units 
                        addPolygons(data =  subset(Sweden_map, as.numeric(KnKod) %in% (control_ID)),
                                    stroke = T, weight = 1, fillOpacity = 0.05, smoothFactor = 0.1,
                                    fillColor = "blue",popup = ~as.character(KnNamn))
            
            return(map)
}
