



Unit_map <- function(weight_data, treat_ID) {
            Sweden_map <- readRDS("./data/Swedish_Kommun_shp")
            # Selected Synthetic control units
            SC_unit <- subset(Sweden_map, as.numeric(KnKod) %in% (weight_data$KnKod))
            
            SC_unit@data$Weight <- sapply(as.numeric(SC_unit@data$KnKod),
                                          function(x) {
                                                      weight_data[(weight_data$KnKod == x),]$w.weight
                                          })
            
            SC_unit@data$Opacity <- sapply(SC_unit@data$Weight ,
                                           function(x) {
                                                       if (x < 0.2) {
                                                                   0.4
                                                       } else if (x >= 0.2 & x < 0.5) {
                                                                   0.7
                                                       } else {
                                                                   1
                                                       }
                                           })
            
            # Treated unit
            Treat_unit <- subset(Sweden_map, as.numeric(KnKod) == treat_ID)
            
            # Return
            return(list("TreatMap" = Treat_unit, "SC_Map" = SC_unit ))
            
}



#result <- Unit_map(weight_data)

