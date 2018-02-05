



Func_clusterPlot <- function(somData, comparable_Var){
            
            
            var_th <- grep(pattern = paste0(".", "scale"), x = colnames(somData) )
            
            #function of pair
            plotPair <- function(somData, var_th2){
                        p0 <-  plot_ly(data = somData, 
                                       x = ~somData[,var_th2[1]] , y = ~somData[,var_th2[2]], 
                                       opacity = 0.8,
                                       color = ~as.factor(cluster), 
                                       type = 'scatter',
                                       mode = 'markers', 
                                       symbol = ~as.factor(cluster), 
                                       marker = list(size = 10),
                                       mode = "markers+text" ,
                                       text = ~somData[,2] , textposition = 'middle right'
                        ) %>% layout( 
                                    xaxis = list( title = colnames(somData)[var_th2[1]],  showgrid = T,showline = F),      
                                    yaxis = list( title = colnames(somData)[var_th2[2]],  showgrid = T,showline = F) )
                        return(p0)
                        
            }
            
            plotSingle <- function(somData, var_th1){
                        p0 <-  plot_ly(data = somData, 
                                       x = ~somData[,var_th1] , 
                                       opacity = 0.8,
                                       color = ~as.factor(cluster), 
                                       type = 'scatter',
                                       mode = 'markers', 
                                       symbol = ~as.factor(cluster), 
                                       marker = list(size = 10),
                                       mode = "markers+text" ,
                                       text = ~somData[,2] , textposition = 'middle right'
                        ) %>% layout( 
                                    xaxis = list( title = colnames(somData)[var_th1],  showgrid = T,showline = F),      
                                    yaxis = list( title = "hist",  showgrid = T,showline = F) 
                                    )
                        return(p0)
                        
            }
            
            
            
            if (length(var_th) == 1){
                        plot0 <- plotSingle(somData, var_th1 = var_th)
            } else if (length(var_th) == 2){
                        plot0 <- plotPair(somData, var_th2 = var_th)
                        
            } else {
                        pairNumber <- floor(length(var_th)/2)
                        singleNumber <- length(var_th)%%2
                        
                        for (i in 1:pairNumber){
                                    a <- 2*i -1; b <-  a + 1
                                    assign(paste0("plot",i), plotPair(somData = somData, var_th2 = c(var_th[a], var_th[b]) ))
                        }
                        
                        if (singleNumber == 1){
                                    assign(paste0("plot", pairNumber + singleNumber), plotSingle(somData = somData, var_th1 = var_th[length(var_th)] ))
                                    
                        }
                        
                        totalPlot <- pairNumber + singleNumber
                        namesplot <- paste0("plot", 1:totalPlot)
                        args.plot <- list()
                        for(i in 1:totalPlot){ args.plot[[i]] <- as.name(namesplot[i])}
                        args.plot$nrows = floor(totalPlot/2)
                        args.plot$margin = 0.1
                        args.plot$titleX = TRUE
                        args.plot$titleY = TRUE
                        
                        plot0 <- do.call(plotly::subplot, args.plot)
                        
            }
           
            return(plot0)
            
}


# 
# data <- read.csv("./data/data_IKEA_1111.csv",comment.char = "#")
# comparable_Year= c(2003,2004,2005)
# cluster_count = 5
# comparable_Var <-c( "Productivity", "Population", "Percent_University","EmployeeIndex", "SalesIndex")
# treat_ID = 2583
# yearVarName <-  "Year"
# transformation = "log"
# IDVarName = "Kommun_code"
# unitVarName = "Kommun_name"
# remove_cityName = c("Kalmar","Karlstad")
# resg2 <- cmp_Func_Cluster_Pool(data, treat_ID, comparable_Year, comparable_Var,transformation = transformation ,
#                                cluster_count, yearVarName, IDVarName, unitVarName, remove_cityName)
# 
# somData <- resg2$data.som
# Func_clusterPlot(somData, comparable_Var, transformation )
