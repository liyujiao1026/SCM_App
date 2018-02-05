
Func_PlaceboTest <- function(control_ID,
                             
                             synth_data , predictors, dependent,
                             match_from, match_to,yearVarName, IDVarName,
                             nameVarName,inv_year
                             
){
            
            nameth <- which(colnames(synth_data) == nameVarName)
            IDth <- which(colnames(synth_data) == IDVarName)
            
           
            control_name_i <- sapply(control_ID, function(x){
                        row_controlID <- which(synth_data[,IDth] == x)[1]
                        return(as.character(synth_data[row_controlID,nameth]))
                        })
            
           
            placebo_gaps <- c()
            
            #run SCM for all units in donar pool
            
            for (i in 1:length(control_ID)) {
                        treat_ID_placebo <- control_ID[i]
                        control_ID_placebo <- setdiff(control_ID, treat_ID_placebo )
                        
                        Placebo_synthResult <-  cmp_Func_Synth(synth_data = synth_data , 
                                                               predictors = predictors ,
                                                               dependent = dependent ,
                                                               
                                                               treat_ID = treat_ID_placebo, 
                                                               control_ID = control_ID_placebo , 
                                                               match_from = match_from, match_to = match_to,
                                                               yearVarName = yearVarName, IDVarName = IDVarName,
                                                               nameVarName = nameVarName,inv_year = inv_year)
                   
                        placebo_gaps_i <- Placebo_synthResult$gaps
                        placebo_gaps <- rbind(placebo_gaps,placebo_gaps_i)
                        
            }
            

            
            
            # gap data
            placebo_gaps_value <- as.vector(placebo_gaps)
            Year <- as.numeric(row.names(placebo_gaps))
            unitID <- rep(control_ID, each = length(unique(Year)))
            control_name <- rep(control_name_i, each = length(unique(Year)))
            options(scipen = 100)
            
            placebo_gaps_data <- data.frame("Year" = Year, "unitID" = unitID ,
                                            "unitName" = control_name, 
                                            "gapsValue" = placebo_gaps_value )
            return(placebo_gaps_data)
            
  
}

library(compiler)
cmp_Func_PlaceboTest <- cmpfun(Func_PlaceboTest)


# Test#

## === set value 
# # # 
# source('./0_source.R', echo = F)
# 
# synth_data <- read.csv("./data/data_SwedishCity.csv")
# treat_ID = 880
# predictors <-  c("Employee") 
# dependent = "Productivity" 
# match_from = 1999
# match_to = 2005
# yearVarName = "Year"
# IDVarName = "Kommun_code"
# nameVarName = "Kommun_name"
# inv_year = 2006
# cluster_count= 5
# comparable_Year = 2004
# comparable_Var = "Productivity"
# IDVarName <- "Kommun_code"
# yearVarName <- "Year"
# unitVarName <- "Kommun_name"
# remove_cityName <- c("Haparanda","G?teborg","Karlstad")
# 
# 
# control_ID_res <- cmp_Func_Cluster_Pool(data = synth_data, treat_ID = treat_ID, 
#                                         cluster_count = cluster_count ,
#                                         comparable_Year = comparable_Year, 
#                                         comparable_Var = comparable_Var,
#                                         IDVarName = IDVarName,
#                                         yearVarName = yearVarName,
#                                         unitVarName = unitVarName,
#                                         remove_cityName = remove_cityName)
# 
# control_ID <- control_ID_res$SC_pool
# control_length <- control_ID_res$comparableUnits_count
# 
# res <- Func_PlaceboTest(control_ID,
#                  
#                  synth_data , predictors, dependent,
#                  match_from, match_to,yearVarName, IDVarName,
#                  nameVarName,inv_year
# )

# plotVal<- res$placeboResult_data
# dotchart(plotVal$placebo_ratio_mse,labels=plotVal$control_name, pch = 20, cex = 0.5,
#          main = "Post-RMSPE/Pre-RMSPE", color = plotVal$color,
#          xlab = "Ratio of MSE")
# 
# 
# 
# dt <- res$placeboResult_data_MSPE
# dotchart(dt$placebo_mse_pre,labels=dt$control_name, pch = 20, cex = 0.5,
#          main = "Post-RMSPE/Pre-RMSPE", color = dt$color,
#          xlab = "Ratio of MSE")

