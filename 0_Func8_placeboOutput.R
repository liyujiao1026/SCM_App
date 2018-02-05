# collect output for the interval 

Func_placeboOut <- function(placeboData_gaps , invYear, bootstrap){
            
            placeboGap <- data.frame(placeboData_gaps)
            colnames(placeboGap) <- c("Year", "unitID", "unitName","gapsValue")
            
            placeboGap$unitID <- as.factor(placeboGap$unitID)
            placeboGap$unitName <- as.character(placeboGap$unitName)
            placeboGap$gapsValue <- as.numeric(placeboGap$gapsValue)
            
            unitID_unique <- unique(placeboGap$unitID)
            unitName_unique <- unique(placeboGap$unitName)
         
            yearLength <- length(unique((placeboGap$Year)))
            
            inv_th <- which(as.numeric(placeboGap$Year) == invYear) 
            
            treat_Row_th <- inv_th[1]
            
                        
            if (treat_Row_th + 3 <= yearLength) {
                        lag3 <- 3
            }else{
                        lag3 <- yearLength - treat_Row_th
            }
            
            if (treat_Row_th >= 3) {
                        head3 <- 3
            }else{
                        head3 <-  treat_Row_th - 1
            }
            
            
            
            
            if (bootstrap == FALSE) { 
            gap_pre1 <- placeboGap$gapsValue[treat_Row_th - 1]
            
            gap_post1 <-  placeboGap$gapsValue[treat_Row_th + 1]
            
            
            gap_post3 <- mean(
                        placeboGap$gapsValue[(treat_Row_th + 1):(treat_Row_th + lag3)] )
            
            gap_pre3 <- mean(
                        placeboGap$gapsValue[(treat_Row_th - head3):(treat_Row_th - 1)])
            
            
            gap_preAll <-  mean(
                        placeboGap$gapsValue[1:(treat_Row_th - 1)])
            
            gap_postAll <-  mean(
                        placeboGap$gapsValue[(treat_Row_th + 1):yearLength ])
            
            
            gap_last_1 <- placeboGap$gapsValue[yearLength]
            
            
            #                         
            mspe_preAll <- mean(
                        (placeboGap$gapsValue[1:(treat_Row_th - 1)]) ^ 2
            )
            
            mspe_postALL <-  mean(
                        (placeboGap$gapsValue[(treat_Row_th + 1):yearLength]) ^ 2
            )
            
            
            
            mspe_ratio <- sqrt(mspe_postALL / mspe_preAll)
            
            
            Value_treat <- c(gap_pre3,gap_pre1,
                             gap_post1,gap_post3,
                             gap_last_1,
                             gap_preAll,gap_postAll,
                             mspe_preAll,mspe_postALL,
                             mspe_ratio
            )
            
            Variable <-  c("gap_pre_3","gap_pre_1",
                           "gap_post_1","gap_post_3",
                           "gap_last_1",
                           "gap_pre_all","gap_post_all",
                           "mspe_pre_all","mspe_post_all",
                           "mspe_ratio"
            )
            
            
            
            } else { 
                        Variable <-  Value_treat <- NULL
            }
            
            
            ## p value & interval

            
            
            control_Row_th <- inv_th[-1]
            
            # pre_1
            gap_pre1_vector <- placeboGap$gapsValue[control_Row_th - 1]
            gap_pre1_interval <-  paste0("[",round(quantile(gap_pre1_vector , 0.05),3) , 
                                         " , ", round(quantile(gap_pre1_vector, 0.95),3),
                                         "]")
            
            
            
            # post_1
            gap_post1_vector <- placeboGap$gapsValue[control_Row_th + 1]
            gap_post1_interval <- paste0("[",round(quantile( gap_post1_vector , 0.05),3) , 
                                         " , ", round(quantile( gap_post1_vector , 0.95),3),
                                         "]")
            
 
            
            # pre 3 lag time
            gap_pre3_vector <- sapply(control_Row_th, 
                                      function(x){ mean(placeboGap$gapsValue[(x - 3):(x - 1)])}
            )
            
            
            gap_pre3_interval <- paste0("[", round(quantile(gap_pre3_vector, 0.05),3),
                                        " , ", round(quantile(gap_pre3_vector, 0.95),3),
                                        "]")
            

            
            
            # post 3 lag time   
            gap_post3_vector <- sapply(control_Row_th, 
                                       function(x){ mean(placeboGap$gapsValue[(x + 1):(x + lag3)])}
            )
            gap_post3_interval <- paste0("[", round(quantile(gap_post3_vector, 0.05),3),
                                         " , ", round(quantile(gap_post3_vector, 0.95),3),
                                         "]")

            
            # last time
            gap_last_1_vector <- sapply(control_Row_th, 
                                         function(x){ 
                                                     lagEnd <- yearLength - treat_Row_th
                                                     last <- placeboGap$gapsValue[x + lagEnd]
                                                     
                                                     return(last)
                                         })
            
            
            gap_last_1_interval <- paste0("[", round(quantile(gap_last_1_vector, 0.05),3),
                                           " , ", round(quantile(gap_last_1_vector, 0.95),3),
                                           "]" )
            
            
            
            
            
            # pre all  
            
            gap_preAll_vector <- sapply(control_Row_th, 
                                        function(x){ 
                                                    mean(placeboGap$gapsValue[(x - (treat_Row_th - 1)):(x - 1)])
                                        })
            
            gap_preAll_interval <- paste0("[", round(quantile(gap_preAll_vector, 0.05),3),
                                          " , ", round(quantile(gap_preAll_vector, 0.95),3),
                                          "]")
            

            
            # post all
            gap_postAll_vector <- sapply(control_Row_th, 
                                         function(x){ 
                                                     lagEnd <- yearLength - treat_Row_th
                                                     
                                                     post <- mean( placeboGap$gapsValue[(x + 1):(x + lagEnd )])
                                                     
                                                     return(post)
                                         })
            
            
            gap_postAll_interval <- paste0("[", round(quantile(gap_postAll_vector, 0.05),3),
                                           " , ", round(quantile(gap_postAll_vector, 0.95),3),
                                           "]" )
            
     
            
            # mpse
            mspe_preAll_vector <- sapply(control_Row_th, 
                                         function(x){ 
                                                     mean((placeboGap$gapsValue[(x - (treat_Row_th - 1)):(x - 1)]) ^ 2)
                                         })
            
            mspe_preAll_interval <- paste0("[", round(quantile(mspe_preAll_vector, 0.05),3)," , ",
                                           round(quantile(mspe_preAll_vector, 0.95),3),
                                           "]")
            
            # mpse - post
            mspe_postAll_vector <- sapply(control_Row_th, 
                                          function(x){ 
                                                      mean((placeboGap$gapsValue[(x + 1):(x + (yearLength - treat_Row_th) )]) ^ 2)
                                          })
            
            mspe_postAll_interval <- paste0("[", round(quantile(mspe_postAll_vector, 0.05),3)," , ",
                                           round(quantile(mspe_postAll_vector, 0.95),3),
                                           "]")
            
            
            
            # ratio
            mspe_ratio_vector <- sqrt(mspe_postAll_vector / mspe_preAll_vector)
            mspe_ratio_interval <- paste0("[", round(quantile(mspe_ratio_vector, c(0.05)),3),
                                          " , ", round(quantile(mspe_ratio_vector, c(0.95)),3),
                                          "]")

            Interval_control <- c(gap_pre3_interval,gap_pre1_interval, 
                                  gap_post1_interval,gap_post3_interval,
                                  gap_last_1_interval,
                                  gap_preAll_interval, gap_postAll_interval,
                                  mspe_preAll_interval, mspe_postAll_interval,
                                  mspe_ratio_interval)
            
            
            if (bootstrap == FALSE) { 
                        
                        # data 1=================#
                        placeboDotPlotData <- rbind( 
                                    data.frame(
                                                "unitID" = unitID_unique[1], 
                                                "unitName" = unitName_unique[1],
                                                "MPSE_pre" = mspe_preAll, 
                                                "MPSE_ratio" = mspe_ratio),
                                    
                                    
                                    data.frame(
                                                "unitID" = unitID_unique[-1], 
                                                "unitName" = unitName_unique[-1],
                                                "MPSE_pre" = mspe_preAll_vector, 
                                                "MPSE_ratio" = mspe_ratio_vector)
                                    
                        )
                        
                        
                        # data 2===============#
                        # pre_1_pValue
                        gap_pre1_v <- c(gap_pre1, gap_pre1_vector)
                        n <-  length(gap_pre1_v)
                        gap_pre1_pValue <- 1 - (rank(gap_pre1_v)[1] /n)
                        
                        # post_1_pValue
                        gap_post1_v <- c(gap_post1, gap_post1_vector)
                        gap_post1_pValue <- 1 - (rank(gap_post1_v)[1] / n)
                        
                        # pre 3 lag time_pValue
                        gap_pre3_v <- c(gap_pre3, gap_pre3_vector)
                        gap_pre3_pValue <- 1 - (rank(gap_pre3_v)[1] / n)
                        
                        # post 3 lag time_pValue
                        gap_post3_v <- c(gap_post3, gap_post3_vector)
                        gap_post3_pValue <- 1 - (rank(gap_post3_v)[1] / n)
                        
                        
                        # last 1 
                        gap_last1_v <- c(gap_last_1, gap_last_1_vector)
                        gap_last1_v_pValue <- 1 - (rank(gap_last1_v)[1] / n)
                        
                        
                        # pre all_pValue  
                        gap_preAll_v <- c(gap_preAll, gap_preAll_vector)
                        gap_preAll_pValue <- 1 - (rank(gap_preAll_v)[1] / n)
                        
                        # post all_pValue
                        gap_postAll_v <- c(gap_postAll, gap_postAll_vector)
                        gap_postAll_pValue <- 1 - (rank(gap_postAll_v)[1] / n)
                        
                        # mpse_pValue
                        mspe_preAll_v <- placeboDotPlotData$MPSE_pre
                        rank_mspe_preAll <- rank(mspe_preAll_v)
                        mspe_preAll_pValue <- 1 - (rank_mspe_preAll[1] /n)
                        
                        # mpsePost_pValue
                        mspe_postAll_v <- c(mspe_postALL, mspe_postAll_vector)
                        rank_mspe_postAll <- rank(mspe_postAll_v)
                        mspe_postAll_pValue <- 1 - (rank_mspe_postAll[1] /n)
                        
                        
                        
                        
                        # ratio_pValue
                        mspe_ratio_v <- placeboDotPlotData$MPSE_ratio
                        rank_mspe_ratio <- rank(mspe_ratio_v)
                        mspe_ratio_pValue <- 1 - (rank_mspe_ratio[1] / n)
                        
                        PlaceboTest_P.value <- c(gap_pre3_pValue, gap_pre1_pValue, 
                                                 gap_post1_pValue, gap_post3_pValue,
                                                 gap_last1_v_pValue,
                                                 gap_preAll_pValue, gap_postAll_pValue,
                                                 mspe_preAll_pValue, mspe_postAll_pValue,
                                                 mspe_ratio_pValue)
                        
                        ## data3 ===============#
                        outputData <- data.frame( Variable, Value_treat, PlaceboTest_P.value) ###, "Placebo_Interval" = Interval_control)
                        
                        
                        ## data4 ===============#
                        fitIndex <- paste0("Median: ", round(quantile(mspe_preAll_vector, 0.5),3), ";  " ,
                                           "\t Sd: ",round(sd(mspe_preAll_vector, 0.5),3)
                        )
                        
            } else {
                        placeboDotPlotData <- fitIndex <- NULL 
                        outputData <- data.frame(Interval_control)
                        
            }
            
            
            

            
            
            
            return(list(
                        "placeboDotPlotData" = placeboDotPlotData,
                        "outputData" = outputData,
                        "FitIndex" = fitIndex
            ))

            
            
}

# 

# placeboData_gaps <- res
# res2 <- Func_placeboOut(placeboData_gaps , invYear = 2007, bootstrap = TRUE)
# names(res2$outputData)
