


Func_GrowthCurve <- function(inv_year,bootstrap_result_matrix, yearVarName, Y.gap_raw){
            
            
            bt <- data.frame(bootstrap_result_matrix)
            colnames(bt) <- c("Year", "unitID", "unitName","gapsValue")
        
            dt <- as.vector(bt$gapsValue)
            
            years_count <- nrow(Y.gap_raw)
            y.nonParboot <- matrix(dt, nrow = years_count)
            
            invTime_th <- which( as.numeric(row.names((Y.gap_raw))) == as.numeric(inv_year))
            y.nonPar_boot <- y.nonParboot[-c(1:invTime_th),]
            
            row.names(y.nonPar_boot) <- rownames(Y.gap_raw)[-c(1:invTime_th)]
            nonParaResult <- t(apply(y.nonPar_boot, 1, function(x){c(quantile(x, 0.025), mean(x), quantile(x, 0.975))}))
            nonParaResult <- round(nonParaResult, 4)
            colnames(nonParaResult) <- c("2.5%", "mean", "97.5%")
            
            
            y.Par_boot <- apply(y.nonPar_boot, 2, function(x){ Func_parametric(x)[[3]][,2] })
            row.names(y.Par_boot) <- rownames(Y.gap_raw)[-c(1:invTime_th)]
            paraResult <- t(apply(y.Par_boot, 1, function(x){c(quantile(x, 0.025), mean(x), quantile(x, 0.975))}))
            paraResult <- round(paraResult, 4)
            colnames(paraResult) <- c("2.5%", "mean", "97.5%")
            
            y_min <- min(bt[,4]) - 0.35*abs(min(bt[,4]))
            y_max <- max(bt[,4]) + 0.35*abs(max(bt[,4]))
            ylimv <- c(y_min, y_max)
            
            return(list("paraResult" = paraResult, "nonParaResult" = nonParaResult, 
                        "ylimv" = ylimv))
            
            }



# data2 <- read.csv("~/Desktop/Intervention_Effect_Study/data/testData/testAbadie.csv")
# resmy <- cmp_Func_Bootstrap(RepTimes = 3,
#                             data = data2 ,
#                             predictors = c("cigsale","lnincome","beer","age15to24") ,
#                                                dependent = "cigsale",
#                                                treat_ID = 3,
#                                                control_ID = setdiff(unique(data2$UnitId),3),
#                                                match_from = 1970, match_to = 1988,
#                                                yearVarName = "year",
#                                                IDVarName = "UnitId", nameVarName = "state",
#                                                inv_year = 1989
#                                         )
# 
# res <- Func_GrowthCurve(inv_year = 1989,bootstrap_result_matrix = resmy, 
#                  yearVarName = "year", Y.gap_raw = Y.gaps)

