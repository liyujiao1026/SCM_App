# Read data


# Func_Synth is the function of SCM
library(Synth)

Func_Synth <- function(synth_data, predictors, dependent, inv_year,
                       treat_ID, control_ID, match_from, match_to,
                       yearVarName, IDVarName, nameVarName,
                       special.predictors = NULL) {
            
            
            Kommun_code_th <- which(colnames(synth_data) == as.character(IDVarName))
            year_th <- which(colnames(synth_data) == as.character(yearVarName))
            name_th <- which(colnames(synth_data) == as.character(nameVarName))
            
            
            maxYear <- max(unique(synth_data[,year_th]))
            synth_data[,Kommun_code_th] <- as.numeric(synth_data[,Kommun_code_th])
            synth_data[,name_th] <- as.character( synth_data[,name_th] )
            
            
            
            dataprep.out <-
                        dataprep(
                                    foo = synth_data,
                                    predictors = predictors,
                                    predictors.op = "mean",
                                    dependent = dependent,
                                    unit.variable = get("IDVarName"),
                                    time.variable = get("yearVarName"),
                                    treatment.identifier = treat_ID,
                                    controls.identifier = control_ID,
                                    special.predictors = special.predictors,
                                    
                                    time.predictors.prior = c(as.numeric(match_from):as.numeric(match_to)),
                                    time.optimize.ssr = c(as.numeric(match_from):as.numeric(match_to)),
                                    unit.names.variable = get("nameVarName"),
                                    time.plot = match_from:maxYear

                        )
            synth.out <- synth(dataprep.out)
            
            Y_treat <- dataprep.out$Y1plot 
            Y_synth <- dataprep.out$Y0plot %*% synth.out$solution.w
                        
            
            gaps <- Y_treat - Y_synth
    
            pred_synth0 <- synth.tab(dataprep.res = dataprep.out,
                                    synth.res = synth.out)$tab.pred
            
            pred_synth <- data.frame("Variable" = row.names(pred_synth0), pred_synth0)
            
            return(list("synth_out" = synth.out,"data_out" = dataprep.out, #"mse_pre" = mse_pre, 
                        "Y_synth" = Y_synth , "Y_treat" = Y_treat,
                        "gaps" = gaps, "pred_synth" = pred_synth
                        ))
}


cmp_Func_Synth <- cmpfun(Func_Synth)



# data3 <- read.csv("~/Desktop/Intervention_Effect_Study/data/testData/testAbadie.csv")
# res <-  Func_Synth(synth_data = data3 ,
#                    predictors = c("cigsale","lnincome","beer","age15to24") ,
#                    dependent = "cigsale",
#                    treat_ID = 3,
#                    control_ID = setdiff(unique(data3$UnitId),3),
#                    match_from = 1970, match_to = 1988,
#                    yearVarName = "year",
#                    IDVarName = "UnitId", nameVarName = "state",
#                    inv_year = 1989
#                    )
# 
# Y.gaps <- res$gaps

# res <-  Func_Synth(synth_data = data3 , predictors = c("Employee") ,dependent = "Productivity" ,
#                    treat_ID = 2583, control_ID = sample(unique(data3$Kommun_code), 20) , match_from = 2002, match_to = 2005,
#                    yearVarName = "Year", IDVarName = "Kommun_code", nameVarName = "Kommun_name",inv_year = 2006,
# 
#                    special.predictors = list(
#                                list("Productivity", c(2001,2004), "mean"),
#                                #list("Productivity", 2003, "mean"),
#                                list("Employee", 2004, "mean")
#                    ))
# 
# 
# path.plot(dataprep.res = res$data_out, synth.res = res$synth_out)
# which(rownames(res$gaps) == as.character("input$inv_year"))
# res$data_out$Y1plot -  res$data_out$Y0plot%*%(res$synth_out$solution.w)
# 
# res$Y_synth
# res$pred_synth
# 



