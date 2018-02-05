# ELM function
Func_ELM <- function(treatName,inv_year, data, varYear, varUnitName, varOutput, varUnitID){
            
            Year_th <- which(colnames(data) == varYear)
            UnitName_th <- which(colnames(data) == varUnitName)
            Output_th <- which(colnames(data) == varOutput)
            UnitID_th <- which(colnames(data) == varUnitID)
            
            #data_ELM <- data[,c(2,3,12)]
            n_controlUnits <- length(unique(data[,UnitID_th]))
            data_ELM <- data.frame( matrix(data[,Output_th], ncol =  n_controlUnits))
            colnames(data_ELM) <- paste0("ID_", as.character(unique(data[,UnitID_th])))
            rownames(data_ELM) <- unique(data[,Year_th])
            
            
            # treat
            treatID <- data[,UnitID_th][which(data[,UnitName_th] == treatName)[1]]
            treat_Column <- which(colnames(data_ELM) == paste0("ID_",treatID))
            
            control_Column <- setdiff(1:n_controlUnits , treat_Column)
            
            
            # write formula and input ELM calculation.
            formula_string <- paste0("ID_",treatID, " ~ ",
                                     paste0(colnames(data_ELM)[control_Column], collapse = " + ") )
            
            formula <- as.formula(formula_string)
            
            
            
            ## train set and test set
            inv_year_th <- which(rownames(data_ELM) == as.character(inv_year))
            data_ELM_pre <- data_ELM[1:(inv_year_th-1), ]
            #data_ELM_post<- data_ELM[(inv_year_th): nrow(data_ELM) ,]
            
            
            model <- elmtrain(formula , data = data_ELM_pre, nhid = 15, actfun = "sig")
            preditction <- predict(model,newdata = data_ELM)
            
            data.frame("Year" = as.numeric(row.names(preditction)), "treatedY" = data_ELM[,treat_Column], "SynthY" = preditction)
}

cmp_Func_ELM <- cmpfun(Func_ELM)





## test

#
# data <- read.csv("./data/data_SwedishCity.csv")
# 
# treatName <- "Haparanda"
# varYear <- "Year"
# varUnitName <- "Kommun_name"
# varOutput <- "Productivity"
# varUnitID <- "Kommun_code"
# inv_year <- 2004
# res <- cmp_Func_ELM(treatName,inv_year, data, varYear, varUnitName, varOutput, varUnitID)
# Func_ELMplot(SynthY = res$SynthY, treatedY = res$treatedY, Year = res$Year, inv_year)
