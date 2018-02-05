

Func_PredictorCorr <- function(data, predictors, dependent, year, yearVar){
            
            predictor_th <- sapply( predictors, function(x){ which(colnames(data) == x)})
            #predictor_name <- names(predictor_th)
            
            
            dependent_th <- which(colnames(data) == dependent)
            year_th <- which(colnames(data) == yearVar)
            
            corr_i_p <- function(year_i){  
                        data_i <-  data[which(data[,year_th] == year_i),]
                        corr_i <- sapply( predictor_th,
                                          function(x){cor.test(data_i[,dependent_th], data_i[,x])$p.value})
                        return(corr_i)
            }            
            
            
            corr_i <- function(year_i){  
                        data_i <-  data[which(data[,year_th] ==  year_i),]
                        corr_i <- sapply( predictor_th,
                                          function(x){cor(data_i[,dependent_th], data_i[,x])})
                        return(corr_i)
            }  
            
            corr.coef <- corr_i(year)
            corr.p_value <- corr_i_p(year)
            result <- data.frame("var" = names(corr.coef),corr.coef, corr.p_value)
            result1 <- result[order(result[,3]),]
            row.names(result1) <- NULL
            return(result1)

}



# # # 
# data <- read.csv("./data/data_SwedishCity.csv",comment.char = "#")
# predictors <- c("Percent_University","Employee","Index_market","Population","Productivity","Output")
# dependent <- "Productivity"
# yearVar <- "Year"
# year <- c(2003)
# res <- Func_PredictorCorr(data, predictors, dependent, year, yearVar)
# res

