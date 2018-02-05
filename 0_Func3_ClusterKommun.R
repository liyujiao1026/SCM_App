
library(kohonen)
# year 2000-2005

Func_Cluster_Pool <- function(data_set, treat_ID, comparable_Year, comparable_Var, 
                              cluster_count, yearVarName, IDVarName, unitVarName, remove_cityName) {
            
            
            if (missing(remove_cityName)){
                        remove_cityName = NULL
            }
            
            
            Kommun_code_th <- which(colnames(data_set) == as.character(IDVarName))
            
            year_th <- which(colnames(data_set) == as.character(yearVarName))
            
            name_th <- which(colnames(data_set) == as.character(unitVarName))
            
            #removeID_th <- which( as.character(data_set[,name_th]) %in% remove_cityName)
            
            if (is.null(remove_cityName)){
                        removeID <- NULL
            }else{
                        removeID_th <- sapply( remove_cityName, function(x){ which(as.character(data_set[,name_th]) == x) })
                        removeID <- unique( data_set[removeID_th,Kommun_code_th])
            }
            
            
            if (cluster_count != 1) {
                        
                        data.i <- data_set[ data_set[,year_th] %in% as.numeric(comparable_Year), ]
                        
                        
                        var_th <- c(Kommun_code_th,name_th,year_th, which(colnames(data.i) %in% as.character(comparable_Var)))
                        
                        data.SOM0 <- data.i[, var_th]
                        
                        
                        Kommun_code_th_2 <- which(colnames(data.SOM0) == as.character(IDVarName))
                        data.SOM1 <- data.SOM0[order(data.SOM0[,Kommun_code_th_2]),]
                        
                        data.SOM2 <- data.SOM1[,c(IDVarName, unitVarName)] %>% unique
                        
                        coll <- which(colnames(data.SOM1) %in% as.character(comparable_Var))
                        
                        for (i in  coll) {
                                    new <- by(data.SOM1[,i], data.SOM1[,IDVarName], mean) %>% as.numeric()
                                    data.SOM2 <- cbind(data.SOM2, new)
                                    colnames(data.SOM2)[colnames(data.SOM2) == "new"] <- colnames(data.SOM1)[i]
                        }
                        data.SOM <- data.SOM2
                        cluster_data <- as.matrix(data.SOM[,colnames(data.SOM) %in% as.character(comparable_Var),
                                                           drop = FALSE])
                        std_data <- round(scale(cluster_data),3)
                        somnet <- som(std_data ,
                                      grid = somgrid(cluster_count, 1, "rectangular"))
                        
                        plot(somnet)
                        data.SOM$cluster <- map(somnet)$unit.classif 
                        
                        treat.cluster <- data.SOM[data.SOM[,IDVarName] == treat_ID,]$cluster
                        
                        clustered_UnitId <- subset(data.SOM, cluster == treat.cluster)[, IDVarName]
                        
                        
                        SC_pool <- setdiff( clustered_UnitId , c(removeID,treat_ID )  )
                        
                        
            } else{
                        SC_pool <- setdiff( unique(data_set[,Kommun_code_th]), c(removeID,treat_ID ) )
            }
            
            comparableUnits_count <- length(SC_pool)
            
            
            colnames(std_data) <- paste0(colnames(std_data), ".", "scale")
            data_som <- data.frame(data.SOM, std_data)
            
            
            
            return(list("SC_pool" = SC_pool, "comparableUnits_count" = comparableUnits_count, "data.som" =  data_som))
}


library(compiler)
cmp_Func_Cluster_Pool <- cmpfun(Func_Cluster_Pool)




##====================================================================================#
# 
# data <- read.csv("./data/data_IKEA_1111.csv",comment.char = "#")
# comparable_Year= c(2003,2004,2005)
# cluster_count = 7
# comparable_Var <- c("Productivity" , "Population", "Percent_University","EmployeeIndex", "SalesIndex")
# treat_ID = 2583
# yearVarName <-  "Year"
# transformation = "log"
# IDVarName = "Kommun_code"
# unitVarName = "Kommun_name"
# remove_cityName = c("Kalmar","Karlstad")
# resg2 <- cmp_Func_Cluster_Pool(data = data, treat_ID, comparable_Year, comparable_Var,
#                               cluster_count, yearVarName, IDVarName, unitVarName)
# resg2$SC_pool
# resg2$data.som[1,]



# data <- read.csv("~/Desktop/SCM_Kca/SCM_R_code/SCM_submit/SCM_simulation/data/Abadie_data.csv")
# comparable_Year= c(1986,1987,1988)
# cluster_count = 3
# comparable_Var <- c("cigsale" ,  "lnincome"  ,"beer" )
# 
# yearVarName <-  "year"
# 
# IDVarName = "UnitId"
# unitVarName = "state"
# 
# resg2 <- cmp_Func_Cluster_Pool(data = data, treat_ID = 3, comparable_Year, comparable_Var,
#                                cluster_count, yearVarName, IDVarName, unitVarName)
# 
# resg2$SC_pool
# resg2$data.som[1,]
# 
# 

