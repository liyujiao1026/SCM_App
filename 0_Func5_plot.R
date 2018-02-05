## plot Function

Func_plot <- function(SynthY = NA, treatedY = NA, Year = NA, 
                      treatName = NA, dependVar = NA, 
                      inv_year,mainName = NULL){ 
            
            
            Legend=c("Treated","Synthetic")
            Legend.position=c("topright")
            
            
            Y.max <-  max(c(treatedY,SynthY))
            Y.min <-  min(c(treatedY,SynthY))
            
            Ylim <- c(
                        (Y.min - .3*Y.min ),
                        (.3*Y.max + Y.max)
            )
            
            
            
            ### plot
            par(mfrow = c(1,2))
            plot(
                        Year,treatedY, t = "l", col = "black",
                        lwd = 2, main= paste0(mainName, " path plot") ,ylab= paste0(treatName, "'s ",dependVar), xlab= "Time",xaxs = "i",yaxs = "i",ylim=Ylim)
            
            lines(
                        Year, SynthY,
                        col = "blue",
                        lty = "dashed",
                        lwd = 2,
                        cex = 4/5
            )
            abline(v = inv_year, col = "black",lty = "dotted",lwd = 2)
            
            
            
            gap <- treatedY - SynthY
#             Ylim_gap <- c(
#                         -7*max(abs(gap)),
#                         7*max(abs(gap)) 
#             )
            
            Ylim_gap <- c(
                        -1.3*(Ylim[2] - Ylim[1]),
                        1.3*(Ylim[2] - Ylim[1])
            )
            
            
            plot(Year, gap, t = "l", col = "black", ylim = Ylim_gap,
                 lwd = 2, main = paste0(mainName, " gap plot"),ylab = "Gap",xlab = "Time",xaxs = "i",yaxs = "i") 
             
            abline(h = 0, col = "black",lty = "dashed",lwd = 2)
            abline(v = inv_year, col = "black",lty = "dotted",lwd = 2)
            
            
}


