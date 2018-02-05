
# library(pacman)
# 
# pacman::p_load(shiny, shinydashboard,raster,leaflet,rgdal,compiler,webshot,ggplot2,Hmisc,plotly,grofit)

#library(elmNN)

Sys.setlocale(category = "LC_ALL", locale = "pt_PT.UTF-8")
#system('defaults write org.R-project.R force.LANG en_US.UTF-8')


library("shiny")
library("shinydashboard")
library("raster")
library("leaflet")
library("rgdal")
library("compiler")
library("webshot")
library("ggplot2")
library("Hmisc")
library("plotly")
library("grofit")
library("drc")

Sys.setenv("plotly_username" = "liyujiao1026")
Sys.setenv("plotly_api_key" = "29fe97z16n")


source('./0_Func1_Synth.R', echo = F)
source('./0_Func2_ControlUnits_Mapdata.R', echo = F)
source('./0_Func3_ClusterKommun.R', echo = F)
source('./0_Func4_finalMap.R', echo = F)
source('./0_Func5_plot.R', echo = F)

source('./0_Func7_placeboTest.R', echo = F, encoding = 'LATIN1')
#source('./0_Func6_ELM.R', echo = F)

source('./0_Func8_placeboOutput.R', echo = F)
source('./0_Func9_Bootstrap.R', echo = F)

source('./0_Funct10_PretestPredictors.R', echo = F)
source('./0_Func11_growthCurve.R', echo = F)
#source('./0_Func12_curve_drc.R', echo = F)
source('./0_Func13_ClusterPlotly.R', echo = F)
source('./0_Func14_Parameter_Smooth.R', echo = F)

