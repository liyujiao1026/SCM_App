#ui.R

source('./0_source.R', echo = F)

## 1. header --------------------------------------------------------------------------##
header <- dashboardHeader(title = "Intervention effect study", titleWidth = 230)


## 2. siderbar --------------------------------------------------------------------------##
siderbar <- dashboardSidebar(
            
            width = 150,
            
            sidebarMenu(
                        menuItem("SCM Estimation", tabName = "SCM_Estimation", icon = icon("play-circle")) ,
                        #menuItem("2. SCM Results", tabName = "SCM_Results", icon = icon("play-circle")) ,
                        menuItem("SCM Inference", tabName = "SCM_inference", icon = icon("play-circle")) ,
                        
                        menuItem("Data", tabName = "data", icon = icon("table"))#,
                        
                        # menuItem("Code", tabName = "source code", icon = icon("code")),
                        # 
                        # menuItem("Mannual", tabName = "mannual", icon = icon("book")),
                        # 
                        # menuItem("Appendix", tabName = "appendix", icon = icon("info-circle"))
            )
)


## 3. body --------------------------------------------------------------------------##
body <- dashboardBody(
            
            tabItems(  
                        # tab 1
                        tabItem(
                                    tabName = "SCM_Estimation",    
                                    fluidRow(
                                                box(width  = 12, solidHeader = TRUE, status = "warning",title = "1. Data import",
                                                    
                                                    column(width = 3,
                                                           
                                                           fileInput("datafile", label = "Upload data"),
                                                           textOutput("defaultData")),
                                                    
                                                    column(width = 3,
                                                           uiOutput("varUnitID")),
                                                    
                                                    column(width = 3,
                                                           uiOutput("varUnitName") ),
                                                    
                                                    column(width = 3,
                                                           uiOutput("varYear"))
                                                )
                                    ),
                                    
                                    fluidRow(
                                                box(width  = 12, solidHeader = TRUE, status = "warning",title = "2. SCM input",
                                                    
                                                    mainPanel(
                                                                
                                                                tabsetPanel(
                                                                            
                                                                            tabPanel("Required fields", #height = "400px",width = "900px",
                                                                                     
                                                                                     
                                                                                     # div(head("Required fields"),
                                                                                     #     style = "color:darkorange;font-size:16px;font-weight: 600;"),
                                                                                     # br(),
                                                                                     column(width = 6,
                                                                                            uiOutput("city_name"),  
                                                                                            
                                                                                            uiOutput("inv_year"),
                                                                                            
                                                                                            uiOutput("dependent_var")),
                                                                                     
                                                                                     column(width = 6,
                                                                                            uiOutput("predictors"),
                                                                                            
                                                                                            div("Matching time*", style = "font-weight:bold"),
                                                                                            uiOutput("matchYear"))
                                                                                     
                                                                            ),   
                                                                            
                                                                            
                                                                            tabPanel("Optional fields",
                                                                                     
                                                                                     fluidRow(  column(width = 12, align = "center",
                                                                                                       div(textOutput("controlLength"), 
                                                                                                           style = "color:darkorange;font-size:15px;font-weight: 600;"))),
                                                                                     
                                                                                     
                                                                                     
                                                                                     
                                                                                     fluidRow(  
                                                                                                 
                                                                                                 column(width = 3,
                                                                                                        #div("Exclude special units?", style = "font-weight:bold"),
                                                                                                        checkboxInput("checkOutlier", label = "Exclude special units", value = FALSE),
                                                                                                        uiOutput("conditional_remove")),
                                                                                                 
                                                                                                 
                                                                                                 column(width = 4,
                                                                                                        #div("Refine donor pool", style = "font-weight:bold"),
                                                                                                        checkboxInput("checkCluster", label = "Refine donor pool", value = FALSE),
                                                                                                        uiOutput("conditional_cluster"),
                                                                                                        uiOutput("cluster_count"),
                                                                                                        uiOutput("donar_var"),
                                                                                                        uiOutput("donar_year"),
                                                                                                        uiOutput("conditional_cluster_button")),
                                                                                                 
                                                                                                 
                                                                                                 column(width = 5,
                                                                                                        #div("Reconsider specical variables?", style = "font-weight:bold"),
                                                                                                        checkboxInput("checkSpecial", label = "Reconsider specical variables", value = FALSE),
                                                                                                        uiOutput("conditionalSpecial"),
                                                                                                        uiOutput("predictor_time_ui")
                                                                                                        
                                                                                                 )
                                                                                     ),
                                                                                     
                                                                                     
                                                                                     fluidRow( 
                                                                                                 #box(width = 12, solidHeader = T, status = "success", title = "Refining results",
                                                                                                 #tableOutput("predictorsTest"),
                                                                                                 plotlyOutput("clusterPlot")
                                                                                                 #)
                                                                                     )
                                                                            )
                                                                            
                                                                ))
                                                )),
                                    
                                    fluidRow(
                                                column(12, align = "center",
                                                       actionButton("Submit", label = "Submit_SCM",icon("paper-plane"), 
                                                                    style = "color: #fff; background-color:#FFA500;border-color:#2e6da4; width:150px"))),
                                    fluidPage(br()),
                                    
                                    fluidRow(
                                                box(width = 12, solidHeader = T, status = "success", title = "3. Estimation Results",
                                                    
                                                    column(width = 12, align = "center", 
                                                           plotOutput("SCM_plot",width = "100%", height = "450px")),
                                                    
                                                    column(width = 12, align = "center", 
                                                           div(head("Treated unit vs. Synthetic control unit"), 
                                                               style = "color:darkgreen;font-size:16px;font-weight: 600;"),
                                                           tableOutput("pred_synth")),
                                                    
                                                    column(width = 12, align = "center",
                                                           div(head("Weight of control units"), 
                                                               style = "color:darkgreen;font-size:16px;font-weight: 600;"),
                                                           tableOutput("weight_data_out")),
                                                    
                                                    
                                                    
                                                    #strong("Mapping the SCM results"),
                                                    leafletOutput("map")
                                                ))
                        ),
                        
                        
                        
                        #tab 2

                        tabItem(tabName = "SCM_inference",
                                box(width  = 12, solidHeader = TRUE, status = "warning",title = "SCM Inference",   
                                    
                                    column(width = 3, offset = 1,
                                           
                                           actionButton("placeboButton", label = "1. placebo test",
                                                        icon("paper-plane"), 
                                                        style = "color: #fff; background-color:#FFA500;border-color:#2e6da4; width:200px")     
                                           
                                    ),
                                    
                                    
                                    
                                    column(width = 3 , offset = 1, 
                                           actionButton("Bootstrap_button", label = "2. Bootstrap interval",icon("paper-plane"), 
                                                        style = "color: #fff; background-color:#FFA500;border-color:#2e6da4; width:200px")
                                    ),
                                    
                                    column(width = 3, offset = 1, 
                                           actionButton("ModelCurveButton", label = "3. Plot intervals",
                                                        icon("paper-plane"), 
                                                        style = "color: #fff; background-color:#FFA500;border-color:#2e6da4; width:200px"))
                                    
                                ),
                                
                                
                                
                                box(width = 12, solidHeader = T, status = "success", title = "Inference Results",
                                    column(width = 7, 
                                           div(head("Model Output"), 
                                               style = "color:darkgreen;font-size:16px;font-weight: 600;"),
                                           br(),
                                           
                                           tableOutput("placeboResult_Table")
                                    ),
                                    
                                    
                                    column(width = 5, 
                                           div(head("Model Diagnosis"), 
                                               style = "color:darkgreen;font-size:16px;font-weight: 600;"),
                                           
                                           br(),
                                           strong("Reliability of SCM: "),
                                           textOutput("SCM_Availability"),
                                           br(),
                                           
                                           strong("Should re-Cluster:"),
                                           textOutput("Clustering_status"),
                                           br(),
                                           
                                           strong("Should excluded following unit(s) due to pre-MSPE :"),
                                           textOutput("outlier_mspe"),
                                           br(),
                                           
                                           strong("Should excluded following unit(s) due to ratio-MSPE :"),
                                           textOutput("outlier_RatioMspe"),
                                           br(),
                                           
                                           strong("Pre-MSPE of units in donor pool:"),
                                           textOutput("FitIndex")
                                           
                                    )
                                ),
                                
                                
                                box(width = 12, solidHeader = T, status = "success", title = "Inference plots",
                                    fluidRow(
                                                column(width = 12,  
                                                       plotOutput("plot_Dot",width = "100%", height = "450px"))
                                    ),
                                    br(),
                                    
                                    fluidRow(column(width = 5,  offset = 1, 
                                                    plotlyOutput("placeboPlot",width = "100%", height = "500px") ),
                                             
                                             column(width = 5,  offset = 1, 
                                                    plotlyOutput("bootPlot",width = "100%", height = "500px") )
                                             
                                    ),
                                    br(),
                 
                                    fluidRow(
                                                column(width = 5, offset = 1,
                                                       plotOutput("non_paraPlot",width = "100%", height = "450px")),
                                                column(width = 5, offset = 1,
                                                       plotOutput("paraPlot",width = "100%", height = "450px")))
                                    
                                    
                                    )
                        ),
                        
                        
                        
                        
                        tabItem(tabName = "data",
                                dataTableOutput("dataTable")
                        ),
                        
                        # tab 6
                        tabItem(tabName = "appendix",
                                textOutput("control_name"),
                                dataTableOutput("placeboData_gaps")
                                
                        )
            )
)


dashboardPage(header,siderbar,body)          

