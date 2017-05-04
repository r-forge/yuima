tabItem(tabName = "cluster",
        fluidRow(
          column(12,
                 h3("Clustering",class = "hTitle"),
                 h4("Select data you want to cluster.", br(),
                    "Choose the distance you want to use and the kind of linkage for the hierarchical cluster analysis.", br(),
                    "Results will be shown below by plotting dendrogram and multidimensional scaling output."),
                 hr(class = "hrHeader")
          )
        ),
        fluidRow(column(12,bsAlert("cluster_alert"))),
        fluidRow(column(12,
                        column(4,
                               h4("Available data"),
                               DT::dataTableOutput("cluster_table_select")
                        ),
                        column(4,
                               h4("Selected data"),
                               DT::dataTableOutput("cluster_table_selected")
                        ),
                        column(4,br(),br(),
                               div(align="center",
                                   selectInput("cluster_linkage", "Linkage", choices = c("Complete"="complete", "Single"="single", "Average"="average", "Ward"="ward.D", "Ward squared"="ward.D2", "McQuitty"="mcquitty", "Median"="median", "Centroid"="centroid")),
                                   selectInput("cluster_distance", "Distance", choices = c("Percentage Increments Distribution"="MYdist_perc", "Increments Distribution"="MYdist_ass", "Markov Operator"="MOdist", "Euclidean"="euclidean", "Maximum"="maximum", "Manhattan"="manhattan", "Canberra"="canberra", "Minkowski"="minkowski")),
                                   shinyjs::hidden(numericInput("cluster_distance_minkowskiPower", label = "Power", value = 2)))
                        )
        )),
        br(),
        fluidRow(column(12,
                        column(2,actionButton("cluster_button_select",label = "Select", align = "center")),
                        bsTooltip("cluster_button_select", title = "Select data to cluster", placement = "top"),
                        column(2,actionButton("cluster_button_selectAll",label = "Select All", align = "center")),
                        bsTooltip("cluster_button_selectAll", title = "Select all data that are displayed", placement = "top"),
                        column(2,actionButton("cluster_button_delete",label = "Delete", align = "center")),
                        bsTooltip("cluster_button_delete", title = "Delete selected data", placement = "top"),
                        column(2,actionButton("cluster_button_deleteAll",label = "Delete All", align = "center")),
                        bsTooltip("cluster_button_deleteAll", title = "Delete all data that are displayed", placement = "top"),           
                        column(4,actionButton("cluster_button_startCluster", label = "Start Clustering", align = "center"))
        )),
        div(id="cluster_charts", align = "center",
            br(),br(),
            hr(class = "hrHeader"),
            fluidRow(
              column(4),
              column(4, uiOutput("cluster_analysis_id"))
            ),
            fluidRow(column(11, div(align="left", uiOutput("cluster_moreInfo")))),
            fluidRow(column(12,
                            column(8, plotOutput("cluster_dendogram", click = "cluster_dendrogram_click")),        
                            column(4, plotOutput("cluster_scaling2D"))
            )),
            br(),
            fluidRow(column(12,
                            column(2, div(actionButton("cluster_button_delete_analysis", label = "Delete"))),
                            column(2, div(actionButton("cluster_button_deleteAll_analysis", label = "Delete All"))),
                            column(2),
                            column(2, div(downloadButton("cluster_button_saveDendogram", label = "Dendrogram"))), 
                            column(2),
                            column(2, div(downloadButton("cluster_button_saveScaling2D", label = "Scaling")))
            ))
        )
)