library(shiny)
# library(leaflet)

# Load functions
source(file.path('./functions/functions.R'), local = TRUE)
# Load modules
for (f in list.files('./modules')) {
  source(file.path('modules', f), local = TRUE)
}

crops <- getCrops()
countries <- readRDS("data/countries.rds")


shinyUI(
fluidPage(
  includeCSS("css/styles.css"),
  tagList(
  shinyjs::useShinyjs(),
  navbarPage(title = "ICARDA FIGS", id = "tabs", collapsible = TRUE,
             tabPanel("Data Extraction", value = 1),
             tabPanel("Climatic Data Analysis", value = 2),
             tabPanel("Trait Analysis", value = 3),
             fluidRow(
               conditionalPanel("input.tabs == 1", tabsetPanel(
                 tabPanel("Passport Data",
                          sidebarPanel(class = "scroll",
                                       HTML('<hr>'),
                                       h4("Extract Accessions"),
                                       HTML('<hr>'),
                                       radioButtons("dataSrc", "", selected = "byCrop", c("Get Accessions by Crop Name" = "byCrop", "Get Accessions by IG" = "byIG", "Upload External Data" = "extData"), inline = FALSE),
                                    ###### Extract Data based on crop ######
                                    conditionalPanel("input.dataSrc == 'byCrop'", 
                                                     getAccessionsCropUI("getAccessionsCrop"),
                                                     actionButton("getAcc", "Get Accessions"), width = 3),
                                    ###### Extract Data based on IG ######
                                    conditionalPanel("input.dataSrc == 'byIG'", 
                                                     uploadDataUI('uploadIGData'),
                                                     selectInput("IG", "Select Identifier", list("")),
                                                     selectInput("oriIG", "Select country(ies)", multiple = T, choices = c("Countries" = "", countries[1]), selected = ""),
                                                     actionButton("getAccIG", "Get Accessions")),
                                    
                                    ###### Extract External Data ######
                                    conditionalPanel("input.dataSrc == 'extData'",
                                                     uploadDataUI('uploadData')),
                                    HTML('<hr>'),
                                    h4("Mapping"),
                                    HTML('<hr>'),
                                    uiOutput("targetVar"),
                                    actionButton("showMap", "Map Accessions"),
                                    HTML('<hr>'),
                                    h4("Download Accessions (.csv)"),
                                    HTML('<hr>'),
                                    downloadButton("downloadAcc", "Download")),
                   mainPanel(class = "scroll",
                     tabsetPanel(id = "main",
                       tabPanel(value = "accResult", "Table",
                                DT::dataTableOutput('table')),
                       tabPanel(value = "accMap", "Map",
                                leaflet::leafletOutput(outputId = "map", height = 600)),
                       tabPanel(value = "accPlot", "Plot",
                                plotly::plotlyOutput(outputId = "plot", height = 600))
                     )
                   )),
                   tabPanel("World Climatic Data",  
                            sidebarPanel(
                              extractWCDataUI("extractWCData"), br(),br(),
                              uiOutput("clVarPlot"), br(),
                              downloadButton("downloadWCData", "Download")
                            ),
                            mainPanel(
                              tabsetPanel(id = "wcMainPanel",
                                          tabPanel(value = "WCTable", "Table",
                                                   DT::dataTableOutput('WCtable')),
                                          tabPanel(value = "WCMap", "Map",
                                                   leaflet::leafletOutput('WCMap', height = 600)                                                                     )
                              )
                            )
                   )
                   )),
                   conditionalPanel("input.tabs == 2",
                                    tabsetPanel(
                                      # tabPanel("Extract World Climatic Data",  
                                      #          sidebarPanel(
                                      #               extractWCDataUI("extractWCData"), br(),br(),
                                      #               uiOutput("clVarPlot"), br(),
                                      #               downloadButton("downloadWCData", "Download")
                                      #            ),
                                      #          mainPanel(
                                      #            tabsetPanel(id = "wcMainPanel",
                                      #                       tabPanel(value = "WCTable", "Table",
                                      #                               DT::dataTableOutput('WCtable')),
                                      #                       tabPanel(value = "WCMap", "Map",
                                      #                                leaflet::leafletOutput('WCMap', height = 600)                                                                     )
                                      #                       )
                                      #          )
                                      # ),
                                      tabPanel("Multivariate Analysis",
                                               sidebarPanel(      multiVarAnalysisUI("multiVarAnalysis"),                               actionButton("resetButton", "Reset"),                                 uiOutput("sliders"),                                                  uiOutput("dataDescription"),                                     uiOutput("mapDlBtns")
                                                                ),
                                               mainPanel(class = "scroll",
                                                 tabsetPanel(id = "subsetMain",
                                                             tabPanel(
                                                               value = "subSumHist", "Summary and Histograms",
                                                               uiOutput("summaryandHists")
                                                             ),
                                                             tabPanel(value = "subsetMap", "Map",
                                                                      leaflet::leafletOutput("subsetMap", height = 600)
                                                             )
                                                 )
                                               )
                                      ),
                                      tabPanel("K-means Clustering",
                                               sidebarPanel(                                                    radioButtons("kmDataSrc", "", selected = "allDataKm", 
                                                                  c("Original Data" = "allDataKm","Filtered Data" = "filtDataKm"), inline = F),                                   shinyWidgets::pickerInput("kmx", "Select Variables", list(""), options = list(`actions-box` = TRUE, `live-search` = TRUE, style = "picker"), multiple = T),                                                               kMeansClusteringUI("kMeansClustering"),                      actionButton("kmeansBtn", "Apply Clustering"),
br(), br(),                                                    downloadButton("downloadClusterData", "Download")
                                                    ),
                                               mainPanel(
                                                 verbatimTextOutput("totkm"),
                                                 leaflet::leafletOutput('mapcluster', height = 600)
                                               )
                                      ),
                                      tabPanel("PCA Analysis",
                                               sidebarPanel(class = "scroll",                                                               radioButtons("pcaDataSrc", "", selected = "allDataPca", 
c("Original Data" = "allDataPca","Filtered Data" = "filtDataPca"), inline = F),
includeMarkdown("Rmd/pca_guide.Rmd"),
shinyWidgets::pickerInput("pca_var", "Select Variables", list(""), options = list(`actions-box` = TRUE, `live-search` = TRUE, style = "picker"), multiple = T),
                                                       pcaSummaryUI("pcaSummary"),
                                                       actionButton("PCAsummary", "Summary"),
                                                       tags$h4("PCA Plot"),
                                                       # pcaPlotUI("pcaPlot"),
                                                       radioButtons("plotRadios", "", selected = "plain", c("Plain" = "plain", "Coloured" = "colored"), inline = TRUE),
                                                       uiOutput("pcaPlotVar"),
                                                       actionButton("PCAPlotButton", "PCA Plot"),
                                                       tags$h4("Map"),
                                                       selectInput("pcaScore", "Choose Score", list("")),
                                                       actionButton("geoPlotButton", "Map")
                                               ),
                                               mainPanel(class = "scroll",
                                                 tabsetPanel(id = "pca",
                                                   tabPanel(value = "pcaSummary","Summary",
                                                            verbatimTextOutput("summaryPca"),
                                                            plotly::plotlyOutput("plotpc")),
                                                   tabPanel(value = "pcaPlot", "PCA Plot",
                                                            plotly::plotlyOutput("pcaPlot", height = 600)),
                                                   tabPanel(value = "geoMap", "Map",
                                                            leaflet::leafletOutput('geoMap', height = 600))
                                                   
                                                 )
                                               )
                                      ),
                                      tabPanel("Core Collection",
                                               sidebarPanel(                                                    includeMarkdown("Rmd/core_collection_guide.Rmd"),     radioButtons("coreDataSrc", "", selected = "allDataCC",
                  c("All data" = "allDataCC",
                    "Filtered data" = "filtDataCC"), 
                    inline = F),
                                                                coreCollectionUI("coreCollection"),
                                                                actionButton("coreButton", "Core Collection"),
                                                                br(), br(),
                                                                downloadButton("coreDLbutton", label = "Download Core Collection")
                                               ),
                                               mainPanel(
                                                      tabsetPanel(id = "coreMain",                                                            tabPanel(value = "dist", "Distribution per Cluster", plotly::plotlyOutput("corePlot")), 
          tabPanel(value = "coreRes", "Table", DT::dataTableOutput("coreDataTable")),
          tabPanel(value = "coreMap", "Map", leaflet::leafletOutput("coreMap", height = 600)))
              )
              )
                                    )
                                    ),
conditionalPanel("input.tabs == 3",
                 sidebarPanel(
                   HTML('<hr>'),
                   h4("Get Traits"),
                   HTML('<hr>'),
                   includeMarkdown("Rmd/getTraits.Rmd"),
                   uiOutput("cropSelected"),
                   # selectInput("crop.trait", "Select a crop", c("Crop" = "", crops[2])),
                   actionButton("getTraits", "Get Traits"),
                   HTML('<hr>'),
                   h4("Get Traits Data"),
                   HTML('<hr>'),
                   selectInput("IG.Trait", "Select IG", c("IG" = "")),
                   selectInput("traitName", "Select Trait", c("Trait" = "")),
                   actionButton("getTraitsData", "Get Traits Data"),
                   # HTML('<hr>'),
                   # h4("Data Exploration"),
                   # HTML('<hr>'),
                   # selectInput("trait.var", "Select Variable", c("Variable" = "", "IG", "Year"))
                   
                              ),
                 mainPanel(
                   tabsetPanel(id = "traitMainPanel",
                               tabPanel(value = "traitTable", "Traits", DT::dataTableOutput("TraitTbl")),
                               tabPanel(value = "traitDataTable", "Traits Data Table", DT::dataTableOutput("TraitDataTbl")),
                               tabPanel(value = "traitDataSum", "Summary", DT::dataTableOutput("TraitDataSum")),
                               tabPanel(value = "traitResPlot", "Plot" , uiOutput("trait.var.val")),
                               tabPanel(value = "traitMap", "Map", leaflet::leafletOutput("traitMap", height = 600))
                               )
                 )
                 )
                   )
             
)
)
)
)