library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(treemap)
library(d3treeR)
library(bslib)
library(bsicons)
library(foreach)
library(doParallel)

# Load functions
source(file.path('./functions/functions.R'), local = TRUE)
# Load modules
for (f in list.files('./modules')) {
  source(file.path('modules', f), local = TRUE)
}

crops <- getCrops()
countries <- readRDS("data/countries.rds")

passport_accordions <- list(
  accordion_panel(
    value = "accResult",
    "Accessions",
    DT::dataTableOutput('table')
  ),
  accordion_panel(
    "Map",
    uiOutput('selectUI_1'),
    leaflet::leafletOutput(outputId = "map")
  ),
  accordion_panel(
    "Bivariate Plot",
    uiOutput('first_var', style = 'display: inline-block; width: 250px; margin-left: 15px; margin-right: 30px;'),
    uiOutput('second_var', style = 'display: inline-block; width: 250px'),
    d3tree2Output("tree_map", height = "600px")
  )
)

clim_accordions <- list(
    accordion_panel(
      value = "climDataMap",
      "Map",
      uiOutput('selectUI_2'),
      leaflet::leafletOutput('WCMap')
  ),
  accordion_panel(
    "Table",
    DT::dataTableOutput('WCtable')
  )
)

sidebar_passport <- layout_sidebar(
  sidebar = sidebar(
    open = "always",
        radioButtons("dataSrc",
                     "",
                     selected = "byCrop",
                     c("Get Accessions by Crop Name" = "byCrop",
                       "Get Accessions by Accession Number" = "byIG",
                       "Upload External Data" = "extData"),
                     inline = FALSE
        ),
        ###### Extract Data based on crop ######
        conditionalPanel("input.dataSrc == 'byCrop'",
                         getAccessionsCropUI("getAccessionsCrop"),
                         actionButton("getAcc", "Get Accessions")
        ),
        ###### Extract Data based on IG ######
        conditionalPanel("input.dataSrc == 'byIG'",
                         uploadDataUI('uploadIGData'),
                         selectInput("IG",
                                     "Select Identifier",
                                     list("")),
                         selectInput("oriIG",
                                     "Select country(ies)",
                                     multiple = TRUE,
                                     choices = c("Countries" = "", countries[1]),
                                     selected = ""),
                         checkboxInput("avail", "Get available accessions", value = TRUE),
                         checkboxInput("coor", "Get georefrenced accessions", value = TRUE),
                         checkboxInput("doi", "Include DOIs", value = FALSE),
                         checkboxInput("other_id", "Include other IDs", value = FALSE),
                         actionButton("getAccIG", "Get Accessions")
                         ),
        ###### Extract External Data ######
        conditionalPanel("input.dataSrc == 'extData'",
                         uploadDataUI('uploadData')
                          ),
        uiOutput("coords"),
        uiOutput("dlButton")
    ),
  accordion(
    id="passport_accd",
    open = FALSE,
    !!!passport_accordions
  )
  
)

sidebar_climdata <- layout_sidebar(
  sidebar = sidebar(
    open = "always",
    extractWCDataUI("extractWCData"),
    actionButton("extractWC", "Get WorldClim Data"),
    downloadButton("downloadWCData", "Download")
  ),
  accordion(
    id = "climData_accd",
    open = FALSE,
    !!!clim_accordions
  )
)

multivar_accordions <- list(
  accordion_panel(
    style = "max-height:30%;",
    value = "subSumHist",
    "Summary and Histograms",
    uiOutput("summaryandHists")
  ),
  accordion_panel(
    value = "subsetMap",
    "Map",
    leaflet::leafletOutput("subsetMap")
  )
)

sidebar_multivar <- layout_sidebar(
  sidebar = sidebar(
    open = "always",
    selectInput("climVarSub", "Select Variables", list(""), multiple = TRUE),
    uiOutput("sliders"),
    actionButton("slidersButton", "Filter Data"),
    actionButton("resetButton", "Reset filters"),
    uiOutput("MapDlBtns")
  ),
  layout_column_wrap(
    fill = FALSE,
    value_box(
      title = "Number of accessions in the subset",
      value = textOutput("rowsNumber"),
      showcase = bs_icon("funnel-fill", size = "1.5em")
    )
  ),
  accordion(
    id = "multivar_accd",
    open = "subSumHist",
    !!!multivar_accordions
  )
)

sidebar_kmeans <- layout_sidebar(
  sidebar = sidebar(
    open = "always",
    radioButtons("kmDataSrc",
                 "",
                 selected = "allDataKm",
                 c("Original Data" = "allDataKm","Filtered Data" = "filtDataKm"),
                 inline = FALSE),
    shinyWidgets::pickerInput("kmx",
                              "Select Variables",
                              list(""),
                              options = list(`actions-box` = TRUE,
                                             `live-search` = TRUE,
                                             style = "picker"),
                              multiple = TRUE),
    kMeansClusteringUI("kMeansClustering"),
    actionButton("kmeansBtn", "Apply Clustering"),
    downloadButton("downloadClusterData", "Download")
  ),
  accordion(
    id = "kmeans_accd",
    open = FALSE,
    verbatimTextOutput("totkm"),
    leaflet::leafletOutput('mapcluster')
  )
)

pca_accordions <- list(
  accordion_panel(
    value = "pcaSummary",
    "Summary",
    verbatimTextOutput("summaryPca"),
    plotly::plotlyOutput("plotpc")
  ),
  accordion_panel(
    value = "pcaPlot",
    "PCA Plot",
    uiOutput('pc_x_axis', style = 'display: inline-block; width: 250px; margin-left: 15px; margin-right: 30px;'),
    uiOutput('pc_y_axis', style = 'display: inline-block; width: 250px; margin-right: 30px;'),
    uiOutput('pca_color_var', style = 'display: inline-block; width: 250px;'),
    plotly::plotlyOutput("pcaPlot")
  ),
  accordion_panel(
    value = "geoMap",
    "Map",
    uiOutput('selectScore'),
    leaflet::leafletOutput('geoMap')
  )
)

sidebar_pca <- layout_sidebar(
  sidebar = sidebar(
    open = "always",
    radioButtons("pcaDataSrc",
                 "",
                 selected = "allDataPca",
                 c("Original Data" = "allDataPca","Filtered Data" = "filtDataPca"),
                 inline = FALSE),
    includeMarkdown("Rmd/pca_guide.Rmd"),
    shinyWidgets::pickerInput("pca_var",
                              "Select Variables",
                              list(""),
                              options = list(`actions-box` = TRUE,
                                             `live-search` = TRUE,
                                             style = "picker"),
                              multiple = TRUE),
    pcaSummaryUI("pcaSummary"),
    actionButton("PCAsummary", "Get result")
  ),
  accordion(
    id = "pca_accd",
    open = FALSE,
    !!!pca_accordions
  )
)

cc_accordions <- list(
  accordion_panel(
    value = "coreMap",
    "Map",
    leaflet::leafletOutput("coreMap")
  ),
  accordion_panel(
    value = "dist",
    "Distribution per Group",
    plotly::plotlyOutput("corePlot")
  ),
  accordion_panel(
    value = "coreRes",
    "Table",
    DT::dataTableOutput("coreDataTable")
  )
)

sidebar_cc <- layout_sidebar(
  sidebar = sidebar(
    open = "always",
    radioButtons("coreDataSrc",
                 "",
                 selected = "allDataCC",
                 c("All data" = "allDataCC", "Filtered data" = "filtDataCC"),
                 inline = FALSE),
    coreCollectionUI("coreCollection"),
    actionButton("coreButton", "Core Collection"),
    downloadButton("coreDLbutton", label = "Download Core Collection")
  ),
  accordion(
    id = "cc_accd",
    open = "coreMap",
    !!!cc_accordions
  )
)

trait_accordions <- list(
  accordion_panel(
    value = "traitTable",
    "Traits Descriptors",
    DT::dataTableOutput("TraitTbl")
  ),
  accordion_panel(
    value = "traitDataTable",
    "Traits Data",
    DT::dataTableOutput("TraitDataTbl")
  ),
  accordion_panel(
    value = "traitDataSum",
    "Summary",
    DT::dataTableOutput("TraitDataSum")
  ),
  accordion_panel(
    value = "traitResPlot",
    "Plot",
    uiOutput("trait.var.val")
  ),
  accordion_panel(
    value = "traitMap",
    "Map",
    leaflet::leafletOutput("traitMap")
  )
)

sidebar_trait <- layout_sidebar(
  sidebar = sidebar(
    open = "always",
    includeMarkdown("Rmd/getTraits.Rmd"),
    uiOutput("cropSelected"),
    actionButton("getTraits", "Get Traits Descriptors"),
    selectInput("IG.Trait",
                "Select IG", 
                c("IG" = "")),
    selectInput("traitName",
                "Select Trait",
                c("Trait" = "")),
    actionButton("getTraitsData", "Get Traits Data")
  ),
  accordion(
    id = "trait_accd",
    open = FALSE,
    !!!trait_accordions
  )
)


shinyUI(
      page_navbar(
        theme = bs_theme(version = 5, bootswatch = "sandstone"),
        title = "ICARDA FIGS",
        fillable = c("2,3"),
                 id = "tabs",
                 collapsible = TRUE,
                 nav_panel("Data Extraction", value = 1),
                 nav_panel("Climate Data Analysis", value = 2),
                 nav_panel("Trait Analysis", value = 3),
                 fluidRow(
                   conditionalPanel("input.tabs == 1",
                                    card(
                                      style = "width:95%; margin-left: 30px;",
                                      card_header("Passport Data"),
                                      sidebar_passport
                                    ),
                                    card(
                                      style = "width:95%;margin-left: 30px;",
                                      card_header("Climate Data"),
                                      sidebar_climdata
                                    )
                                    )
      ),
                   conditionalPanel("input.tabs == 2",
                                    navset_card_underline(
                                      nav_panel("Multivariate Filtering",
                                                sidebar_multivar
                                               ),
                                      nav_panel("K-means Clustering",
                                                sidebar_kmeans
                                               ),
                                      nav_panel("PCA Analysis",
                                                sidebar_pca
                                               ),
                                      nav_panel("Core Collection",
                                                sidebar_cc
                                               )
                                      )
                                    ),
                   conditionalPanel("input.tabs == 3",
                                      sidebar_trait
                                    )
                 )
  )