extractWCDataUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("lng"), "Select Longitude", list("")),
    selectInput(ns("lat"), "Select Latitude", list("")),
    selectInput(ns("var"), "Select climatic variable(s)", multiple = T, c("Var" = "", c('average temperature'='tavg', 'minimum temperature'='tmin', 'maximum temperature'='tmax', 'precipitation'='prec', 'solar radiation'='srad', 'water vapor pressure'='vapr', 'wind speed'='wind', 'Bioclimatic variables'='bio'))),
    #selectInput(ns("res"), "Select a resolution", choices =  c("2.5","5","10"), selected = 2.5),
    actionButton(ns("extractWC"), "Extract World Clim Data"),br(),br(),
    uiOutput(ns("bio_vars"))
  )
}

extractWCDataMod <- function(input, output, session, rv){
  
  observe({
    req(rv$datasetInput)
    dataSetColNames <- names(rv$datasetInput)
    #lng <- search4pattern(pattern = c("long*","lng"), obj = dataSetColNames)
    #lat <- search4pattern(pattern = c("^lat*"), obj = dataSetColNames)
    updateSelectInput(session, "lng", label = "Select Longitude", choices = c("",dataSetColNames))
    updateSelectInput(session, "lat", label = "Select Latitude", choices = c("",dataSetColNames))
  })

  WCdata <- eventReactive(input$extractWC,{
    rv$extractWC <- input$extractWC
    isolate( rv$lng <- input$lng) ## longitude column name from the dataset obtained in the first module
    rv$lat <- isolate(input$lat) ## latitude column name from the dataset obtained in the first module
    var <- input$var ## climatic variable
    #res <- input$res ## resolution
    
    req(rv$datasetInput)
    req(rv$lng, rv$lat, var)
    print(input$var)

    withProgress(message = "Extracting World Clim Data ...", {
      WCdata <- extractWCdata(rv$datasetInput, long = rv$lng, lat = rv$lat, var = var)
    })
    WCdata
    
  })
  
  
    output$bio_vars <- renderUI({
      if("bio" %in% input$var){
        includeMarkdown("Rmd/bio_vars.Rmd")
      }
    })
  
  
  return(WCdata)
  
}

