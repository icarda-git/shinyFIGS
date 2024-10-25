extractWCDataUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("var"), "Select climatic variable(s)", multiple = T, c("Var" = "", c('average temperature'='tavg', 'minimum temperature'='tmin', 'maximum temperature'='tmax', 'precipitation'='prec', 'solar radiation'='srad', 'water vapor pressure'='vapr', 'wind speed'='wind', 'Bioclimatic variables'='bio'))),
    uiOutput(ns("bio_vars"))
  )
}

extractWCDataMod <- function(input, output, session, rv){

  WCdata <- reactive({
    req(rv$datasetInput)
    df_cleaned <- rv$datasetInput %>%
      filter(PopulationType!="Genetic stock" & PopulationType!="Unreleased breeding material")
    withProgress(message = "Extracting World Clim Data ...", {
      WCdata <- extractWCdata(df_cleaned, long = rv$lng, lat = rv$lat, var = input$var)
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

