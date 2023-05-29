getAccessionsCropUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("crop"), "Select a crop", c("Crop" = "", crops[2])),
    selectInput(ns("ori"), "Select country(ies)", multiple = TRUE, choices = c("Countries" = "", countries[1]), selected = ""),
    checkboxInput(ns("coor"), "coordinates", value = TRUE),
    checkboxInput(ns("doi"), "DOI", value = FALSE),
    checkboxInput(ns("avail"), "Availability", value = FALSE)
  )
}

getAccessionsCropMod <- function(input, output, session, rv){
  
  datasetInputCrop <- reactive({
  
    # getAccessions function parameters
    countryName <- input$ori
    rv$crop <- input$crop
    coor <- input$coor
    doi <- input$doi
    available <- input$avail
    rv$countryCode <- countryCode(countryName = countryName)
    
    # query ICARDA database
    withProgress(message = "Querying ICARDA DB ...", {
      df <- icardaFIGSr::getAccessions(crop = rv$crop, ori = rv$countryCode, coor = coor, doi = doi, taxon = TRUE, collectionYear = TRUE, available = available)
    })

    df <- as.data.frame(df)
    df[["PopulationType"]] <- factor(df[["PopulationType"]])
    df[["Country"]] <- factor(df[["Country"]])
    df[["Taxon"]] <- factor(df[["Taxon"]])
    df[["CollectionYear"]] <- as.integer(df[["CollectionYear"]])
    
    if(available){
      df[["AVAIL_LBN"]] <- factor(df[["AVAIL_LBN"]])
      df[["AVAIL_MA"]] <- factor(df[["AVAIL_MA"]])
    }
    
    df
  })
  
  return(datasetInputCrop)
  }