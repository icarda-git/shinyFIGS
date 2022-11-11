getAccessionsCropUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("crop"), "Select a crop", c("Crop" = "", crops[2])),
    selectInput(ns("ori"), "Select country(ies)", multiple = T, choices = c("Countries" = "", countries[1]), selected = ""),
    checkboxInput(ns("coor"), "coordinates", value = TRUE),
    checkboxInput(ns("doi"), "DOI", value = FALSE),
    checkboxInput(ns("taxon"), "Taxon", value = FALSE),
    checkboxInput(ns("colYear"), "Collection Year", value = FALSE),
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
    taxon <- input$taxon
    collectionYear <- input$colYear
    available <- input$avail
    rv$countryCode <- countryCode(countryName = countryName)
    
    # query ICARDA database
    withProgress(message = "Querying ICARDA DB ...", {
      df <- icardaFIGSr::getAccessions(crop = rv$crop, ori = rv$countryCode, coor = coor, doi = doi, taxon = taxon, collectionYear = collectionYear, available = available)
    })

    df <- as.data.frame(df)
    df[["PopulationType"]] <- factor(df[["PopulationType"]])
    df[["Country"]] <- factor(df[["Country"]])
    
    if(taxon){
      df[["Taxon"]] <- factor(df[["Taxon"]])
    }
    
    if(available){
      df[["AVAIL_LBN"]] <- factor(df[["AVAIL_LBN"]])
      df[["AVAIL_MA"]] <- factor(df[["AVAIL_MA"]])
      df[["AVAIL_SYR"]] <- factor(df[["AVAIL_SYR"]])
    }
    
    # if(collectionYear){
    #   df[["CollectionYear"]] <- factor(df[["CollectionYear"]])
    # }
    
    df
  })
  print("data extracted...")
  return(datasetInputCrop)
  }