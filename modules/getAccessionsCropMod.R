getAccessionsCropUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("crop"), "Select a crop", c("Crop" = "", crops[2])),
    selectInput(ns("ori"), "Select country(ies)", multiple = TRUE, choices = c("Countries" = "", countries[1]), selected = ""),
    checkboxInput(ns("coor"), "coordinates", value = TRUE),
    checkboxInput(ns("doi"), "DOI", value = FALSE),
    checkboxInput(ns("avail"), "Availability", value = FALSE),
    checkboxInput(ns("other_id"), "Other IDs", value = FALSE)
  )
}

getAccessionsCropMod <- function(input, output, session, rv){
  
  datasetInputCrop <- reactive({
  
    # getAccessions function parameters
    countryName <- input$ori
    crop <- input$crop
    coor <- input$coor
    doi <- input$doi
    available <- input$avail
    other_id <- input$other_id
    rv$countryCode <- countryCode(countryName = countryName)
    
    # query ICARDA database
    withProgress(message = "Querying ICARDA DB ...", {
      df <- icardaFIGSr::getAccessions(crop = crop, ori = rv$countryCode, 
                                       coor = coor, doi = doi, taxon = TRUE, 
                                       collectionYear = TRUE, available = available,
                                       other_id = other_id)
    })

    df <- as.data.frame(df)
    #remove columns having all rows empty
    df <- Filter(function(x) !(all(x=="")), df)
    
    df[["PopulationType"]] <- factor(df[["PopulationType"]])
    df[["Country"]] <- factor(df[["Country"]])
    df[["Taxon"]] <- factor(df[["Taxon"]])
    df[["CollectionYear"]] <- as.integer(df[["CollectionYear"]])
    
    if(available){
      df["Availability"] <- factor(df["Availability"])
    }
    
    if(other_id){
      other_column_names <- c("OTHER_ACCENUMB","OTHER_ACCENUMB_INST",
                              "LOCAL_NAME","BREEDER_DESIGNATION",
                              "CULTIVAR_NAME","GENETIC_STOCK",
                              "COMMON_NAME","PEDIGREE")
      other_column_names <- intersect(names(df), other_column_names)
      df[other_column_names] <- lapply(df[other_column_names], factor)
    }
    
    df
  })
  
  return(datasetInputCrop)
  }