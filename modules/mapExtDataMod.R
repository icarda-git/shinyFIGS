uploadDataUI <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns("extData"), "Upload CSV File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"))
  )
}

uploadDataMod <- function(input, output, session){
  readDataCSV <- reactive({
    req(input$extData)
    inextFile <- input$extData
    df <- read.csv(inextFile$datapath, header = T, sep = ",")
    return(df)
  })
  return(readDataCSV)
}
