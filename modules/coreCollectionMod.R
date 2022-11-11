coreCollectionUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("allocMeth"), "Select allocation method", choices = c("Propotional" = "Pro","Logarythmic" = "Log","D2 allocation method" = "D2","D3 allocation method" = "D3"), selected = "Propotional"),
    selectInput(ns("clustMeth"), "Select cluster analysis method", choices = c( "average", "single", "complete", "ward"), selected = "ward"),
    numericInput(ns("fraction"), "Select fraction", 0.1, min = 0, max = 1, step = 0.1)
  )
}

coreCollectionMod <- function(input, output, session, rv){

  core <- reactive({
    
    alloc <- isolate(input$allocMeth)
    cluster_method <- isolate(input$clustMeth)
    fraction <- isolate(input$fraction) 
    
    req(rv$data4core)
    
    groups <- rv$data4core$cluster
    
    #### fixing daisy error regarding character columns
    entireDataColnames <- names(rv$data4core)
    lapply(entireDataColnames, function(coln){
      if(is.character(rv$data4core[[coln]])){
        rv$data4core[[coln]] <- as.factor(rv$data4core[[coln]])
      }
    })
    withProgress(message = "Developing Core Collection ...", {
    core <- ccChooser::stratcc(x = rv$data4core, groups = groups, alloc = alloc, fraction = fraction, clustering = T, cluster_method = cluster_method)
    })
    core

   })
 
  return(core)
}