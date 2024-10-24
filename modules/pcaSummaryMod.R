pcaSummaryUI <- function(id){
  ns <- NS(id)
  tagList(
    #shinyWidgets::pickerInput(ns("PCAvar"), "Select Variables", list(""), options = list(`actions-box` = TRUE, `live-search` = TRUE, style = "picker"), multiple = T),
    # selectInput(ns("method"), "Select PCA method", choices = c("svd","nipals","rnipals","bpca","ppca","svdImpute","robustPca","nlpca","llsImpute","llsImputeAll"), selected = "svd"),
    selectInput(ns("scale"), "Select Scale", choices = c("uv","vector","pareto","none"), selected = "uv"),
    checkboxInput(ns("center"), "Center", value = TRUE)
  )
}
pcaSummaryMod <- function(input, output, session, rv){
  
  # observe({
  #   shinyWidgets::updatePickerInput(session, "PCAvar", choices = rv$worldClimVar)
  # })
  pc <- reactive({
    # varSel <- input$PCAvar
    req(rv$pca_var, rv$data4pca)
    rv$dataWCSub <- rv$data4pca %>% dplyr::select(all_of(rv$pca_var)) ## selected columns of WC data 
    completeVec <- stats::complete.cases(rv$data4pca[ ,names(rv$dataWCSub)]) 
    rv$completeData <- rv$data4pca[completeVec, ]
    
    if(!rv$filteredPca & !is.null(rv$clusterDataAll[[1]])){
      Cluster <-  factor(rv$clusterDataAll[[1]]$cluster)
      rv$completeData <- cbind(Cluster, rv$data4pca[completeVec, ])
    }
    else if(rv$filteredPca & !is.null(rv$clusterDataFilt[[1]])){
      Cluster <-  factor(rv$clusterDataFilt[[1]]$cluster)
      rv$completeData <- cbind(Cluster, rv$data4pca[completeVec, ])
    }
    withProgress(message = "Executing PCA ...", {
    pc <- pcaMethods::pca(stats::na.omit(rv$dataWCSub), method = 'svd', nPcs = 5, scale = input$scale, center = input$center)
    })
    pc
  })
  
   return(pc)
}