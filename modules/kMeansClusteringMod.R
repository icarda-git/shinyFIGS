kMeansClusteringUI <- function(id){
  ns <- NS(id)
  tagList(
    numericInput(ns("k.min"), "k-min", 3, min = 2),
    numericInput(ns("k.max"), "k-max", 10, min = 2)
  )
}
kMeansClusteringMod <- function(input, output, session, rv){

  returnedList <- reactive({
    
    k.min <- isolate(input$k.min)
    k.max <- isolate(input$k.max)
    dataKMx <- rv$dataKMx
    
    cl <- makeCluster(4)
    clusterExport(cl, varlist="dataKMx", envir = environment())
    
    withProgress(message = "Getting optimal value of k ...", {
      k.range <- k.min:k.max
      sil <- parSapply(cl, k.range, FUN = function(k){
        mean(cluster::silhouette(stats::kmeans(dataKMx, centers = k, nstart = 20)$cluster, dist(dataKMx))[, 3])})
      stopCluster(cl)
      
      opt.k.idx <- which.max(sil)
      k.optimum <- k.range[opt.k.idx]
    })    
    
    withProgress(message = "K-means clustering ...", {
      data.cluster <- stats::kmeans(rv$dataKMx, centers =  k.optimum, nstart = 20)
    })

    data.combined <- rv$data4cluster
    data.combined$cluster <- as.factor(data.cluster$cluster)

  
    returnedList <- list(data.cluster, data.combined)
    returnedList
  })
  return(returnedList)
  
}