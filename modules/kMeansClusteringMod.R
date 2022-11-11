kMeansClusteringUI <- function(id){
  ns <- NS(id)
  tagList(
    # shinyWidgets::pickerInput(ns("kmx"), "Select Variables", list(""), options = list(`actions-box` = TRUE, `live-search` = TRUE, style = "picker"), multiple = T),
    #numericInput(ns("kmcenters"), "centers", 3, min = 1)
    numericInput(ns("k.min"), "k-min", 3, min = 2),
    numericInput(ns("k.max"), "k-max", 10, min = 2)
    #actionButton(ns("kmeansBtn"), "Clustering and Mapping")
  )
}
kMeansClusteringMod <- function(input, output, session, rv){
  
  # observe({
  #   shinyWidgets::updatePickerInput(session, "kmx", label = "Select Variables", choices = rv$worldClimVar)
  # })
  
  k.optimum <- reactive({
    #kmx <- input$kmx
    k.min <- isolate(input$k.min)
    k.max <- isolate(input$k.max)
    
    # dataKMx <- rv$data4cluster %>% select(all_of(kmx))
    # rv$dataKMx <- stats::na.omit(dataKMx)
    
    withProgress(message = "Getting Optimum k ...", {
      k.range <- k.min:k.max
      sil <- sapply(k.range, FUN = function(k){
        mean(cluster::silhouette(stats::kmeans(rv$dataKMx, centers = k, nstart = 20)$cluster, dist(rv$dataKMx))[, 3])})
      print(sil)
      opt.k.idx <- which.max(sil)
      k.optimum <- k.range[opt.k.idx]
      k.optimum
    })
  })

  returnedList <- reactive({
      req(k.optimum())
      # kmx <- input$kmx
      # k.min <- input$k.min
      # k.max <- input$k.max
      # 
      # dataKMx <- rv$data4cluster %>% select(all_of(kmx))
      # dataKMx <- stats::na.omit(dataKMx)
      # 
      # withProgress(message = "Getting Optimum k ...", {
      #   sil <- c(0, sapply(k.min:k.max, FUN = function(k){ mean(cluster::silhouette(stats::kmeans(dataKMx, centers = k)$cluster, dist(dataKMx))[, 3])}))
      #   
      #   k.optimum <- which.max(sil)
      # })
    
    withProgress(message = "K-means clustering ...", {
      #print('k-means...')
      data.cluster <- stats::kmeans(rv$dataKMx, centers =  k.optimum(), nstart = 20)
      #print(paste('data.cluster: ',nrow(rv$dataKMx)))
    })
    #desiredCols <- names(rv$dataKMx)

    #completeCas <- stats::complete.cases(rv$data4cluster[,desiredCols])
    #data.combined <- rv$data4cluster[complete.cases(rv$dataKMx), ]
    #compWCdata <- rv$data4cluster[completeCas, ]
    
    #print(paste('data.combined: ',nrow(compWCdata)))
    data.combined <- rv$data4cluster
    data.combined$cluster <- as.factor(data.cluster$cluster)

    # p <- ggplot(compWCdata, aes(Longitude, Latitude))
  
    returnedList <- list(data.cluster, data.combined)
    returnedList
  })
  return(returnedList)
  
}