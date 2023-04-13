library(shiny)
library(dplyr)
library(leaflet)


source(file.path('./functions/functions.R'), local = TRUE)

for (f in list.files('./modules')) {
  source(file.path('modules', f), local = TRUE)
}

countries <- readRDS("data/countries.rds")

function(input, output, session) {
  
  rv <- reactiveValues()
  
  shinyjs::disable("downloadAcc")
  
  # create map
  output$map <- renderLeaflet(
    leaflet() %>%
      setView(0, 0, zoom = 2) %>%
      addProviderTiles('Esri.WorldGrayCanvas')
  )
  
  output$WCMap <- renderLeaflet(
    leaflet() %>%
      setView(0, 0, zoom = 2) %>%
      addProviderTiles('Esri.WorldGrayCanvas')
  )
  
  output$subsetMap <- renderLeaflet(
    leaflet() %>%
      setView(0, 0, zoom = 2) %>%
      addProviderTiles('Esri.WorldGrayCanvas')
  )
  
  output$mapcluster <- renderLeaflet(
    leaflet() %>%
      setView(0, 0, zoom = 2) %>%
      addProviderTiles('Esri.WorldGrayCanvas')
  )
  
  output$geoMap <- renderLeaflet(
    leaflet() %>%
      setView(0, 0, zoom = 2) %>%
      addProviderTiles('Esri.WorldGrayCanvas')
  )
  
  output$coreMap <- renderLeaflet(
    leaflet() %>%
      setView(0, 0, zoom = 2) %>%
      addProviderTiles('Esri.WorldGrayCanvas')
  )
  
  map <- leafletProxy("map")
  WCMap <- leafletProxy("WCMap")
  subsetMap <- leafletProxy("subsetMap")
  mapcluster <- leafletProxy("mapcluster")
  geoMap <- leafletProxy("geoMap")
  coreMap <- leafletProxy("coreMap")
  
  observe({
    if(input$dataSrc == 'extData')
      hideTab(inputId = "main", target = "accPlot")
    else
      showTab(inputId = "main", target = "accPlot")
  })
  
  #Extract data from ICARDA DB by crop name
  datasetInputCrop <- callModule(getAccessionsCropMod, "getAccessionsCrop", rv)
  
  observeEvent(input$getAcc,{
    rv$passportCrop <- datasetInputCrop()
    updateTabsetPanel(session, 'main', selected = 'accResult')
    shinyjs::enable("downloadAcc")
  })
  
  #extract accessions based on IG
  dataIG <- callModule(uploadDataMod, "uploadIGData")
  
  observe({
    columns <- names(dataIG())
    updateSelectInput(session, "IG", label = "Select Identifier Column", choices = columns)
  })
  
  datasetInputIG <- eventReactive(input$getAccIG, {
    IG <- input$IG
    countryName <- input$oriIG
    countryCodeIG <- countryCode(countryName = countryName)
    withProgress(message = "Querying ICARDA DB ...", {
    df <- icardaFIGSr::getAccessions(IG = dataIG()[[IG]], coor = TRUE, ori = countryCodeIG)
    })
    df
  })
  
  observeEvent(input$getAccIG,{
    updateTabsetPanel(session, 'main', selected = 'accResult')
    shinyjs::enable("downloadAcc")
  })
    
  #get uploaded data
  dataUpload <- callModule(uploadDataMod, "uploadData")

  observe({
    req(dataUpload())
    rv$datasetInput <- dataUpload()
  })
    
  #output: table + map 
    
  output$table <- DT::renderDataTable({
    
    if(input$dataSrc == 'byCrop'){
      req(rv$passportCrop)
      DT::datatable(rv$passportCrop, filter = list(position = "top", clear = FALSE), options = list(pageLength = 10, scrollX = TRUE), callback = DT::JS(" //hide column filters for specific columns
      $.each([2, 5, 6, 7, 8], function(i, v) {
                                     $('input.form-control').eq(v).hide()
                                     });"))
    }
    
    else{
      
      if(input$dataSrc == 'byIG'){
        rv$datasetInput <- datasetInputIG()
      }
      
      if(input$dataSrc == 'extData'){
        rv$datasetInput <- dataUpload()
      }
        
      DT::datatable(rv$datasetInput, options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  # update filter dropdowns
  # credit to @mikmart in https://github.com/rstudio/DT/pull/982
  filterable_sets <- eventReactive(input$table_search_columns, {
    if(input$dataSrc == 'byCrop'){
    # Get separate filtered indices
    fi <- Map(DT::doColumnSearch, rv$passportCrop, input$table_search_columns);
  
    # Find available rows after filtering
    ai <- lapply(seq_along(fi), function(j) {Reduce(intersect, fi[-j])});
    
    # Get the corresponding data
    lapply(Map(`[`, rv$passportCrop, ai), function(x){
      if (is.factor(x)) droplevels(x) else x
    })
    }
  })
  
  # update the columns filters
  proxy <- DT::dataTableProxy("table")
  observeEvent(filterable_sets(), {
    DT::updateFilters(proxy, filterable_sets())
  })
  
  observeEvent(input$table_rows_all,{
    if(input$dataSrc == 'byCrop'){
      rv$datasetInput <- rv$passportCrop[input$table_rows_all,]
      rv$datasetInput
    }
  })
  
  rv$ycolumns <- reactive({
    names(rv$datasetInput)
  })
  
  output$selectUI_1 <- renderUI({
    freezeReactiveValue(input, "y")
    selectInput("y", "Select a variable", choices = c("None", rv$ycolumns()))
  })
  
  output$coords <- renderUI({
    if(input$dataSrc == "extData"){
      coord <- list(
        selectInput("long", "Select longitude column", c("", rv$ycolumns())),
        selectInput("lat", "Select latitude column", c("", rv$ycolumns()))
      )
      do.call(tagList, coord)
    }
  })
  
  observe({
    if(input$dataSrc=="extData"){
      rv$lng <- input$long
      rv$lat <- input$lat
    }
    else{
      rv$lng <- "Longitude"
      rv$lat <- "Latitude"
    }
  })
  
  observe({
    req(rv$datasetInput, input$y, rv$lng, rv$lat)
    y <- input$y
    
    mapAccessions(map, df = rv$datasetInput, long = rv$lng, lat = rv$lat, y = y)
  })
  
  # Statistical plot of variables from dataset extracted by crop name
  output$first_var <- renderUI({
    req(rv$ycolumns())
    rv$vars_plotted <- c('Country','PopulationType','Taxon')
    selectInput("var_plot", "Select a variable", choices = c(rv$vars_plotted))
  })
  
  output$plot <- plotly::renderPlotly({
    req(rv$datasetInput)
    plotly::plot_ly(rv$datasetInput, x = rv$datasetInput[[input$var_plot]], color = "#ff8103") %>%
      plotly::add_histogram()
  })
  
  output$second_var <- renderUI({
    req(rv$ycolumns())
    vars_plotted <- setdiff(rv$vars_plotted, c(input$var_plot))

    selectInput("var2_plot", "Select a second variable", choices = c(vars_plotted))
  })
  
  output$bi_plot <- plotly::renderPlotly({
    req(rv$datasetInput)
    plotly::plot_ly(rv$datasetInput, x = rv$datasetInput[[input$var_plot]], color = rv$datasetInput[[input$var2_plot]]) %>%
      plotly::add_histogram() 
  })
  
  output$downloadAcc <- downloadHandler(
    filename = function() {
      paste0("passport_data",".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$datasetInput, file, row.names = FALSE)
    }
  )
  
  ########################################################
  ############  Extracting World Clim Data   #############
  ########################################################
  
  WCdata <- callModule(extractWCDataMod, "extractWCData", rv)
  
  observeEvent(input$extractWC,{
    freezeReactiveValue(rv, "dfSub")
    rv$WCdata <- WCdata()
    updateTabsetPanel(session, "wcMainPanel", selected = "WCTable")
  })
  
  output$selectUI_2 <- renderUI({
    rv$climateVars <- search4pattern(c('tavg*', 'tmin*', 'tmax*', 'prec*', 'bio*', 'srad*', 'vapr*', 'wind*'), names(rv$WCdata))
    freezeReactiveValue(input, "clim_var")
    selectInput("clim_var", "Select a variable", choices = c("None",rv$climateVars), selected="None")
  })
  
  output$WCtable <- DT::renderDataTable({
    DT::datatable(rv$WCdata, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  observe({
    req(rv$lng, rv$lat, rv$WCdata, input$clim_var)
    mapAccessions(WCMap, df = rv$WCdata, long = rv$lng, lat = rv$lat, y = input$clim_var)
  })
  
  output$downloadWCData <- downloadHandler(
    filename = function() {
      paste0("WorldClimData",".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$WCdata, file, row.names = F)}
  )

  observe({
    rv$WCDataNames <- names(rv$WCdata)
    rv$worldClimVar <- search4pattern(c('tavg*', 'tmin*', 'tmax*', 'prec*', 'bio*', 'srad*', 'vapr*', 'wind*'), rv$WCDataNames)
  })
  
  ##############################################################################
  ###################  Climate Variables based Subsetting   ####################
  ############################################################################## 
  
  climVarSub <- callModule(multiVarAnalysisMod, "multiVarAnalysis", rv)
  observe({
    rv$climVarSub <- climVarSub()
  })
  
  observeEvent(c(rv$slidersBtn, input$resetButton),{
    updateTabsetPanel(session, "subsetMain", selected = "subSumHist")
    req(rv$climVarSub)
    output$sliders <- renderUI({
      
      sliders <- lapply(1:length(rv$climVarSub), function(i) {
        
        inputName <- rv$climVarSub[i]
        min <- min(rv$WCdata[[inputName]], na.rm = T)
        max <- max(rv$WCdata[[inputName]], na.rm = T)
        
        list(
          sliderInput(inputName, inputName, min = min, max = max, value = c(min,max))
        )
      })
      do.call(tagList, sliders)
    })
    
    output$summaryandHists <- renderUI({
      summariesandHists <- lapply(1:length(rv$climVarSub), function(i) {
        printname <- paste("sumClimVar", i, sep = "")
        plotname <- paste("histo", i, sep = "")
        list(verbatimTextOutput(printname),
             plotly::plotlyOutput(plotname, height = 300)
        )
      })
      do.call(tagList, unlist(summariesandHists, recursive = F))
    })
  })
  
  observe({
    req(rv$climVarSub)
    
    lapply(
      X = 1:length(rv$climVarSub),
      FUN = function(i){
        
        inputVar <- rv$climVarSub[i]
        plotname <- paste("histo", i, sep = "")
        printname <- paste("sumClimVar", i, sep = "")
        
        evalInVar <- input[[inputVar]]
        req(evalInVar)
        subsettingCnd <- ""
        
        observeEvent(input[[inputVar]], {
          for(i in 1:(length(rv$climVarSub)-1)){
            inputVari <- rv$climVarSub[i]
            
            ### defining the subsetting condition "subsettingCnd"
            subsettingCnd <- paste0(subsettingCnd,"(rv$WCdata$", inputVari, " >= input$", inputVari,"[1]", ") & (rv$WCdata$", inputVari, " <= input$", inputVari,"[2]) & !is.na(rv$WCdata$",inputVari,") & ")
          }
          lastVar <- rv$climVarSub[length(rv$climVarSub)]
          subsettingCnd <- paste0(subsettingCnd, "(rv$WCdata$", lastVar, " >= input$", lastVar,"[1]", ") & (rv$WCdata$", lastVar, " <= input$", lastVar,"[2]) & !is.na(rv$WCdata$",lastVar,")")
          
          rv$dfSub <- rv$WCdata[eval(parse(text = subsettingCnd)),]
          for(i in 1:length(rv$climVarSub)){
            
            newMin = min(rv$dfSub[[rv$climVarSub[i]]], na.rm = T)
            newMax = max(rv$dfSub[[rv$climVarSub[i]]], na.rm = T)
            updateSliderInput(session, inputId = rv$climVarSub[i], value = c(newMin,newMax) )
          }
        })
        output[[printname]] <- renderPrint({
          print(inputVar)
          summary(rv$dfSub[[inputVar]])
        })
        
        output[[plotname]] <- plotly::renderPlotly({
          plotly::plot_ly(rv$dfSub, x = rv$dfSub[[inputVar]], color = "#ff8103") %>% plotly::add_histogram() %>% plotly::add_annotations(
            inputVar, x = 0.5, y = 1, 
            xref = "paper", yref = "paper", showarrow = FALSE
          )
          #hist(rv$dfSub[[inputVar]], col = "#ff8103", border = "white", xlab = inputVar, main = NULL)
        })
        
        # output[[plotname]] <- renderPlot({
        #   hist(rv$dfSub[[inputVar]], col = "#ff8103", border = "white", xlab = inputVar, main = NULL)
        # })
      })
  })
  
  output$dataDescription <- renderUI({
    req(rv$climVarSub)
    verbatimTextOutput("rowsNumber") 
  })
  
  output$rowsNumber <- renderPrint({
    print(paste("Number of accessions: ", nrow(rv$dfSub)))
  })
  
  output$MapDlBtns <- renderUI({
    req(rv$climVarSub)
    #MapDlBtns <- list(
    #actionButton("mapSubBtn", "Map Data"),
    downloadButton("DataSubset", "Download")
    #)
    #do.call(tagList, MapDlBtns)
  })
  
  observe({
    req(rv$lng, rv$lat, rv$WCdata, rv$dfSub)
    map_two_dfs(subsetMap, rv$WCdata, rv$dfSub, lng = rv$lng, lat = rv$lat, type = "Data Subset")
  })
  
  output$DataSubset <- downloadHandler(
    filename = function() {
      paste0("WorldClimDataSubset",".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$dfSub, file, row.names = F)}
  )
  
  ########################################################
  ################  K-Means Clustering   #################
  ########################################################
  
  observe({
    shinyWidgets::updatePickerInput(session, "kmx", label = "Select Variables", choices = rv$worldClimVar)
  })
  
  observeEvent(input$kmeansBtn, {
    
    if(input$kmDataSrc == "allDataKm"){
      rv$data4cluster <- rv$WCdata
    }
    else if (input$kmDataSrc == "filtDataKm"){
      rv$data4cluster <- rv$dfSub
    }
    
    dataKMx <- rv$data4cluster %>% select(all_of(input$kmx))
    
    rv$data4cluster <- rv$data4cluster[complete.cases(dataKMx), ]
    dataKMx <- dataKMx[complete.cases(dataKMx),]
    rv$dataKMx <- scale(dataKMx)
    
    cluster.res <- callModule(kMeansClusteringMod, "kMeansClustering", rv)
    rv$clusterRes <- cluster.res()
    
    if(input$kmDataSrc == "allDataKm"){
      rv$clusterDataAll <- rv$clusterRes
    }
    else if (input$kmDataSrc == "filtDataKm"){
      rv$clusterDataFilt <- rv$clusterRes
    }
  
    output$totkm <- renderPrint({
      req(rv$clusterRes)
      paste("tot.withinss: ", rv$clusterRes[[1]]$tot.withinss," betweenss: ", rv$clusterRes[[1]]$betweenss)
    })
  })
  
  observe({
    #output$mapcluster <- leaflet::renderLeaflet({
    req(rv$lng, rv$lat, rv$clusterRes)
    pal <- leaflet::colorFactor(
      palette = "viridis",
      domain = rv$clusterRes[[2]]$cluster
    )
    
    mapcluster %>% clearMarkers() %>%
      clearControls() %>% removeLayersControl() %>%
      leaflet::addCircleMarkers(data = rv$clusterRes[[2]],
                                lng = rv$clusterRes[[2]][[rv$lng]],
                                lat = rv$clusterRes[[2]][[rv$lat]],
                                color = ~pal(rv$clusterRes[[2]][["cluster"]]),
                                radius = 2,
                                fill = TRUE,
                                fillColor = ~pal(rv$clusterRes[[2]][["cluster"]]),
                                label = ~rv$clusterRes[[2]][["cluster"]],
                                fillOpacity = 1, weight = 0.1) %>%
      leaflet::addLegend(pal = pal, values = rv$clusterRes[[2]][["cluster"]], opacity = 1,  title = 'Cluster')
  })
  
  output$downloadClusterData <- downloadHandler(
    filename = function() {
      paste0("ClusterData",".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$clusterRes[[2]], file, row.names = F)}
  )
  
  
  ########################################################
  ###################  PCA Analysis   ####################
  ########################################################  
  
  observe({
    shinyWidgets::updatePickerInput(session, "pca_var", label = "Select Variables", choices = rv$worldClimVar)
  })
  
  observeEvent(input$PCAsummary, {
    if(input$pcaDataSrc == "allDataPca"){
      rv$data4pca <- rv$WCdata
      rv$filteredPca <- FALSE
    }
    else if (input$pcaDataSrc == "filtDataPca"){
      rv$data4pca <- rv$dfSub
      rv$filteredPca <- TRUE
    }
    rv$pca_var <- input$pca_var
    pcSummary <- callModule(pcaSummaryMod, "pcaSummary", rv)
    rv$pcaSummary <- pcSummary()
    rv$pcScores <- as.data.frame(rv$pcaSummary@scores)
    updateTabsetPanel(session, 'pca', selected = 'pcaSummary')
  })

  observeEvent(input$PCAPlotButton, {
    updateTabsetPanel(session, 'pca', selected = 'pcaPlot') 
  })
    
  output$summaryPca <- renderPrint({
    req(rv$pcaSummary)
    input$PCAsummary
    summary(rv$pcaSummary)
  })
  
  output$plotpc <- plotly::renderPlotly({
    req(rv$pcaSummary)
    #get R2cum from pcaRes
    R2cum = as.data.frame(rv$pcaSummary@R2cum)
    R2cum = R2cum %>% rename_at(1,~"R2cum")

    plotly::plot_ly(R2cum, y = ~R2cum, type = 'scatter', mode = 'lines', fill = 'tozeroy', color = "#ff8103") %>%
      plotly::layout(xaxis = list(title = list(text ='Components')), yaxis = list(title = list(text ='Cumulative Explained Variance')))
    # plot(rv$pcaSummary, col = "#ff8103", border = "white")
  })
  
  observe({
    if(input$plotRadios == 'plain'){
      shinyjs::hide("pcaPlotVar")
     } 
    else if(input$plotRadios == 'colored') {
      
      output$pcaPlotVar <- renderUI({
        selectInput("pcaPlotVar", label = "Select Variable", choices = names(rv$completeData)) 
      }) 
      
      shinyjs::show("pcaPlotVar")
     }
    }) 
    
  output$pcaPlot <- plotly::renderPlotly({
    input$PCAPlotButton
    if(input$plotRadios == 'plain'){
      #rv$pcaPlot + ggplot2::geom_point()
      color = NULL
    }
    else if(input$plotRadios == 'colored')
    {
      req(input$pcaPlotVar)
      color = rv$completeData[[input$pcaPlotVar]]
      # rv$pcaPlot + ggplot2::geom_point(ggplot2::aes(colour = rv$completeData[[input$pcaPlotVar]])) + ggplot2::labs(colour = input$pcaPlotVar)
    }
    R2.percentage <- 100 * rv$pcaSummary@R2
    
    axis = list(showline=FALSE,
                zeroline=FALSE,
                gridcolor='#ffff',
                ticklen=4,
                titlefont=list(size=13))
    
    plotly::plot_ly(rv$pcScores) %>%
      plotly::add_trace(
        type = 'splom',
        marker = list(
          size = 7,
          line = list(
            width = 1,
            color = 'rgb(230,230,230)'
          )
        ),
        dimensions = list(
          list(label=paste('PC 1 (',toString(round(R2.percentage[1],1)),'%)',sep = ''), values=~PC1),
          list(label=paste('PC 2 (',toString(round(R2.percentage[2],1)),'%)',sep = ''), values=~PC2),
          list(label=paste('PC 3 (',toString(round(R2.percentage[3],1)),'%)',sep = ''), values=~PC3),
          list(label=paste('PC 4 (',toString(round(R2.percentage[4],1)),'%)',sep = ''), values=~PC4),
          list(label=paste('PC 5 (',toString(round(R2.percentage[5],1)),'%)',sep = ''), values=~PC5)
        ),
        color = color) %>%  
      plotly::style(diagonal = list(visible = FALSE)) %>%  
      plotly::layout(
        legend=list(title=list(text=input$pcaPlotVar)),
        hovermode='closest',
        dragmode= 'select',
        plot_bgcolor='rgba(240,240,240,0.95)',
        xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
        yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
        xaxis2=axis,
        xaxis3=axis,
        xaxis4=axis,
        xaxis5=axis,
        yaxis2=axis,
        yaxis3=axis,
        yaxis4=axis,
        yaxis5=axis
      )
    
  })
      
  ##Mapping pca results
  output$selectScore <- renderUI({
    freezeReactiveValue(input, "pcaScore")
    selectInput("pcaScore", "Select a score", choices = names(rv$pcScores))
  })
  
  observe({
    req(rv$lng, rv$lat, rv$completeData, rv$pcScores, input$pcaScore)
    
    pal <- leaflet::colorNumeric(
      palette = "viridis",
      domain = rv$pcScores[[input$pcaScore]]
    )

    geoMap %>% clearMarkers() %>%
      clearControls() %>% removeLayersControl() %>%
      leaflet::addCircleMarkers(data = rv$completeData,
                                lng = rv$completeData[[rv$lng]],
                                lat = rv$completeData[[rv$lat]],
                       color = ~pal(rv$pcScores[[input$pcaScore]]),
                       radius = 2,
                       fill = TRUE,
                       fillColor = ~pal(rv$pcScores[[input$pcaScore]]),
                       label = ~rv$pcScores[[input$pcaScore]],
                       fillOpacity = 1, weight = 0.1) %>%
      leaflet::addLegend(pal = pal,
                         values = rv$pcScores[[input$pcaScore]],
                         opacity = 1,
                         title = input$pcaScore)
  })
  
  ##############################################################################
  ##############################  Core Collection   ############################
  ##############################################################################  
  
  core <- callModule(coreCollectionMod, "coreCollection", rv)
  observeEvent(input$coreButton, {
    if(input$coreDataSrc == "allDataCC"){
      rv$data4core <- rv$clusterDataAll[[2]]
    }
    else if (input$coreDataSrc == "filtDataCC"){
      rv$data4core <- rv$clusterDataFilt[[2]]
    }
    rv$core <- core()
  })
    output$corePlot <- plotly::renderPlotly({
      req(rv$core)
      plotly::plot_ly(rv$core, x = ~cluster, color = "#ff8103") %>% plotly::add_histogram()
    })
    
    output$coreDataTable <- DT::renderDataTable({
      req(rv$core)
      DT::datatable(rv$core, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    observe({
      req(rv$lng, rv$lat, rv$data4core, rv$core)
      map_two_dfs(coreMap, rv$data4core, rv$core, lng = rv$lng, lat = rv$lat, type = "Core Data")
    })
    
    output$coreDLbutton <- downloadHandler(
      filename = function() {
        paste0("coreCollectionData",".csv", sep = "")
      },
      content = function(file) {
        write.csv(rv$core, file, row.names = F)}
    )
    
  #' Traits Analysis
  #'
  #'
    
    output$cropSelected <- renderUI({
      req(rv$crop)
      verbatimTextOutput("selectedCrop") 
    })
    
    output$selectedCrop <- renderPrint({
      print(rv$crop)
    })
    
    observeEvent(input$getTraits,{
      withProgress(message = "Getting Traits ...", {
        rv$traits <- icardaFIGSr::getTraits(rv$crop)
      })
      updateTabsetPanel(session, 'traitMainPanel', selected = 'traitTable')
    })
    
    observe({
      updateSelectInput(session, "IG.Trait", label = "Select IG Column", choices = names(rv$datasetInput))
      updateSelectInput(session, "traitName", label = "Select Trait", choices = rv$traits[['Trait']])
      
    })
    
    observeEvent(input$getTraitsData,{
      withProgress(message = "Getting Traits Data ...", {
        traitId <- rv$traits[rv$traits$Trait == input$traitName, 'ID']
        traitsData <- icardaFIGSr::getTraitsData(IG = rv$datasetInput[[input$IG.Trait]], traitID = as.numeric(traitId))
       
        rv$traitsData <- traitsData %>% mutate_at(input$IG.Trait, funs(round(., 2)))
        rv$traitsData[['YEAR']] = as.factor(rv$traitsData[['YEAR']])
        
        if(!is.na(subset(rv$traits, ID==as.numeric(traitId))$Options)){
        #if(!is.na(rv$traits[rv$traits$ID == as.numeric(traitId), 'Options'])){
          last_column = colnames(rv$traitsData)[length(colnames(rv$traitsData))]
          rv$traitsData[[last_column]] = factor(rv$traitsData[[last_column]])
        }
      })
      updateTabsetPanel(session, 'traitMainPanel', selected = 'traitDataTable')
    })
    
    output$TraitTbl <- DT::renderDataTable(server = FALSE, {
      DT::datatable(rv$traits, 
                    extensions = 'Buttons',
                    options = list(dom = "Bfrtip",
                                   pageLength = 10,
                                   buttons = c('csv', 'excel')))
    })
    
    output$TraitDataTbl <- DT::renderDataTable(server = FALSE, {
      DT::datatable(rv$traitsData,
                    filter = list(position = "top", clear = FALSE),
                    extensions = 'Buttons',
                    options = list(dom = "Bfrtip",
                                   pageLength = 10,
                                   buttons = c('csv', 'excel'),
                                   scrollX = TRUE))
    })
    
    output$TraitDataSum <- DT::renderDataTable(server = FALSE, {
      input$getTraitsData
      rv$traitName <- isolate(input$traitName)
      rv$field.name <- as.character(rv$traits[rv$traits$Trait == rv$traitName, 'Field Name'])
      withProgress(message = "Calculating summary ...", {
        
        if(is.na(rv$traits[rv$traits$Trait == rv$traitName, 'Options'])){
          rv$isTraitNum <- TRUE
          rv$traitSummary <- rv$traitsData %>% group_by(IG) %>% summarise(across(rv$field.name, mean)) %>% mutate_at(rv$field.name, funs(round(., 2)))
        }
        
        else{
          rv$isTraitNum <- FALSE
          rv$traitSummary <- rv$traitsData %>% group_by(IG) %>% summarise(across(rv$field.name, max.frequency)) %>% mutate(across(where(is.character), as.factor))
        }
        
        DT::datatable(rv$traitSummary,
                      filter = list(position = "top", clear = FALSE),
                      extensions = 'Buttons',
                      options = list(scrollX = TRUE,
                                     dom = "Bfrtip",
                                     pageLength = 10,
                                     buttons = c('csv', 'excel'),
                                     columnDefs = list(list(targets = 1, searchable = FALSE))))
      })
      
    })
    
    output$trait.var.val <- renderUI({
      req(rv$traitsData)
      traitPlts <- list(
        #plotly::plotlyOutput("traitBoxPlot"),
        plotly::plotlyOutput("exptIGFreq"),
        plotly::plotlyOutput("yearIGFreq"),
        #selectInput("year.hist", "Select Year", c("Year" = "")),
        plotly::plotlyOutput("hist.or.barplot")
      )
      do.call(tagList, traitPlts)
    })
    
    output$exptIGFreq <- plotly::renderPlotly({
      req(rv$traitsData)
      freq.by.expt <- rv$traitsData %>% group_by(EXPT) %>% summarize(count_IG=length(unique(IG)))
      
      plotly::plot_ly(freq.by.expt, x = freq.by.expt[['EXPT']], y = ~count_IG, type = 'bar', color = "#ff8103") %>%
        plotly::layout(yaxis = list(title = ""), title = list(text = 'No. of unique IGs per EXPT', y = 0.9))
    })
    
    output$yearIGFreq <- plotly::renderPlotly({
      req(rv$traitsData)
      freq.by.year <- rv$traitsData %>% group_by(YEAR) %>% summarize(count_IG=length(unique(IG)))
      
      plotly::plot_ly(freq.by.year, x = freq.by.year[['YEAR']], y = ~count_IG, type = 'bar', color = "#ff8103") %>%
        plotly::layout(yaxis = list(title = ""), title = list( text = 'No. of unique IGs per year', y = 0.9))
      
    })
    
    output$hist.or.barplot <- plotly::renderPlotly({
      
      req(rv$traitsData)
      
      if(!rv$isTraitNum){
        
        freq.by.cat <- rv$traitsData %>% group_by(across(all_of(rv$field.name))) %>% summarize(count_IG=length(unique(IG))) 
        
        plotly::plot_ly(freq.by.cat, x = freq.by.cat[[rv$field.name]], y = ~count_IG, type = 'bar', color = "#ff8103") %>% 
          plotly::layout(yaxis = list(title = ""), title = list(text = 'No. of unique IGs per category', y = 0.9))
        
        #make frequency graph per experiment
        
      }
      else{
        
        # histogram by EXPT
        experiments <- rv$traitsData %>%
          dplyr::arrange(-desc(YEAR)) %>%
          dplyr::select(EXPT)
      
        expts <- unique(experiments[["EXPT"]])
      
        plotly::plot_ly(rv$traitsData, x = rv$traitsData[[rv$field.name]],
                      transforms = list(
                        list(
                          type = 'filter',
                          target = ~EXPT,
                          operation = '=',
                          value = expts[1]
                        )),
                      color = "#ff8103") %>%
          plotly::add_histogram() %>%
          plotly::layout(
            xaxis = list(title = rv$traitName),
            yaxis = list(title = "No. of accessions"),
            updatemenus = list(
              list(
                x = 0.1,
                y = 1.07,
                xref = 'paper',
                yref = 'paper',
                yanchor = 'top',
                type = 'dropdown',
                active = 0,
                buttons = create_buttons(expts)
              )
            ))
      }
      #make histogram of summaries
      
    })
    
    #map traits summary
    #observe({
    output$traitMap <- leaflet::renderLeaflet({
      #req(rv$traitSummary, rv$isTraitNum)
      filtered.traits.sum <- rv$traitSummary[input$TraitDataSum_rows_all,]
      
      #merge rv$datasetInput and rv$traitSummary on IG
      trait.coordinates <- merge(rv$datasetInput, filtered.traits.sum, by = "IG")
      
      if(rv$isTraitNum){
        pal <- leaflet::colorBin(
          palette = c("#2d7436", "#ff8103"),
          domain = trait.coordinates[[rv$field.name]],
          bins = 6
        )
      }
      else{
        pal <- leaflet::colorFactor(
          palette = c("#2d7436", "#ff8103"),
          domain = trait.coordinates[[rv$field.name]]
        )
      }
      
      leaflet::leaflet(data = trait.coordinates) %>% 
        leaflet::addTiles() %>%
        leaflet::addProviderTiles('Esri.WorldGrayCanvas')  %>%
        leaflet::addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                                  color = "black",
                                  radius = 3,
                                  fillColor = ~pal(trait.coordinates[[rv$field.name]]),
                                  label = ~trait.coordinates[[rv$field.name]],
                                  fillOpacity = 0.7, stroke = TRUE, weight = 0.3) %>%
        leaflet::addLegend(pal = pal, values = ~trait.coordinates[[rv$field.name]], opacity = 1,  title = rv$traitName)
    })
}