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
  
  #Extract data from ICARDA by crop name
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
      DT::datatable(rv$passportCrop, filter = list(position = "top", clear = FALSE), options = list(pageLength = 10, autoWidth = TRUE), callback = DT::JS(" //hide column filters for specific columns
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
        
      DT::datatable(rv$datasetInput, options = list(pageLength = 10))
    }
  })
    
  observeEvent(input$table_rows_all,{
    if(input$dataSrc == 'byCrop'){
      
      rv$datasetInput <- rv$passportCrop[input$table_rows_all,]
      rv$datasetInput <- droplevels(rv$datasetInput)
      print(levels(rv$datasetInput[['PopulationType']]))
      rv$datasetInput
    }
  })

  rv$ycolumns <- reactive({
    names(rv$datasetInput)
  })
    
  output$targetVar <- renderUI({
    req(rv$ycolumns())
    coordTarget <- list(
      selectInput("long", "Select Longitude", c("", rv$ycolumns())),
      selectInput("lat", "Select Latitude", c("", rv$ycolumns())),
      selectInput("y", "Select the target variable to be mapped", choices = c("", rv$ycolumns()))
    )
    do.call(tagList, coordTarget)
  })
    
  observeEvent(input$showMap,{
    updateTabsetPanel(session, 'main', selected = 'accMap')
  })

  observeEvent(input$plotVarBtn,{
    updateTabsetPanel(session, 'main', selected = 'accPlot')
  })

  output$plotVar <- renderUI({
    #req(rv$factNumCols)
    selectInput("varPlotted", "Select a variable", choices = c("", rv$ycolumns()))
  })

  output$map <- leaflet::renderLeaflet({
    req(rv$datasetInput)
    
    y <- isolate(input$y)
    if (input$showMap == 0 || y == "") {
      y <- NULL
    }

    mapAccessions(df = rv$datasetInput, long = input$long, lat = input$lat, y = y)
  })
    
  ## Frequency tables of two vars
  # ***********************************************************#
  # bw <- icardaFIGSr::getAccessions("Bread Wheat", taxon=T)
  # contingency_table <- table(bw$Taxon, bw$PopulationType)
  # ggpubr::ggballoonplot(as.data.frame.matrix(contingency_table), fill = "value", color = "lightgray",
  #                       +               size = 10, show.label = TRUE)+
  #   +     ggpubr::gradient_fill(c("blue", "white", "red"))
  #************************************************************#
  
  # Statistical plot of variables from dataset extracted by crop name
  output$plot <- plotly::renderPlotly({
    #renderPlot({
    chosenVarPlot <- isolate(input$varPlotted)
    input$plotVarBtn
    # if(is.numeric(rv$datasetInput[[chosenVarPlot]])){
      plotly::plot_ly(rv$datasetInput, x = rv$datasetInput[[chosenVarPlot]], name = chosenVarPlot, color = "#ff8103") %>% plotly::add_histogram() %>%
        plotly::layout(margin = list(b = 100), xaxis = list(tickangle = 45))
      
      #hist(rv$datasetInput[[chosenVarPlot]], col = "#ff8103", border = "white", xlab = chosenVarPlot, main = NULL)
    # }
    # else if(is.factor(rv$datasetInput[[chosenVarPlot]]) | is.character(rv$datasetInput[[chosenVarPlot]])) {
    #   
    #   plotly::plot_ly(x = table(rv$datasetInput[[chosenVarPlot]]), y = table(rv$datasetInput[[chosenVarPlot]]), type='bar')
    #   
    #   par(mar = c(27, 4, 1, 0))
    #   ylim <- c(0, 1.1*max(table(rv$datasetInput[[chosenVarPlot]])))
    #     
    #   xx <- barplot(table(rv$datasetInput[[chosenVarPlot]]), ylim = ylim, xlab = "", ylab = "", col = "#ff8103", border = "white", las = 2)
    #   text(x = xx, y = table(rv$datasetInput[[chosenVarPlot]]), labels = table(rv$datasetInput[[chosenVarPlot]]), pos = 3, cex = 0.8, col = "black")
    # }
  })
  
  output$downloadAcc <- downloadHandler(
    filename = function() {
      paste0("Accessions",".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$datasetInput, file, row.names = F)
    }
  )
  ########################################################
  ############  Extracting World Clim Data   #############
  ########################################################
  
  rv$WCdata <- callModule(extractWCDataMod, "extractWCData", rv)
  
  observeEvent(rv$extractWC,{
    updateTabsetPanel(session, "wcMainPanel", selected = "WCTable")
  })
  
  output$clVarPlot <- renderUI({
    climateVars <- search4pattern(c('tavg*', 'tmin*', 'tmax*', 'prec*', 'bio*', 'srad*', 'vapr*', 'wind*'), names(rv$WCdata()))
    req(climateVars)
    clVarInputBtn <- list(
      selectInput("clVar", "Select the climate variable to be mapped", choices = c("", climateVars)),
      actionButton("mapClVar", "Map")
    )
    do.call(tagList, clVarInputBtn)
  })
  
  observeEvent(input$mapClVar,{
    updateTabsetPanel(session, "wcMainPanel", selected = "WCMap")
  })
               
  output$WCMap <- leaflet::renderLeaflet({
    ## get climate variable
    clVar <- isolate(input$clVar)
    input$mapClVar
      
    mapAccessions(df = rv$WCdata(), long = rv$lng, lat = rv$lat, y = clVar)
  })
  
  output$WCtable <- DT::renderDataTable({
    DT::datatable(rv$WCdata(), options = list(pageLength = 10))
  })
  
  output$downloadWCData <- downloadHandler(
    filename = function() {
      paste0("WorldClimData",".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$WCdata(), file, row.names = F)}
  )

  observe({
    rv$WCDataNames <- names(rv$WCdata())
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
        min <- min(rv$WCdata()[[inputName]], na.rm = T)
        max <- max(rv$WCdata()[[inputName]], na.rm = T)
        
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
             #plotOutput(plotname, height = 300)
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
            subsettingCnd <- paste0(subsettingCnd,"(rv$WCdata()$", inputVari, " >= input$", inputVari,"[1]", ") & (rv$WCdata()$", inputVari, " <= input$", inputVari,"[2]) & !is.na(rv$WCdata()$",inputVari,") & ")
          }
          lastVar <- rv$climVarSub[length(rv$climVarSub)]
          subsettingCnd <- paste0(subsettingCnd, "(rv$WCdata()$", lastVar, " >= input$", lastVar,"[1]", ") & (rv$WCdata()$", lastVar, " <= input$", lastVar,"[2]) & !is.na(rv$WCdata()$",lastVar,")")
          
          rv$dfSub <- rv$WCdata()[eval(parse(text = subsettingCnd)),]
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
  
  observeEvent(input$mapSubBtn,{
    updateTabsetPanel(session, 'subsetMain', selected = 'subsetMap') 
  })
  
  output$dataDescription <- renderUI({
    req(rv$climVarSub)
    verbatimTextOutput("rowsNumber") 
  })
  
  output$rowsNumber <- renderPrint({
    print(paste("Number of accessions: ", nrow(rv$dfSub)))
  })
  
  output$mapDlBtns <- renderUI({
    req(rv$climVarSub)
    mapDlBtns <- list(
      actionButton("mapSubBtn", "Map Data"),
      downloadButton("DataSubset", "Download")
  )
    do.call(tagList, mapDlBtns)
  })
  
  output$subsetMap <- leaflet::renderLeaflet({
    req(input$mapSubBtn)
    map_two_dfs(rv$WCdata(), rv$dfSub, lng = rv$lng, lat = rv$lat, type = "Data Subset")
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
      rv$data4cluster <- rv$WCdata()
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
    
    output$mapcluster <- leaflet::renderLeaflet({
      req(rv$clusterRes)
      pal <- leaflet::colorFactor(
        palette = "viridis",
        domain = rv$clusterRes[[2]]$cluster
      )
      
      leaflet::leaflet(data = rv$clusterRes[[2]]) %>% leaflet::addTiles() %>%
        leaflet::addProviderTiles('Esri.WorldGrayCanvas')  %>%
        leaflet::addCircleMarkers(lng = cluster.res()[[2]][[rv$lng]], lat = rv$clusterRes[[2]][[rv$lat]],
                                  color = ~pal(cluster),
                                  radius = 2,
                                  fill = TRUE,
                                  fillColor = ~pal(cluster),
                                  label = ~cluster,
                                  fillOpacity = 1, weight = 0.1) %>%
        leaflet::addLegend(pal = pal, values = ~cluster, opacity = 1,  title = 'Cluster')
    })
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
  # observe({
  #   if(input$pcaDataSrc == "allDataPca"){
  #     rv$data4pca <- rv$WCdata()
  #     rv$filteredPca <- FALSE
  #   }
  #   else if (input$pcaDataSrc == "filtDataPca"){
  #     rv$data4pca <- rv$dfSub
  #     rv$filteredPca <- TRUE
  #   }
  # })
  
  observeEvent(input$PCAsummary, {
    if(input$pcaDataSrc == "allDataPca"){
      rv$data4pca <- rv$WCdata()
      rv$filteredPca <- FALSE
    }
    else if (input$pcaDataSrc == "filtDataPca"){
      rv$data4pca <- rv$dfSub
      rv$filteredPca <- TRUE
    }
    rv$pca_var <- input$pca_var
    #req(varSel, rv$data4pca)
    pcSummary <- callModule(pcaSummaryMod, "pcaSummary", rv)
    rv$pcaSummary <- pcSummary()
    rv$pcScores <- as.data.frame(rv$pcaSummary@scores)
    updateTabsetPanel(session, 'pca', selected = 'pcaSummary')
  })
    
  observeEvent(input$PCAPlotButton,{
    updateTabsetPanel(session, 'pca', selected = 'pcaPlot') 
  })
    
  observeEvent(input$geoPlotButton,{
    updateTabsetPanel(session, 'pca', selected = 'geoMap') 
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
  
  #pcaPlot <- callModule(pcaPlotMod, "pcaPlot", rv)
  
  # observeEvent(input$PCAPlotButton, {
  #   rv$pcaPlot <- pcaPlot()
  # })
  
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
        plot_bgcolor='rgba(240,240,240, 0.95)',
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
      
  ## geographic plot of pca results
  observe({
    updateSelectInput(session, "pcaScore", label = "Choose Score", choices = names(rv$pcScores))
  })
  
  output$geoMap <- leaflet::renderLeaflet({
    # req(input$pcaScore)
    pcaScore <- isolate(input$pcaScore)
    input$geoPlotButton
    
    pal <- leaflet::colorNumeric(
      palette = "viridis",
      domain = rv$pcScores[[pcaScore]]
      )

    leaflet::leaflet(data = rv$completeData) %>% leaflet::addTiles() %>%
      leaflet::addProviderTiles('Esri.WorldGrayCanvas')  %>%
      leaflet::addCircleMarkers(lng = rv$completeData[[rv$lng]], lat = rv$completeData[[rv$lat]],
                       color = ~pal(rv$pcScores[[pcaScore]]),
                       radius = 2,
                       fill = TRUE,
                       fillColor = ~pal(rv$pcScores[[pcaScore]]),
                       label = ~rv$pcScores[[pcaScore]],
                       fillOpacity = 1, weight = 0.1) %>%
      leaflet::addLegend(pal = pal, values = ~rv$pcScores[[pcaScore]], opacity = 1,  title = pcaScore)
    
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
      # ylim <- c(0, 1.1*max(table(rv$core$cluster)))
      # xx <- barplot(table(rv$core$cluster), ylim = ylim, xlab = 'Cluster', ylab = "Number of Accessions", col = "#ff8103", border = "white")
      # 
      # text(x = xx, y = table(rv$core$cluster), label = table(rv$core$cluster), pos = 3, cex = 0.8, col = "black")
      plotly::plot_ly(rv$core, x = ~cluster, color = "#ff8103") %>% plotly::add_histogram()
    })
    
    output$coreDataTable <- DT::renderDataTable({
      req(rv$core)
      DT::datatable(rv$core, options = list(pageLength = 10))
    })
    
    output$coreMap <- leaflet::renderLeaflet({
      map_two_dfs(rv$data4core, rv$core, lng = rv$lng, lat = rv$lat, type = "Core Data")
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
        print(traitId)
        rv$traitsData <- traitsData %>% mutate_at(input$IG.Trait, funs(round(., 2)))
        rv$traitsData[['YEAR']] = as.factor(rv$traitsData[['YEAR']])
        if(!is.na(rv$traits[rv$traits$ID == as.numeric(traitId), 'Options'])){
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
                                   buttons = c('csv', 'excel')))
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
                      options = list(dom = "Bfrtip",
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
    output$traitMap <- leaflet::renderLeaflet({
      
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
        leaflet::addProviderTiles('Esri.WorldTopoMap')  %>%
        leaflet::addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                                  color = "black",
                                  radius = 3,
                                  fillColor = ~pal(trait.coordinates[[rv$field.name]]),
                                  label = ~trait.coordinates[[rv$field.name]],
                                  fillOpacity = 0.7, stroke = TRUE, weight = 0.3) %>%
        leaflet::addLegend(pal = pal, values = ~trait.coordinates[[rv$field.name]], opacity = 1,  title = rv$traitName) 
      
    })
    
}