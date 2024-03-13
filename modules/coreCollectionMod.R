coreCollectionUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("group"), "Select a group", list("")),
    selectInput(ns("uid"), "Select the unique identifier", list("")),
    selectInput(ns("allocMeth"), "Select allocation method", choices = c("Propotional" = "Pro","Logarythmic" = "Log","D2 allocation method" = "D2","D3 allocation method" = "D3"), selected = "Propotional"),
    selectInput(ns("clustMeth"), "Select cluster analysis method", choices = c( "average", "single", "complete", "ward"), selected = "ward"),
    numericInput(ns("fraction"), "Select fraction", 0.1, min = 0, max = 1, step = 0.1)
  )
}

coreCollectionMod <- function(input, output, session, rv){

  observe({
    updateSelectInput(session, "group", label = "Select a group", choices = colnames(rv$data4core))
    updateSelectInput(session, "uid", label = "Select the unique identifier", choices = colnames(rv$data4core))
  })
  
  core <- reactive({
    
    alloc <- input$allocMeth
    cluster_method <- input$clustMeth
    fraction <- input$fraction
    group <- input$group
    uid <- input$uid
    
    data4core <- rv$data4core
    rownames(data4core) <- data4core[[uid]]
    climate_columns <- search4pattern(c('tavg*', 'tmin*', 'tmax*', 
                                        'prec*', 'bio*', 'srad*', 
                                        'vapr*', 'wind*'), names(data4core))
    select_columns <- c(group, climate_columns)
    
    data4core_sub <- data4core[select_columns]
    data4core_sub_na_omit <- data4core_sub[complete.cases(data4core_sub[[group]]), ]
    
    data4core_sub_na_omit[[group]] <- droplevels(data4core_sub_na_omit[[group]])
    new_levels <- levels(as.factor(data4core_sub_na_omit[[group]]))
    data4core_sub_na_omit[[group]] <- unclass(as.factor(data4core_sub_na_omit[[group]]))
    
    withProgress(message = "Developing Core Collection ...", {
      core <- ccChooser::stratcc(x = data4core_sub_na_omit, groups = data4core_sub_na_omit[[group]], 
                               alloc = alloc, fraction = fraction, 
                               clustering = TRUE, cluster_method = cluster_method)
    })
    
    core[[group]] <- plyr::mapvalues(core[[group]], sort(unique(core[[group]])), 
                                           new_levels)
    core[[uid]] <- as.numeric(rownames(core))
    core <- merge(x = core, y = rv$data4core)
    core <- core %>% relocate(climate_columns, .after = last_col())
    
    list(core, group)
   })
 
  return(core)
}