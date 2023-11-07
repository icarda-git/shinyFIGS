#' @title Getting List of Crops Available in ICARDA's Genebank Documentation System
#' @description Return a list with codes and names of available crops.
#' @return A list containing all crops available in ICARDA's Genebank Documentation System.
#' @details The crop codes and names are fetched from ICARDA's online server.

getCrops <- function() {
  result <- read.csv("https://grs.icarda.org/web_services/getCrops.php")
  df <- data.frame(colnames(result)[1],colnames(result)[2])
  names(df) <- c("CropCode","CropName")
  names(result) <- c("CropCode","CropName")
  crops <- rbind(df,result)
  return(crops)
}

#' @title Extracting historical climatic data from WorldClim 2.1
#' @description Return a data frame based on specified climatic variables. 
#' @param sites object of class "data.frame" with coordinates of sites from which to extract data.
#' @param long character. Name of column from \code{sites} with longitude.
#' @param lat character. Name of column from \code{sites} with latitude.
#' @param res numeric. Spatial resolution. Default 2.5
#' @param var character. Climatic variable(s) to be extracted: 'tavg', 'tmin', 'tmax', 'prec', 'bio', 'srad', 'vapr', 'wind'
#' @return An object of class "data.frame" with specified climatic variables for coordinates in \code{sites}.

extractWCdata <- function(sites, long, lat, var, res = 2.5){
  
  #remove records having NA coordinates
  out <- list(
    is.na(sites[[long]]), 
    is.na(sites[[lat]])
  )
  
  outReduced <- !Reduce("|", out)
  sites <- sites[outReduced,]
  xy <- cbind(sites[[long]], sites[[lat]])
  sp <- sp::SpatialPoints(xy)
  
  for (ivar in var){
    
    rasterfile <- .getRasterData(var = ivar, res = res)
    
    for (i in 1:length(names(rasterfile))){
      
      f.name <- names(rasterfile)[i]
      var.name <- sub(paste(".*",res,"m_", sep = ''), "", f.name)
      print(var.name)
      sites[, var.name] <- raster::extract(rasterfile[[i]], sp, method = 'simple')
      sites[, var.name] <- round(sites[, var.name], 3)
      
    }
    
  }
  
  return(sites)
}

#'
#'
#'
#'
#'

.getRasterData <- function(var, res){
  
  stopifnot(var %in% c('tavg', 'tmin', 'tmax', 'prec', 'bio', 'srad', 'vapr', 'wind'))
  
  path <- getwd()
  path <- paste(path, '/WorldClim_', res, '/', sep='')
  dir.create(path, showWarnings=FALSE)
  
  theurl = paste("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_", res,"m_", var,".zip", sep='')
  
  zip <- paste('wc2.1_', res, 'm_', var ,'.zip', sep='')
  zipfile <- paste(path, zip, sep='')
  
  if (var  != 'bio') {
    tiffiles <- paste('wc2.1_', res, 'm_', var, '_', sprintf("%02d",1:12), '.tif', sep='')
  } else {
    tiffiles <- paste('wc2.1_', res, 'm_', var,'_', 1:19, '.tif', sep='')	
  }
  
  files <- paste(path, tiffiles, sep='')
  fc <- sum(file.exists(files))
  
  if ( fc < length(files) ) {
    if (!file.exists(zipfile)) {
      .download(theurl, zipfile)
      if (!file.exists(zipfile))	{ 
        message("\n Could not download file -- perhaps it does not exist") 
      }
    }	
    utils::unzip(zipfile, exdir=dirname(zipfile))
  }
  
  st <- raster::stack(files)
  
  raster::projection(st) <- "+proj=longlat +datum=WGS84"
  return(st)
}

#'
#'
#'
#'
#'

.download <- function(url, filename) {
  fn <- paste(tempfile(), '.download', sep='')
  res <- utils::download.file(url=url, destfile=fn, method="auto", quiet = FALSE, mode = "wb", cacheOK = TRUE)
  if (res == 0) {
    w <- getOption('warn')
    on.exit(options('warn' = w))
    options('warn'=-1) 
    if (! file.rename(fn, filename) ) { 
      file.copy(fn, filename)
      file.remove(fn)
    }
  } else {
    stop('could not download the file' )
  }
}


#'Map accessions using leaflet package
#'
#'
#'

mapAccessions <- function(map, df, long, lat, y){
  
  if(y == "None"){
    leaflet_map <- map %>% clearMarkers() %>%
      clearControls() %>% removeLayersControl() %>%
      leaflet::addCircleMarkers(data = df, lng = df[[long]], lat = df[[lat]],
                       color = "#2d7436",
                       radius = 1.5,
                       fill = TRUE,
                       fillColor = "#2d7436",
                       fillOpacity = 1, stroke = TRUE, weight = 0.1) 
  }
  else {
    ## omit NAs in y column
    df.na.omit <- df[!is.na(df[[y]]), ]

    if (is.numeric(df[[y]])){
      pal <- leaflet::colorNumeric(
        palette = c("viridis"),
        domain = df[[y]],
        na.color = "#808080"
      )
    }
    else {
      pal <- leaflet::colorFactor(
        palette = c("viridis"),
        domain = df[[y]],
        na.color = "#808080"
      )
    }
    
    leaflet_map <- map %>% clearMarkers() %>%
      clearControls() %>% removeLayersControl() %>%
      leaflet::addCircleMarkers(data = df.na.omit,
                                lng = df.na.omit[[long]],
                                lat = df.na.omit[[lat]],
                                color = "black",
                                radius = 1.5,
                                fill = TRUE,
                                fillColor = ~pal(df.na.omit[[y]]),
                                label = ~df.na.omit[[y]],
                                stroke = TRUE,
                                fillOpacity = 1,
                                weight = 0.1,
                                group = "withoutNAs") %>%
      leaflet::addCircleMarkers(data = df, lng = df[[long]], lat = df[[lat]],
                                color = "black",
                                radius = 1.5,
                                fill = TRUE,
                                fillColor = ~pal(df[[y]]),
                                label = ~df[[y]],
                                fillOpacity = 1, stroke = TRUE, weight = 0.1, group = "withNAs") %>%
      leaflet::addLegend("bottomright", pal = pal, values = df[[y]], opacity = 1,  title = y) %>%
      addLayersControl(baseGroups = c("withNAs","withoutNAs"),
                       options = layersControlOptions(collapsed = FALSE))
  }
  leaflet_map
}

#' this function gets the country ISO3 code giving the country name
#'
#'
#'

countryCode <- function(countryName){
  if(is.null(countryName)) code <- NULL
  else code <- countries %>% distinct() %>% filter(ADMIN %in% countryName) %>% pull(ADM0_A3)
  return(code)
}

#' search a given pattern in an object
#'
#'
#'

search4pattern <- function(pattern, obj){
  matchItems <- grep(pattern = paste0(pattern, collapse = "|"), x = obj, ignore.case = T, value = T)
  return(matchItems)
}

#' Map two datasets using leaflet package
#'
#'
#'
#'

map_two_dfs <- function(map, df1, df2, lng, lat, type){
  df1$Aggregated <- "Overall Data"
  df2$Aggregated <- type
  d <- rbind(df1, df2)
  d_overall <- subset(d, Aggregated == "Overall Data")
  d_subset <- subset(d, Aggregated == type)
  
  pal <- leaflet::colorFactor(c("#2d7436", "#ED7506"), domain = c("Overall Data", type))
  
  map %>% leaflet::clearMarkers() %>%
    leaflet::clearControls() %>% leaflet::removeLayersControl() %>%
    leaflet::addMapPane(type, zIndex = 450) %>%
    leaflet::addMapPane("Overall Data", zIndex = 410) %>%
    leaflet::addCircleMarkers(data = d_overall, lng = d_overall[[lng]], lat = d_overall[[lat]],
                     radius = 2,
                     color = "black",
                     fill = TRUE,
                     fillColor = "#2d7436",
                     fillOpacity = 0.8, stroke = TRUE, weight = 0.1, group = "Overall Data") %>% 
      leaflet::addCircleMarkers(data = d_subset, lng = d_subset[[lng]], lat = d_subset[[lat]],
                     radius = 2,
                     color = "black",
                     fillColor = "#ED7506",
                     stroke = TRUE, fillOpacity = 0.8, weight = 0.1, group = type) %>%
      leaflet::addLayersControl(overlayGroups = c("Overall Data", type),
                     options = layersControlOptions(collapsed = FALSE)) 
  # %>%
  #   leaflet::addLegend(pal = pal, values = d[["Aggregated"]], opacity = 1,  title = '')
}

#' get the value that has maximun occurences in a vector, or choose a value randomly if occurences are equivalent
#'
#'
#'

max.frequency <- function(x){
  t <- table(x)
  elts <- names(t)[t == max(t)]
  if(length(elts) > 1) max.frq <- sample(elts, 1)
  else max.frq = elts
  return(max.frq)
}

#'
#'
#'

create_buttons <- function(vars) {
  lapply(
    vars,
    FUN = function(var) {
      button <- list(
        method = 'restyle',
        args = list('transforms[0].value', var),
        label = var
      )
    }
  )
}

#'Generate the dropdown list from variables for X axis
#'
#'
x_axis_dd_list <- function(df, vars) {
  lapply(
    vars,
    FUN = function(var) {
      button <- list(
        method = 'update',
        args = list(list(x = list(df[[var]]))),
        label = var
      )
    }
  )
}

make_sliders <- function(x, var) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = floor(rng[1]), max = ceiling(rng[2]), 
                value = c(floor(rng[1]),ceiling(rng[2])))
}

render_hists <- function(output, x, var){
  plotname <- paste("histo", var, sep = "")
  output[[plotname]] <- plotly::renderPlotly({
    plotly::plot_ly(x, x = x[[var]], color = "#ff8103") %>% 
    plotly::add_histogram() %>%
    plotly::add_annotations(var, x = 0.5, y = 1, 
      xref = "paper", yref = "paper", showarrow = FALSE)
    })
}

render_prints <- function(output, x, var){
  printname <- paste("sumClimVar", var, sep = "")
  output[[printname]] <- renderPrint({
    print(var)
    summary(x[[var]])
  })
}

filter_var <- function(x, val) {
  !is.na(x) & x >= val[1] & x <= val[2]
}

update_slider <- function(session, x, var) {
  newMin <- floor(min(x, na.rm = TRUE))
  newMax <- ceiling(max(x, na.rm = TRUE))
  updateSliderInput(session, inputId = var, value = c(newMin,newMax))
}
