pcaPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("xAxis"), "Select X Axis", list("")),
    selectInput(ns("yAxis"), "Select Y Axis", list(""))
  )
}

pcaPlotMod <- function(input, output, session, rv){
  observe({
    req(rv$pcaSummary)
    rv$gfp_scores <- as.data.frame(rv$pcaSummary@scores)
    updateSelectInput(session, "xAxis", label = "Select X Axis", choices = names(rv$gfp_scores))
    updateSelectInput(session, "yAxis", label = "Select Y Axis", choices = names(rv$gfp_scores), selected = names(rv$gfp_scores)[[2]])
    })
  p <- reactive({
    p <- ggplot2::ggplot(rv$gfp_scores, ggplot2::aes_string(input$xAxis, input$yAxis))
    p
    })
  return(p)
  }