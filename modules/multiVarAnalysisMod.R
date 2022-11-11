multiVarAnalysisUI <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("climVarSub"), "Select Variables", list(""), multiple = T),
    actionButton(ns("slidersButton"), "Filter Data")
  )
}
multiVarAnalysisMod <- function(input, output, session, rv){
  
  observe({
      updateSelectInput(session, "climVarSub", label = "Select Variables", choices = rv$worldClimVar)
    
  })
  
  climVarSub <- eventReactive(input$slidersButton, {
    rv$slidersBtn <- input$slidersButton
    isolate(climVarSub <- input$climVarSub)
    climVarSub
  })
  
  return(climVarSub)
  
}