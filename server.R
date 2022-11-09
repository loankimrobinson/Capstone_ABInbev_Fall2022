source("global.R")
shinyServer(function(session, input, output) {
  source(file.path("server", "inputs.R"),  local = TRUE)$value
  source(file.path("server", "kpi.R"),  local = TRUE)$value
  source(file.path("server", "dashboard_model.R"),  local = TRUE)$value

  output$cards_out <- renderUI({
    
    args <- lapply(seq_along(img.src), function(i,img.src) {
      card(img.src[[i]])
    },img.src = img.src )
    
    args$cellArgs <- list(
      style = "width: auto; height: auto; margin: 5px;")
    
    
    do.call(shiny::flowLayout, args)
    
  })
  
  
  output$cards_partner_out <- renderUI({
    
    args <- lapply(seq_along(img.src.partner), function(i,img.src) {
      card(img.src[[i]])
    },img.src = img.src.partner)
    
    args$cellArgs <- list(
      style = "width: auto; height: auto; margin: 5px;")
    
    
    do.call(shiny::flowLayout, args)
    
  })
})


#======================================

#======================================
