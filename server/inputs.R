

output$year_input <- renderUI({
  choices <- as.character(unique(stella$year))
  pickerInput2(
    inputId = "year",
    label = "Years",
    multiple = TRUE,
    inline = TRUE,
    width = "100%",
    choices = choices ,
    selected = choices ,
    options = list(
      `actions-box` = TRUE,
      `live-search` = TRUE,
      `virtual-scroll` = TRUE,
      `style` = "btn-inline",
      size = 14),
    choicesOpt = list(
      content = choices
    )
  )
})

output$sku_names_input <- renderUI({
  choices <- as.character(unique(stella$sku_name))
  choices <- c("Stella Artois Beer",choices[choices != "Stella Artois Beer"][order(choices[choices != "Stella Artois Beer"])])
  pickerInput2(
    inputId = "sku_names",
    label = "Products",
    multiple = TRUE,
    inline = TRUE,
    width = "100%",
    choices = choices ,
    selected = "Stella Artois Beer"  ,
    options = list(
      `actions-box` = TRUE,
      `live-search` = TRUE,
      `virtual-scroll` = TRUE,
      `multiple-seperation` = "\n",
      `style` = "btn-inline",
      size = 14),
    choicesOpt = list(
      content = choices
    )
  )
})


output$submit_input <- renderUI({
  actionButton("submit", "Apply Filter", width = "40%",style = "font-weight:bold;font-style:italic;margin-top:15px;border-color:#e3af32;background-color:#e3af32;color:white;")
})