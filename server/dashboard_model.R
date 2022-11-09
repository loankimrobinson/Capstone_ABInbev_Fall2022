



dt_summary <- reactive({
  req(values$data)
  
  dt_customer <- values$data %>% group_by(y_qtr) %>% summarise(total_count := sum(purchased_product_total_cnt),
                                                               total_usd := sum(product_total_usd),
                                                               customer := n())
  dt_customer$hovertext <- paste0("<b><i>Quarter: ", dt_customer$y_qtr, "</i></b>", "<br>",
                      "<b><i>Number of Orders: ", formatC(dt_customer$total_count, format="f", big.mark=",", digits=0), "</i></b>", "<br>",
                      "<b><i>Sale Revenue: ",formatC(dt_customer$total_usd, format="f", big.mark=",", digits=0), "</i></b>", "<br>",
                      "<b><i>Number of Customers: ",dt_customer$customer, "</i></b>", "<br>")
  return(dt_customer)
})

view <- reactive({
  if(input$view_summary == "Number of the Orders"){
    temp <- "total_count"
  }else if(input$view_summary == "Sale Revenue"){
    temp <- "total_usd"
  }else{
    temp <- "customer"
  }
  return(temp)
})

output$summary_plot <- renderPlotly({
  y_var <- view()
  p <- bar_box_horizontal(plot_data = dt_summary(),
                          x_var = "y_qtr",
                          y_var = y_var,
                          color_var =  "y_qtr",
                          text = y_var,
                          title =  input$view_summary,
                          colorRampPalette = FALSE,
                          color_fill = c("#636466","#e3af32","#636466","#e3af32","#636466","#e3af32","#636466","#e3af32"),
                          hovertext = "hovertext",
                          x_var_label = "",
                          y_var_label = "",
                          legend = "none",
                          source = "summary_plot")
  values$summary_plot <- p
})

output$summary_plot_dl<- downloadHandler(
  filename = function() { paste("summary_plot_",Sys.Date(), '.html', sep='') },
  content = function(file) {
    htmlwidgets::saveWidget(values$summary_plot,file = file)
  }
)


plotly_event_out <- reactiveVal(NULL)

observeEvent(event_data("plotly_click", source = "summary_plot"),{
  plotly_event_out(event_data("plotly_click", source = "summary_plot")$y)
  if(!is.null(plotly_event_out())){
    dt <- values$data
    dt <- dt[dt$y_qtr == plotly_event_out(), !colnames(dt) %in% c("order_sk","sku_id","m_y","year")]
    names(dt)[names(dt) == "purchased_product_total_cnt"] <- "total_cnt"
    values$table <- dt
  }
})


observeEvent(plotly_event_out(),{
  if(nrow(values$table) >= 1 && !is.null(values$table) && nrow(dt_summary()) >= 1){
    showModal(modalDialog(
      output$plotly_out_dt <- DT::renderDT(server = FALSE,{
        if(is.null(plotly_event_out()))return(NULL)
        dt_tb(values$table,"data_plot",filter = FALSE)
      }),
      title = "Sale Data ",
      easyClose=FALSE,
      size = "l",
      footer = actionButton("closeModal",label = "Close")
    ))
  }
})

observeEvent(input$closeModal, {
  removeModal()
})



output$summary_plot_out_box <- renderUI({
  div(
    style = "position: relative",
    tabBox(
      id = "med_b",
      width = NULL,
        div(style="max-height:600px; overflow-y: scroll; overflow-x: scroll;position: relative",
            plotlyOutput("summary_plot", width = "100%",height= "300px")),
        div(
          style = "position: absolute; right: 0.5em; bottom: 1em;",
          dropdown(
            selectInput("view_summary", 
                        HTML("<span style='color: #035AAA;font-weight:bold;text-align:left;'>View by:</span>"), 
                        choices = c("Number of the Orders", "Sale Revenue", "Number of the Customers"), 
                        selected = "Number of the Orders",multiple = F,width="100%"),
            size = "m",
            width = "250px",
            icon = icon("arrow-up"),
            up = TRUE,
            right = TRUE
          )
        ),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em;",
        downloadButton("summary_plot_dl", label = NULL,icon = icon("download"),class = "btn-m", title = "",
                         style = "position: absolute; right: 10px;bottom:10px;color:#636466;")
        )
      )
    )
})



#================================
# RFM Plot
# Average Revenue Per User Matrix for R&F Scores

output$rfm_plot <- renderPlotly({
  dt <-values$rfm_model_dt
  p <- heatmap_plot(data = dt$rfm_heatmap)
  values$rfm_plot <- p
})

output$rfm_plot_dl <- downloadHandler(
  filename = function() { paste("rfm_plot_",Sys.Date(), '.html', sep='') },
  content = function(file) {
    htmlwidgets::saveWidget(values$rfm_plot,file = file)
  }
)

output$rfm_plot_out_box <- renderUI({
  div(
    style = "position: relative",
    tabBox(
      id = "med_b",
      width = NULL,
      div(style="max-height:600px; overflow-y: scroll; overflow-x: scroll;position: relative",
          plotlyOutput("rfm_plot", width = "100%",height= "300px")),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em;",
        downloadButton("rfm_plot_dl", label = NULL,icon = icon("download"),class = "btn-m", title = "",
                       style = "position: absolute; right: 10px;bottom:10px;color:#636466;")
      )
    )
  )
})

plotly_event_rfm_out <- reactiveVal(NULL)

observeEvent(event_data("plotly_click", source = "rfm_heatmap"),{
  plotly_event_rfm_out(event_data("plotly_click", source = "rfm_heatmap")$x)
  if(!is.null(plotly_event_rfm_out())){
    dt <- values$rfm_model_dt$rfm_heatmap
    dt <- dt[dt$recency_score == plotly_event_rfm_out(),]
    dt[,3] <- round(dt[,3],1)
    colnames(dt) <- c("Frequency Score","Recency Score","Monetary")
    values$table_rfm_out <- dt
    
  }
})


observeEvent(plotly_event_rfm_out(),{
  if(nrow(values$table_rfm_out) >= 1 && !is.null(values$table_rfm_out)){
    showModal(modalDialog(
      output$plotly_rfm_out_dt <- DT::renderDT(server = FALSE,{
        if(is.null(plotly_event_rfm_out()))return(NULL)
        dt_tb(values$table_rfm_out,"data_plot",filter = FALSE)
      }),
      title = "Average Revenue Per User Matrix for R&F Scores Table",
      easyClose=FALSE,
      size = "m",
      footer = actionButton("closeModal",label = "Close")
    ))
  }
})

#===========================================

output$segment_plot <- renderPlotly({
  dt <- values$rfm_model_dt$rfm_distribution
  dt <- dt[dt$n != 0,c("segment","n")]
  dt <- dt[order(desc(dt$n)),]
  dt$segment <- factor(dt$segment, levels = dt$segment)
  p <- bar_box_horizontal(plot_data = dt,
                          x_var = "segment",
                          y_var = "n",
                          color_var =  "segment",
                          text = "n",
                          title =  "Segment Distribution",
                          colorRampPalette = FALSE,
                          color_fill = c("#636466","#e3af32"),
                          hovertext = "n",
                          x_var_label = "",
                          y_var_label = "",
                          legend = "none",
                          source = "segment_plot")
  values$segment_plot <- p
})

output$segment_plot_dl<- downloadHandler(
  filename = function() { paste("segment_plot_",Sys.Date(), '.html', sep='') },
  content = function(file) {
    htmlwidgets::saveWidget(values$segment_plot,file = file)
  }
)


segment_plot_event_out <- reactiveVal(NULL)

observeEvent(event_data("plotly_click", source = "segment_plot"),{
  segment_plot_event_out(event_data("plotly_click", source = "segment_plot")$y)
  if(!is.null(segment_plot_event_out())){
    dt <- values$rfm_model_dt$rfm_segments 
    dt <- dt[dt$segment== segment_plot_event_out(),]
    values$table_rfm_segments <- dt
  }
})


observeEvent(segment_plot_event_out(),{
  if(nrow(values$table_rfm_segments) >= 1 && !is.null(values$table_rfm_segments)){
    showModal(modalDialog(
      output$segment_plot_out_dt <- DT::renderDT(server = FALSE,{
        if(is.null(segment_plot_event_out()))return(NULL)
        dt_tb(values$table_rfm_segments ,"table_rfm_segments ",filter = FALSE)
      }),
      title = "Segement Distribution Data ",
      easyClose=FALSE,
      size = "l",
      footer = actionButton("closeModal",label = "Close")
    ))
  }
})


output$segment_plot_out_box <- renderUI({
  div(
    style = "position: relative",
    tabBox(
      id = "med_b",
      width = NULL,
      div(style="max-height:600px; overflow-y: scroll; overflow-x: scroll;position: relative",
          plotlyOutput("segment_plot", width = "100%",height= "300px")),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em;",
        downloadButton("segment_plot_dl", label = NULL,icon = icon("download"),class = "btn-m", title = "",
                       style = "position: absolute; right: 10px;bottom:10px;color:#636466;")
      )
    )
  )
})

#===================================
output$waterfall_plot <- renderPlotly({
  dt_customer <- values$data %>% group_by(m_y ) %>% summarise(total_usd := sum(product_total_usd),  
                                                               total_count := sum(purchased_product_total_cnt))
  dt_customer  <- dt_customer[order(as.yearmon(dt_customer$m_y,format="%B %Y")),]
  dt_customer$diff_usd <- dt_customer$total_usd-lag(dt_customer$total_usd,default=first(dt_customer$total_usd))
  dt_customer$text <- ifelse(dt_customer$diff_usd >0, paste0("+$", dt_customer$diff_usd ), paste0("-$",dt_customer$diff_usd ))
  dt_customer$colour <- ifelse(dt_customer$diff_usd >0, "#636466", "#e3af32")
  dt_customer$diff_usd[1] <- dt_customer$total_usd[1]
  dt_customer$colour[1] <- "black"
  dt_customer$hovertext <- paste0("<b><i>Month: ", dt_customer$m_y,"</i></b>", "<br>",
                                  "<b><i>Number of Orders: ", formatC(dt_customer$total_count, format="f", big.mark=",", digits=0), "</i></b>", "<br>",
                                  "<b><i>Previous Sale Revenue: ",formatC(dt_customer$total_usd, format="f", big.mark=",", digits=0), "</i></b>", "<br>",
                                  "<b><i>Number of Customers: ",dt_customer$diff_usd, "</i></b>", "<br>")

  dt_customer$m_y <- factor(dt_customer$m_y, levels=c(dt_customer$m_y))
  dt_customer$measure= c("total", rep("relative",nrow(dt_customer)-1))

  fig <- plot_ly(dt_customer,name = "20", type = "waterfall", measure = ~measure,
                 x = ~m_y, textposition = "outside", y= ~diff_usd, text =~diff_usd,
                 connector = list(line = list(color= "rgb(63, 63, 63)")),
                 decreasing = list(marker = list(
                   color = "#e3af32", line = list(color = "#e3af32", width = 2)
                 )),
                 increasing = list(marker = list(
                   color = "#636466", line = list(color = "#636466", width = 2)
                 )),
                 totals = list(marker = list(
                   color = "black", line = list(color = 'black', width = 3)
                 )))
  
  p <- fig %>%
    layout(
      margin =list( l=30,r=10,b=10,t=40),
      autosize = TRUE,
      showlegend = FALSE,
      title = "Profit and loss statement",
      xaxis = list(
        title = "",
        tickfont = "16",
        ticks = "outside"
      ),
      yaxis = list(title = "",tickprefix ='$'),
      waterfallgap = "0.3",
      
      shapes = list(
        list(
          type = "rect",
          fillcolor = "black",
          line = list(color = 'black', width = 3),
          opacity = 1,
          x0 = -0.4,
          x1 = 0.4,
          xref = "x",
          y0 = 0.0,
          y1 = dt_customer$diff_usd[1],
          yref = "y"
        )
      )
    )
  values$waterfall_plot <- p
})

output$waterfall_plot_dl<- downloadHandler(
  filename = function() { paste("waterfall_plot_",Sys.Date(), '.html', sep='') },
  content = function(file) {
    htmlwidgets::saveWidget(values$waterfall_plot,file = file)
  }
)

output$waterfall_plot_out_box <- renderUI({
  div(
    style = "position: relative",
    tabBox(
      id = "med_b",
      width = NULL,
      div(style="max-height:600px; overflow-y: scroll; overflow-x: scroll;position: relative",
          plotlyOutput("waterfall_plot", width = "100%",height= "400px")),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em;",
        downloadButton("waterfall_plot_dl", label = NULL,icon = icon("download"),class = "btn-m", title = "",
                       style = "position: absolute; right: 10px;bottom:10px;color:#636466;")
      )
    )
  )
})


output$map <- renderLeaflet({
  #color_hightlight <- 
  factpal <- colorFactor(topo.colors(47), sale_map_dt$sku_name)
  factpal <- colorFactor(rainbow(47), sale_map_dt$sku_name)
  factpal <- colorFactor(heat.colors(47), sale_map_dt$sku_name)
  
  sale_map_dt$colors <- ifelse(sale_map_dt$sku_name %in% values$sku_names, rainbow(length(values$sku_names))[1:length(values$sku_names)],"#636466" )
  sale_map_dt$radius <- ifelse(sale_map_dt$sku_name %in%  values$sku_names, sale_map_dt$product_total_usd * 800,sale_map_dt$product_total_usd * 500)
  
  
  #https://bootstrappers.umassmed.edu/bootstrappers-courses/pastCourses/rCourse_2016-04/Additional_Resources/Rcolorstyle.html#heat.colors
  #http://leaflet-extras.github.io/leaflet-providers/preview/index.html
  fig<-  leaflet::leaflet(sale_map_dt) %>%
    setView(lng = -93.85, lat = 37.45, zoom = 4) %>% 
    #setView(lng = -115.2828, lat =  36.1693 , zoom = 6.1) %>%
    addProviderTiles( "CartoDB.Voyager") %>% # Esri.WorldStreetMap
    addCircles(lng = ~lng,
               lat = ~lat,
               weight = 1,
               radius = ~ radius,
               fillOpacity = 0.7,
               popup = ~customerID,
               color = ~colors,#~factpal(sku_name),
               label = ~labels,
               #fillColor = "red",
               #stroke = FALSE,
               group = "circles") %>%
    leaflet.extras::addResetMapButton() %>%
    # addLegend(pal = pal, 
    #           values = ~Q16, 
    #           group = "circles", 
    #           position = "bottomleft",
    #           opacity = 0.6, # Opacity of legend
    #           title = "Products") %>% 
    addLayersControl(overlayGroups = c("circles")) %>%
    leaflet.extras::addSearchFeatures(
      targetGroups = "circles",
      options = searchFeaturesOptions(
        zoom=7, openPopup = TRUE, firstTipSubmit = TRUE,
        autoCollapse = TRUE, hideMarkerOnCollapse = TRUE,
      ))# %>%
  
  values$plot_region <- fig
})

# output$map <- renderLeaflet({
#   fig <- leaflet::leaflet(states) %>%
#     addTiles() %>%
#     addProviderTiles("OpenStreetMap.Mapnik") %>% 
#     #addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
#     addProviderTiles("CartoDB.Positron") %>%
#     setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
#     addPolygons(
#       weight = 2,
#       opacity = 1,
#       color = "white",
#       dashArray = "3",
#       fillOpacity = 0.7,
#       fillColor = ~pal(bins_dt$max_totalsale),
#       popup = pop_up,
#       group = "states") %>% addResetMapButton() %>%
#     addLabelOnlyMarkers(lng = ~X, lat = ~Y, label = ~ID,
#                         labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE,
#                                                     
#                                                     offset = c(0,0),
#                                                     style = list(
#                                                       "color" = "gray", 
#                                                       "font-family" = "serif",
#                                                       "font-style" = "normal",
#                                                       "box-shadow" = "1px 1px rgba(0,0,0,0.25)",
#                                                       "font-size" = "14px",
#                                                       "border-color" = "rgba(0,0,0,0.5)",
#                                                       "padding" = "2px" 
#                                                     ))) %>%
#     # these markers will be "invisible" on the map:
#     addMarkers(
#       data = states, lng = ~X, lat = ~Y, label = ~ID,
#       group = 'cities', # this is the group to use in addSearchFeatures()
#       # make custom icon that is so small you can't see it:
#       icon = makeIcon( 
#         iconUrl = "http://leafletjs.com/examples/custom-icons/leaf-green.png",
#         iconWidth = 1, iconHeight = 1
#       )
#     ) %>%
#     addSearchFeatures(
#       targetGroups = "cities", # group should match addMarkers() group
#       options = searchFeaturesOptions(
#         zoom=10, openPopup = TRUE, firstTipSubmit = TRUE, 
#         autoCollapse = TRUE, hideMarkerOnCollapse = TRUE,
#       )) %>% 
#     addControl("<P><B>Hint!</B> Search for ...<br/><ul><li>New York</li>
#           <li>Boston</li><li>Hartford</li><li>Philadelphia</li><li>Pittsburgh</li>
#           <li>Providence</li></ul></P>",className = "fieldset {border: 0;}", position = 'bottomright'
#     )
#   
#   values$plot_region <- fig
# })


output$plot_region <- renderUI({
  div(
    style = "position: relative",
    tabBox(
      id = "med_b",
      width = NULL,
      div(style="max-height:600px;position: relative",leafletOutput("map", width = "100%",height= "400px")),
      div(
        style = "position: absolute; left: 4em; bottom: 0.5em;",
        downloadButton("download_region", label = NULL,icon = icon("download"),class = "btn-m", title = "",
                       style = "position: absolute; right: 10px;bottom:10px;color:#636466;")
      )
    )
  )
})

output$download_region <- downloadHandler(
  filename = function() {'sale_region.html'},
  content = function(file) {
    htmlwidgets::saveWidget(as_widget(values$plot_region), file)
  }
)


output$drinkers_had_a_heavy <- renderUI({
  out <- tags$iframe(src="https://ourworldindata.org/grapher/drinkers-had-a-heavy-session-in-past-30-days",
                     onload="resizeIframe(this);",
                     style="height:600px;width:100%;border:none;overflow:hidden;scrolling:no;")
  
  return(out)
})



