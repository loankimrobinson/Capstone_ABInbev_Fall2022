rm(list=ls())

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyWidgets)
library(shinycssloaders)
library(shinyBS)
library(fullPage)



library(ggplot2)
library(plotly)
library(stringr)
library(ggplotify)
library(ggrepel) #label
library(RColorBrewer)



library(purrr)
library(DT)
library(scales)
library(tidyverse)
library(data.table)
library(glue)
library(tidyr)
library(reshape2)

library(leaflet)
#devtools::install_github('bhaskarvk/leaflet.extras')
library(leaflet.extras)# to add addSearchFeatures #http://leaflet-extras.github.io/leaflet-providers/preview/index.html
library(sp)
library(raster)
library(rgeos)
library(sf)
library(maps)
library(tools)



library(rmarkdown)
library(kableExtra)
library(htmlwidgets)
library(zoo)
library(rfm)






# #https://rdrr.io/cran/leaflet/man/addLegend.html
# sale_map_dt$labels <- sprintf("<strong style='color: #00d084;font-size:17px;'>%s</strong>
#                          
#                           <br/><strong style='color: red;font-size:14px;'>Customer ID: </strong><em style='font-size:14px;'>%g</em>
#                           <br/><strong style='color: #3EACA8;font-size:14px;'>Product: </strong><em style='font-size:14px;'>%s</em>
#                           <br/><strong style='color: #00d084;font-size:14px;'>Purchase: </strong><em style='font-size:14px;'>%s</em>
#                           <br/><strong style='color: #00d084;font-size:14px;'>Orders: </strong><em style='font-size:14px;'>%s</em> 
#                           <br/><strong style='color: #FF8000;font-size:14px;'>Gender: </strong><em style='font-size:14px;'>%s</em>  
#                               ",
#                               sale_map_dt$city, 
#                               sale_map_dt$customerID,
#                               sale_map_dt$sku_name, 
#                               sale_map_dt$product_total_usd,
#                               sale_map_dt$purchased_product_total_cnt,
#                               sale_map_dt$gender) %>% lapply(htmltools::HTML) 
# 
# str(sale_map_dt)
# 
# factpal <- colorFactor(topo.colors(47), sale_map_dt$sku_name)
# factpal <- colorFactor(rainbow(47), sale_map_dt$sku_name)
# factpal <- colorFactor(heat.colors(47), sale_map_dt$sku_name)
# #https://bootstrappers.umassmed.edu/bootstrappers-courses/pastCourses/rCourse_2016-04/Additional_Resources/Rcolorstyle.html#heat.colors
# #http://leaflet-extras.github.io/leaflet-providers/preview/index.html
# map_out <-  leaflet::leaflet(sale_map_dt) %>% 
#   setView(lng = -115.2828, lat =  36.1693 , zoom = 6.1) %>%  
#   addProviderTiles( "CartoDB.Voyager") %>% # Esri.WorldStreetMap
#   addCircles(lng = ~lng, 
#              lat = ~lat, 
#              weight = 1, 
#              radius = ~ product_total_usd*500, 
#              fillOpacity = 0.7, 
#              popup = ~customerID, 
#              color = ~factpal(sku_name),
#              label = ~labels,
#              #fillColor = "red",
#              #stroke = FALSE,
#              group = "circles") %>% 
#   leaflet.extras::addResetMapButton() %>%
#   addLayersControl(overlayGroups = c("circles")) %>%
#   leaflet.extras::addSearchFeatures(
#     targetGroups = "circles",
#     options = searchFeaturesOptions(
#       zoom=7, openPopup = TRUE, firstTipSubmit = TRUE,
#       autoCollapse = TRUE, hideMarkerOnCollapse = TRUE,
#     ))# %>%
# 
# map_out 




pickerInput2 <- function (inputId, label = NULL, choices, selected = NULL, multiple = FALSE, 
                          options = list(), choicesOpt = NULL, width = NULL, inline = FALSE, ratio = c(3,9)) 
{
  if (ratio[1] + ratio[2] != 12) stop("`ratio` has to add up 12.")
  choices <- shinyWidgets:::choicesWithNames(choices)
  selected <- restoreInput(id = inputId, default = selected)
  if (!is.null(options) && length(options) > 0) 
    names(options) <- paste("data", names(options), sep = "-")
  if (!is.null(width)) 
    options <- c(options, list(`data-width` = width))
  if (!is.null(width) && width %in% c("fit")) 
    width <- NULL
  options <- lapply(options, function(x) {
    if (identical(x, TRUE)) 
      "true"
    else if (identical(x, FALSE)) 
      "false"
    else x
  })
  maxOptGroup <- options[["data-max-options-group"]]
  selectTag <- tag("select", shinyWidgets:::dropNulls(options))
  selectTag <- tagAppendAttributes(tag = selectTag, id = inputId, 
                                   class = "selectpicker form-control")
  selectTag <- tagAppendChildren(tag = selectTag, shinyWidgets:::pickerSelectOptions(choices, 
                                                                                     selected, choicesOpt, maxOptGroup))
  if (multiple) 
    selectTag$attribs$multiple <- "multiple"
  divClass <- "form-group shiny-input-container"
  labelClass <- "control-label"
  if (inline) {
    divClass <- paste(divClass, "form-horizontal")
    selectTag <- tags$div(class = paste0("col-sm-", ratio[2]), selectTag)
    labelClass <- paste(labelClass, paste0("col-sm-", ratio[1]))
  }
  pickerTag <- tags$div(class = divClass, style = if (!is.null(width)) 
    paste0("width: ", validateCssUnit(width), ";"), if (!is.null(label)) 
      tags$label(class = labelClass, `for` = inputId, label), 
    selectTag)
  shinyWidgets:::attachShinyWidgetsDep(pickerTag, "picker")
}


# read data
# zipcode_data <- read.csv("data/zipcode_data.csv", stringsAsFactors = F)
customer_data <- read.csv("data/custs_0.csv", stringsAsFactors = F)
stella <- read.csv("data/stella_purchase_sample.csv")
stella$Is_Stella <- factor(stella$Is_Stella , levels = c(1,0), labels = c("Stella Beer", "Other Product"))
stella$m_y <- format(as.Date(stella$order_date,format='%m/%d/%Y'),format='%B %Y')
stella$month <- lubridate::month(as.Date(stella$order_date,format='%m/%d/%Y'),label = TRUE, abbr = FALSE)
stella$year <- lubridate::year(as.Date(stella$order_date,format='%m/%d/%Y'))
stella$weekday <- lubridate::wday(as.Date(stella$order_date,format='%m/%d/%Y'), label = TRUE, abbr = FALSE)
stella$y_qtr <- as.character(as.yearqtr(as.Date(stella$order_date,format='%m/%d/%Y')))
stella  <- stella[order(as.yearmon(stella$m_y,format="%B %Y")),]
stella$sku_name <- gsub(" - Cloud Data Segment","",stella$sku_name )
stella$sku_name <- gsub("\\ - .*","",stella$sku_name)
stella$sku_name <- ifelse(stella$sku_name == "Stella Artois", "Stella Artois Beer", stella$sku_name)
stella$AOV <- stella$product_total_usd/stella$purchased_product_total_cnt
stella <- merge(stella ,customer_data, all.x = TRUE, by.x = "customerID", by.y = "id")
stella_beer <- stella %>% filter(sku_name == "Stella Artois Beer")

#====================================================================
library(usmap)
statepop$state <- statepop$abbr
statepop$location <- statepop$full
sale_map_dt <- merge(stella ,statepop[,c("state","location")], all.x = TRUE, by.y = "state", by.x = "st")

states <- read_rds("data/states.rds")
states$ID <- tools::toTitleCase(states$ID)
sale_map_dt <- sale_map_dt[!is.na(sale_map_dt$location), ]
sale_map_dt <- sale_map_dt[order(sale_map_dt$location,match(sale_map_dt$location,states$ID)),]



#https://rdrr.io/cran/leaflet/man/addLegend.html
sale_map_dt$labels <- sprintf("<strong style='color: #00d084;font-size:17px;'>%s</strong>

                          <br/><strong style='color: red;font-size:14px;'>Customer ID: </strong><em style='font-size:14px;'>%g</em>
                          <br/><strong style='color: #3EACA8;font-size:14px;'>Product: </strong><em style='font-size:14px;'>%s</em>
                          <br/><strong style='color: #00d084;font-size:14px;'>Purchase: </strong><em style='font-size:14px;'>%s</em>
                          <br/><strong style='color: #00d084;font-size:14px;'>Orders: </strong><em style='font-size:14px;'>%s</em>
                          <br/><strong style='color: #FF8000;font-size:14px;'>Gender: </strong><em style='font-size:14px;'>%s</em>
                              ",
                              sale_map_dt$city,
                              sale_map_dt$customerID,
                              sale_map_dt$sku_name,
                              sale_map_dt$product_total_usd,
                              sale_map_dt$purchased_product_total_cnt,
                              sale_map_dt$gender) %>% lapply(htmltools::HTML)


bins_dt  <- sale_map_dt %>% group_by(location) %>% summarise( max_totalsale := max(sum(product_total_usd )))
bins <- seq(min(bins_dt$max_totalsale ), max(bins_dt$max_totalsale ), length = 10)
pal <- colorBin("YlOrRd", domain = bins_dt$max_totalsale, bins = bins)



popup_dt  <- sale_map_dt  %>% group_by(location,  year) %>% summarise( total_usd := sum(product_total_usd ),
                                                                       total_orders :=  sum(purchased_product_total_cnt))
popup_dt <- popup_dt[order(popup_dt$year), ]
bins_dt$color <- c(pal(bins_dt$max_totalsale))
popup_dt <- merge(popup_dt , bins_dt[, c("location","color")], all.x = TRUE, by = "location")


popup_dt$location_text <- sprintf("<br/><strong style='color: #FF8000;font-size:14px;'>Location: </strong><em style='font-size:14px;'>%s</em>", popup_dt$location)
popup_dt$year_per <- paste0(sprintf("<br/><strong style='color: #FF8000;font-size:14px;'></strong><em style='font-size:14px;'>%s</em>",popup_dt$year),
                            sprintf("<br/><strong style='color: #FF8000;font-size:14px;'>Total USD: </strong><em style='font-size:14px;'>%s</em>",popup_dt$total_usd ),
                            sprintf("<br/><strong style='color: #FF8000;font-size:14px;'>Total Orders: </strong><em style='font-size:14px;'>%s</em>",popup_dt$total_orders))

popup_new <- popup_dt[order( popup_dt$year),] %>% group_by(location) %>% summarise( pop := paste0(year_per, collapse = "<br>"))
popup_new  <- merge(popup_new  , popup_dt[!duplicated(popup_dt$location), c("location","location_text")], all.x = TRUE, by = "location")

popup_new$final <- paste0(popup_new$location_text,"<br>",popup_new$pop)
pop_up <- popup_new$final[match(states$ID, popup_new$location)]


#==================================================================


dt_processing <- function(data, analysis_date = max(as.Date(stella$order_date,format='%m/%d/%Y'))){
  dt_customer <- data%>% group_by(customerID,order_date) %>% summarise(total_usd := sum(product_total_usd),
                                                                       total_count := sum(purchased_product_total_cnt))
  dt_customer$order_date <- format(as.Date(dt_customer$order_date,format='%m/%d/%Y'))
  dt_customer <- dt_customer %>% group_by(customerID) %>% arrange(order_date) %>% mutate(next_purchase :=lead(order_date, default = last(order_date)),
                                                                                          days_between_purchase:= as.integer(difftime(next_purchase,order_date, units = "days"))
  )
  dt_customer  <- dt_customer[ order(dt_customer$customerID, dt_customer$order_date),c("customerID","order_date","next_purchase","days_between_purchase","total_count","total_usd")]
  dt_rfm <- dt_customer %>% group_by(customerID) %>% summarise(most_recent_visit := max(order_date),
                                                               average_days_between_purchase := mean(days_between_purchase),
                                                               revenue := sum(total_usd),
                                                               number_of_orders := sum(total_count))
  dt_rfm$recency_days = as.integer(difftime(analysis_date,dt_rfm$most_recent_visit, units = "days")) + 1
  lt_out <- list(dt_customer , dt_rfm)
  names(lt_out) <- c("dt_customer","dt_rfm")
  return(lt_out)
}
dt_rfm <- dt_processing(stella_beer)
dt_customer <- dt_rfm$dt_customer
dt_rfm <- dt_rfm$dt_rfm

# Repurchase Rate Default
Repurchase_Rate <- dt_customer[dt_customer$days_between_purchase <= 30 & dt_customer$days_between_purchase > 0,]
cus <- length(unique(dt_customer$customerID))
rep_cus <- length(unique(Repurchase_Rate$customerID))
Repurchase_Rate_Default <- (rep_cus/cus)*100




# rmf_model <- function(data,analysis_date = max(as.Date(stella$order_date,format='%m/%d/%Y'))){
#   #===========================
#   # Segment	Description
#   # Champions	Bought recently, buy often and spend the most
#   # Loyal Customers	Buy on a regular basis. Responsive to promotions.
#   # Potential Loyalist	Recent customers with average frequency.
#   # Recent Customers	Bought most recently, but not often.
#   # Promising	Recent shoppers, but haven’t spent much.
#   # Customers Needing Attention	Above average recency, frequency and monetary values. May not have bought very recently though.
#   # About To Sleep	Below average recency and frequency. Will lose them if not reactivated.
#   # At Risk	Purchased often but a long time ago. Need to bring them back!
#   # Can’t Lose Them	Used to purchase frequently but haven’t returned for a long time.
#   # Hibernating	Last purchase was long back and low number of orders. May be lost.
#   rfm_result <- rfm::rfm_table_customer(data , customerID, number_of_orders,recency_days, revenue, analysis_date)
#   rfm_df <- rfm_result$rfm
#   rfm_df$Segment <- ""
#   rfm_df$Segment[which(rfm_df$recency_score %in% c(1,2) & rfm_df$frequency_score%in% c(1,2))] <-"Hibernating"
#   rfm_df$Segment[which(rfm_df$recency_score %in% c(1,2) & rfm_df$frequency_score%in% c(3,4))] <-"At risk"
#   rfm_df$Segment[which(rfm_df$recency_score %in% c(1,2) & rfm_df$frequency_score%in% c(5))] <-"Can’t Lose Them"
#   rfm_df$Segment[which(rfm_df$recency_score %in% c(3) & rfm_df$frequency_score%in% c(1,2))] <-"About To Sleep"
#   rfm_df$Segment[which(rfm_df$recency_score %in% c(3) & rfm_df$frequency_score%in% c(3))] <-"Need attention"
#   rfm_df$Segment[which(rfm_df$recency_score %in% c(3,4) & rfm_df$frequency_score%in% c(4,5))] <-"Loyal customers"
#   rfm_df$Segment[which(rfm_df$recency_score %in% c(4) & rfm_df$frequency_score%in% c(1))] <-"Promising"
#   rfm_df$Segment[which(rfm_df$recency_score %in% c(5) & rfm_df$frequency_score%in% c(1))] <-"New customers"
#   rfm_df$Segment[which(rfm_df$recency_score %in% c(4,5) & rfm_df$frequency_score%in% c(2,3))] <-"Potential loyalists"
#   rfm_df$Segment[which(rfm_df$recency_score %in% c(5) & rfm_df$frequency_score%in% c(4,5))] <-"Champions"
#   return(rfm_df )
#   
# }
#=================================================
# Segment	Description
# Champions	Bought recently, buy often and spend the most
# Loyal Customers	Buy on a regular basis. Responsive to promotions.
# Potential Loyalist	Recent customers with average frequency.
# Recent Customers	Bought most recently, but not often.
# Promising	Recent shoppers, but haven’t spent much.
# Customers Needing Attention	Above average recency, frequency and monetary values. May not have bought very recently though.
# About To Sleep	Below average recency and frequency. Will lose them if not reactivated.
# At Risk	Purchased often but a long time ago. Need to bring them back!
# Can’t Lose Them	Used to purchase frequently but haven’t returned for a long time.
# Hibernating	Last purchase was long back and low number of orders. May be lost.
segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")
recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

segment_names_data <- data.frame(segment = segment_names,
                                 recency_lower = recency_lower,
                                 recency_upper  = recency_upper,
                                 frequency_lower =  frequency_lower,
                                 frequency_upper = frequency_upper,
                                 monetary_lower = monetary_lower,
                                 monetary_upper = monetary_upper)
segment_names_data$recency_bin <- paste0(segment_names_data$recency_upper, '-', segment_names_data$recency_lower)
segment_names_data$frequency_bin <- paste0(segment_names_data$frequency_upper, '-', segment_names_data$frequency_lower)
segment_names_data$monetary_bin <- paste0(segment_names_data$monetary_upper, '-', segment_names_data$monetary_lower)

# RFM Model
rfm_model <- function(data,analysis_date = max(as.Date(stella$order_date,format='%m/%d/%Y'))){
  #===========================
  # Segment	Description
  # Champions	Bought recently, buy often and spend the most
  # Loyal Customers	Buy on a regular basis. Responsive to promotions.
  # Potential Loyalist	Recent customers with average frequency.
  # Recent Customers	Bought most recently, but not often.
  # Promising	Recent shoppers, but haven’t spent much.
  # Customers Needing Attention	Above average recency, frequency and monetary values. May not have bought very recently though.
  # About To Sleep	Below average recency and frequency. Will lose them if not reactivated.
  # At Risk	Purchased often but a long time ago. Need to bring them back!
  # Can’t Lose Them	Used to purchase frequently but haven’t returned for a long time.
  # Hibernating	Last purchase was long back and low number of orders. May be lost.
  rfm_result <- rfm::rfm_table_customer(data , customerID, number_of_orders,recency_days, revenue, analysis_date)
  rfm_segments <- rfm_segment(rfm_result, segment_names, recency_lower, recency_upper,
                              frequency_lower, frequency_upper, monetary_lower, monetary_upper)
  rfm_heatmap <- rfm::rfm_heatmap_data(rfm_result)
  rfm_hist <- rfm::rfm_hist_data(rfm_result)
  rfm_barchart <- rfm::rfm_barchart_data(rfm_result)


  rfm_distribution <- rfm_segments %>% count(segment) %>% arrange(desc(n))
  rfm_distribution  <- merge(rfm_distribution, segment_names_data[, c("segment","recency_bin","frequency_bin","monetary_bin")],all.y = TRUE, by = "segment")
  rfm_distribution[is.na(rfm_distribution)] <- 0
  rfm_distribution <- rfm_distribution[order(desc(rfm_distribution$n)),]
  rfm_dt_out <- list(rfm_result$rfm,rfm_segments,rfm_distribution,rfm_heatmap,rfm_hist,rfm_barchart)
  
  
  
  names(rfm_dt_out) <- c("rfm_result","rfm_segments","rfm_distribution","rfm_heatmap","rfm_hist", "rfm_barchart")
  
  return(rfm_dt_out)
  
}
rfm_model_dt_default  <- rfm_model(dt_rfm)

# Customer Segment	Recency Score Range	Frequency & Monetary Combined Score Range
# Champions	4-5	4-5
# Loyal Customers	2-5	3-5
# Potential Loyalist	3-5	1-3
# Recent Customers	4-5	0-1
# Promising	3-4	0-1
# Customers Needing Attention	2-3	2-3
# About To Sleep	2-3	0-2
# At Risk	0-2	2-5
# Can’t Lose Them	0-1	4-5
# Hibernating	1-2	1-2
# Lost	0-2	0-2
heatmap_plot <- function(data){
  text_dt <- data
  df <- reshape2::dcast(data, frequency_score ~ recency_score, value.var="monetary")
  row.names(df) <- df$frequency_score
  df$frequency_score <- NULL
  df  <- as.matrix(df)
  col <- colorRamp(c("#fbf2df","#c7941b"))
  p <- plotly::plot_ly(
    x = row.names(df),
    y = row.names(df),
    z = df, 
    xgap = 2,
    ygap = 2,
    type = "heatmap", 
    colors = col,
    source = "rfm_heatmap",
    # colors = "YlGnBu", 
    # #colorscale=colorScale ,
    # reversescale =F,
    # colors = "Reds",#"YlGnBu", 
    reversescale =F,
    hovertemplate="Recency: %{x}\nFrequency: %{y}\nMonetary: %{z}\n<extra></extra>"
  ) %>% add_annotations(x = text_dt$recency_score,
                        y = text_dt$frequency_score,
                        text = round(text_dt$monetary,1), 
                        showarrow = F,
                        font=list(color='#4a1e15')) %>% hide_colorbar() %>% layout(yaxis = list(title = "FrequencyScore"),
                                                                                 xaxis = list(title = "Recency Score"),
                                                                                 margin =list( l=30,r=10,b=10,t=40),
                                                                                 title = list(text = "Average Revenue Per User Matrix for R&F Scores",
                                                                                              font = list(size = 14,color = "#4a1e15")))
  
  
  p
  
}
#heatmap_plot(data = rfm_model_dt_default$rfm_heatmap)


#================================
# Color button for DT package
js_col <- function(col1 = "#636466", col2 = "#3d3e3f", col3 = "#231f20"){
  glue::glue('$("button.buttons-copy").css("background","',col1,'").css("color","white").css("border-color","',col1,'"); 
        $("button.buttons-print").css("background","',col2,'").css("color","white").css("border-color","',col2,'");
        $("button.buttons-collection").css("background","',col3,'").css("color","white").css("border-color","',col3,'");
         return table;')
}
# table function
dt_tb <- function(dt, file_name, filter = TRUE, pageLength = 20 ){
  if(isTRUE(filter)){
    filter = 'top'
    searching = TRUE
  }else{
    filter = 'none'
    searching = FALSE
  }
  datatable(dt, 
            rownames = FALSE,
            #escape = FALSE,
            callback = JS(js_col()),
            filter = filter, # filter option
            extensions = c('Buttons'), # add buttons feature
            options = list(dom = "Blfrtip",
                           initComplete = JS("function(settings, json) {","$(this.api().table().header()).css({'background-color': '#636466', 'color': '#fff'});","}"),
                           #columnDefs = list(list(className = 'dt-left', targets = "_all")),
                           #ordering = T,
                           scrollX = TRUE,
                           class = 'cell-border stripe',
                           #orientation = 'landscape',
                           lengthChange = FALSE,
                           searching = FALSE,
                           pageLength = pageLength, # 10 rows in 1 page
                           infor = FALSE,
                           #autoWidth = T,
                           # List of button here
                           buttons = list(
                             list(extend = 'copy',
                                  exportOptions = list(
                                    modifier = list(page = "all") # set list(page = "current") for current page
                                  )),
                             list(extend = 'print',
                                  exportOptions = list(
                                    modifier = list(page = "all")
                                  )),
                             list(extend = 'collection',
                                  text = 'Download',# Change label of button from Collection to Download
                                  buttons = list(
                                    list(extend = "csv",
                                         filename = file_name,
                                         title = file_name,
                                         exportOptions = list(
                                           modifier = list(page = "all")
                                         )
                                    ),
                                    list(extend = "excel",
                                         filename = file_name,
                                         title = file_name,
                                         exportOptions = list(
                                           modifier = list(page = "all")
                                         )
                                    ),
                                    list(extend = "pdf",
                                         filename = file_name,
                                         title = file_name,
                                         orientation = 'landscape',
                                         exportOptions = list(
                                           modifier = list(page = "all")
                                         )
                                    )
                                  )
                             )# end button with collection
                           )# end button
            )) 
}

card <- function(.img) {
  # If you want circle replace <img src="', .img, '" style="width:80%; padding-top:20px;">  to 
  # <img src="', .img, '" style="width:80%; padding-top:20px; border-radius:50% "> 
  HTML(
    paste0(
      '<div class="onlycard">
      <img src="', .img, '" style="width:90px; padding-top:5px;"> 
      </div>')
  )
}


img.src <- c("Bud Light.png", "Budweiser.png", "Busch.png", "Estrella.png", 
             "Hoegaarden.png", "Landshark.png","Natural Light.png", "Patagonia.png", 
             "Presidente.png", "Shock Top.png", "Stella Artois.png","Ultra.png"
)

#dput(list.files("www"))
img.src.partner <- c("10 Barrel Brewing Co black.png", "AMB_logo.png", 
                     "BluePoint_logo.png", "Breckenridge_logo.png",
                     "Cisco_logo.png", "Devils Backbone.png", 
                     "Elysian black.png","Four Peaks.png", "Golden Road Brewery Logo.png", 
                     "GooseIsland_logo.png", "karbach-logo-updated.png", 
                     "kona-logo.png", "Landshark.png",
                     "Omission_logo.png", "platform-logo-updated.png", 
                     "redhook-logo.png", "square-mile-logo.png", 
                      "veza-sur-logo-updated.png", 
                     "virtue-cider-logo-updated.png", "wicked-weed-logo-updated.png", 
                     "widmer-logo.png")#, "wynwood-logo.png")


trimmer <- function(x,break_limit){ sapply(strwrap(str_to_title(x), break_limit, simplify=FALSE), paste, collapse="\n")}

bar_box_horizontal <- function(trimmer = 22, autorange = FALSE, plot_data,x_var,y_var,color_var,text,hovertext,title = "",x_var_label="",y_var_label="", colorRampPalette = TRUE,color_fill = c("#C2CFE7","#6787c4"), legend = "bottom", type_var="bar",alpha = 1,stack = TRUE,tickangle=0,mode_var="lines+markers", source = "summary_out") {
  legend_in <- legend
  if(legend_in  == "top"){
    legend <- list(orientation = "h",xanchor = "center",y = 1.0,x = 0.5)
    showlegend <- TRUE
  }else if(legend_in  == "bottom"){
    legend <- list(itemwidth = 29,orientation = "h",xanchor = "center",size = 12,y =-0.1,x = 0.5,
                   title=list(size = 12,color = "#4a1e15"),
                   font = list(size = 12,color = "#4a1e15"))
    showlegend <- TRUE
  }else if(legend_in  == "right"){
    legend <- list(orientation = "v",yanchor = "center",y = 1,x = 0.8)
    showlegend <- TRUE
  }else if(legend_in  == "left"){
    legend <- list(orientation = "v",yanchor = "center",y = 0.5,x = -0.15)
    showlegend <- TRUE
  }else{
    showlegend <- FALSE
  }
  if(isTRUE(colorRampPalette)){
    length_color <- length(unique(plot_data[[color_var]]))
    color_fill_out <-  colorRampPalette(color_fill)(length_color)
  }else{
    color_fill_out <- color_fill
  }
  
  if(isTRUE(stack)){
    barmode = 'stack'
  }else{
    barmode = 'group'
  }
  
  if(isFALSE(autorange)){
    autorange = "reversed"
  }else{
    autorange  = TRUE
  }
  
  if("total" %in% colnames(plot_data)){
    range <- c(ifelse(min(plot_data[["total"]]) >= 0, 0, min(plot_data[["total"]])*2), max(plot_data[["total"]])+(max(plot_data[["total"]])/7))
  }else{
    range <- c(ifelse(min(plot_data[,y_var]) >= 0, min(plot_data[,y_var]) - min(plot_data[,y_var])/5 , min(plot_data[,y_var])*2), max(plot_data[,y_var])+(max(plot_data[,y_var])/7))
  }
  label <- unique(as.character(plot_data[[x_var]]))
  tick.text <- trimmer(label, trimmer)
  
  
  p <- plotly::plot_ly(plot_data, 
                       x = ~ get(y_var), 
                       color = ~ get(color_var),
                       y = ~ get(x_var),
                       customdata = ~ get(color_var),
                       text = ~ formatC(get(text), format="f", big.mark=",", digits=0),
                       hoverinfo = "text",
                       hovertext = ~hovertext,
                       textposition = c('outside'),
                       textfont = list(size = 11, color = "#4a1e15"),
                       type = type_var,
                       alpha = 1,
                       colors = color_fill_out,
                       orientation = 'h',
                       source = source)
  plot <- p %>% layout(
    barmode = barmode,
    bargap = 0.2, bargroupgap = 0.1,
    font = list(color = 'gray',size = 10),
    hoverlabel = list(font=list(size=11)),
    showlegend =  showlegend,
    title = list(text = title,font = list(size = 15,color = "#4a1e15")),
    margin =list( l=30,r=10,b=10,t=40),
    xaxis = list(
      # autorange = "reversed",
      range = range,
      fixedrange = TRUE,
      tickfont = list(
        
        size = 11,
        color = "#4a1e15"
      ),
      titlefont = list(
        
        size =  13,
        color = "#4a1e15"
      ),
      title = x_var_label,
      zeroline = FALSE,
      tickmode = "array",
      color = "#4a1e15"
      # tickvals=~ c(0, get(y_var)),
      # ticktext=~ c(0, get(y_var))
    ),
    yaxis = list(
      autorange =  autorange,
      tickfont = list(
        
        size = 11,
        color = "#4a1e15"),
      titlefont = list(
        
        size =  13,
        color = "#4a1e15"),
      title = y_var_label,
      zeroline = FALSE,
      tickmode = "array",
      zeroline = FALSE,
      tickmode = "array",
      tickvals = label,
      ticktext = tick.text,
      # tickvals=~ c(0,get(x_var)),
      # ticktext=~ c(0,get(x_var)),
      color = "#4a1e15"),
    legend = legend)
  return(plot)
}










