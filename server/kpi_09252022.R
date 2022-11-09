

values <- reactiveValues(data = stella_beer , 
                         dt_present = stella_beer  %>% filter(m_y == "May 2022"),
                         dt_previous = stella_beer  %>% filter(m_y == "April 2022"),
                         year = as.character(unique(stella$y_qtr)), 
                         dt_customer = dt_customer,
                         dt_rfm = dt_rfm,
                         Repurchase_Rate = paste0(formatC( Repurchase_Rate_Default, format="f", big.mark=",", digits=2), "% in 30 Days"),
                         sku_names = "Stella Artois Beer")


# observe({
#   print(values$Repurchase_Rate)
# })


observeEvent(input$submit,{
  values$year <- input$year
  values$sku_names <- input$sku_names
  
  dt <- stella %>% filter(year %in% c(values$year) & sku_name %in% c(values$sku_names))
  previous_month <- unique(dt$m_y)
  previous_month <- ifelse( "May 2022" %in% previous_month,"April 2022", previous_month[length(previous_month )])
  
  
  dt_present <- stella %>% filter(m_y == "May 2022" & sku_name %in% c(values$sku_names))
  dt_previous <- stella %>% filter(m_y== previous_month  & sku_name %in% c(values$sku_names))
  
  dt_rfm <- dt_processing(dt)
  dt_customer <- dt_rfm$dt_customer
  dt_rfm <- dt_rfm$dt_rfm
  
  
  Repurchase_Rate <- dt_customer[dt_customer$days_between_purchase <= 30 & dt_customer$days_between_purchase > 0,]
  cus <- length(unique(dt_customer$customerID))
  rep_cus <- length(unique(Repurchase_Rate$customerID))
  Repurchase_Rate_Default <- (rep_cus/cus)*100
  values$Repurchase_Rate <- paste0(formatC( Repurchase_Rate_Default, format="f", big.mark=",", digits=2), "% in ", input$repurchase_window)
  
  
  
  values$data <- dt
  values$dt_previous <- dt_previous
  values$dt_present <- dt_present
  values$dt_customer  <- dt_customer 
  values$dt_rfm <- dt_rfm

})


#=============================================
# Sale REvenue  KOIs

output$sale_rev <- renderText({
  dt <- values$data 
  sale_rev <- sum(dt$product_total_usd)
  sale_rev <- formatC( sale_rev, format="f", big.mark=",", digits=0)
  HTML(paste0("<span style='font-size:28px;padding-top:5px;padding-bottom:5px;color: #231f20; font-weight:bold;'>$", sale_rev,"</span><span style='font-size:18px;'> </span>"))
})


output$sale_rev_change <- renderText({
  sale_rev_pre <- sum(values$dt_previous$product_total_usd)
  sale_rev_present <- sum(values$dt_present$product_total_usd)
  sale_rev <- ((sale_rev_present - sale_rev_pre)/sale_rev_pre)*100
  
  sale_rev <- formatC(sale_rev, format="f", big.mark=",", digits=2)
  sale_rev <- ifelse(sale_rev >0, paste0("+",sale_rev), sale_rev )
  HTML(paste0("<span style='font-size:11px;color: #231f20; font-style:italic'>Change from Previous Month</span><span style='font-size:15px;padding-top:0px;color: #231f20; font-weight:bold;'>", sale_rev,"%</span>
              <span style='font-size:11px;color: #231f20; font-style:italic'></span>"))
})

observeEvent(input$sale_rev_dt,{
  dt <- values$data 
  dt <- dt %>% group_by(m_y) %>% summarise(revenue := sum(product_total_usd),
                                           orders := sum(purchased_product_total_cnt)
                                           )
  dt <- dt[order(as.yearmon(dt$m_y,format="%B %Y")),]
  colnames(dt) <- c("Month Year", "Sale Revenue", "Total Orders")
  showModal(modalDialog(
    title = paste0("Sale Revenue by Month"),
    output$sale_rev_dt <- DT::renderDT({
      dt_tb(dt,"sale_revenue_data", filter = F, pageLength = 15)
    }),
    easyClose=TRUE,
    size = 'l',
    footer = actionButton("closeModal",label = "Close")
  ))
})


#==============================
# AVERAGE TIME BETWEEN ORDERS KPI

output$ave_time <- renderText({
  dt <-  values$dt_customer
  ave_time <- mean(dt$days_between_purchase)
  ave_time <- formatC( ave_time, format="f", big.mark=",", digits=2)
  HTML(paste0("<span style='font-size:28px;padding-top:5px;padding-bottom:5px;color: #231f20; font-weight:bold;'>", ave_time," Days</span><span style='font-size:18px;'> </span>"))
})

observeEvent(input$ave_time_dt,{
  dt <-  values$dt_customer
  colnames(dt) <- c("CustomerID", "Order Date", "Next Order Date", "Days Between Order", "Total Orders", "Total Purchases")
  showModal(modalDialog(
    title = paste0("AVERAGE TIME BETWEEN ORDERS"),
    output$sale_rev_dt <- DT::renderDT({
      dt_tb(dt,"ave_time_dt", filter = F, pageLength = 15)
    }),
    easyClose=TRUE,
    size = 'l',
    footer = actionButton("closeModal",label = "Close")
  ))
})


#==============================
# AVERAGE REVENUE by CUSTOMERS

output$ave_sale_rev <- renderText({
  dt <- values$data 
  dt <- dt %>% group_by(customerID) %>% summarise(total_usd := sum(product_total_usd)) 
  ave_sale_rev <- mean(dt$total_usd)
  ave_sale_rev <- formatC(ave_sale_rev, format="f", big.mark=",", digits=2)
  HTML(paste0("<span style='font-size:28px;padding-top:5px;padding-bottom:5px;color: #231f20; font-weight:bold;'>$",ave_sale_rev,"</span><span style='font-size:18px;'> </span>"))
})

observeEvent(input$ave_sale_rev_dt,{
  dt <- values$data 
  dt <- dt %>% group_by(customerID) %>% summarise(total_usd := sum(product_total_usd)) 
  colnames(dt) <- c("CustomerID", "Total Purchases")
  showModal(modalDialog(
    title = paste0("AVERAGE REVENUE BY CUSTOMERS"),
    output$ave_sale_rev_dt <- DT::renderDT({
      dt_tb(dt,"ave_sale_rev_dt", filter = F, pageLength = 15)
    }),
    easyClose=TRUE,
    size = 'l',
    footer = actionButton("closeModal",label = "Close")
  ))
})


#=============================
# REPURCHASE RATE


output$rep_sale_rev <- renderText({
  Repurchase_Rate <- values$Repurchase_Rate
  HTML(paste0("<span style='font-size:28px;padding-top:5px;padding-bottom:5px;color: #231f20; font-weight:bold;'>", Repurchase_Rate,"</span><span style='font-size:18px;'> </span>"))
})


observeEvent(input$repurchase_window,{
  dt <-  values$dt_customer
  if(input$repurchase_window == "30 Days"){
    time = 30
  }else if(input$repurchase_window == "14 Days"){
    time = 14 
  }else{
    time = 7
  }
  Repurchase_Rate <- dt[dt$days_between_purchase <= time & dt$days_between_purchase > 0,]
  cus <- length(unique(dt$customerID))
  rep_cus <- length(unique(Repurchase_Rate$customerID))
  Repurchase_Rate_Default <- (rep_cus/cus)*100
  values$Repurchase_Rate <- paste0(formatC( Repurchase_Rate_Default, format="f", big.mark=",", digits=2), "% in ", input$repurchase_window)
})

#==============================

# Close removeModal =====================
observeEvent(input$closeModal, {
  removeModal()
})







