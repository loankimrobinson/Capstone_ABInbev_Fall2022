source("global.R")
shinyUI(
  fluidPage(
    shinyWidgets::useShinydashboard(),
    shinyjs::useShinyjs(),
    tags$head(
      includeCSS(file.path('www', 'style.css')),
      tags$style(".btn-inline {background: #e3af32; border: solid 1px #e3af32;border-radius: 0px;}",
                ".btn-inline .filter-option-inner-inner { font-size: 14px; color:white; font-style:italic;}"
      ),
      tags$head(tags$style(HTML('.navbar-static-top {background-color:#231f20;border-color:#231f20;color:white;}',
                                '.navbar-default .navbar-nav>.active>a {background-color:#231f20;border-color:#231f20;color:white;}'))),
    ),
    navbarPage(
      id = "nar",
      selected = "homeTab",
      windowTitle="AB inBev Tracker",
      title=div("",tags$script(HTML("var header = $('.navbar > .container-fluid');header.append('<div style=\"float:left;\"><a href=\"https://www.ab-inbev.com\"><img src=\"logo_1.svg\" alt=\"alt\" style=\"float:left;width:200px;height:50px;padding-left:10px;\"> </a></div>');console.log(header)"))),
      tabPanel(
        title = "Home",
        id    = "homeTab",
        value = "homeTab",
        name = "homeTab",
        fluidRow(
          column(4,
                 div(style = "text-align:center;padding-left:10px; padding-right:10px;background-color:#e3af32;color:white;",class = "card",
                     br(),
                     h2("Welcome to AB InBev Sale Dashboard Application",style = "color:#4a1e15;font-weight:bold;text-align:center;padding-left:50px;padding-right:50px;" ),
                     p("Founded more than 160 years ago, Anheuser-Busch is one of America's most iconic companies and the undisputed leader of the U.S. beer industry."),
                     p("We are proud of our history and heritage in this country and we remain committed to brewing the great-tasting, 
                     high-quality beers that have satisfied beer drinkers for generations."),
                     br(),
                     p("In addition to many of America's most recognizable beer brands — including industry-leading craft brands — 
                     we proudly brew a number of other products within our Beyond Beer segment. Through our broad portfolio, 
                     we are committed to meeting our customers' needs — for every occasion."),
                     br(),
                     p("To learn more about our products, and see a full list of our more than 100 brands, please visit ", a(href = "https://www.tapintoyourbeer.com/", target = "_blank","tapintoyourbeer.com/")),
                     h2("OUR BEERS",style = "color:#4a1e15;font-weight:bold;text-align:center;padding-left:50px;padding-right:50px;" ),
                     p("Since the 1850s, we have been proud to brew America's most-loved beers. "),
                     p("Today, our portfolio of amazing brands continues to lead the industry by delivering what you, our customers, want."),
                     p("From flavored extensions like Bud Light Lime and Orange, to Michelob ULTRA Pure Gold, the first major beer to be USDA organic certified, 
                       we work tirelessly to deliver the strongest portfolio of beer brands in the industry."),br(),br()
                     )
                 ),
          column(8,
                 br(),
                 h3("AB InBEV BEER PRODUCTS",style = "color:#4a1e15;font-weight:bold;text-align:center;padding-left:50px;padding-right:50px;" ),
                 br(),
                 uiOutput("cards_out"), 
                 br(),br(),br(),br(),br(),
                 h3("BREWERS COLLECTIVE OUR CRAFT PARTNERS",style = "color:#4a1e15;font-weight:bold;text-align:center;padding-left:50px;padding-right:50px;" ),
                 br(),br(),
                 uiOutput("cards_partner_out"))
          
          
        )
      ),
      tabPanel(
        title = "Dashboard",
        id    = "dashboardTab",
        value = "dashboardTab",
        name = "dashboardTab",
        
        fluidRow(
          div(id= "first_row", style = "background-color:#636466;height:60px;",##636466 #231f20
              fixedRow(
                column(width = 4,style = "color:white;", uiOutput("year_input")),
                column(width = 4,style = "color:white;", uiOutput("sku_names_input")),
                column(width = 4,style = "color:white;", uiOutput("submit_input"))
              )
          )
        ),br(),
        fluidRow(
          box(width = 3,solidHeader = TRUE,
              title = p("SALE REVENUE",actionButton("sale_rev_dt", label = NULL,icon = icon("plus"),class = "btn-xs", title = "SALE REVENUE DATA",style = "position: absolute; right: 10px;bottom:10px;color:white;")),
              htmlOutput("sale_rev") ,footer = ""),
          box(width = 3,solidHeader = TRUE,
              title = p("AVERAGE TIME between ORDERS",actionButton("ave_time_dt", label = NULL,icon = icon("plus"),class = "btn-xs", title = "AVERAGE TIME BETWEEN ORDERS DATA",style = "position: absolute; right: 10px;bottom:10px;color:white;")),
              htmlOutput("ave_time") ,footer = ""),
          box(width = 3,solidHeader = TRUE,
              title = p("AVERAGE REVENUE by CUSTOMERS",actionButton("ave_sale_rev_dt", label = NULL,icon = icon("plus"),class = "btn-xs", title = "Sale Evenue Data",style = "position: absolute; right: 10px;bottom:10px;color:white;")),
              htmlOutput("ave_sale_rev") ,footer = ""),
          box(width = 3,solidHeader = TRUE,title = p("REPURCHASE RATE"),
              div(
                style = "position: absolute; right: 1em; top: 0.5em;",
                tags$style(".glyphicon{display: none;}"),
                dropdown(
                  selectInput("repurchase_window", "Time Period", choices = c("30 Days", "14 Days", "7 Days"), selected = "30 Days", width = "120px"),
                  size = "xs",
                  icon = icon("play"),
                  up = FALSE,
                  right = TRUE)),
              htmlOutput("rep_sale_rev") ,footer = ""),
        ),
        fluidRow(
         column(width = 4,uiOutput("summary_plot_out_box")),
         column(width = 4,uiOutput("rfm_plot_out_box")),
         column(width = 4,uiOutput( "segment_plot_out_box"))
        ),
        fluidRow(
          column(width = 5,uiOutput("waterfall_plot_out_box")),
          column(width=7, uiOutput("plot_region"))
        )
      ),
      tabPanel(
        title = "Contact",
        id    = "contactTab",
        value = "contactTab",
        name = "contactTab",
        source(file.path("ui", "contact.R"),  local = TRUE)$value,
      )
    ),
    br(),
    div(style = "background-color:#231f20;border-color:#231f20;color:white;",
    source(file.path("ui", "footer.R"),  local = TRUE)$value)
    
 )
)


