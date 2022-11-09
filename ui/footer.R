
fluidRow(
    tags$div(
      tags$footer("Data source:",
                  tags$a("AB inBev",
                         href = "https://www.anheuser-busch.com/brands/",
                         target = "_blank")),
      tags$footer(("Built with"),
                  tags$a(href = "http://www.r-project.org/",
                         target = "_blank",
                         "R,"),
                  tags$a(href = "http://shiny.rstudio.com",
                         target = "_blank",
                         "Shiny"),
                  ("&"),
                  tags$a(href = "http://www.rstudio.com/products/shiny/shiny-server",
                         target = "_blank",
                         "Shiny Server."),
                  br(),
                  tags$a(
                    href = "https://www.anheuser-busch.com/brands/",
                    target = "_blank",
                    tags$img( src = "logo_1.svg",
                              height='40px;',
                              width = 100,
                              alt = "logo"))
                  
      )
    ),
    align = "center"
  )