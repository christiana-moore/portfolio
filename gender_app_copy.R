#Creating a Shiny app in R with the legislatoR package (2019)

##Server side code 
server <- shinyServer(function(input, output){
  require(legislatoR)
  require(shiny)
  require(tidyverse)
  require(ggplot2)
  
###Data cleaning 
  leg <- reactive({
    leg <- get_core(legislature = input$leg)
    leg_p <- get_political(legislature = input$leg)
    
    leg <- leg %>% dplyr::select(pageid, name, sex)
    leg_p <- leg_p %>% dplyr::select(pageid, session_start)
    
    leg <- merge(leg, leg_p, by = "pageid")
    
    leg$session_start <- word(leg$session_start,1,sep = "\\-")
    
    leg <- leg %>%
      group_by(session_start, sex) %>%
      summarize(N = n()) %>%
      ungroup() %>%
      complete(session_start, sex,
               fill = list(N = 0, freq = 0)) %>%
      filter(!is.na(sex))
  })
  
years <- reactive({seq(input$years[1], input$years[2], by = 1)
  })
  
output$gender <- renderPlot({
    leg <- leg()
    plot <- leg %>% filter(session_start %in% years()) %>%
      ggplot(aes(x = session_start, y = N, group = sex, color = sex)) 
    
    plot <- plot + geom_point() + geom_line() + labs(x = "Legislative Session Start Date", y = "Number of Representatives")  
   print(plot)
  }
  )
  }
  )

##UI side code
ui <- fluidPage(
    plotOutput("gender"),
    hr(),
    fluidRow(
      column(3,
             h4("Gender in National Legislatures"),
             h5("An interactive tool showcasing the gender composition of select national legislatures from 1970-present. Source data from the legislatoR package (2019)")
             ),
      column(3,
    selectInput("leg", "Legislature:",
                list("Austrian Nationalrat" = "aut", 
                     "Canadian House of Commons" = "can", 
                     "Czech Poslanecka Snemovna" = "cze",
                     "French Assemblée" = "fra",
                     "German Bundestag" = "deu",
                     "Irish Dail" = "irl",
                     "Scottish Parliament" = "sco",
                     "US House" = "usa_house",
                     "US Senate" = "usa_senate"))),
    column(4, offset = 1,
        sliderInput("years", "Date Range", min=1970, max=2019,
                value=c(1970,2019), dragRange = TRUE, sep="")
        )))
      


shinyApp(ui = ui, server = server)
