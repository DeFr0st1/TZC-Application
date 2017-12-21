#!/usr/bin/env Rscript
rm(list = ls())
list.of.packages <- c("shiny", "httr", "jsonlite", "lubridate", "formattable", "shinydashboard", "rsconnect")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0){ install.packages(new.packages)}

library(shiny)
library(httr)
library(jsonlite)
library(lubridate)
library(formattable)
library(shinydashboard)
library(rsconnect)


diff.get <- GET(url = "http://tzc.explorerz.top:3004/api/getdifficulty")
tmp<- rawToChar(diff.get$content)
diff.tmp <- fromJSON(tmp)

price.get <- GET(url = "https://api.coinmarketcap.com/v1/ticker/trezarcoin/")
tmp <- rawToChar(price.get$content)
price <- fromJSON(tmp)

denom <- 4294967296
multi.factor <- 1000
diff <- diff.tmp$`proof-of-work`
reward <- 100
day <- 86400
hour <- 3600


output.table <- data.frame(matrix(nr=5, nc=6))
names(output.table) <- c("Per", "Pool Fee %", "Est. Rewards", "Revenue $", "Cost $", "Profit $")
output.table[1,1] <- "Hour"
output.table[2,1] <- "Day"
output.table[3,1] <- "Week"
output.table[4,1] <- "Month"
output.table[5,1] <- "Year"


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Interactive Data", icon = icon("th"), tabName = "interactivedata",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
      h3("All data is live"),
        fluidRow(
            column(
                width = 10, offset = 2,
                valueBoxOutput("difficulty"),
                valueBoxOutput("price")
            )

        ),
        fluidRow(
            box(title = "Enter Parameters Here", status = "primary",
              numericInput("hash", "Hash Rate:", 1656, min = 1, max = 1e9, step = 1),
              numericInput("pwrusg", "Power Usage:", 300, min = 1, max = 1e9, step = 1),
              numericInput("pwrcst", "Power Cost:", 0.1, min = 0.01, max = 0.5, step = .01),
              numericInput("poolfee", "Pool Fee %:", 0, min = 0.1, max = 5, step = 0.1),
              numericInput("tzcdiff", "Difficulty:", diff, min = 1, max = 1e9, step = 1),
              numericInput("blkrwd", "Block Reward:", reward, min = 1, max = 1e9, step = 1),
              numericInput("tzcprc", "TZC Price $:", as.numeric(price$price_usd), min = 0.009, max = 5, step = 0.001)
            ),
            box(title = "Your Rewards", status = "primary",
              tableOutput("table"))
          ),
      fluidRow(
        column(width = 12, offset = 4,
           htmlOutput("donate")
        )
      )
        ),
    tabItem(tabName = "interactivedata",
      h2("Interactive Data")             
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "DeFr0st TZC vBeta"),
  sidebar,
  body
)

server <- function(input, output) {
  
  output$difficulty <- renderValueBox({
    valueBox(
      round(diff,2), "Difficulty", color = "light-blue", icon = icon("legal")
    )
  })
  output$price <- renderValueBox({
    valueBox(
      round(as.numeric(price$price_usd),4), "Price $USD", color = "green", icon = icon("dollar")
    )
  })
  
  
  output$table <- renderTable({ 
    
    output.table[1,2] <- input$poolfee
    output.table[2,2] <- input$poolfee
    output.table[3,2] <- input$poolfee
    output.table[4,2] <- input$poolfee
    output.table[5,2] <- input$poolfee
    
    output.table[1,3] <- (((input$hash*input$blkrwd*hour)/(input$tzcdiff*denom))*multi.factor) * (1-(input$poolfee/100))
    output.table[2,3] <- (((input$hash*input$blkrwd*day)/(input$tzcdiff*denom))*multi.factor) * (1-(input$poolfee/100))
    output.table[3,3] <- (((input$hash*input$blkrwd*(day*7))/(input$tzcdiff*denom))*multi.factor) * (1-(input$poolfee/100))
    output.table[4,3] <- (((input$hash*input$blkrwd*(day*30))/(input$tzcdiff*denom))*multi.factor) * (1-(input$poolfee/100))
    output.table[5,3] <- (((input$hash*input$blkrwd*(day*365))/(input$tzcdiff*denom))*multi.factor) * (1-(input$poolfee/100))

    output.table[1,4] <- currency(input$tzcprc * output.table[1,3])
    output.table[2,4] <- currency(input$tzcprc * output.table[2,3])
    output.table[3,4] <- currency(input$tzcprc * output.table[3,3])
    output.table[4,4] <- currency(input$tzcprc * output.table[4,3])
    output.table[5,4] <- currency(input$tzcprc * output.table[5,3])

    output.table[1,5] <- currency((input$pwrusg/1000)*input$pwrcst)
    output.table[2,5] <- currency(((input$pwrusg*24)/1000)*input$pwrcst)
    output.table[3,5] <- currency(((input$pwrusg*(24*7))/1000)*input$pwrcst)
    output.table[4,5] <- currency(((input$pwrusg*(24*30))/1000)*input$pwrcst)
    output.table[5,5] <- currency(((input$pwrusg*(24*365))/1000)*input$pwrcst)
    
    output.table[1,6] <- output.table[1,4]-output.table[1,5]
    output.table[2,6] <- output.table[2,4]-output.table[2,5]
    output.table[3,6] <- output.table[3,4]-output.table[3,5]
    output.table[4,6] <- output.table[4,4]-output.table[4,5]
    output.table[5,6] <- output.table[5,4]-output.table[5,5]

   output.table
    })
  
  output$donate <- renderUI({
    HTML(paste(
    "Donate:",
    "",
    "TZC: Ttk84BWjYGmpKDAAmcPRcmJjhxY6RJq8eG",
    "VIVO: VGQFRwi7s6c6S5ihzJzc8EQMX35tKUuEEX", 
    "BTC: 1LuWhENdBEMhsEosCwbtvUpEJJpAWQ3dZX",
    "ZEC: t1HzSVhGTVHRE3orbA6iJLWpce9XiruTVDb",
    "LTC: LU3X572D3Zu3adespXBq4CAH3CbDpT89V5",
    "ETH: 0xc03cfd2c036ba8d462b7ecf0529ed8090172d6cc",
    "ETC: 0xcc958ef58974d952559d901b5956ac9e5b8a7b4b",
    "",
    "Twitter: https://twitter.com/DeFr0st1",
    "Github: https://github.com/DeFr0st1",
    sep = '<br/>'))
  })
}
shinyApp(ui, server)



