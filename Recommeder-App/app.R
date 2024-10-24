#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(arules)
library(igraph)
library(arulesViz)
library(DT) # Ensure you load the DT package for data tables

# Function to read transactions from a CSV file
get.txn <- function(data.path, columns) {
  transactions.obj <- read.transactions(file = data.path, format = "single", sep = ",", cols = columns, 
                                        rm.duplicates = FALSE, header = TRUE, quote = "", skip = 0, encoding = "Unknown")
  return(transactions.obj)
}

# Function to generate association rules
get.rules <- function(support, confidence, transactions) {
  parameters = list(support = support, confidence = confidence, minlen = 2, maxlen = 10, target = "rules")
  rules <- apriori(transactions, parameter = parameters)
  return(rules)
}

# Function to find the best rules based on leverage
find.rules <- function(transactions, support, confidence, topN = 10) {
  all.rules <- get.rules(support, confidence, transactions)
  rules.df <- data.frame(rules = labels(all.rules), all.rules@quality)
  other.im <- interestMeasure(all.rules, transactions = transactions)
  rules.df <- cbind(rules.df, other.im[, c('conviction', 'leverage')])
  best.rules.df <- head(rules.df[order(-rules.df$leverage), ], topN)
  return(best.rules.df)
}

# Function to plot the graph
plot.graph <- function(cross.sell.rules) {
  edges <- unlist(lapply(cross.sell.rules['rules'], strsplit, split = '=>'))
  g <- graph(edges = edges)
  return(g)
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  columns <- c(1, 2)
  path <- 'data/data.csv' # path to the data file
  transactions.obj <- get.txn(path, columns) # Ensure this uses the defined path
  
  cross.sell.rules <- reactive({
    support <- input$Support
    confidence <- input$confidence
    cross.sell.rules <- find.rules(transactions.obj, support, confidence)
    return(cross.sell.rules)
  })
  
  gen.rules <- reactive({
    support <- input$Support
    confidence <- input$confidence
    gen.rules <- get.rules(support, confidence, transactions.obj)
    return(gen.rules)
  })
  
  output$rulesTable <- DT::renderDataTable({
    cross.sell.rules()
  })
  
  output$graphPlot <- renderPlotly({
    g <- plot.graph(cross.sell.rules())
    plot(g)
  })
  
  output$explorePlot <- renderPlotly({
    plot(x = gen.rules(), method = NULL, measure = "support", shading = "lift", interactive = FALSE)
  })
}

# Define UI for application
ui <- fluidPage(
  headerPanel("Cross Sell Analysis"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("Support", "Support threshold:", min = 0.01, max = 1.0, value = 0.01),
      sliderInput("confidence", "Confidence", min = 0.05, max = 1.0, value = 0.05)
    ),
    mainPanel(
      tabsetPanel(
        id = 'xsell',
        tabPanel("Rules", DT::dataTableOutput("rulesTable")),
        tabPanel("Graph", plotlyOutput("graphPlot")),
        tabPanel("Explore", plotlyOutput("explorePlot"))
      )
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server)

