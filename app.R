library(shiny)
library(ggplot2)
library(quantmod)
# Get stock data
stock_symbol <- "AAPL"
start_date <- "2023-01-01"
end_date <- "2023-07-01"

stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)
# UI
ui <- fluidPage(
  titlePanel("Stock Portfolio Dashboard"),
  dateRangeInput(
    "date_range",
    "Select Date Range:",
    start = "2023-01-01",
    end = "2023-07-01"),
  selectInput(
    "time_frame",
    "Select Time Frame:",
    choices = c("Daily", "Weekly", "Monthly")
  ),
  plotOutput("stock_chart")
)

# Server
server <- function(input, output)
{
  output$stock_chart <- renderPlot({
# Filter by selected date range
    filtered_data <- stock_data[
      index(stock_data) >=
        input$date_range[1] &
        index(stock_data) <=
        input$date_range[2]
    ]
# Apply selected time frame
    if(input$time_frame == "Weekly") {
      filtered_data <- 
        to.weekly(filtered_data)
    }
    if (input$time_frame == "Monthly") {
      filtered_data <- 
        to.monthly(filtered_data)
    }
# Convert stock data for ggplot
    chart_data <- data.frame(
      Date = index(filtered_data),
      Close = 
        as.numeric(Cl(filtered_data))
    )
# Line Chart
    ggplot(chart_data, aes(x = 
    Date,y = Close)) +
      geom_line() +
      labs(
        title = "AAPL Stock Price",
        x = "Date",
        y = "Closing Price") +
      theme_minimal()
  })
}
shinyApp(ui, server)