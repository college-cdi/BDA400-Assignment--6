library(shiny)
library(ggplot2)
library(quantmod)
# Get stock data
stock_symbol <- "AAPL"

stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = "2023-01-01",
  to = "2023-07-01",
  auto.assign = FALSE
)
# UI
ui <- fluidPage(
  titlePanel("Stock Portfolio Dashboard"),
  dateRangeInput(
    "date_range",
    "Select Date Range:",
    start = "2023-01-01",
    end = "2023-07-01",
),
  selectInput(
    "time_frame",
    "Select Time Frame:",
    choices = c("Daily", "Weekly", "Monthly")
  ),
checkboxGroupInput(
  "technical_indicators",
  "Select Technical Indicators:",
  choices = c("Moving Avegares", "RSI", "MACD")
),

  plotOutput("stock_chart"
))

# Server
server <- function(input, output)
{
  output$stock_chart <- renderPlot({
    # Filter  date range
    filtered_data <- stock_data[
      index(stock_data) >=
        input$date_range[1] &
        index(stock_data) <=
        input$date_range[2]
    ]
  
    # Change time frame
    if(input$time_frame == "Weekly") {
      filtered_data <- 
        to.weekly(filtered_data)
    }
   else if (input$time_frame == "Monthly") {
      filtered_data <- 
        to.monthly(filtered_data)
    }
    # Convert to date frame
    chart_data <- data.frame(
      Date = as,Date(index(filtered_data)),
      Close = 
        as.numeric(Cl(filtered_data))
    )
    # TECHNICAL INDICATORS
    if (nrow(chart_data)>= 20){
      chart_data$SMA20 <- SMA(
        chart_data$Close,
        n = 20)
    } else {
      chart_data$SMA20 <- NA_real_}
    if (nrow(chart_data) >= 50) {
      chart_data$SMA50 <- SMA(chart_data$Close, n = 50)
    } else {
      chart_data$SMA50 <- NA_real_} 
    
    # Calculate RSI
    if(nrow(chart_data) >= 14) {
      chart_data$RSI <- RSI(
        chart_data$Close, n = 14)
    } else {
    chart_data$RSI <- NA_real_ }
    # Calculate MACD
    if(nrow(chart_data) >= 35) {
      macd_values <- MACD(
        chart_data$Claose,
        nFast = 12,
        nSlow = 26,
        nSig = 9
      )
    chart_data$MACD <- 
      as.numeric( macd_values[, 1]) 
    } else {
      chart_data$MACD <- NA_real_}
# Trading Rules
    chart_data$Signal <- "Hold"
# Previous days's moving averages
    previous_SMA20 <- c(NA,
    head(chart_data$SMA20, -1))
    previous_SMA50 <- c(NA, 
    head(chart_data$SMA50, -1))
# Buy when SMA20 crosses above SMA50
    buy_signal <- chart_data$SMA20 > chart_data$SMA50 &
      previous_SMA20 <= previous_SMA50
# Sell when SMA20 crosses below SMA50
    sell_signal <- chart_data$SMA20< chart_data$SMA50 &
      previous_SMA20 >= previous_SMA50
    chart_data$Signal[
      !is.na(buy_signal) & buy_signal] <- "BUY"
    chart_data$Signal[
      !is.na(sell_signal) & sell_signal] <- "SELL"
# Create Stock Price Chart
    p <- ggplot(
      chart_data, aes(x = Date, y = Close)
    ) +
      geom_line(
        linewidth = 0.8
      ) + 
      labs( 
        title = paste(
        stock_symbol,
        "Stock Price with Technical Analysis"),
        x = "Date", y = "Closing Price"
      ) +
        theme_minimal()
# ADD MOVING AVERAGES
    if( "Moving Averages" %in%
        input$technical_indicators){
      p <- p +
        geom_line( aes(y = SMA20),
                   linetype = "dashed",
                   linewidth = 0.8) +
        geom_line( aes(y = SMA50),
                   linetype = "dotted",
                   linewidth = 0.8)
    }
# ADD RSI
    if( "RSI" %in%
        input$technical_indicators){
      p <- p +
        geom_line( aes(y = RSI),
                   linetype = "dashed",
                   linewidth = 0.7) 
    }
# ADD MACD
    if( "MACD" %in%
        input$technical_indicators){
      p <- p +
        geom_line( aes(y = MACD),
                   linetype = "dotted",
                   linewidth = 0.7) }
# ADD BUY AND SELL ANNOTATIONS
    signal_data <- chart_data [
      chart_data$Signal == "Buy"!
        chart_data$Signal == "Sell", ]
    if (nrow(signal_data) > 0) {
      p <- p +
        geom_point( data = signal_data, aes( x = Date, y = Close, shape = Signal),
                    size = 3 ) +
        geom_text ( data = signal_data,
                    aes( x = Date, y = Close, label = Signal),
                    vjust = -1.2, size = 4)}
# Display chart
    print(p)
  })
}
# RUN SHINY APPLICATION
    shinyApp(
      ui = ui,
      server = server
    )
    