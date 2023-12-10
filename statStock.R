library(tidyverse)
library(rvest)
library(shiny)
library(treemapify)
library(tidyquant)
library(dplyr)
library(corrplot)
library(bslib)
library(keras)
library(randomForest)
library(rpart)
library(gbm)


# Turn on thematic for theme-matched plots
thematic::thematic_shiny(font = "auto")
theme_set(theme_bw(base_size = 16))



ui = page_navbar(
  title="Stat Stock",
  theme = bs_theme(bootswatch = "vapor", version = 5),
  sidebar = sidebar(
    textInput('item', 'Enter Stock Names (comma-separated):', value = "TSLA, GS, MSFT, AAPL"),
    dateRangeInput('dateRange', 'Select Date Range (up to 6 months difference) :', start = "2023-05-05", end = "2023-11-25"),
    textOutput("status_message"),
    radioButtons("maType", "Select Moving Average:",
                 choices = c("None", "VWMA", "SMA", "Bollinger Bands"),
                 selected = "None"),
    radioButtons("logTypeInput","Select log:",choices = c("None","log2","log10"),selected = "None"),
    radioButtons("statVar", "Select which variable:", choices = c("Close*", "Open", "High", "Adj Close**", "Volume"), selected = "Close*"),
    actionButton("submit", "Submit"),
    radioButtons("modelType","Select Machine Learning Model:", choices = c("LinearRegression","BoostedRegression","RecursivePartitioningTrees"), selected ="LinearRegression"),
    radioButtons("daysToPredict","How many days after the end date to predict:", choices = c(5,10,20,40), selected = 20 ),
      ),
    
  nav_panel(
    "At a Glance",
 
  layout_columns(
    card( 
      card_header(
        "At a Glance"
      ),
      min_height = '100%',
      full_screen = TRUE,
      title = "At a glance",
      plotOutput("candlestickChart")
    )
  )),
  nav_panel(
    "Data Visualizations",
  layout_columns(
    card(
      card_header(
        "Line Graph"
      ),
      min_height = "80%",
      full_screen = TRUE,
      plotOutput("plot3")
    )),
  layout_columns(
    card(
      card_header(
        "Violin Plot"
      ),
      full_screen = TRUE,
      plotOutput("plot1")
    ),
    card(
      card_header(
        "Histogram"
      ),
      full_screen = TRUE,
      plotOutput("plot4")
    ))
  ), nav_panel(
      "Statistics",
      layout_columns(
        card(
          verbatimTextOutput("correlationText"),
          full_screen = TRUE,
          title = "Correlation Text"
        ),
        card(
          card_header("Pearson Correlation Matrix [-1,1] where 1 is correlated, -1 is no correlation."),
          full_screen = TRUE,
          title = "Correlation Matrix",
          plotOutput("plot2")
        )
      )), nav_panel(
        "Machine Learning",
        verbatimTextOutput("machineLearningPredictions"),
        layout_columns(
        card(
          title = "Machine Learning Output",
          full_screen = TRUE,
          plotOutput("lmPlot",height=800)
        )
        ),
        layout_columns(
          
          card(
            "Model Health",
            verbatimTextOutput("modelHealth"),
            style = "max-height: 400px; overflow-y: auto;"
          )
        )

        
        
      )
    
    )
    
    
  
   
  
    



  


server = function(input, output, session){
  #bs_themer()
  scrapedDataValues = reactiveValues(scrapedData = NULL,scrapingStatus = "Not started")
  selectedMAType = reactive({
    input$maType
  })
  vaR = reactive({
    input$statVar

  })
  outputVaR = renderPrint({
    vaR()
  })
  daysFuture = reactive({
    input$daysToPredict
  })
  outputDays = renderPrint({
    daysFuture()
  })
  output$machineLearningPredictions = renderText({
    paste("Machine Learning Predictions for", vaR(), "in", daysFuture(), "days. Dotted Line is Projection.")
  })
  logType = reactive({
    input$logTypeInput
  })
  mLType = reactive({
    input$modelType
  })
  output$status_message = renderText({
    # Display the scraping status
    paste("Scraping status: ", scrapedDataValues$scrapingStatus)
  })
  
  # Linear Regression Plot Function
  output$lmPlot = renderPlot({
    
    algType = as.character(mLType())
    switch(algType,
           "LinearRegression"={
             lm_results_data = lm_results()
           },
           "RecursivePartitioningTrees"={
             lm_results_data = forestResults()
           },
           "BoostedRegression"={
             lm_results_data = rForestResults()
           }
           )
    
    
    # Combine results for all stocks into a single data frame
    data = do.call(rbind, lapply(lm_results_data, function(x) x$predictions))
    scrapedData = scrapedDataValues$scrapedData
    
    # Check if scrapedData is not empty
    if (!is.null(scrapedData)) {
      # Binds all the dataframes by row, making one long dataframe
      df = bind_rows(scrapedData, .id = "Stock")
      df = as.data.frame(df)
      df = cleanData(df)
    }
    
    ggplot(data, aes(x = Date, y = Predicted, color = Stock, group = Stock)) +
      geom_line(aes(y = Predicted), linetype = "dashed") +
      geom_line(data = df, aes(x = Date, y = `Close*`)) +
      labs(x = "Date", y = "Predicted Value")
  })
  output$modelHealth = renderPrint({
    algType = as.character(mLType())
    switch(algType,
           "LinearRegression"={
             results = lm_results()
           },
           "RecursivePartitioningTrees"={
             results = forestResults()
           },
           "BoostedRegression"={
             results = rForestResults()
           }
    )
    
    
    # Check if results is a list
    if (is.list(results)) {
      
      # Loop over results for each stock
      for (stock_result in results) {
        stockName = unique(stock_result$predictions$Stock)
        print(stockName)
        # Extract model name
        model_name = stock_result$model$coefficients
        
        # Extract R-squared value
        rsquared = stock_result$rsquared
        
        # Print model name and R-squared value for the current stock
        cat("Coeffiencts for", stockName, ":", model_name, "\n")
        cat("R-squared for", stockName, ":", rsquared, "\n\n")
      }
      
    } else {
      cat("Invalid structure for results. Expected a list.\n")
    }
  })
  
  
  output$correlationText = renderPrint({
    scrapedData = scrapedDataValues$scrapedData
    
    # Check if scrapedData is not empty
    if (!is.null(scrapedData)) {
      # Binds all the dataframes by row, making one long dataframe
      df = bind_rows(scrapedData, .id = "Stock")
      df = as.data.frame(df)
      df = cleanData(df)
      
      # Add the code for plotting here
      correlationMatrix = createTable(df)
      
      
      # Return the correlation matrix for verbatimTextOutput
      return(correlationMatrix)
    }
  })
  
  
  
  output$candlestickChart = renderPlot({
    scrapedData = scrapedDataValues$scrapedData
    maType = selectedMAType()
    if (!is.null(scrapedData)) {
      df = bind_rows(scrapedData, .id = "Stock")
      df = as.data.frame(df)
      df = cleanData(df)
      candleStick(df, maType)
    }
  })
  
  output$plot1 = renderPlot({
    scrapedData = scrapedDataValues$scrapedData
    
    # Check if scrapedData is not empty
    if (!is.null(scrapedData)) {
      # Binds all the dataframes by row, making one long dataframe
      df = bind_rows(scrapedData, .id = "Stock")
      df = as.data.frame(df)
      df = cleanData(df)
      
      
      
      
      violinPlot(df, as.character(vaR()))
      
    }
  })
  
  output$plot2 = renderPlot({
    scrapedData = scrapedDataValues$scrapedData
    
    # Check if scrapedData is not empty
    if (!is.null(scrapedData)) {
      # Binds all the dataframes by row, making one long dataframe
      df = bind_rows(scrapedData, .id = "Stock")
      df = as.data.frame(df)
      df = cleanData(df)
      correlationMatrix = createTable(df)
      # Add the code for plotting here
      createTable(df)
    }
  })
  
  output$plot3 = renderPlot({
    scrapedData = scrapedDataValues$scrapedData
    
    # Check if scrapedData is not empty
    if (!is.null(scrapedData)) {
      # Binds all the dataframes by row, making one long dataframe
      df = bind_rows(scrapedData, .id = "Stock")
      df = as.data.frame(df)
      df = cleanData(df)
      
      # Add the code for plotting here
      graph(df, as.character(vaR()))
    }
  })
  
  output$plot4 = renderPlot({
    scrapedData = scrapedDataValues$scrapedData
    
    # Check if scrapedData is not empty
    if (!is.null(scrapedData)) {
      # Binds all the dataframes by row, making one long dataframe
      df = bind_rows(scrapedData, .id = "Stock")
      df = as.data.frame(df)
      df = cleanData(df)
      
      # Add the code for plotting here
      histogram(df,as.character(vaR()))
      
    }
  })
  
  
  

#user input for items
userInputs = eventReactive(input$submit, {
    list(
      items = unlist(strsplit(input$item, ",\\s*")),
      timeSet = as.Date(input$dateRange)
    )
  })

scrapper = function(item, timeSet) {
  base_url = "https://finance.yahoo.com/quote/"
  data_list = list()
  data = data.frame()
  
  for (i in 1:length(item)) {
    #cannot just use numeric Dates, had to use POSIXct 
    start_date = as.numeric(as.POSIXct(timeSet[1]))
    end_date = as.numeric(as.POSIXct(timeSet[2]))
    
    #create the url call
    url = paste0(base_url, item[i], "/history?period1=", start_date, "&period2=", end_date, "&interval=1d&filter=history&frequency=1d&includeAdjustedClose=true")
    print(url)
    #html data from the loaded page
    page = read_html(url)
    
    #extract and clean data using pipe operator f1 -> f2 -> f3 = table_data
    table_data = page %>%
      html_nodes("table") %>%
      html_table(header = TRUE, fill = TRUE)
    
    #make sure the table isn't empty
    if (length(table_data) > 0) {
      #Clean the data by selecting only the required columns
      table_data_clean = table_data[[1]] %>%
        filter(!grepl("\\*", Date)) %>%
        select(Date, Open, High, Low, `Close*`, `Adj Close**`, Volume)
      
      # Store the cleaned data frame in the list
      data_list[[item[i]]] = table_data_clean
      #Append table
      data = bind_rows(data, table_data_clean)
    } else {
      cat("Table not found for", item[i], "\n")
    }
  }
  
  return(data_list)
}

cleanData = function(df){
  #need to fix types in our dataframe, Volume and Date were previously Character types
  
  
  df$Volume = as.numeric(gsub(",", "", df$Volume))
  df$Date = as.Date(df$Date, format="%b%d,%Y")
  df$`Close*` = as.numeric(df$`Close*`)
  df$Open = as.numeric(df$Open)
  df$High = as.numeric(df$High)
  df$Low = as.numeric(df$Low)
  df$`Adj Close**`= as.numeric(df$`Adj Close**`)
 
  
  na.omit(df)
  return(df)
}
#rfResults = reactiveValues(model = NULL, predictions = NULL, mse = NULL, rsquared = NULL)

observe({
  
  print("Submit button clicked!")
  
  #Print the user inputs
  cat("Items: ", userInputs()$items, "\n")
  cat("Time Set: ", userInputs()$timeSet, "\n")
  scrapedDataValues$scrapingStatus = "In progress"
  #Scrape data
  scrapedData = scrapper(userInputs()$items, userInputs()$timeSet)
  scrapedDataValues$scrapedData = scrapedData
  scrapedDataValues$scrapingStatus = "Done"
  
})

#general time series line graph 
graph = function(df, y) {
  
  ggplot(df, aes(x = Date, y = !!sym(y),color=Stock)) +
    geom_line() +  
    labs(title = paste("Security -", y),
         x = "Date",
         y = y) 
}

#Tree map diagram histogram = function(df){
treeMap = function(df, y) {
  # Find the minimum and maximum dates
  minDate = min(df$Date)
  maxDate = max(df$Date)
  
  # Calculate the change in y values and the percentage change
  df = df %>%
    group_by(Stock) %>%
    summarize(yChange = (max(!!sym(y)) - min(!!sym(y))),
              PercentChange = (max(!!sym(y)) - min(!!sym(y))) / abs(min(!!sym(y))) * 100,
              Direction = ifelse(max(!!sym(y)) > (min(!!sym(y)) + max(!!sym(y))) / 2, "Up", "Down"))
  
  ggplot(df, aes(area = PercentChange, fill = Direction, label = paste0(Stock, "\n", sprintf("%.2f%%", PercentChange)))) +
    treemapify::geom_treemap() +
    geom_treemap_text(place = "centre", size = 12) +
    scale_fill_manual(values = c("Up" = "green", "Down" = "red")) +  
    labs(title = paste("Movement of", y, "Direction for timeframe", minDate, maxDate))
}
#histogram of the data
#The geom_histogram uses sturges rule as a means to calculate bin size
#since it is a dynmaic app, I figured it is good enough to get the point across
histogram = function(df,y){
  ggplot(df,aes(x=!!sym(y),color=Stock))+
  geom_histogram(fill="white", alpha=0.3, position="identity")
}

violinPlot = function(df, y){
  ggplot(df, aes(x = factor(Stock), y = !!sym(y), fill = Stock)) +
    geom_violin(trim = FALSE, scale = "width", alpha = 0.8) +
    geom_boxplot(width = 0.2, fill = "white", color = "black", outlier.shape = NA)
    
  
}

#using maFun (which is a cool way to do different moving averages)
candleStick = function(df, maType) {
  
  switch(maType,
         "None" = {
           ggplot(df, aes(x = Date, y = `Close*`, group = Stock)) +
             geom_candlestick(aes(open = Open, high = High, low = Low, close = `Close*`)) +
             facet_wrap(~ Stock, ncol = 2, scales = "free_y") 
         },
         "VWMA" = {
           ggplot(df, aes(x = Date, y = `Close*`, volume = Volume, group = Stock)) +
             geom_candlestick(aes(open = Open, high = High, low = Low, close = `Close*`)) +
             geom_ma(aes(group = Stock), ma_fun = VWMA, n = 15, wilder = TRUE) +
              
             facet_wrap(~ Stock, ncol = 2, scales = "free_y") 
         },
         "SMA" = {
           ggplot(df, aes(x = Date, y = `Close*`, group = Stock)) +
             geom_candlestick(aes(open = Open, high = High, low = Low, close = `Close*`)) +
             geom_ma(aes(group = Stock), ma_fun = SMA,  color = "darkblue", size = 1) +
            
             facet_wrap(~ Stock, ncol = 2, scale = "free_y") 
         },
         "Bollinger Bands" = {
           
           ggplot(df, aes(x = Date, y = `Close*`, 
                          open = Open, high = High, low = Low, close = `Close*`, 
                          group = Stock)) +
             geom_barchart() +
             geom_bbands(aes(group = Stock), ma_fun = SMA, sd = 2,  linetype = 5) +
             facet_wrap(~ Stock, ncol = 2, scales = "free_y") },
         stop("Invalid statType. Please choose one of: none, VWAP, SMA, bollingerbands")
  )
}





lm_results = reactive({
  scrapedData = scrapedDataValues$scrapedData
  daysToAdd = as.numeric(daysFuture())
  mlAlgorithm = as.character
  if (!is.null(scrapedData)) {
    df = bind_rows(scrapedData, .id = "Stock")
    df = as.data.frame(df)
    df = cleanData(df)
    
    df$Close_lag = lag(df$`Close*`)
    
  
    # Remove NA values introduced by lag
    df = na.omit(df)
    
    # Initialize a list to store results for each stock
    results_list = list()
    
    # Loop over unique stocks
    for (stock in unique(df$Stock)) {
      # Subset data for the current stock
      stock_data = df[df$Stock == stock, ]
      
      # Predict days in advance:
      predictedData = data.frame(
        Date = seq(max(stock_data$Date) + 1, by = 1, length.out = daysToAdd),
        Stock = rep(stock, daysToAdd),
        Close_lag = rep(tail(stock_data$`Close*`, daysToAdd), daysToAdd)
      )
      
     
      lm_model = lm(`Close*` ~Close_lag , data = stock_data)
      predictions = predict(lm_model, newdata = predictedData)
      
      #Combine original data with predictions, date, stock, and value
      results_df = data.frame(
        Date = predictedData$Date,
        Stock = predictedData$Stock,
        Predicted = predictions
      )
      print(summary(lm_model))
      #Calculate R-squared
      rsquared = summary(lm_model)$r.squared
      
      #Store results for the current stock
      results_list[[stock]] = list(model = lm_model, predictions = results_df, rsquared = rsquared)
      
    }
    
    return(results_list)
  }
})

#need to properly measure model health
forestResults = reactive({
  scrapedData = scrapedDataValues$scrapedData
  daysToAdd = as.numeric(daysFuture())
  mlAlgorithm = as.character
  if (!is.null(scrapedData)) {
    df = bind_rows(scrapedData, .id = "Stock")
    df = as.data.frame(df)
    df = cleanData(df)
    
    df$Close_lag = lag(df$`Close*`)
    
    
    df = na.omit(df)
    results_list = list()
    for (stock in unique(df$Stock)) {
      # Subset data for the current stock
      stock_data = df[df$Stock == stock, ]
      
      # Predict days in advance:
      predictedData = data.frame(
        Date = seq(max(stock_data$Date) + 1, by = 1, length.out = daysToAdd),
        Stock = rep(stock, daysToAdd),
        Close_lag = rep(tail(stock_data$`Close*`, daysToAdd), daysToAdd)
      )
      
      #partitioning Tree, not linear model. 
      lm_model = rpart(`Close*` ~Close_lag , data = stock_data,cp = 0.01)
      
      # Predictions
      predictions = predict(lm_model, newdata = predictedData)
      
      # Combine original data with predictions, date, stock, and value
      results_df = data.frame(
        Date = predictedData$Date,
        Stock = predictedData$Stock,
        Predicted = predictions
      )
      
      # Calculate R-squared
      rsquared = summary(lm_model)$r.squared
      print(summary(lm_model))
      # Store results for the current stock
      results_list[[stock]] = list(model = lm_model, predictions = results_df, rsquared = rsquared)
      
    }
    
    return(results_list)
  }
})

#Need to properly measure model health
rForestResults = reactive({
  scrapedData = scrapedDataValues$scrapedData
  daysToAdd = as.numeric(daysFuture())
  mlAlgorithm = as.character
  if (!is.null(scrapedData)) {
    df = bind_rows(scrapedData, .id = "Stock")
    df = as.data.frame(df)
    df = cleanData(df)
    
    df$Close_lag = lag(df$`Close*`)
    
    print("In that gbm yo")
    # Remove NA values introduced by lag
    df = na.omit(df)
    results_list = list()
    for (stock in unique(df$Stock)) {
      # Subset data for the current stock
      stock_data = df[df$Stock == stock, ]
      
      # Predict days in advance:
      predictedData = data.frame(
        Date = seq(max(stock_data$Date) + 1, by = 1, length.out = daysToAdd),
        Stock = rep(stock, daysToAdd),
        Close_lag = rep(tail(stock_data$`Close*`, daysToAdd), daysToAdd)
      )
      
      #generalizedBoostedRegression
      #https://cran.r-project.org/web/packages/gbm/gbm.pdf
      lm_model =gbm(`Close*` ~ Close_lag, data = stock_data)
      
      # Predictions
      predictions = predict(lm_model, newdata = predictedData)
      
      # Combine original data with predictions, date, stock, and value
      results_df = data.frame(
        Date = predictedData$Date,
        Stock = predictedData$Stock,
        Predicted = predictions
      )
      
      # Calculate R-squared
      rsquared = summary(lm_model)$r.squared
      print(summary(lm_model))
      # Store results for the current stock
      results_list[[stock]] = list(model = lm_model, predictions = results_df, rsquared = rsquared)
      
    }
    
    return(results_list)
  }
  
})

createTable = function(df) {
  stockNames= unique(df$Stock)
  
  # Reshape the data using spread, brings the rows to the columns by stock and Close values
  df2= spread(df, key = Stock, value = `Close*`)
  df2 = df2 %>% select(-Open, -High, -Low, -`Adj Close**`, -Volume)
  
  # Group by Date and summarize to handle repeated dates
  df2 = df2 %>%
    group_by(Date) %>%
    summarise_all(function(x) ifelse(all(is.na(x)), NA, na.omit(x)[1]))
  
  # Print correlation between specific stocks (e.g., msft and tsla)
  #print(cor(df2[, stockNames], use = "complete.obs"))
  
  # Create a correlation matrix for all stocks
  correlationMatrix = cor(df2[, stockNames], use = "complete.obs")
  
  print(correlationMatrix)
  
  corrplot.mixed(correlationMatrix, lower = 'shade', upper = 'pie', order = 'hclust')
  return(correlationMatrix)
                 #title = paste("Correlation between", paste(stockNames, collapse = ", ")))
}




#model predictions

  

  
#get user input
# item = userInput()
# #scrape data/web crawler
# scrappedData = scrapper(item$items, item$timeSet)
# df = bind_rows(scrappedData, .id = "Stock")
# df = as.data.frame(df)
# df = cleanData(df)
# print(df)
# graph(df,"Close*")
# treeMap(df,"Close*")
# histogram(df, "Close*")
# violinPlot(df,"Close*")
# #make a map specifically for financial data?
# candleStick(df,"bollingerbands")
# createTable(df)
}
#scrape financial data from Yahoo Finance



shinyApp(ui, server)