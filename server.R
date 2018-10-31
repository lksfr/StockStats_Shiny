library(shiny)
library(semantic.dashboard)
library(ggfortify)
library(ggplot2)
library(shinyjqui)
library(highcharter)
library(plotly)
library(DT)
library(xts)
library(dplyr)
library(dygraphs)
library(normtest)
library(cluster)
library(GLDEX)
library(quantmod)
library(fitdistrplus)


  
  shinyServer(function(input, output) {
    
    #retrieving stock data from yahoo using the quantmod package
    dataInput <- reactive({
      getSymbols(input$symb, src = "yahoo", 
                 from = input$dates[1],
                 to = input$dates[2],
                 auto.assign = FALSE)
    })
    
    ##############making plots resizable #########################
    output$plot.ui <- renderUI({
            plotOutput("plot", width = paste0(input$width, "px"), height = input$height)
        })

     output$plotly.ui <- renderUI({
            plotlyOutput("plot2", width = paste0(input$width_log, "px"), height = input$height_log)
        })

     output$log_box_ui <- renderUI({
            plotlyOutput("log_box", width = paste0(input$width_log, "px"), height = input$height_log)
        })

     output$log_hist_ui <- renderUI({
            plotlyOutput("log_hist", width = paste0(input$width_log, "px"), height = input$height_log)
        })

     output$acf_ui <- renderUI({
            plotOutput("acf", width = paste0(input$width_acf, "px"), height = input$height_acf)
        })

     output$sq_acf_ui <- renderUI({
            plotOutput("sq_acf", width = paste0(input$width_acf, "px"), height = input$height_acf)
        })

     output$fit_ui <- renderUI({
            plotOutput("fit", width = paste0(input$width_fit, "px"), height = input$height_fit)
        })

     ##############making plots resizable #########################
     
     #overview plots
    output$plot <- renderPlot({ 

      zoo.data <- as.zoo(dataInput())

      cols = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
      colnames(zoo.data) = cols

      if (input$log) zoo.data[,cols] = log(zoo.data[,cols])

      if(input$adj) {

      	zoo.data = zoo.data[,6]

      	#potential future addition
      	#if(input$locate == T)  {
     		#output$info <- renderText({
    		#paste0("stock price=", input$plot_click$y, "\ndays since start=", input$plot_click$x)
  	#	})
     #}
      }
      
      width=1000
     plot(x=zoo.data, main="Stock Data", xlab="Date", col="#2f538c")

    }) 

    #first log return plot
    output$plot2 <- renderPlotly({ 

    	log_returns = as.data.frame(dataInput())
    	
    	cols = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    	colnames(log_returns) = cols

    	log_returns <- cbind(Date = rownames(log_returns), log_returns)

    	logs = as.data.frame(diff(log(log_returns[,6])))

    	log_returns = cbind(log_returns$Date[2:nrow(log_returns)],logs)
    	colnames(log_returns) = c("date", "log_return")
    	log_returns[,1] = as.Date(log_returns[,1])

    	plot_ly(log_returns, x = ~date, y = ~log_return, type = 'scatter', mode = 'lines',
        hoverinfo = 'text',
        text = ~paste('Log Return: ', log_return,
        				"</br>",
                      '</br> Date: ', date)) %>% layout(autosize = F, width = input$width_log, height = input$height_log)




    }) 

    #log return boxplot
    output$log_box = renderPlotly({

    	log_returns = as.data.frame(dataInput())
    
    	cols = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    	colnames(log_returns) = cols

    	log_returns <- cbind(Date = rownames(log_returns), log_returns)

    	logs = as.data.frame(diff(log(log_returns[,6])))

    	log_returns = cbind(log_returns$Date[2:nrow(log_returns)],logs)
    	colnames(log_returns) = c("date", "log_return")
    	log_returns[,1] = as.Date(log_returns[,1])

    	 plot_ly(log_returns,
                y = ~log_return,
                type = 'violin',
                box = list(
                visible = T
              ),
         meanline = list(
        visible = T
      ),
      x0 = 'Stock'
    ) %>% 
    layout(width = input$width_log, height = input$height_log,
      yaxis = list(
        title = "log return",
        zeroline = F
      ),
      title="Boxplot of log returns"
    )

  })

    #log return histogram
    output$log_hist = renderPlotly({

    	log_returns = as.data.frame(dataInput())
    	

    	cols = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    	colnames(log_returns) = cols

    	log_returns <- cbind(Date = rownames(log_returns), log_returns)


    	logs = as.data.frame(diff(log(log_returns[,6])))

    	log_returns = cbind(log_returns$Date[2:nrow(log_returns)],logs)
    	colnames(log_returns) = c("date", "log_return")
    	log_returns[,1] = as.Date(log_returns[,1])

    	plot_ly(log_returns, x= ~log_return, type = "histogram",
             histnorm = "probability") %>% 
    	layout(width = input$width_log, height = input$height_log,
      	yaxis = list(
        title = "density",
        zeroline = F
      	),
      	xaxis = list(
        title = "Log Return",
        zeroline = F
      	),
      	title="Histogram of log returns"
    	) 




    	})

    #acf plot log returns
     output$acf = renderPlot({

     	log_returns = as.data.frame(dataInput())
    	
     	cols = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    	colnames(log_returns) = cols

    	log_returns <- cbind(Date = rownames(log_returns), log_returns)

     	logs = as.data.frame(diff(log(log_returns[,6])))

     	log_returns = cbind(log_returns$Date[2:nrow(log_returns)],logs)
     	colnames(log_returns) = c("date", "log_return")
     	log_returns[,1] = as.Date(log_returns[,1])
     	

     	acf(log_returns$log_return, main="Log Returns ACF")
		
	
    	})

     #acf plot squared log returns
     output$sq_acf = renderPlot({

     	log_returns = as.data.frame(dataInput())
    	
     	cols = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    	colnames(log_returns) = cols

    	log_returns <- cbind(Date = rownames(log_returns), log_returns)

     	logs = as.data.frame(diff(log(log_returns[,6])))

     	log_returns = cbind(log_returns$Date[2:nrow(log_returns)],logs)
     	colnames(log_returns) = c("date", "log_return")
     	log_returns[,1] = as.Date(log_returns[,1])
     	log_returns[,2] = log_returns[,2]^2

     	acf(log_returns$log_return, main="Squared Log Returns ACF")
		
	
    	})

     #skewness value box
     output$skewness <- renderValueBox({

     	log_returns = as.data.frame(dataInput())
    	
     	cols = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    	colnames(log_returns) = cols

    	log_returns <- cbind(Date = rownames(log_returns), log_returns)

     	logs = as.data.frame(diff(log(log_returns[,6])))

     	log_returns = cbind(log_returns$Date[2:nrow(log_returns)],logs)
     	colnames(log_returns) = c("date", "log_return")
     	log_returns[,1] = as.Date(log_returns[,1])
     	

     	 skew = skewness(log_returns$log_return)

      valueBox(
        subtitle = "Skewness",
        value = round(skew,2),
        icon("zoom-in icon"),
        width = 5,
        size = "small"
        
      )
      
    })
     
     #kurtosis value box
     output$kurtosis <- renderValueBox({

     	log_returns = as.data.frame(dataInput())
    	

     	cols = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    	colnames(log_returns) = cols

    	log_returns <- cbind(Date = rownames(log_returns), log_returns)


     	logs = as.data.frame(diff(log(log_returns[,6])))

     	log_returns = cbind(log_returns$Date[2:nrow(log_returns)],logs)
     	colnames(log_returns) = c("date", "log_return")
     	log_returns[,1] = as.Date(log_returns[,1])
     	

     	 kurtosis = kurtosis(log_returns$log_return)

      valueBox(
        subtitle = "Kurtosis",
        value = round(kurtosis,2),
        icon("zoom-in icon"),
        width = 5,
        size = "small"
        
      )
      
    })

     #ajb value box
     output$ajb <- renderValueBox({

     	log_returns = as.data.frame(dataInput())
    	
     	cols = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    	colnames(log_returns) = cols

    	log_returns <- cbind(Date = rownames(log_returns), log_returns)

     	logs = as.data.frame(diff(log(log_returns[,6])))

     	log_returns = cbind(log_returns$Date[2:nrow(log_returns)],logs)
     	colnames(log_returns) = c("date", "log_return")
     	log_returns[,1] = as.Date(log_returns[,1])
     	

     	 ajb <- as.numeric(ajb.norm.test(log_returns$log_return)$statistic)

      valueBox(
        subtitle = "Adj. Jarque-Bera",
        value = round(ajb,2),
        icon("zoom-in icon"),
        width = 5,
        size = "small"
        
      )
      
    })

     #Cullen_Frey graph
     output$fit <- renderPlot({

     	log_returns = as.data.frame(dataInput())
    	
     	cols = c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    	colnames(log_returns) = cols

    	log_returns <- cbind(Date = rownames(log_returns), log_returns)

     	logs = as.data.frame(diff(log(log_returns[,6])))

     	log_returns = cbind(log_returns$Date[2:nrow(log_returns)],logs)
     	colnames(log_returns) = c("date", "log_return")
     	log_returns[,1] = as.Date(log_returns[,1])
     	
     	
     	 descdist(log_returns$log_return, discrete = F, boot=100)
     	 
        
      
      
    })

      #screen capture
     observeEvent(input$btn, {
       system("screencapture -i ~/Desktop/dashboard.jpg")
     })

    output$tbl <- renderDataTable(as.data.frame(dataInput())) 
    
#downlaod button commented out for now
    
  #   output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste(input$symb, ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(dataInput(), file, row.names = FALSE)
  #   }
  # )
     
})

