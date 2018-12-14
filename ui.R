#install.packages("semantic.dashboard")
#install.packages(("DT"))
library(semantic.dashboard)
library(shiny)
library(ggplot2)
library(shinyjqui)
library(highcharter)
library(plotly)
library(DT)
library(shinyjs)
library(dygraphs)
library(normtest)
library(fitdistrplus)
#install.packages("rsconnect")
library(rsconnect)




####################################
#####General Layout and Sidebar#####
####################################

dashboardPage(
  title = "StockStats",
  
  #defining the dashboard header
  dashboardHeader(
                  menuItem("GitHub", href = "https://github.com/lksfr"),
                  menuItem("LinkedIn", href = "https://www.linkedin.com/in/lukas-k-frei/")),
  
  #defining the menu items
  dashboardSidebar( title="StockStats",
    size = "thin", color = "teal", side = "left", visible = F,
    
    sidebarMenu(
      menuItem(tabName = "intro", "Introduction", icon = icon("info")),
      menuItem(tabName = "distr", "Dashboard", icon = icon("chart line")),
      menuItem(tabName = "data", "Data", icon = icon("table")),
      menuItem(tabName = "further", "Future", icon = icon("calendar")),
      menuItem(tabName = "about", "About", icon = icon("id badge"))

              )
),

dashboardBody(
  tabItems(
    selected = 1,
    tabItem(
        
        ####################################
        #####Intro Tab######################
        ####################################
        
        tabName = "intro",
        fluidRow(),
        fluidRow(
         
          #setting the logo
          tags$h1(class="ui center aligned icon header",
                 tags$i(class="circular chart area icon", style = "color:#133c7c;"),
                  "StockStats", style = "color:#133c7c;")
                  
          ),
        
          #first text segment
          fluidRow(h2("What is StockStats?")),
          
                            
         fluidRow(
          tags$div(class="ui blue segment", style="font-size:105%;" ,
                   tags$strong("StockStats provides you with statistical analyses of stock market data.
                    While doing so, StockStats is mainly concerned with identifying the most appropriate distribution 
                    for the specific stock in question, enabling you to quickly gain an insight into the
                    statistic properties of said stock. Additional background information:")
                   )
          ),
        
        #list of explanatory website links
        fluidRow(
        withTags(
           div(class = "ui relaxed divided list",
               div(class = "item",
                   i(class = "chart bar icon",
                     style="height: 24px"),
                   div(class = "content",
                       a(class = "header", 
                         href = "https://towardsdatascience.com/intro-to-descriptive-statistics-252e9c464ac9",
                         target="_blank",
                         style="font-size:110%;" ,
                         "Statistical Measures"),
                       div(class = "description", style="font-size:100%;" ,
                           "Expected Value, Variance, Skewness, Kurtosis")
                       )
                   ),
               
               div(class = "item",
                   i(class = "chart bar icon"),
                   div(class = "content",
                       a(class = "header",
                         href="https://en.wikipedia.org/wiki/Jarque???Bera_test",
                         target="_blank",
                         style="font-size:110%;" ,
                         "Adj. Jarque-Bera Test"),
                       div(class = "description", style="font-size:100%;" ,
                           "Testing Normality")
                       )
                   ),
               
               div(class = "item",
                   i(class = "chart bar icon"),
                   div(class = "content",
                       a(class = "header",
                         href="https://en.wikipedia.org/wiki/Autocorrelation",
                         target="_blank",
                         style="font-size:110%;" ,
                         "Autocorrelation"),
                       div(class = "description", style="font-size:100%;" ,
                           "ACF of Log Returns and Squared Log Returns provided")
                       )
                   ),
               
               div(class = "item",
                   i(class = "chart bar icon"),
                   div(class = "content",
                       a(class = "header",
                         href="https://cran.r-project.org/web/packages/fitdistrplus/fitdistrplus.pdf",
                         target="_blank",
                         style="font-size:110%;" ,
                         "Cullen and Frey Graph"),
                       div(class = "description", style="font-size:100%;" ,
                           "More info on page 17 of the fitdistrplus documentation")
                       )
                   )

           )
            
        )


),
        fluidRow(),

         fluidRow( h2("Why StockStats?")),

         #second text segment
         fluidRow(
          tags$div(class="ui blue segment", style="font-size:105%;" ,
                   tags$strong("I have always been fascinated by the idea of statistically analyzing stocks in order to 
                    gain insights you would not get otherwise. Thus, building a Shiny app that incorporates these features
                    hopefully not only benefits myself, but also others.")
                   )
          )

     ),

      tabItem(

        ####################################
        #####Dashboard Tab##################
        ####################################
        
        tabName = "distr",
        fluidRow(
          withTags(
            h1("Interactive Dashboard")
          ),
          
          #introduction to dashboard
          fluidRow(
            p("Instructions: First, enter a Stock Symbol (Yahoo Finance data). Then, set a date range (year-month-day) below.
              Now, check all boxes of plots and statistics you'd like to see."),
            
            p("Customize your dashboard by adjusting the size of the displayed plots as well as by dragging them around.",
              a(href="https://www.marketwatch.com/tools/quotes/lookup.asp", "Stock Symbol Lookup",target="_blank")
              ),
            br()

            )

        ),
        
   
  fluidRow(
    
      #stock symbol and date range selection
      column(2,
    
      textInput("symb", "", "AAPL"),
      br(),
      br(),
  
    
      dateRangeInput("dates", 
        "From",
        start = "2013-01-01", 
        end = as.character(Sys.Date())
        )
      ),
      
      #whitespace
      column(1),
   
    #overview graphs
      column(3, 
        h4("Stock Price Overview"),

        checkboxInput("show_graph", "Show Stock Price Plots", 
          value = FALSE),

        checkboxInput("log", "Log Scale", 
          value = FALSE),
      
        checkboxInput("adj", "Only Adjusted", 
          value = FALSE),

      #checkboxInput("locate", "Locate", 
      #    value = FALSE),
      
      br(),
      br(),
      
      #sliderinput to adjust plot size
      div(style="width:150px;height:100px",
          sliderInput("width", "Plot Width (px)", min = 0, max = 700, value = 300),
          sliderInput("height", "Plot Height (px)", min = 0, max = 700, value = 500)
      )
      
    ),

      #adjusted log returns plots
      column(3, 
        h4("Log Returns Adjusted"),

        checkboxInput("show_logs", "Show Log Line Graph", 
          value = FALSE),
      
        checkboxInput("show_box", "Show Log Box Plot", 
          value = FALSE),
      
        checkboxInput("show_hist", "Show Log Histogram", 
          value = FALSE),
      
        br(),
        br(),

        #sliderinput to adjust plot size
        div(style="width:150px;height:150px",
            sliderInput("width_log", "Plot Width (px)", min = 0, max = 700, value = 300),
            sliderInput("height_log", "Plot Height (px)", min = 0, max = 700, value = 500)
        )
      
     ),

      #ACF plots
      column(3, 
        h4("ACF"),

        checkboxInput("show_acf", "Show ACF", 
          value = FALSE),
        checkboxInput("show_sq_acf", "Show Squared ACF", 
          value = FALSE),
      
        br(),
        br(),
        br(),

        #slider inputs to adjust plot size
        div(style="width:150px;height:150px",
          sliderInput("width_acf", "Plot Width (px)", min = 0, max = 700, value = 300),
          sliderInput("height_acf", "Plot Height (px)", min = 0, max = 700, value = 500)
        )
      
      ),
      
      #statistical properties & cullen and frey graph
      column(3, 
        h4("Statistical Measures"),

        checkboxInput("show_skew", "Skewness of Log Returns", 
          value = FALSE),
        checkboxInput("show_kurtosis", "Kurtosis of Log Returns", 
          value = FALSE),
      #  checkboxInput("show_ajb", "Adj. Jarque-Bera of Log Returns", 
      #    value = FALSE),
        checkboxInput("show_fit", "Cullen & Frey Graph", 
          value = FALSE),
      
      br(),
      br(),
      
        
        
        #slider inputs to adjust plot size
        div(style="width:150px;height:150px",
          sliderInput("width_fit", "Plot Width (px)", min = 0, max = 700, value = 300),
          sliderInput("height_fit", "Plot Height (px)", min = 0, max = 700, value = 500)
        )

      )

   ),
  
  
    #Plot outputs
    fluidRow(
      
      #making plots draggable 
      jqui_draggable(
        
        #only show plots if checkbox checked
        conditionalPanel(condition = "input.show_graph",
          div(
          uiOutput("plot.ui", click = "plot_click")
          #verbatimTextOutput("info") potential future enhancement of making standard plot interactive

              )
          )
        ),
      
      #show log line plot
      jqui_draggable(
        conditionalPanel(condition = "input.show_logs",
          uiOutput("plotly.ui")

              )
        ),

      #show log box plot
      jqui_draggable(
        conditionalPanel(condition = "input.show_box",
          uiOutput("log_box_ui")

              )
        ),

      #show log histogram
      jqui_draggable(
        conditionalPanel(condition = "input.show_hist",
          uiOutput("log_hist_ui")

              )
        ),
      
      #show acf plot of log returns
      jqui_draggable(
        conditionalPanel(condition = "input.show_acf",
          uiOutput("acf_ui")

              )
        ),
      
      #show acf plot of squared log returns
      jqui_draggable(
        conditionalPanel(condition = "input.show_sq_acf",
          uiOutput("sq_acf_ui")

              )
        ),
      
      #show skewness value box
      jqui_draggable(
        conditionalPanel(condition = "input.show_skew",
          valueBoxOutput("skewness")

              )
        ),
      
      #show kurtosis value box
      jqui_draggable(
        conditionalPanel(condition = "input.show_kurtosis",
          valueBoxOutput("kurtosis")

              )
        ),
      
      #show adj. jarque-bera result value box
      jqui_draggable(
        conditionalPanel(condition = "input.show_ajb",
          valueBoxOutput("ajb")

              )
        ),
      
     #show cullen & frey graph   
     jqui_draggable(
        conditionalPanel(condition = "input.show_fit",
          uiOutput("fit_ui")

              )
        )

    ),

  #Screenshow of dashboard action button
  fluidRow(div(class="ui segment",
    actionButton("btn", "Dashboard Screenshot Selector"),
    p("Will turn cursor into screenshot selector: drag window of dashboard you are interested in capturing.
      Will save a JPEG to your desktop. Currently only works in local version on Mac, not shinyapps.io.")
    
              )
            )
  
  ),

      ####################################
      #####Data Tab#######################
      ####################################

      tabItem(
        
        fluidRow(
        h1("Data")),
        tabName = "data",
        
        #outputting the dataframe with used values
        fluidRow(
         dataTableOutput("tbl")
        ),
        
        fluidRow(
          downloadButton("downloadData", "Download")
          )
      ),


      ####################################################
      #####Future of StockStats Tab#######################
      ####################################################

      tabItem(
        tabName = "further",
        fluidRow(
        withTags(
          h1("What's Next for StockStats?")
          ),
        
        #whitespace
        br()

        ),
        
        #ui segment showing the progress of working on StockStats
        fluidRow(
          withTags(
            div(class = "ui ordered steps",
              div(class = "completed step",
                div(class = "content",
                div(class = "title",
                          "Make StockStats work"),
                div(class = "description",
                          "Making StockStats work reliably"))),
              
              div(class = "active step",
                div(class = "content",
                div(class = "title",
                          "Improving StockStats' Interface"),
                div(class = "description",
                          "Improving the UX of StockStats"))),
              
              div(class = "disabled step",
                div(class = "content",
                div(class = "title",
                            "Expand Analysis"),
                div(class = "description",
                            "Adding more refined and specific analysis tools")))
              )
            )
          ),
        
        #descriptive ui segments
        fluidRow(),
        
        fluidRow(
          withTags(
            div(class = "ui compact segments",
              div(class = "ui green segment", br(), style="font-weight:bold;",
                p("Firstly, StockStats should reliably work. In its current implementation, 
                  I have used the quantmod package to retrieve Yahoo Finance data."),
                br()
              ),
              
            div(class = "ui blue segment",br(),style="font-weight:bold;",
              p("Next, improving the UX of StockStats will make its use more intuitive. 
                This step mainly focuses on the dashboard. "),
              br()
            ),
            
            div(class = "ui grey segment",br(),style="font-weight:bold;",
              p("Thirdly, the current analysis could be expanded, both regarding the statistical analysis as well as forecasting features."),
              br()
            )
          )
        )
      )
  ),


      #####################################
      #####About Tab#######################
      #####################################

      tabItem(
        tabName = "about",
        
        sidebarLayout(
          #profile
          column(width=5,
             sidebarPanel(
                withTags(
                  div(class = "ui card",
                    div(class = "image",
                      img(src = "Pic.jpg")),
                    div(class = "content",
                      a(class = "header",
                      "Lukas"),
                    div(class = "meta",
                      span(class = "date",
                      "From Mannheim, Germany"),
                      i(class="de flag")),
                   div(class = "description",
                      "Lukas is a data enthusiast currently living in New York City.")),
                  div(class = "extra content",
                      a(i(class = "graduation cap icon"),
                      "BS in Business Administration")))))
          ),
          
           #motivation and short bio
           column(width=10,
                  
           mainPanel(
              div(class = "ui segments",
                  style="padding-left: 50px;",
                  
               div(class = "ui segment", 
                   br(),
                   h4("Motivation", style="font-weight:bold;"), 
                   p(style="line-height:170%;",

                            "As a Business Administration graduate of the University of Mannheim, Germany, I have always been
                            very interested in decision-making, especially in finance. Combining finance with data science and statitsics
                            seemed like the logical thing to do for me. Therefore, I created the StockStats Shiny app that is supposed to
                            enable like-minded data and finance enthusiasts to quickly gain insights into the staistical properties of a
                            given stock in question.", br(), br(), "StockStats provides a fast but sophisticated overview of stocks not available
                            on finance platforms, which may be used for prediction or other areas of decision-making in finance. In the future,
                            implementing these capabilities into StockStats would significantly increase its value."),
                   
                   br()),
               div(class = "ui segment", 
                   br(),
                   h4("About Me", 
                   style="font-weight:bold;"), 
                   p(style="line-height:170%;",
                     
                            "My fascination with decision-making in all its forms lead me to pursue a BS in Business Administration at the University of Mannheim,", a(href="https://www.timeshighereducation.com/world-university-rankings/2018/subject-ranking/business-and-economics#!/page/0/length/25/sort_by/rank/sort_order/asc/cols/stats", "consistently ranked the No. 1 Business School in Germany and Top 20 worldwide", target="_blank"), ".",
                            br(),
                            br(),
                            "During my semester abroad at Fudan University in Shanghai, China, I attended several lectures introducing AI, Data Science, and Machine Learning.
                            I became fascinated, thus I started learning some basic HTML. Soon after, I started searching for ways to effectively pursue a career
                            combining what I had learned before in Business and Finance with Data Science, which led me to apply to the New York City Data Science Academy.", br(),br(),
                             " Currently, I am enrolled in the 12 Week Data Science Bootcamp at NYCDSA, learning Python, R, and a variety of relevant packages and modules."),
                          br()
                   )
               )
              )
           )
        )
      )
    )
  ), theme = "simplex"
)