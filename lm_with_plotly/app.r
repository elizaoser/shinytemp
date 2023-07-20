#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("plotly", update = TRUE)

#install.packages("shiny")
#install.packages("tidyverse")
#install.packages("plotly")
#install.packages("shinyMatrix")

#devtools::install_github("ropensci/plotly")
library(shiny)
library(ggplot2)
library(plotly)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Linear Modeling Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
            
            # Checkbox to display linear model plot
            checkboxGroupInput("checkbox", "Linear Model", c("Linear Model" = "Linear Model")),
            
            radioButtons("var1", "Select the file type", 
                         choices = c("png", "pdf"), 
                         selected = "png")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("distPlot"),
           plotlyOutput("lmPlot"), 
           downloadButton("down1","Download the plot"),
           downloadButton("down2","Download the linear model plot"),
           textOutput("summary"),
           tableOutput("contents")
           
        #   downloadablePlotUI("Linear Model", 
         #          downloadtypes = c("png"), 
          #         download_hovertext = "Download the plot here!",
           #        height = "500px", 
            #       btn_halign = "left")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
     
    
#    output$distPlot <- renderPlot({
 #       ggplot(dataInput(), aes(x = dataInput()$x, y = dataInput()$y)) +
#              geom_point(colour = 'red') +
#              ggtitle('y vs x') +
#              xlab('x') +
#              ylab('y')
#    })
    
#    output$lmPlot <- renderPlot({
 #       if (length(input$checkbox) == 0) {
#            print("No Linear Model")
 #          }
  #      else {
#            ggplot(dataInput(), aes(x = dataInput()$x, y = dataInput()$y)) +
 #                 geom_point(colour = 'red') +
 #                 geom_line(aes(x = dataInput()$x, y = predict(lm(formula = y ~ x,
 #                           data = dataInput()), newdata = dataInput())),
 #                           colour = 'blue') +
  #                ggtitle('Model of y vs x') +
 #                 xlab('x') +
 #                 ylab('y')
 #       }
 #   })
    

    output$distPlot <- renderPlotly({
        plot_ly(dataInput(), x = ~x, y = ~y, type = 'scatter', mode = 'markers', opacity = 1, name = "Data")
          })

    output$lmPlot <- renderPlotly({
          if (length(input$checkbox) == 0) {
            return(NULL)  # Return NULL to avoid errors when checkbox is empty
          } else {
            plot_ly(dataInput(), x = ~x, y = ~y, type = "scatter", mode = "markers",
            opacity = 0.65, name = "Data") %>%
              add_trace(type = "scatter", mode = "lines",
                x = ~x, y = ~predict(lm(y ~ x, data = dataInput())),
                line = list(color = "darkblue"), name = "Trendline")
          }
        })

    output$down1 <- downloadHandler(
    filename =  function() {
      paste("data", input$var1, sep=".")
    },
    content = function(file) {
      if (input$var1 == "png") {
        png(file) # open the png device
      } else {
        pdf(file) # open the pdf device
      }
      print(
        plot_ly(dataInput(), x = dataInput()$x, y = dataInput()$y, type = 'scatter', mode = 'markers')
      ) # draw the plot
      dev.off()  # turn the device off
    }
  )
    
    
  output$down2 <- downloadHandler(
    filename =  function() {
      paste("data", input$var1, sep=".")
    },
    content = function(file) {
      if (input$var1 == "png") {
        png(file) # open the png device
      } else {
        pdf(file) # open the pdf device
      }
      print(
         plot_ly(dataInput(), x = dataInput()$x, y = dataInput()$y, type = 'scatter', mode = 'markers',
                      opacity = 0.65, trendline = 'ols', trendline_color_override = 'darkblue')
      ) # draw the plot
      dev.off()  # turn the device off
    }
  )
    # downloadablePlot("Linear Model",
    #             logger = ss_userAction.Log,
     #            filenameroot = "Linear_Model",
      #           aspectratio = 1.33,
       #          downloadfxns = list(png = myplotfxn),
        #         visibleplot = myplotfxn)
    
   
        output$summary <- renderPrint({
        if (length(input$checkbox) == 1) {
        print(summary(lm(formula = y ~ x,
               data = dataInput())))
         }
        else {
            print(NULL)
            }
        })
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
