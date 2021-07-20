library(shiny)
library(ggplot2)

ui <- fluidPage(
    
    
    
    titlePanel("Old Faithful Geyser Data"),
    
    
    sidebarLayout(
        sidebarPanel(
            actionButton("go", "Go"),
            
            
            
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            
            tags$hr(),
            
            
            checkboxInput("header", "Header", TRUE),
            
            
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            
            tags$hr(),
            
            
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
        ),
        
        
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("lmPlot"),
            textOutput("model"),
            tableOutput("contents")
            
        )
    )
)


server <- function(input, output) {
    
    ggplotRegression <- function (fit) {
        
        require(ggplot2)
        
        ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
            geom_point() +
            stat_smooth(method = "lm", col = "red") +
            labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                               "Intercept =",signif(fit$coef[[1]],5 ),
                               " Slope =",signif(fit$coef[[2]], 5),
                               " P =",signif(summary(fit)$coef[2,4], 5)))
    }
    
    
    
    
    
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    
    
    
    fit <- eventReactive(input$go, {
        lm(y~x, data=dataInput())})
    
    
    
    output$distPlot <- renderPlot({
        ggplot(data=dataInput(), aes(x=x, y=y))+geom_point()
    })
    
    output$lmPlot <- renderPlot({
        ggplotRegression(fit())
    })
    
    output$model <- renderPrint({
        paste("Adj R2 = ",signif(summary(fit())$adj.r.squared, 5),
              "Intercept =",signif(fit()$coef[[1]],5 ),
              " Slope =",signif(fit()$coef[[2]], 5),
              " P =",signif(summary(fit())$coef[2,4], 5))
        
    })
    
    
    
    output$contents <- renderTable({
        
        
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
    
}


shinyApp(ui = ui, server = server)