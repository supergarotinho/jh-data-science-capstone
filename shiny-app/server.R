library(shiny)
source("predict-model-class.R")
sampleSize <- 0.01
knnModel <- readRDS(file = paste0("data/final-model-",sampleSize,".rds"))

## Executes when load to server

shinyServer(function(input, output, clientData, session) {
    
    ## Executes for each session
    predictedWords <<- c()

    output$wordsControls <- renderUI({
        ## Predict
        text <- input$text
        predictedWords <<- predictNextWord(theObject = knnModel,text = text,5)
        buttonNames <- c("one","two","three","four","five")
        visibility <- lapply(rep(1:5), function (i) if (length(predictedWords) >= i) "" else "display: none;")
        
        div(
            actionButton("one", predictedWords[1], style=visibility[1]), 
            actionButton("two", predictedWords[2], style=visibility[2]),
            actionButton("three", predictedWords[3], style=visibility[3]),
            actionButton("four", predictedWords[4], style=visibility[4]),
            actionButton("five", predictedWords[5], style=visibility[5])
        )
    })
    
    observeEvent(input$one, {
        updateTextInput(session, "text",
                        value = paste(input$text,predictedWords[1])
        )
    })
    
    observeEvent(input$two, {
        updateTextInput(session, "text",
                        value = paste(input$text,predictedWords[2])
        )
    })
    
    observeEvent(input$three, {
        updateTextInput(session, "text",
                        value = paste(input$text,predictedWords[3])
        )
    })
    
    observeEvent(input$four, {
        updateTextInput(session, "text",
                        value = paste(input$text,predictedWords[4])
        )
    })
    
    observeEvent(input$five, {
        updateTextInput(session, "text",
                        value = paste(input$text,predictedWords[5])
        )
    })
})