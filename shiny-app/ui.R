shinyUI(
    fluidPage(
        div(class="header clearfix", style="padding-bottom: 5px; border-bottom: 1px solid #E5E5E5;",
            h3("Data science capstone project - Predicting next word",class="text-muted")
        ),
        br(),
        br(),
        
        fluidRow(
            column(4, 
                div(class="panel panel-warning",
                    div(class="panel-heading",
                        h3(class="panel-title","Instructions")
                    ),
                    div(class="panel-body",
                        p("As you type some text in the input field, the system will show some probable next words."),
                        p("Then, you can click on the word and the system will add this word to the text.")
                    )
                )
            ),
            column(6,
                   div(class="panel panel-info",
                       div(class="panel-heading",
                           h3(class="panel-title","The running app")
                       ),
                       div(class="panel-body",
                           #p("Enter the text"),
                           tags$textarea(id = "text", 
                                     placeholder = "Enter text...",
                                     style = "width: 100%;",
                                     class="form-control",
                                     ""
                                     ),
                           hr(),
                           p("Choose the next word:", style = "color:#888888;"),
                           uiOutput("wordsControls")
                           
                   )
                )
            )
        )
        
    )
)