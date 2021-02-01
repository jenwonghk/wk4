library(shiny); library(ggplot2); library(dplyr); library(caret); library(gbm); library(e1071)

ui <- fluidPage(
  titlePanel("Use Hair & Eye Color To Predict Gender (Have Fun!)"), 
  tags$br(),
  sidebarLayout(
    sidebarPanel(width=3,
                 
                 selectInput("hair", "Select Hair Color:", c("Black"="Black",
                                                             "Blond"="Blond",
                                                             "Brown"="Brown",
                                                             "Others"="Others")),
                 
                 selectInput("eye", "Select Eye Color:", c("Eye-Blue"="Eye-Blue",
                                                           "Eye-Brown"="Eye-Brown",
                                                           "Eye-Others"="Eye-Others")),
                 
                 actionButton("do1", "Submit")
    ),
    
    mainPanel(width=9,
              tabsetPanel(
                
                tabPanel("Documentation",
                         
                         h4("Purpose:"),
                         h5("Use hair color and eye color to predict the person is a Male or a Female."),
                         tags$br(),
                         
                         h4("How to use this app?"),
                         tags$b("Steps in using this app for prediction:"),
                         tags$br(),
                         
                         tags$ol(
                           tags$li("Input: On the left panel"),
                           tags$ul(
                             tags$li("Select Hair Color"),
                             tags$li("Select Eye Color"),
                             tags$li("Click Submit"),
                             tags$br()
                           ),
                           tags$li("Result: On main panel page 'Predicted Result & Model Used'"),
                           tags$ul(
                             tags$li(" Wait about 30 seconds for the model training"),
                             tags$br(),
                             tags$li("On section: 'Predicted Gender & Probability'"),
                             h6("e.g.  [1] Male (this is the predicted gender)"),
                             h6("The decimal below is its probability"),
                             tags$br(),
                             
                             tags$li("On section: 'Confusion Matrix of the Model used'"),
                             h6("You can see how accurate the model is."),
                             h6("The accuracy (Sensitivity = 0.8478) in predicting 'Female' is higher than 
                                'Male' (Specificity = 0.3902)"),
                             
                             h6("In the future, we look forward to have more data so that the prediction accuracy rate
                                will be lifted up.")
                             
                             
                             )
                           )
                         
                ),
                
                tabPanel("Predicted Result & Model Used",
                         tags$br(),
                         
                         h4("Predicted Gender and Probability"),
                         verbatimTextOutput("result"),
                         textOutput("prob"),
                         tags$br(),
                         
                         h4("Confusion Matrix of the Model Used"),
                         verbatimTextOutput("text2")
                ),
                
                
                tabPanel("Dataset being used", 
                         
                         h4("Browse 1st three rows of data"),
                         tableOutput("contents"), 
                         tags$br(),
                         
                         h4("Dimension of data"),
                         verbatimTextOutput("size"),
                         tags$br(),
                         
                         h4("Distribution of the data"),
                         plotOutput("graph1")  #qplot
                ), 
                
                
                tabPanel("Contact",
                         tags$br(),
                         h5("Any queries"),
                         h5("jenwonghk@gmail.com")
                )
                
              )
                )
  )
  )

server <- function(input, output, session) {
  
  df <- eventReactive(input$do1, {
    
    dat <- read.csv("HairEyeColor.csv", header=TRUE)
    
    dat_disagg = dat %>% 
      slice(rep(1:n(), Freq)) %>% 
      select(-Freq)
    
    set.seed(333)
    shuffle = sample(nrow(dat_disagg))
    dat_disagg1 <- dat_disagg[shuffle, ]
    
    levels(dat_disagg1$Hair)[levels(dat_disagg1$Hair)=="Red"] <- "Others"
    levels(dat_disagg1$Eye)[levels(dat_disagg1$Eye)=="Green"] <- "Eye-Others"
    levels(dat_disagg1$Eye)[levels(dat_disagg1$Eye)=="Hazel"] <- "Eye-Others"
    levels(dat_disagg1$Eye)[levels(dat_disagg1$Eye)=="Blue"] <- "Eye-Blue"
    levels(dat_disagg1$Eye)[levels(dat_disagg1$Eye)=="Brown"] <- "Eye-Brown"
    
    dat_disagg1$Hair <- as.factor(dat_disagg1$Hair)
    dat_disagg1$Eye <- as.factor(dat_disagg1$Eye)
    
    dat_disagg1 <- as.data.frame(dat_disagg1)
    
  })
  
  
  output$contents <- renderTable({
    head(df(),3)
  })
  
  output$size <- renderPrint({dim(df())})
  
  output$graph1 <- renderPlot({
    qplot(Hair, data=df(), color=Hair, fill=Hair, facets=Sex~Eye)
  })
  
  x <- reactive({
    
    withProgress(message = "Model is Training", value = 1.0, {
      
      set.seed(333)
      inTrain = createDataPartition(df()$Sex, p=0.85)[[1]]
      training = df()[inTrain, ]
      testing = df()[-inTrain, ]
      
      ctrl <- trainControl(method = "boot")
      set.seed(333)
      modFit <- train(Sex ~ ., 
                      data = training,
                      method = "gbm",
                      trControl = ctrl,
                      verbose=FALSE)
      pred <- predict(modFit, testing[, -3])
      return(list(CM=confusionMatrix(pred, testing[,3]), modFit=modFit))
    })
  })
  
  output$text2 <- renderPrint({x()[[1]]})
  
  x1 <- reactive({
    testing1 = data.frame(Hair=input$hair, Eye=input$eye)
    return(list(prob=(predict(x()[[2]], testing1, type="prob")),
                result=predict(x()[[2]], testing1)))
  })
  
  output$result <- renderPrint({x1()[[2]]})
  output$prob <- renderText({max(x1()[[1]])})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

