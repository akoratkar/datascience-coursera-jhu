#
# This is the server logic definition of a Shiny web application for an analysis of
# mileage (mpg) for automatic and manual transmission cars. Employing regression modeling, it
# further explores the relationship between fuel efficiency and a set of variables in mtcars..
# This is shinification of the project I did for the project for the Regression Modeling Course.
#

library(shiny)

# Define server logic for application that lets the choose the regression model and then presents the
# the results of the same.
shinyServer(function(input, output) {

  # First display the summary of the relationships between the various variables. It helps
  # confirm that we have wt, cyl, disp with p-value < 0.05 and hence significant 
  output$allsummary<-renderPrint({summary(aov(mpg~., data=mtcars))})

  modelchosen<-reactive({input$modeltype})

  # Run the regression based on the chosen model
  model<-reactive({

    modeltype<-modelchosen()

    if (modeltype=="lm") model<-lm(mpg~am, data=mtcars)
    if (modeltype=="lm_wt_am") model<-lm(mpg~wt+am, data=mtcars)
    if (modeltype=="lm_disp_am") model<-lm(mpg~disp+am, data=mtcars)
    if (modeltype=="lm_cyl_am") model<-lm(mpg~cyl+am, data=mtcars)
    if (modeltype=="lm_wt_cyl_am") model<-lm(mpg~wt+cyl+am, data=mtcars)
    model

  })

  # This outputs the summary of the executed model
  output$summary<-renderPrint({summary(model())})
  
  # Base model to compare the chosen model against
  basemodel<-reactive({
    
    modeltype<-modelchosen()
    if (modeltype=="lm") basemodel<-lm(mpg~am, data=mtcars)
    if (modeltype=="lm_wt_am") basemodel<-lm(mpg~wt, data=mtcars)
    if (modeltype=="lm_disp_am") basemodel<-lm(mpg~disp, data=mtcars)
    if (modeltype=="lm_cyl_am") basemodel<-lm(mpg~cyl, data=mtcars)
    if (modeltype=="lm_wt_cyl_am") basemodel<-NULL
    basemodel
    
  })
  
  # Output the variance table for the nested models
  # Nested Regression Model. R-Squared indicates that a high 83.0338317 % regression variance 
  # is explained by the model. We conclude wt and cyl are significant contributors to mpg
  # and lm(mpg~wt+cyl+am, data=mtcars) is the most optimal model.
  output$anova<-renderPrint({
      if (!is.null(basemodel())){
        suppressWarnings(anova(basemodel(), model(), data=mtcars))
      }
      else{
        suppressWarnings(anova(lm(mpg~wt, data=mtcars), lm(mpg~wt+cyl, data=mtcars), lm(mpg~wt+cyl+disp, data=mtcars), model(), data=mtcars))
      }
    
  })
  
  # Plot 1: Full Plot of the chosen model
  output$modelplot <- renderPlot({

    par(mfrow = c(2, 2))
    plot(model(), main = paste("Plot 1: ", modelchosen()))

  })


  
  # Plot 3:  Pairwise graph for potentially significant variables
  # This depicts the high level relationship among the variables
  output$pairwiseplot <- renderPlot({

    mtcars_wca <- mtcars[, c(1, 2, 6, 9)]
    pairs(mtcars_wca, panel=panel.smooth,col=11 + mtcars_wca$w, main="Plot 2: Pairwise plot for mpg, cyl, wt, am")
  })

  # Plot 4: MPG vs Transmission Type
  # Primary relationship being investigated
  output$mpgvsamplot <- renderPlot({

    suppressWarnings(boxplot(mpg ~ am, data = mtcars, main="Plot 3: MPG vs Transmission Type", xlab = "Transmission Type", ylab="MPG", notch=TRUE, col=c("lightblue","lightgreen")))

  })


})
