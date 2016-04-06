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

  output$allsummary<-renderPrint({summary(aov(mpg~., data=mtcars))})

  modelchosen<-reactive({input$modeltype})

  model<-reactive({

    modeltype<-modelchosen()

    if (modeltype=="lm") model<-lm(mpg~am, data=mtcars)
    if (modeltype=="lm_wt_am") model<-lm(mpg~wt+am, data=mtcars)
    if (modeltype=="lm_disp_am") model<-lm(mpg~disp+am, data=mtcars)
    if (modeltype=="lm_cyl_am") model<-lm(mpg~cyl+am, data=mtcars)
    if (modeltype=="lm_wt_cyl_am") model<-lm(mpg~wt+cyl+am, data=mtcars)
    model

  })

  output$summary<-renderPrint({summary(model())})

  ##Plot 1: Full Plot of the chosen model
  output$modelplot <- renderPlot({

    par(mfrow = c(2, 2))
    plot(model(), main = paste("Plot 1: ", modelchosen()))

  })

  ##Nested Regression Model
  output$variancetable <- renderPrint({

    wca_model<-lm(mpg~wt+cyl+am, data=mtcars)
    w_v_wcda<-anova(lm(mpg~wt, data=mtcars), lm(mpg~wt+cyl, data=mtcars), lm(mpg~wt+cyl+disp, data=mtcars), lm(mpg~wt+cyl+disp+am, data=mtcars))
    w_v_wcda

  })

  ##Plot 3:  Pairwise graph for potentially significant variables
  output$pairwiseplot <- renderPlot({

    mtcars_wca <- mtcars[, c(1, 2, 6, 9)]
    pairs(mtcars_wca, panel=panel.smooth,col=11 + mtcars_wca$w, main="Plot 2: Pairwise graph for mpg, cyl, wt, am")
  })

  ##Plot 3: MPG vs Transmission Type
  output$mpgvsamplot <- renderPlot({

    suppressWarnings(boxplot(mpg ~ am, data = mtcars, main="Plot 3: MPG vs Transmission Type", xlab = "Transmission Type", ylab="MPG", notch=TRUE, col=c("lightblue","lightgreen")))

  })


})
