#
# This is the user-interface definition of a Shiny web application for an analysis of
# mileage (mpg) for automatic and manual transmission cars. Employing regression modeling, it
# further explores the relationship between fuel efficiency and a set of variables in mtcars..
# This is shinification of the project I did for the project for the Regression Modeling Course.
#

library(shiny)

# Define UI for application that lets the choose the regression model and then presents the
# the results of the same.
shinyUI(fluidPage(

  # Application title
  titlePanel("Shiny Application for Regression Modeling for Mileage Analysis"),

  # Sidebar with radio buttons to choose the desired regression model
  sidebarLayout(
    sidebarPanel(

      radioButtons("modeltype", "Choose the Regression Model to run:", c("Basic Regression (outcome:mpg, regressor:am)" = "lm",
                                                                  "Multiple Regression (outcome: mpg, regressor: wt+am)" = "lm_wt_am",
                                                                  "Multiple Regression (outcome: mpg, regressor: disp+am)" = "lm_disp_am",
                                                                  "Multiple Regression (outcome: mpg, regressor: cyl+am)" = "lm_cyl_am",
                                                                  "Multiple Regression (outcome: mpg, regressor: wt+cyl+am)" = "lm_wt_cyl_am")),

      h5("------------------------------------------------"),
      h5(helpText("Summary of all variables")),
      verbatimTextOutput("allsummary")
    ),

    # Show the results of the regression model
    mainPanel(

      h4("Legend"),

      h5("0: Model Summary & 1: Model Plot depend on the chosen model."),
      h5("2: Variance Table & 3: Pair Wise Plot are for the dataset."),

      tabsetPanel(

        tabPanel("0: Model Summary", verbatimTextOutput("summary")),
        tabPanel("1: Model Plot", plotOutput("modelplot")),
        tabPanel("2: Nested Regression Variance Table", verbatimTextOutput("variancetable")),
        tabPanel("3: Pair Wise Plot", plotOutput("pairwiseplot"))
        #tabPanel("4: MPG vs Transmission Plot", plotOutput("mpgvsamplot"))
        )

    )
  )
))
