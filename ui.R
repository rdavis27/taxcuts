library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Comparison of Tax Plans"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Tax Plans to Compare"),
        selectInput("taxname1", "Tax Plan 1",
                    choices = c("Current 2017", "Current 2018", "House Cuts", "House Cuts w/o Family Credits"),
                    selected = "Current 2017"),
        selectInput("taxname2", "Tax Plan 2",
                    choices = c("Current 2017", "Current 2018", "House Cuts", "House Cuts w/o Family Credits"),
                    selected = "House Cuts"),
        selectInput("examples", "Tax Examples",
                    choices = c("Example 1", "Example 2", "Example 3", "Example 4"),
                    selected = "Example 1")
      ),
      wellPanel(
        h4("General Items"),
        selectInput("filing", "Filing status",
                    choices = c("Single", "Married filing jointly")),
        numericInput("children", "Number of children under age 17", value = 0, min = 0, step = 1),
        numericInput("otherdep", "Number of other dependents", value = 0, min = 0, step = 1),
        numericInput("wages", "Wages, salaries, tips, etc.", value = 0, min = 0, step = 1000)
      ),
      wellPanel(
        h4("Deductions"),
        numericInput("medical", "Medical and dental expenses (% over 10% of income)", value = 0, min = 0, max = 99, step = 1),
        numericInput("stateloc", "State & local income or sales taxes (% of income)", value = 0, min = 0, max = 99, step = 1),
        numericInput("property", "Real estate property taxes", value = 0, min = 0, step = 1000),
        numericInput("mortgage", "Mortgage interest", value = 0, min = 0, step = 1000),
        numericInput("charity", "Charitable contributions", value = 0, min = 0, step = 1000)
      ),
      wellPanel(
        h4("Scales"),
        numericInput("wagemin", "Wage Minimum", value = 0, min = 0, step = 1000),
        numericInput("wagemax", "Wage Maximum", value = 200000, min = 0, step = 1000),
        numericInput("wagestep", "Wage Step", value = 1000, min = 0, step = 100),
        numericInput("taxcutmin", "Taxcut Minimum", value = -100, step = 10),
        numericInput("taxcutmax", "Taxcut Maximum", value = 100, step = 10)
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      verbatimTextOutput("taxPrint"),
      plotOutput("taxPlot")
    )
  )
))
