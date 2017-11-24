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
                    choices = c("Current 2017", "Current 2018",
                                "House 2017", "House 2017 w/o Family Credits", "House 2018", "House 2018 w/o Family Credits",
                                "Senate 2018 w/ $1650 Child Credit", "Senate 2018"),
                    selected = "Current 2017"),
        selectInput("taxname2", "Tax Plan 2",
                    choices = c("Current 2017", "Current 2018",
                                "House 2017", "House 2017 w/o Family Credits", "House 2018", "House 2018 w/o Family Credits",
                                "Senate 2018 w/ $1650 Child Credit", "Senate 2018"),
                    selected = "Senate 2018"),
        selectInput("examples", "Tax Examples",
                    choices = c("Example 1 - Family of Four",
                                "Example 2 - Single Mother",
                                "Example 3 - Single Taxpayer",
                                "Example 4 - Homeowners, High Tax State",
                                "Example 5 - Family of Four (Senate)",
                                "Example 6 - Single Parent, One Child (Senate)",
                                "Example A - Single Itemizer",
                                "Example B - Married Itemizer",
                                "Example C - Single, Repealed Deductions",
                                "Example D - Married, Repealed Deductions",
                                "Example E - Single, High Tax State",
                                "Example F - Married, High Tax State",
                                "Example G - Single, 1 Non-Child Dependent",
                                "Example H - Married, 2 Non-Child Dependents",
                                "Example I - Single, 2 Non-Child Dependent",
                                "Example J - Married, 4 Non-Child Dependents"),
        selected = "Example 5 - Family of Four (Senate)")
      ),
      wellPanel(
        h4("General Items"),
        selectInput("filing", "Filing status",
                    choices = c("Single", "Head of Household", "Married filing jointly")),
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
        numericInput("charity", "Charitable contributions", value = 0, min = 0, step = 1000),
        numericInput("repealed", "Misc. repealed deductions", value = 0, min = 0, step = 1000)
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
      tabsetPanel(
        tabPanel("Tax Cuts",
          htmlOutput("taxPrint"),
          HTML("<h4><b>Taxcut (percent)</b></h4>"),
          plotOutput("taxPlot"),
          HTML("<h4><b>Change in After-Tax Income (percent)</b></h4>"),
          plotOutput("incomePlot")
        ),
        tabPanel("Effective Taxes",
          HTML("<h4><b>Effective Tax Rates of Both Tax Plans (percent)</b></h4>"),
          plotOutput("efftaxPlot")
        ),
        tabPanel("Comparison of Plans",
                 htmlOutput("rulePrint")
        )
      )
    )
  )
))
