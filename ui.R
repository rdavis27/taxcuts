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
                                "Senate 2018 w/ $1650 Child Credit", "Senate 2018", "Conference"),
                    selected = "Current 2018"),
        selectInput("taxname2", "Tax Plan 2",
                    choices = c("Current 2017", "Current 2018",
                                "House 2017", "House 2017 w/o Family Credits", "House 2018", "House 2018 w/o Family Credits",
                                "Senate 2018 w/ $1650 Child Credit", "Senate 2018", "Conference"),
                    selected = "Conference"),
        selectInput("examples", "Tax Examples",
                    choices = c("Example 1 - Family of Four (House)",
                                "Example 2 - Single Mother (House)",
                                "Example 3 - Single Taxpayer (House)",
                                "Example 4 - Homeowners, High Tax State (House)",
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
                                "Example J - Married, 4 Non-Child Dependents",
                                "Example K - Single Itemizer, $36,000 Income",
                                "Example L - Married Itemizer, $72,000 Income",
                                "Example M - Single, $30,000, no kids (Tax Foundation)",
                                "Example N - Single, $52,000, 2 kids (Tax Foundation)",
                                "Example O - Single, $75,000, no kids (Tax Foundation)",
                                "Example P - Married, $85,000, 2 kids (Tax Foundation)",
                                "Example Q - Married, $165,000, 2 kids, itemizing (Tax Foundation)"),
                                #"Example Q - Married, $165,000, 2 kids, itemizing (Tax Foundation)",
                                #"Example R - Married, $325,000, 3 kids, itemizing (Tax Foundation)"),
                    selected = "Example A - Single Itemizer"),
        selectInput("taxadj1", "Tax Plan 1 Adjustment",
                    choices = c("No adjustment",
                                "Brackets only",
                                "Exemptions only",
                                "Standard Deduction only",
                                "Child Tax Credit only",
                                "Dependent Credit only",
                                "Standard Deduction, Exemptions & all Credits",
                                "Standard Deduction, Exemptions, all Credits & Brackets",
                                "Add $300 to CTC Refundability (per Rubio)"),
                    selected = "No adjustment")
      ),
      wellPanel(
        h4("General Items"),
        selectInput("filing", "Filing status",
                    choices = c("Single", "Head of Household", "Married filing jointly")),
        numericInput("children", "Number of children under age 17", value = 0, min = 0, step = 1),
        numericInput("otherdep", "Number of other dependents", value = 0, min = 0, step = 1),
        numericInput("wages", "Wages, salaries, tips, etc.", value = 0, min = 0, step = 1000),
        numericInput("deferred", "Tax-deferred retirement contributions", value = 0, min = 0, step = 100),
        numericInput("highwage", "Highest wages (0 to exclude payroll taxes)", value = 0, min = 0, step = 1000)
      ),
      wellPanel(
        h4("Deductions (+dollars or -percent)"),
        numericInput("medical", "Medical and dental expenses (eligible amount)", value = 0, min = 0, step = 1000),
        numericInput("stateloc", "State & local income or sales taxes", value = 0, min = -99, step = 1000),
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
        numericInput("taxcutmax", "Taxcut Maximum", value = 100, step = 10),
        numericInput("nsmall",    "Decimal places", value = 0, min = 0, step = 1),
        checkboxInput("bigmark", "Comma separator", value = TRUE),
        checkboxInput("lockplans", "Lock Tax Plans", value = FALSE),
        selectInput("titleopt", "Title Options",
                    choices = c("Use default title",
                                "Generate title",
                                "Use title below"),
                    selected = "Use default title"),
        textInput("title", "Title", value = ""),
        checkboxInput("appendTax", "Append Tax Name", value = TRUE)
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Tax Cuts",
          htmlOutput("taxPrint"),
          HTML("<h4><b>Taxcut (dollars)</b></h4>"),
          plotOutput("taxPlotDollars"),
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
        ),
        tabPanel("Calculation of Taxes",
                 htmlOutput("taxCalcPrint")
        )
      )
    )
  )
))
