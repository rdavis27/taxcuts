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
                                "Senate 2018 w/ $1650 Child Credit", "Senate 2018", "Final Bill"),
                    selected = "Current 2018"),
        selectInput("taxname2", "Tax Plan 2",
                    choices = c("Current 2017", "Current 2018",
                                "House 2017", "House 2017 w/o Family Credits", "House 2018", "House 2018 w/o Family Credits",
                                "Senate 2018 w/ $1650 Child Credit", "Senate 2018", "Final Bill"),
                    selected = "Final Bill"),
        selectInput("examples", "Tax Examples",
                    choices = c("Example H1 - Family of Four (House)",
                                "Example H2 - Single Mother (House)",
                                "Example H3 - Single Taxpayer (House)",
                                "Example H4 - Homeowners, High Tax State (House)",
                                "Example S1 - Family of Four (Senate)",
                                "Example S2 - Single Parent, One Child (Senate)",
                                "Example T1 - Single, $30,000, no kids (Tax Foundation)",
                                "Example T2 - Single, $52,000, 2 kids (Tax Foundation)",
                                "Example T3 - Single, $75,000, no kids (Tax Foundation)",
                                "Example T4 - Married, $85,000, 2 kids (Tax Foundation)",
                                "Example T5 - Married, $165,000, 2 kids, itemizing (Tax Foundation)",
                                #"Example T6 - Married, $325,000, 3 kids, itemizing (Tax Foundation)",
                                "Example X1 - Single Itemizer",
                                "Example X2 - Married Itemizer",
                                "Example X3 - Single, Repealed Deductions",
                                "Example X4 - Married, Repealed Deductions",
                                "Example X5 - Single, High Tax State",
                                "Example X6 - Married, High Tax State",
                                "Example X7 - Single, 1 Non-Child Dependent",
                                "Example X8 - Married, 2 Non-Child Dependents",
                                "Example X9 - Single, 2 Non-Child Dependent",
                                "Example X10 - Married, 4 Non-Child Dependents",
                                "Example X11 - Single Itemizer, $36,000 Income",
                                "Example X12 - Married Itemizer, $72,000 Income"),
                    selected = "Example X1 - Single Itemizer"),
        selectInput("taxadj1", "Tax Plan 1 Adjustment",
                    choices = c("No adjustment",
                                "Brackets only",
                                "Exemptions only",
                                "Standard Deduction & Exemptions",
                                "Child Tax Credit & Exemptions",
                                "Dependent Credit & Exemptions",
                                "Standard Deduction, Exemptions & all Credits",
                                "Standard Deduction, Exemptions, all Credits & Brackets",
                                "Add $300 to CTC Refundability (per Rubio)"),
                    selected = "No adjustment"),
        selectInput("xvariable", "X Variable",
                    choices = c("Wages",
                                "Medical and dental expenses",
                                "State and local taxes",
                                "Real estate property taxes",
                                "Mortgage interest",
                                "Charitable contributions",
                                "Misc. repealed deductions"),
                    selected = "Wages")
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
        numericInput("percentmin", "Percent Minimum", value = -100, step = 10),
        numericInput("percentmax", "Percent Maximum", value = 100, step = 10),
        numericInput("nsmall",    "Decimal places", value = 0, min = 0, step = 1),
        checkboxInput("bigmark", "Comma separator", value = TRUE),
        checkboxInput("lockplans", "Lock Tax Plans", value = FALSE),
        selectInput("titleopt", "Title Options",
                    choices = c("Use default title",
                                "Generate title",
                                "Generate title and prepend title below",
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
