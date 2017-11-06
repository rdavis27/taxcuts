library(shiny)

taxdefs  <- read.csv("taxdefs.csv",  strip.white = TRUE, sep = ",")
brackets <- read.csv("brackets.csv", strip.white = TRUE, sep = ",")
incdef   <- NULL

shinyServer(function(input, output, session) {

  getTaxdef <- function(taxname, filing){
    rowStart <- brackets[brackets$Name == taxname & brackets$Filing == filing & brackets$Prop == "Start",]
    rowRate  <- brackets[brackets$Name == taxname & brackets$Filing == filing & brackets$Prop == "Rate",]
    rowAdd   <- brackets[brackets$Name == taxname & brackets$Filing == filing & brackets$Prop == "Add",]
    rowDef   <- taxdefs[ taxdefs$Name == taxname  & taxdefs$Filing  == filing,]
    
    return(list(
      Start = as.numeric(rowStart[5:(4+as.numeric(rowStart[4]))]),
      Rate  = as.numeric(rowRate[5:(4+as.numeric(rowRate[4]))]),
      Add   = as.numeric(rowAdd[5:(4+as.numeric(rowAdd[4]))]),
      Exempt      = rowDef$Exempt,
      StdDeduct   = rowDef$StdDeduct,
      ChildCredit = rowDef$ChildCredit,
      DepCredit   = rowDef$DepCredit,
      Medical     = rowDef$Medical,
      StateLoc    = rowDef$StateLoc,
      Property    = rowDef$Property,
      Mortgage    = rowDef$Mortgage,
      Charity     = rowDef$Charity
    ))
  }
  getIncdef <- reactive({
    return(list(
      wages      = input$wages,
      children   = input$children,
      dependents = input$otherdep,
      medical    = input$medical,
      stateloc   = input$stateloc,
      property   = input$property,
      mortgage   = input$mortgage,
      charity    = input$charity
    ))
  })
  calcPretax <- function(td, income){
    inc <- max(0, income)
    i <- max(which(td$Start <= inc))
    pretax <- (income-td$Start[i]) * (td$Rate[i]/100) + td$Add[i]
  }
  calcTax <- function(td, id, wages){
    Exempt      <- as.numeric(td["Exempt"])
    StdDeduct   <- as.numeric(td["StdDeduct"])
    ChildCredit <- as.numeric(td["ChildCredit"])
    DepCredit   <- as.numeric(td["DepCredit"])
    
    if (wages < 0) wages <- as.numeric(id["wages"])
    children <- as.numeric(id["children"])
    dependents <- as.numeric(id["dependents"])
    medical <-  as.numeric(td["Medical"])  * as.numeric(id["medical"])  * wages / 100.0
    stateloc <- as.numeric(td["StateLoc"]) * as.numeric(id["stateloc"]) * wages / 100.0
    property <- as.numeric(td["Property"]) * as.numeric(id["property"])
    mortgage <- as.numeric(td["Mortgage"]) * as.numeric(id["mortgage"])
    charity  <- as.numeric(td["Charity"])  * as.numeric(id["charity"])
    #print(paste0(medical,"|",stateloc,"|",property,"|",mortgage,"|",charity)) #DEBUG
    CalcDeduct <- medical + stateloc + property + mortgage + charity
    Deduct <- StdDeduct
    if (Deduct < CalcDeduct) Deduct <- CalcDeduct
    
    adjinc <- wages - Deduct - (children + dependents) * Exempt
    pretax <- calcPretax(td, adjinc)
    tax <- pretax - children * ChildCredit - dependents * DepCredit
    tax
  }
  clearDeductions <- function(){
    updateNumericInput(session, "medical",  value = 0)
    updateNumericInput(session, "stateloc", value = 0)
    updateNumericInput(session, "property", value = 0)
    updateNumericInput(session, "mortgage", value = 0)
    updateNumericInput(session, "charity",  value = 0)
  }
  observeEvent(input$examples, {
    example <- input$examples
    #print(paste0("example=",example))
    clearDeductions()
    if (example == "Example 1"){
      updateNumericInput(session, "wages", value = 59000)
      updateNumericInput(session, "children", value = 2)
      updateNumericInput(session, "otherdep", value = 2)
      updateNumericInput(session, "filing", value = "Married filing jointly")
      Released <<- c("1582","400","-1182","")
      Title <<- "Example 1 - Family of Four Making $59,000 Per Year"
    }
    else if (example == "Example 2"){
      updateNumericInput(session, "wages", value = 30000)
      updateNumericInput(session, "children", value = 1)
      updateNumericInput(session, "otherdep", value = 1)
      updateNumericInput(session, "filing", value = "Single")
      Released <<- c("","< -1000","< -700","")
      Title <<- "Example 2 - Single Mother Making $30,000 Per Year"
    }
    else if (example == "Example 3"){
      updateNumericInput(session, "wages", value = 48000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 1)
      updateNumericInput(session, "filing", value = "Single")
      Released <<- c("5173","3872","1301","")
      Title <<- "Example 3 - Firefighter Making $48,000 Per Year"
    }
    else if (example == "Example 4"){
      updateNumericInput(session, "wages", value = 115000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 2)
      updateNumericInput(session, "filing", value = "Married filing jointly")
      updateNumericInput(session, "stateloc", value = 7.64347)
      updateNumericInput(session, "property", value = 6900)
      updateNumericInput(session, "mortgage", value = 8400)
      Released <<- c("12180","11050","1130","")
      Title <<- "Example 4 - New Homeowners Making $115,000 Per Year in a High Tax State"
    }
  })
  output$taxPrint <- renderPrint({
    filing <- input$filing
    if (filing == "Married filing jointly") filing = "Married"
    taxname1 <- input$taxname1
    if (taxname1 == "House Cuts w/o Family Credits") taxname1 = "House Cuts2"
    taxname2 <- input$taxname2
    if (taxname2 == "House Cuts w/o Family Credits") taxname2 = "House Cuts2"
    taxes1 <- calcTax(getTaxdef(taxname1, filing), getIncdef(), -1)
    taxes2 <- calcTax(getTaxdef(taxname2, filing), getIncdef(), -1)
    Names <- c(input$taxname1, input$taxname2, "Change", "% Change")
    Taxes <- c(taxes1, taxes2, taxes2-taxes1, 100*(taxes2-taxes1)/taxes1)
    df <- data.frame(Names, Taxes, Released)
    print(df)
  })
  output$taxPlot <- renderPlot({
    filing <- input$filing
    if (filing == "Married filing jointly") filing = "Married"
    taxname1 <- input$taxname1
    if (taxname1 == "House Cuts w/o Family Credits") taxname1 = "House Cuts2"
    taxname2 <- input$taxname2
    if (taxname2 == "House Cuts w/o Family Credits") taxname2 = "House Cuts2"
    wages <- seq(input$wagemin, input$wagemax, input$wagestep)
    df <- data.frame(wages)
    df$taxes1 <- 0
    df$taxes2 <- 0
    df$taxcut <- 0
    taxdef1 <- getTaxdef(taxname1, filing)
    taxdef2 <- getTaxdef(taxname2, filing)
    incdef  <- getIncdef()
    for (i in 1:length(df$wages)){
      df$taxes1[i] <- calcTax(taxdef1, incdef, wages[i])
      df$taxes2[i] <- calcTax(taxdef2, incdef, wages[i])
      df$taxcut[i] <- 100 * (df$taxes1[i] - df$taxes2[i]) / df$taxes1[i]
      if (
        df$taxes1[i] <= 0 |
        df$taxes2[i] <= 0 |
        df$taxcut[i] < input$taxcutmin |
        df$taxcut[i] > input$taxcutmax
      ){
        df$taxcut[i] <- NA
      }
    }
    #par(mfrow=c(3,1))
    plot(df$wages, df$taxcut, xlab = "Wages", ylab = "Taxcut (percent)")
    title(main = Title)
    #plot(df$wages, 100*df$taxes1/df$wages)
    #plot(df$wages, 100*df$taxes2/df$wages)
    grid(col = "lightgray")
    abline(v = input$wages, col = "red")
  })
  # output$distPlot <- renderPlot({
  # 
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2]
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  # 
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  # 
  # })

})
