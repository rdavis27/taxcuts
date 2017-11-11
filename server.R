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
    lenStart <- as.numeric(rowStart[4])
    lenRate  <- as.numeric(rowRate[4])
    lenAdd   <- as.numeric(rowAdd[4])
    Start    <- as.numeric(rowStart[5:(4+lenStart)])
    Rate     <- as.numeric(rowRate[5:(4+lenRate)])
    if (lenAdd > 0){
      Add   <- as.numeric(rowAdd[5:(4+lenAdd)])
    } else {
      Add <- 0
      for (i in 2:lenStart){
        Add <- c(Add, ((Start[i]-Start[i-1]) * Rate[i-1] / 100.0) + Add[i-1])
      }
      #print(paste0("Add=",Add)) #DEBUG
    }
    
    return(list(
      Start       = Start,
      Rate        = Rate,
      Add         = Add,
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
      Released <<- c("5173","3872","-1301","")
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
      Released <<- c("12180","11050","-1130","")
      Title <<- "Example 4 - New Homeowners Making $115,000 Per Year in a High Tax State"
    }
    else if (example == "Example A"){
      updateNumericInput(session, "wages", value = 25000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 1)
      updateNumericInput(session, "filing",   value = "Single")
      updateNumericInput(session, "property", value = 4000)
      updateNumericInput(session, "mortgage", value = 4000)
      updateNumericInput(session, "charity",  value = 4000)
      Released <<- c("","","","")
      Title <<- "Example A - Single Person Making $25,000 Per Year with $12,000 in Deductions"
    }
    else if (example == "Example B"){
      updateNumericInput(session, "wages", value = 50000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 2)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      updateNumericInput(session, "property", value = 8000)
      updateNumericInput(session, "mortgage", value = 8000)
      updateNumericInput(session, "charity",  value = 8000)
      Released <<- c("","","","")
      Title <<- "Example B - Married Couple Making $50,000 Per Year with $24,000 in Deductions"
    }
    else if (example == "Example C"){
      updateNumericInput(session, "wages", value = 25000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 2)
      updateNumericInput(session, "filing",   value = "Single")
      Released <<- c("","","","")
      Title <<- "Example C - Single Person Making $25,000 Per Year with 1 Non-Child Dependent"
    }
    else if (example == "Example D"){
      updateNumericInput(session, "wages", value = 50000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 4)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      Released <<- c("","","","")
      Title <<- "Example D - Married Couple Making $50,000 Per Year with 2 Non-Child Dependents"
    }
    cat(file=stderr(), paste0(example,"|",input$taxname1,"|",input$taxname2,"\n"))
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
    #if (taxname1 == "Senate 2018" | taxname2 == "Senate 2018"){
    #  cat("<font color=\"red\">WARNING: Using Current 2018 income brackets as Senate 2018 income brackets have not yet been released</font>")
    #}
    cat("<h4>Comparison of Taxes</h4>")
    cat("<pre>")
    print(df)
    cat("</pre>")
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
  output$incomePlot <- renderPlot({
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
      df$aftertax[i] <- 100 * ((wages[i]-df$taxes2[i]) - (wages[i]-df$taxes1[i])) / (wages[i]-df$taxes1[i])
      if (
        df$taxes1[i] <= 0 |
        df$taxes2[i] <= 0 |
        df$taxcut[i] < input$taxcutmin |
        df$taxcut[i] > input$taxcutmax
      ){
        df$taxcut[i] <- NA
        df$aftertax[i] <- NA
      }
    }
    #par(mfrow=c(3,1))
    plot(df$wages, df$aftertax, xlab = "Wages", ylab = "Change in after-tax income (percent)")
    title(main = Title)
    #plot(df$wages, 100*df$taxes1/df$wages)
    #plot(df$wages, 100*df$taxes2/df$wages)
    grid(col = "lightgray")
    abline(v = input$wages, col = "red")
  })
  output$rulePrint <- renderPrint({
    filing <- input$filing
    if (filing == "Married filing jointly") filing = "Married"
    taxname1 <- input$taxname1
    if (taxname1 == "House Cuts w/o Family Credits") taxname1 = "House Cuts2"
    taxname2 <- input$taxname2
    if (taxname2 == "House Cuts w/o Family Credits") taxname2 = "House Cuts2"
    td1 <- getTaxdef(taxname1, filing)
    td2 <- getTaxdef(taxname2, filing)
    id  <- getIncdef()
    Start1 <- td1$Start
    Rate1  <- td1$Rate
    Start2 <- td2$Start
    Rate2  <- td2$Rate
    n1 <- length(Start1)
    n2 <- length(Start2)
    ir1 <- ir2 <- 0
    is1 <- is2 <- 1
    s1 <- Start1[is1]
    s2 <- Start2[is2]
    ss <- rr1 <- rr2 <- NULL
    while (is1 <= n1 | is2 <= n2){
      if (s1 == s2){
        ir1 <- ir1+1
        ir2 <- ir2+1
        r1 <- Rate1[ir1]
        r2 <- Rate2[ir2]
        ss  <- c(ss,  s1)
        rr1 <- c(rr1, r1)
        rr2 <- c(rr2, r2)
        is1 <- is1+1
        is2 <- is2+1
        s1 <- s2 <- 999999999
        if (is1 <= n1) s1 <- Start1[is1]
        if (is2 <= n2) s2 <- Start2[is2]
      } else if (s1 < s2){
        ir1 <- ir1+1
        r1 <- Rate1[ir1]
        ss  <- c(ss,  s1)
        rr1 <- c(rr1, r1)
        rr2 <- c(rr2, r2)
        is1 <- is1+1
        s1 <- 999999999
        if (is1 <= n1) s1 <- Start1[is1]
      } else {
        ir2 <- ir2+1
        r2 <- Rate2[ir2]
        ss  <- c(ss,  s2)
        rr1 <- c(rr1, r1)
        rr2 <- c(rr2, r2)
        is2 <- is2+1
        s2 <- 999999999
        if (is2 <= n2) s2 <- Start2[is2]
      }
    }
    Diff <- rr2 - rr1
    df <- data.frame("Start"=ss, "Rate_1"=rr1, "Rate_2"=rr2, "Change"=Diff)
    cat("<h4>Comparison of Brackets</h4>")
    cat("<pre>")
    print(df)
    cat("</pre>")
    Rules  <- c("Exemption Amount",
                "Standard Deduction",
                "Child Credit",
                "Family Credit")
    Rule1 <- c(td1$Exempt, td1$StdDeduct, td1$ChildCredit, td1$DepCredit)
    Rule2 <- c(td2$Exempt, td2$StdDeduct, td2$ChildCredit, td2$DepCredit)
    RDiff <- Rule2 - Rule1
    rdf <- data.frame("Tax_Rule"=Rules, "Plan_1"=Rule1, "Plan_2"=Rule2, "Change"=RDiff)
    cat("<h4>Comparison of Tax Rules</h4>")
    cat("<pre>")
    print(rdf)
    cat("</pre>")
    Deducts <- c("Medical & Dental (% over 10% of income)",
                 "State & Local income/sales tax (% of income)",
                 "Real estate property taxes",
                 "Mortgage interest",
                 "Charitable contributions")
    Deduct1 <- c(td1$Medical, td1$StateLoc, td1$Property, td1$Mortgage, td1$Charity)
    Deduct2 <- c(td2$Medical, td2$StateLoc, td2$Property, td2$Mortgage, td2$Charity)
    ddf <- data.frame("Deduction"=Deducts, "Plan_1"=Deduct1, "Plan_2"=Deduct2)
    cat("<h4>Comparison of Deductions (0=not deductible, 1=deductible)</h4>")
    cat("<pre>")
    print(ddf)
    cat("</pre>")
    cat("Source: <A HREF=\"https://www.finance.senate.gov/imo/media/doc/11.9.17%20Chairman's%20Mark.pdf\">Description of the Chairman's Mark of the \"Tax Cuts and Jobs Act\"</A> (Senate).")
    cat("<br><br>")
    cat("NOTE: Blog post on this application can be found at <A HREF=\"http://usbudget.blogspot.com/2017/11/the-problems-with-taxpayer-examples.html\">this link</A>.")
  })
})
