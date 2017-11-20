library(shiny)

taxdefs  <- read.csv("taxdefs.csv",  strip.white = TRUE, sep = ",")
brackets <- read.csv("brackets.csv", strip.white = TRUE, sep = ",")
eitcdefs <- read.csv("eitc.csv",     strip.white = TRUE, sep = ",")
incdef   <- NULL
last_example <- ""

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
      Charity     = rowDef$Charity,
      Repealed    = rowDef$Repealed,
      EITC        = rowDef$EITC
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
      charity    = input$charity,
      repealed   = input$repealed
    ))
  })
  calcPretax <- function(td, income){
    inc <- max(0, income)
    i <- max(which(td$Start <= inc))
    pretax <- (income-td$Start[i]) * (td$Rate[i]/100) + td$Add[i]
  }
  calcEITC <- function(name, wages, children, filing){
    ee <- eitcdefs[eitcdefs$Name == as.character(name) & as.numeric(eitcdefs$C) == children,]
    if (identical(filing, "Married filing jointly")){
      ee$Wage2 <- ee$Wage2 + ee$Madd
      ee$Wage3 <- ee$Wage3 + ee$Madd
    }
    if (wages >= ee$Wage3){
      eitc <- 0
    } else if (wages < ee$Wage1){
      eitc <- wages * ee$Per1/100.0
    } else if (wages <= ee$Wage2){
      eitc <- ee$Ymax
    } else {
      eitc <- (ee$Wage3 - wages) * ee$Per2/100.0
    }
    eitc
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
    repealed <- as.numeric(td["Repealed"]) * as.numeric(id["repealed"])
    EITCname <- as.character(td["EITC"])
    eitc     <- calcEITC(EITCname, wages, children, input$filing)
    #print(paste0(medical,"|",stateloc,"|",property,"|",mortgage,"|",charity"|",repealed)) #DEBUG
    CalcDeduct <- medical + stateloc + property + mortgage + charity + repealed
    Deduct <- StdDeduct
    if (Deduct < CalcDeduct) Deduct <- CalcDeduct
    
    adjinc <- wages - Deduct - (children + dependents) * Exempt
    pretax <- calcPretax(td, adjinc)
    tax <- pretax - children * ChildCredit - dependents * DepCredit - eitc
    tax
  }
  clearDeductions <- function(){
    updateNumericInput(session, "medical",  value = 0)
    updateNumericInput(session, "stateloc", value = 0)
    updateNumericInput(session, "property", value = 0)
    updateNumericInput(session, "mortgage", value = 0)
    updateNumericInput(session, "charity",  value = 0)
    updateNumericInput(session, "repealed", value = 0)
  }
  resetWageLimits <- function(){
    updateNumericInput(session, "wagemin",  value = 0)
    updateNumericInput(session, "wagemax",  value = 200000)
  }
  getShortTaxName2 <- function(){
    taxname2 <- input$taxname2
    name <- ""
    if (taxname2 == "Current 2017"){
      name <- "2017"
    }
    else if (taxname2 == "Current 2018"){
      name <- "2018"
    }
    else if (taxname2 == "House Bill"){
      name <- "House"
    }
    else if (taxname2 == "House Bill w/o Family Credits"){
      name <- "House w/o FC"
    }
    else if (taxname2 == "Senate Bill"){
      name <- "Senate"
    }
    else if (taxname2 == "Senate Bill w/ $2000 Child Credit"){
      name <- "Senate w/ $2000 CC"
    }
    name
  }
  getMidTaxName <- function(longTaxName){
    midTaxName <- longTaxName
    if (longTaxName == "House Bill w/o Family Credits")     midTaxName <- "House Bill2"
    if (longTaxName == "Senate Bill w/ $2000 Child Credit") midTaxName <- "Senate Bill2"
    midTaxName
  }
  parenTaxName2 <- function(){
    name <- getShortTaxName2()
    if (name != ""){
      name <- paste0(" (",getShortTaxName2(),")")
    }
    name
  }
  observeEvent(input$examples, {
    example <- substr(input$examples, 1, 9)
    #print(paste0("example=",example))
    clearDeductions()
    resetWageLimits()
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
      updateNumericInput(session, "mortgage", value = 6000)
      updateNumericInput(session, "charity",  value = 6000)
      Released <<- c("","","","")
      Title <<- "Example A - Single Person Making $25,000 Per Year with $12,000 in Deductions"
    }
    else if (example == "Example B"){
      updateNumericInput(session, "wages", value = 50000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 2)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      updateNumericInput(session, "mortgage", value = 12000)
      updateNumericInput(session, "charity",  value = 12000)
      Released <<- c("","","","")
      Title <<- "Example B - Married Couple Making $50,000 Per Year with $24,000 in Deductions"
    }
    else if (example == "Example C"){
      updateNumericInput(session, "wages", value = 25000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 1)
      updateNumericInput(session, "filing",   value = "Single")
      updateNumericInput(session, "repealed", value = 15000)
      Released <<- c("","","","")
      Title <<- "Example C - Single Person Making $25,000 Per Year with $15,000 in Repealed Deductions"
    }
    else if (example == "Example D"){
      updateNumericInput(session, "wages", value = 50000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 2)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      updateNumericInput(session, "repealed", value = 30000)
      Released <<- c("","","","")
      Title <<- "Example D - Married Couple Making $50,000 Per Year with $30,000 in Repealed Deductions"
    }
    else if (example == "Example E"){
      updateNumericInput(session, "wages", value = 470000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 1)
      updateNumericInput(session, "filing",   value = "Single")
      updateNumericInput(session, "stateloc", value = 10)
      updateNumericInput(session, "wagemin",  value = 10000)
      updateNumericInput(session, "wagemax",  value = 2000000)
      Released <<- c("","","","")
      #Title <<- "Example E - Single Person with 10 Percent of Income in State & Local Income/Sales Taxes"
      Title <<- "Example E - Single Person with 10 Percent of Income in State and Local Taxes"
    }
    else if (example == "Example F"){
      updateNumericInput(session, "wages", value = 530000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 2)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      updateNumericInput(session, "stateloc", value = 10)
      updateNumericInput(session, "wagemin",  value = 10000)
      updateNumericInput(session, "wagemax",  value = 2000000)
      Released <<- c("","","","")
      #Title <<- "Example F - Married Couple with 10 Percent of Income in State & Local Income/Sales Taxes"
      Title <<- "Example F - Married Couple with 10 Percent of Income in State and Local Taxes"
    }
    else if (example == "Example G"){
      updateNumericInput(session, "wages", value = 25000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 2)
      updateNumericInput(session, "filing",   value = "Single")
      Released <<- c("","","","")
      Title <<- "Example G - Single Person Making $25,000 Per Year with 1 Non-Child Dependent"
    }
    else if (example == "Example H"){
      updateNumericInput(session, "wages", value = 50000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 4)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      Released <<- c("","","","")
      Title <<- "Example H - Married Couple Making $50,000 Per Year with 2 Non-Child Dependents"
    }
    else if (example == "Example I"){
      updateNumericInput(session, "wages", value = 25000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 3)
      updateNumericInput(session, "filing",   value = "Single")
      Released <<- c("","","","")
      Title <<- "Example G - Single Person Making $25,000 Per Year with 2 Non-Child Dependent"
    }
    else if (example == "Example J"){
      updateNumericInput(session, "wages", value = 50000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 6)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      Released <<- c("","","","")
      Title <<- "Example H - Married Couple Making $50,000 Per Year with 4 Non-Child Dependents"
    }
  })
  output$taxPrint <- renderPrint({
    filing <- input$filing
    if (filing == "Married filing jointly") filing = "Married"
    taxname1 <- getMidTaxName(input$taxname1)
    taxname2 <- getMidTaxName(input$taxname2)
    taxes1 <- calcTax(getTaxdef(taxname1, filing), getIncdef(), -1)
    taxes2 <- calcTax(getTaxdef(taxname2, filing), getIncdef(), -1)
    Names <- c(input$taxname1, input$taxname2, "Change", "% Change")
    Taxes <- c(taxes1, taxes2, taxes2-taxes1, 100*(taxes2-taxes1)/taxes1)
    df <- data.frame(Names, Taxes, Released)
    cat("<h4>Comparison of Taxes</h4>")
    cat("<pre>")
    cat(paste0(filing,", ",input$children," children, ",input$otherdep," dependents, ",input$wages," in wages\n\n"))
    print(df)
    eitc <- calcEITC("2017", input$wages, input$children, input$filing)
    if (eitc > 0){
      cat(paste0("\nNote: Taxes for both plans include an EITC of $", eitc, " (using the 2017 formula)\n"))
    }
    cat("</pre>")
  })
  taxdata <- reactive({
    filing <- input$filing
    if (filing == "Married filing jointly") filing = "Married"
    taxname1 <- getMidTaxName(input$taxname1)
    taxname2 <- getMidTaxName(input$taxname2)
    wages <- seq(input$wagemin, input$wagemax, input$wagestep)
    df <- data.frame(wages)
    df$taxes1 <- 0
    df$taxes2 <- 0
    taxdef1 <- getTaxdef(taxname1, filing)
    taxdef2 <- getTaxdef(taxname2, filing)
    incdef  <- getIncdef()
    for (i in 1:length(df$wages)){
      df$taxes1[i] <- calcTax(taxdef1, incdef, wages[i])
      df$taxes2[i] <- calcTax(taxdef2, incdef, wages[i])
    }
    if (input$wages != 0){
      cat(file=stderr(), paste0(input$taxname1,"|",input$taxname2,"|",input$examples,"#",
                                input$filing,"|", input$children,"|",input$otherdep,"|",input$wages,"#",
                                input$medical,"|",input$stateloc,"|",input$property,"|",input$mortgage,"|",input$charity,"|",input$repealed,"#",
                                input$wagemin,"|",input$wagemax, "|",input$wagestep,"|",input$taxcutmin,"|",input$taxcutmax,"\n"))
    }
    # if (last_example == input$examples){
    # } else {
    #   last_example <<- input$examples # skip since this will rerun after example sets parameters
    # }
    return(df)
  })
  output$taxPlot <- renderPlot({
    df <- taxdata()
    df$taxcut <- 100 * (df$taxes1 - df$taxes2) / df$taxes1
    df$taxcut[df$taxes1 <= 0 | df$taxes2 <= 0] <- NA
    df$taxcut[df$taxcut < input$taxcutmin | df$taxcut > input$taxcutmax] <- NA
    plot(df$wages, df$taxcut, xlab = "Wages", ylab = "Taxcut (percent)")
    title(main = paste0(Title,parenTaxName2()))
    grid(col = "lightgray")
    abline(v = input$wages, col = "red")
  })
  output$incomePlot <- renderPlot({
    df <- taxdata()
    df$aftertax <- 100 * ((df$wages-df$taxes2) - (df$wages-df$taxes1)) / (df$wages-df$taxes1)
    plot(df$wages, df$aftertax, xlab = "Wages", ylab = "Change in after-tax income (percent)")
    title(main = paste0(Title,parenTaxName2()))
    grid(col = "lightgray")
    abline(v = input$wages, col = "red")
  })
  output$efftaxPlot <- renderPlot({
    df <- taxdata()
    efftax1 <- 100 * df$taxes1 / df$wages
    efftax1[efftax1 < 0] <- NA
    efftax2 <- 100 * df$taxes2 / df$wages
    efftax2[efftax2 < 0] <- NA
    efftax  <- matrix(c(efftax1, efftax2), length(efftax1), 2)
    matplot(df$wages, efftax, type = "l", xlab = "Wages", ylab = "Effective Tax Rate (percent)")
    legend("topleft", c("Tax Plan 1", "Tax Plan 2"), col = c(1,2), fill = c(1,2))
    title(main = paste0(Title,parenTaxName2()))
    grid(col = "lightgray")
    abline(v = input$wages, col = "red")
  })
  output$rulePrint <- renderPrint({
    df <- taxdata() # log message on change
    filing <- input$filing
    if (filing == "Married filing jointly") filing = "Married"
    taxname1 <- getMidTaxName(input$taxname1)
    taxname2 <- getMidTaxName(input$taxname2)
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
                 "Charitable contributions",
                 "Misc. repealed deductions")
    Deduct1 <- c(td1$Medical, td1$StateLoc, td1$Property, td1$Mortgage, td1$Charity, td1$Repealed)
    Deduct2 <- c(td2$Medical, td2$StateLoc, td2$Property, td2$Mortgage, td2$Charity, td2$Repealed)
    ddf <- data.frame("Deduction"=Deducts, "Plan_1"=Deduct1, "Plan_2"=Deduct2)
    cat("<h4>Comparison of Deductions (0=not deductible, 1=deductible)</h4>")
    cat("<pre>")
    print(ddf)
    cat("</pre>")
    cat("<pre>")
    cat("Sources: <A HREF=\"https://waysandmeans.house.gov/wp-content/uploads/2017/11/WM_TCJA_TaxPayerExamples.pdf\">Taxpayer Examples from the Committee on Ways and Means</A> (House).\n")
    cat("         <A HREF=\"https://waysandmeans.house.gov/wp-content/uploads/2017/10/WM_TCJA_PolicyOnePagers.pdf\">The Tax Cuts & Jobs Act - Communications and Policy Details</A> (House).\n")
    cat("         <A HREF=\"https://www.finance.senate.gov/imo/media/doc/11.9.17%20Chairman's%20Mark.pdf\">Description of the Chairman's Mark of the \"Tax Cuts and Jobs Act\"</A> (Senate).\n")
    cat("<br>")
    cat("Notes: Blog post on this application can be found at <A HREF=\"http://usbudget.blogspot.com/2017/11/the-problems-with-taxpayer-examples.html\">this link</A>.")
    cat("</pre>")
  })
})
