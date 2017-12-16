library(shiny)

taxdefs  <- read.csv("taxdefs.csv",  strip.white = TRUE, sep = ",")
brackets <- read.csv("brackets.csv", strip.white = TRUE, sep = ",")
eitcdefs <- read.csv("eitc.csv",     strip.white = TRUE, sep = ",")
incdef   <- NULL
last_example <- ""
Title <- reactiveVal("")
TitleDefault <- ""

shinyServer(function(input, output, session) {

  getTaxdefAdj <- function(taxname, filing, td2){
    td <- getTaxdef(taxname, filing)
    taxadj1 <- input$taxadj1
    
    if (taxadj1 == "Brackets only"){
      td1 <- td
      td <- td2
      td["Start"] <- td1["Start"]
      td["Rate"]  <- td1["Rate"]
      td["Add"]   <- td1["Add"]
    }
    else if (taxadj1 == "Exemptions only"){
      td1 <- td
      td <- td2
      td["Exempt"]    <- as.numeric(td1["Exempt"])
    }
    else if (taxadj1 == "Standard Deduction only"){
      td1 <- td
      td <- td2
      td["StdDeduct"] <- td1["StdDeduct"]
    }
    else if (taxadj1 == "Child Tax Credit only"){
      td1 <- td
      td <- td2
      td["ChildCredit"] <- td1["ChildCredit"]
      td["CCRef"]       <- td1["CCRef"]
      td["CCMin"]       <- td1["CCMin"]
      td["CCMax"]       <- td1["CCMax"]
    }
    else if (taxadj1 == "Dependent Credit only"){
      td1 <- td
      td <- td2
      td["DepCredit"] <- td1["DepCredit"]
    }
    else if (taxadj1 == "Standard Deduction, Exemptions & all Credits"){
      td1 <- td
      td <- td2
      td["StdDeduct"]   <- td1["StdDeduct"]
      td["Exempt"]      <- as.numeric(td1["Exempt"])
      td["ChildCredit"] <- td1["ChildCredit"]
      td["CCRef"]       <- td1["CCRef"]
      td["CCMin"]       <- td1["CCMin"]
      td["CCMax"]       <- td1["CCMax"]
      td["DepCredit"]   <- td1["DepCredit"]
      td["ParCredit"]   <- td1["ParCredit"]
    }
    else if (taxadj1 == "Standard Deduction, Exemptions, all Credits & Brackets"){
      td1 <- td
      td <- td2
      td["StdDeduct"]   <- td1["StdDeduct"]
      td["Exempt"]      <- as.numeric(td1["Exempt"])
      td["ChildCredit"] <- td1["ChildCredit"]
      td["CCRef"]       <- td1["CCRef"]
      td["CCMin"]       <- td1["CCMin"]
      td["CCMax"]       <- td1["CCMax"]
      td["DepCredit"]   <- td1["DepCredit"]
      td["ParCredit"]   <- td1["ParCredit"]
      td["Start"]       <- td1["Start"]
      td["Rate"]        <- td1["Rate"]
      td["Add"]         <- td1["Add"]
    }
    else if (taxadj1 == "Add $300 to CTC Refundability (per Rubio)"){
      td1 <- td
      td <- td2
      td["CCRef"]       <- as.numeric(td2["CCRef"]) - 300
    }
    td
  }
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
      ParCredit   = rowDef$ParCredit,
      Medical     = rowDef$Medical,
      StateLoc    = rowDef$StateLoc,
      Property    = rowDef$Property,
      Mortgage    = rowDef$Mortgage,
      Charity     = rowDef$Charity,
      Repealed    = rowDef$Repealed,
      EITC        = rowDef$EITC,
      SSMax       = rowDef$SSMax,
      CCRef       = rowDef$CCRef,
      CCMin       = rowDef$CCMin,
      CCMax       = rowDef$CCMax,
      StMax       = rowDef$StMax
    ))
  }
  getIncdef <- reactive({
    nparents = 1
    if (input$filing == "Married filing jointly") nparents = 2
    return(list(
      wages      = input$wages,
      deferred   = input$deferred,
      highwage   = input$highwage,
      children   = input$children,
      dependents = input$otherdep,
      parents    = nparents,
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
    wages    <- chknumeric(wages)
    children <- chknumeric(children)
    eechildren <- min(children, 3)
    ee <- eitcdefs[eitcdefs$Name == as.character(name) & as.numeric(eitcdefs$C) == eechildren,]
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
  numformat <- function(n){
    if (input$bigmark){
      format(round(n, input$nsmall), nsmall=input$nsmall, big.mark=",")
    } else {
      format(round(n, input$nsmall), nsmall=input$nsmall)
    }
  }
  chknumeric <- function(n, def=0){
    if (is.list(n)) n <- unlist(n)
    if (is.na(n)) n <- as.integer(def)
    n
  }
  getTaxItems <- function(td, id, wages){
    Exempt      <- as.numeric(td["Exempt"])
    StdDeduct   <- as.numeric(td["StdDeduct"])
    ChildCredit <- as.numeric(td["ChildCredit"])
    DepCredit   <- as.numeric(td["DepCredit"])
    ParCredit   <- as.numeric(td["ParCredit"])
    
    if (wages < 0) wages <- chknumeric(id["wages"])
    deferred <- chknumeric(id["deferred"])
    highwage <- chknumeric(id["highwage"])
    if (deferred < 0) deferred <- -deferred * wages / 100.0
    if (highwage < 0) highwage <- -highwage * wages / 100.0
    children <- chknumeric(id["children"])
    dependents <- chknumeric(id["dependents"])
    parents  <- as.numeric(id["parents"])
    medical  <- as.numeric(td["Medical"])  * chknumeric(id["medical"])
    stateloc <- as.numeric(td["StateLoc"]) * chknumeric(id["stateloc"])
    property <- as.numeric(td["Property"]) * chknumeric(id["property"])
    mortgage <- as.numeric(td["Mortgage"]) * chknumeric(id["mortgage"])
    charity  <- as.numeric(td["Charity"])  * chknumeric(id["charity"])
    repealed <- as.numeric(td["Repealed"]) * chknumeric(id["repealed"])
    if (medical  < 0) medical  <- -medical  * wages / 100.0
    if (stateloc < 0) stateloc <- -stateloc * wages / 100.0
    if (property < 0) property <- -property * wages / 100.0
    if (mortgage < 0) mortgage <- -mortgage * wages / 100.0
    if (charity  < 0) charity  <- -charity  * wages / 100.0
    if (repealed < 0) repealed <- -repealed * wages / 100.0
    EITCname <- as.character(td["EITC"])
    SSMax    <- as.numeric(td["SSMax"])
    CCRef    <- as.numeric(td["CCRef"])
    CCMin    <- as.numeric(td["CCMin"])
    CCMax    <- as.numeric(td["CCMax"])
    StMax    <- as.numeric(td["StMax"])
    eitc     <- calcEITC(EITCname, wages, children, input$filing)
    #print(paste0(medical,"|",stateloc,"|",property,"|",mortgage,"|",charity"|",repealed)) #DEBUG
    taxadj1 <- input$taxadj1
    if (taxadj1 == "Standard Deduction only*"){
      Exemptions <- parents * Exempt
    }
    else if (taxadj1 == "Child Tax Credit only*"){
      Exemptions <- children * Exempt
    }
    else if (taxadj1 == "Dependent Credit only*"){
      Exemptions <- dependents * Exempt
    }
    else{
      Exemptions <- (children + dependents + parents) * Exempt
    }
    StTax <- stateloc + property
    if (StMax > 0 & StTax > StMax){
      StSub <- StMax - StTax
      stateloc <- stateloc + StSub
      if (stateloc < 0){
        property <- property + stateloc
        stateloc <- 0
      }
    }
    CalcDeduct <- medical + stateloc + property + mortgage + charity + repealed
    Deduct <- StdDeduct
    if (StdDeduct < CalcDeduct) Deduct <- CalcDeduct
    
    income <- wages - deferred
    adjinc <- income - Deduct - Exemptions
    if (adjinc < 0){
      Exemptions <- Exemptions + adjinc
      if (Exemptions < 0){
        Deduct <- Deduct + Exemptions
        Exemptions <- 0
      }
      adjinc <- 0
    }
    if (StdDeduct < CalcDeduct){
      items <- c(0, wages, -deferred, -Exemptions,          0, -CalcDeduct, 0, -medical, -stateloc, -property, -mortgage, -charity, -repealed, 0)
    } else {
      items <- c(0, wages, -deferred, -Exemptions, -StdDeduct,           0, 0, -medical, -stateloc, -property, -mortgage, -charity, -repealed, 0)
    }

    pretax <- calcPretax(td, adjinc)
    fica <- 0
    if (highwage > 0){
      wage1 <- highwage
      wage2 <- wages - highwage
      maxwage1 <- ifelse(wage1 < SSMax, wage1, SSMax)
      maxwage2 <- ifelse(wage2 < SSMax, wage2, SSMax)
      fica <- 0.062 * (maxwage1 + maxwage2) + 0.0145 * (wage1 + wage2)
    }
    # Child Tax Credit refundability
    totCC <- children * ChildCredit
    refCC <- (income - CCMin)  * 0.15
    totCCRef <- CCRef * children
    if (refCC < 0) refCC <- 0
    if (refCC > totCCRef) refCC <- totCCRef
    nonrefCC <- totCC - refCC
    subCC <- floor((income - CCMax)/1000) * 50
    # CTC Phase Out
    if (subCC > 0){
      totCC <- totCC - subCC
      refCC <- totCC
      nonrefCC <- 0
    }
    if (totCC < 0){
      totCC <- 0
      refCC <- 0
      nonrefCC <- 0
    }
    totDC <- dependents * DepCredit
    totPC <- parents * ParCredit
    #inctax <- pretax - totCC - dependents * DepCredit - parents * ParCredit - eitc
    inctax <- pretax - nonrefCC - totDC - totPC
    if (inctax < 0){
      totPC <- totPC + inctax
      if (totPC < 0){
        totDC <- totDC + totPC
        totPC <- 0
        if (totDC < 0){
          nonrefCC <- nonrefCC + totDC
          totDC <- 0
        }
      }
      inctax <- 0
    }
    inctax <- inctax - refCC - eitc
    totCC  <- nonrefCC + refCC
    tottax <- fica + inctax
    items <- c(items, adjinc, 0, pretax, -totCC, -totDC, -totPC, -eitc, 0, inctax, fica, 0, tottax)
    items
  }
  calcTax <- function(td, id, wages){
    taxItems <- getTaxItems(td, id, wages)
    tax <- taxItems[length(taxItems)]
    tax
  }
  genTitle <- function(td1, td2, id){
    titleopt <- input$titleopt
    if (titleopt == "Generate title"){
      filing <- input$filing
      if (filing == "Head of Household") filing = "Household"
      if (filing == "Married filing jointly") filing = "Married"
      items1 <- getTaxItems(td1, id, -1)
      items2 <- getTaxItems(td2, id, -1)
      children <- chknumeric(input$children)
      otherdep <- chknumeric(input$otherdep)
      wages <- chknumeric(input$wages)
      deferred <- chknumeric(input$deferred)
      title <- filing
      if (children == 0) title <- paste0(title, ", no children")
      else if (children == 1) title <- paste0(title, ", 1 child")
      else title <- paste0(title, ", ", children, " children")
      if (otherdep == 1) title <- paste0(title, ", 1 other dependent")
      else if (otherdep > 1) title <- paste0(title, ", ", otherdep, " other dependents")
      #title <- paste0(title, ", wage of ", wages)
      title <- paste0(title, ", ", wages, " income")
      if (deferred > 0) title <- paste0(title, ", ", deferred, " deferred")
      if (items1[6] == 0) title <- paste0(title, ", standard deduction")
      else{
        deductions1 <- items1[8]+items1[9]+items1[10]+items1[11]+items1[12]+items1[13]
        deductions2 <- items2[8]+items2[9]+items2[10]+items2[11]+items2[12]+items2[13]
        repealed <- deductions2 - deductions1
        retained <- -deductions1 - repealed
        if (retained != 0){
          title <- paste0(title, ", ", retained, " itemized")
        }
        if (repealed != 0){
          title <- paste0(title, ", ", repealed, " repealed")
        }
      }
      Title <<- title
    }
    else if (titleopt == "Use title below"){
      Title <<- input$title
    }
    else{
      Title <<- TitleDefault
    }
  }
  clearTaxItems <- function(){
    updateNumericInput(session, "children", value = 0)
    updateNumericInput(session, "otherdep", value = 0)
    updateNumericInput(session, "deferred", value = 0)
    updateNumericInput(session, "highwage", value = 0)
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
  setHouse2017 <- function(){
    if (!input$lockplans){
      updateNumericInput(session, "taxname1",  value = "Current 2017")
      updateNumericInput(session, "taxname2",  value = "House 2017")
    }
  }
  setSenate2018 <- function(){
    if (!input$lockplans){
      updateNumericInput(session, "taxname1",  value = "Current 2018")
      updateNumericInput(session, "taxname2",  value = "Senate 2018")
    }
  }
  setSenate2017_2018 <- function(){
    if (!input$lockplans){
      updateNumericInput(session, "taxname1",  value = "Current 2017")
      updateNumericInput(session, "taxname2",  value = "Senate 2018")
    }
  }
  setConference <- function(){
    if (!input$lockplans){
      updateNumericInput(session, "taxname1",  value = "Current 2018")
      updateNumericInput(session, "taxname2",  value = "Conference")
    }
  }
  # Convert name from ui.R or data files to short name for column
  getShortTaxName <- function(taxname){
    name <- ""
    #name <- taxname #DEBUG
    if (taxname == "Current 2017"){
      name <- "2017"
    }
    else if (taxname == "Current 2018"){
      name <- "2018"
    }
    else if (taxname == "House 2017"){
      name <- "House 2017"
    }
    else if (taxname == "House 2017 w/o Family Credits"){
      name <- "House 2017 w/o FC"
    }
    else if (taxname == "House 2018"){
      name <- "House 2018"
    }
    else if (taxname == "House 2018 w/o Family Credits"){
      name <- "House 2018 w/o FC"
    }
    else if (taxname == "Senate 2018 w/ $1650 Child Credit"){
      name <- "Senate 2018 w/ $1650 CC"
    }
    else if (taxname == "Senate 2018"){
      name <- "Senate 2018"
    }
    else if (taxname == "House 2017b"){
      name <- "House 2017b"
    }
    else if (taxname == "House 2018b"){
      name <- "House 2018b"
    }
    else if (taxname == "Senate 2018a"){
      name <- "Senate 2018a"
    }
    else if (taxname == "Conference"){
      name <- "Conference"
    }
    name
  }
  # Convert name from ui.R to name in data files
  getMidTaxName <- function(longTaxName){
    midTaxName <- longTaxName
    if (longTaxName == "House 2017 w/o Family Credits")     midTaxName <- "House 2017b"
    if (longTaxName == "House 2018 w/o Family Credits")     midTaxName <- "House 2018b"
    if (longTaxName == "Senate 2018 w/ $1650 Child Credit") midTaxName <- "Senate 2018a"
    midTaxName
  }
  parenTaxName2 <- function(){
    name <- ""
    if (input$appendTax){
      name <- getShortTaxName(input$taxname2)
      if (name != ""){
        name <- paste0(" (",name,")")
      }
    }
    name
  }
  observeEvent(input$examples, {
    example <- substr(input$examples, 1, 9)
    #print(paste0("example=",example))
    clearTaxItems()
    clearDeductions()
    resetWageLimits()
    setConference()
    if (example == "Example 1"){
      setHouse2017()
      updateNumericInput(session, "wages", value = 59000)
      updateNumericInput(session, "children", value = 2)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing", value = "Married filing jointly")
      Released <<- c("1582","400","-1182","")
      Title <<- "Example 1 - Family of Four Making $59,000 Per Year"
    }
    else if (example == "Example 2"){
      setHouse2017()
      updateNumericInput(session, "wages", value = 30000)
      updateNumericInput(session, "children", value = 1)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing", value = "Head of Household")
      Released <<- c("","< -1000","< -700","")
      Title <<- "Example 2 - Single Mother Making $30,000 Per Year"
    }
    else if (example == "Example 3"){
      setHouse2017()
      updateNumericInput(session, "wages", value = 48000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing", value = "Single")
      Released <<- c("5173","3872","-1301","")
      Title <<- "Example 3 - Firefighter Making $48,000 Per Year"
    }
    else if (example == "Example 4"){
      setHouse2017()
      updateNumericInput(session, "wages", value = 115000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing", value = "Married filing jointly")
      updateNumericInput(session, "stateloc", value = -7.64347)
      updateNumericInput(session, "property", value = 6900)
      updateNumericInput(session, "mortgage", value = 8400)
      Released <<- c("12180","11050","-1130","")
      Title <<- "Example 4 - New Homeowners Making $115,000 Per Year in a High Tax State"
    }
    else if (example == "Example 5"){
      setSenate2017_2018()
      updateNumericInput(session, "wages", value = 73000)
      updateNumericInput(session, "children", value = 2)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing", value = "Married filing jointly")
      Released <<- c("3683","1499","-2184","nearly 60 percent")
      Title <<- "Example 5 - Family of Four Earning $73,000 Per Year"
    }
    else if (example == "Example 6"){
      setSenate2017_2018()
      updateNumericInput(session, "wages", value = 41000)
      updateNumericInput(session, "children", value = 1)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing", value = "Head of Household")
      Released <<- c("1865","488","-1377","nearly 75 percent")
      Title <<- "Example 6 - Single Parent with One Child Earning $41,000 Per Year"
    }
    else if (example == "Example A"){
      updateNumericInput(session, "wages", value = 25000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Single")
      updateNumericInput(session, "mortgage", value = 6000)
      updateNumericInput(session, "charity",  value = 6000)
      Released <<- c("","","","")
      Title <<- "Example A - Single Person Making $25,000 Per Year with $12,000 in Deductions"
    }
    else if (example == "Example B"){
      updateNumericInput(session, "wages", value = 50000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      updateNumericInput(session, "mortgage", value = 12000)
      updateNumericInput(session, "charity",  value = 12000)
      Released <<- c("","","","")
      Title <<- "Example B - Married Couple Making $50,000 Per Year with $24,000 in Deductions"
    }
    else if (example == "Example C"){
      updateNumericInput(session, "wages", value = 25000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Single")
      updateNumericInput(session, "repealed", value = 15000)
      Released <<- c("","","","")
      Title <<- "Example C - Single Person Making $25,000 Per Year with $15,000 in Repealed Deductions"
    }
    else if (example == "Example D"){
      updateNumericInput(session, "wages", value = 50000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      updateNumericInput(session, "repealed", value = 30000)
      Released <<- c("","","","")
      Title <<- "Example D - Married Couple Making $50,000 Per Year with $30,000 in Repealed Deductions"
    }
    else if (example == "Example E"){
      updateNumericInput(session, "wages", value = 470000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Single")
      updateNumericInput(session, "stateloc", value = -10)
      updateNumericInput(session, "wagemin",  value = 10000)
      updateNumericInput(session, "wagemax",  value = 2000000)
      Released <<- c("","","","")
      #Title <<- "Example E - Single Person with 10 Percent of Income in State & Local Income/Sales Taxes"
      Title <<- "Example E - Single Person with 10 Percent of Income in State and Local Taxes"
    }
    else if (example == "Example F"){
      updateNumericInput(session, "wages", value = 530000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      updateNumericInput(session, "stateloc", value = -10)
      updateNumericInput(session, "wagemin",  value = 10000)
      updateNumericInput(session, "wagemax",  value = 2000000)
      Released <<- c("","","","")
      #Title <<- "Example F - Married Couple with 10 Percent of Income in State & Local Income/Sales Taxes"
      Title <<- "Example F - Married Couple with 10 Percent of Income in State and Local Taxes"
    }
    else if (example == "Example G"){
      updateNumericInput(session, "wages", value = 25000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 1)
      updateNumericInput(session, "filing",   value = "Single")
      Released <<- c("","","","")
      Title <<- "Example G - Single Person Making $25,000 Per Year with 1 Non-Child Dependent"
    }
    else if (example == "Example H"){
      updateNumericInput(session, "wages", value = 50000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 2)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      Released <<- c("","","","")
      Title <<- "Example H - Married Couple Making $50,000 Per Year with 2 Non-Child Dependents"
    }
    else if (example == "Example I"){
      updateNumericInput(session, "wages", value = 25000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 2)
      updateNumericInput(session, "filing",   value = "Single")
      Released <<- c("","","","")
      Title <<- "Example I - Single Person Making $25,000 Per Year with 2 Non-Child Dependent"
    }
    else if (example == "Example J"){
      updateNumericInput(session, "wages", value = 50000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 4)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      Released <<- c("","","","")
      Title <<- "Example J - Married Couple Making $50,000 Per Year with 4 Non-Child Dependents"
    }
    else if (example == "Example K"){
      updateNumericInput(session, "wages", value = 36000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Single")
      updateNumericInput(session, "mortgage", value = 6000)
      updateNumericInput(session, "charity",  value = 6000)
      Released <<- c("","","","")
      Title <<- "Example K - Single Person Making $36,000 Per Year with $12,000 in Deductions"
    }
    else if (example == "Example L"){
      updateNumericInput(session, "wages", value = 72000)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      updateNumericInput(session, "mortgage", value = 12000)
      updateNumericInput(session, "charity",  value = 12000)
      Released <<- c("","","","")
      Title <<- "Example L - Married Couple Making $72,000 Per Year with $24,000 in Deductions"
    }
    else if (example == "Example M"){
      updateNumericInput(session, "wages", value = 30000)
      updateNumericInput(session, "highwage", value = 30000)
      updateNumericInput(session, "deferred", value = 2600)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Single")
      Released <<- c("4331","3953","-379","-9")
      Title <<- "Example M - Single, $30,000, no kids"
    }
    else if (example == "Example N"){
      updateNumericInput(session, "wages", value = 52000)
      updateNumericInput(session, "highwage", value = 52000)
      updateNumericInput(session, "deferred", value = 4000)
      updateNumericInput(session, "children", value = 2)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Head of Household")
      Released <<- c("5198","3306","-1892","-36")
      Title <<- "Example N - Single, $52,000, 2 kids"
    }
    else if (example == "Example O"){
      updateNumericInput(session, "wages", value = 75000)
      updateNumericInput(session, "highwage", value = 75000)
      updateNumericInput(session, "deferred", value = 5500)
      updateNumericInput(session, "children", value = 0)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Single")
      Released <<- c("16104","14327","-1777","-11")
      Title <<- "Example O - Single, $75,000, no kids"
    }
    else if (example == "Example P"){
      updateNumericInput(session, "wages", value = 85000)
      updateNumericInput(session, "highwage", value = 85000)
      updateNumericInput(session, "deferred", value = 5500)
      updateNumericInput(session, "children", value = 2)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      Released <<- c("11035","8782","-2254","-20")
      Title <<- "Example P - Married, $85,000, 2 kids"
    }
    else if (example == "Example Q"){
      updateNumericInput(session, "wages", value = 165000)
      updateNumericInput(session, "highwage", value = -57.57575758) # 100*95000/165000
      updateNumericInput(session, "deferred", value = -12.12121212) # 100*20000/165000
      updateNumericInput(session, "children", value = 2)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      updateNumericInput(session, "stateloc", value = -5)
      updateNumericInput(session, "property", value = -2.060606061) # 100*3400/165000
      updateNumericInput(session, "mortgage", value = -5.76969697) # 100*9520/165000
      updateNumericInput(session, "charity",  value = -2.5)
      Released <<- c("29345","27122","-2224","-8")
      Title <<- "Example Q - Married, $165,000, 2 kids, itemizing"
    }
    else if (example == "Example R"){
      updateNumericInput(session, "wages", value = 325000)
      updateNumericInput(session, "highwage", value = -76.92307692) # 100*250000/325000
      updateNumericInput(session, "deferred", value = -11.38461538) # 100*37000/325000
      updateNumericInput(session, "children", value = 3)
      updateNumericInput(session, "otherdep", value = 0)
      updateNumericInput(session, "filing",   value = "Married filing jointly")
      updateNumericInput(session, "stateloc", value = -5)
      updateNumericInput(session, "property", value = -3.076923077) # 100*10000/325000
      updateNumericInput(session, "mortgage", value = -6.892307692) # 100*22400/325000
      updateNumericInput(session, "charity",  value = -2.5)
      #updateNumericInput(session, "wagemin",  value = 10000)
      updateNumericInput(session, "wagemax",  value = 400000)
      Released <<- c("71629","64456","-7173","-10")
      Title <<- "Example R - Married, $325,000, 3 kids, itemizing"
    }
    TitleDefault <<- Title
  })
  output$taxPrint <- renderPrint({
    filing <- input$filing
    if (filing == "Head of Household") filing = "Household"
    if (filing == "Married filing jointly") filing = "Married"
    taxname1 <- getMidTaxName(input$taxname1)
    taxname2 <- getMidTaxName(input$taxname2)
    taxdef2 <- getTaxdef(taxname2, filing)
    taxdef1 <- getTaxdefAdj(taxname1, filing, taxdef2)
    incdef  <- getIncdef()
    genTitle(taxdef1, taxdef2, incdef)
    taxes1 <- calcTax(taxdef1, incdef, -1)
    taxes2 <- calcTax(taxdef2, incdef, -1)
    Names <- c(input$taxname1, input$taxname2, "Change", "% Change")
    Taxes <- c(taxes1, taxes2, taxes2-taxes1, 100*(taxes2-taxes1)/taxes1)
    wages <- as.numeric(incdef["wages"])
    aftertax1 <- wages-taxes1
    aftertax2 <- wages-taxes2
    AfterTax <- c(aftertax1, aftertax2, aftertax2-aftertax1, 100*(aftertax2-aftertax1)/aftertax1)
    Taxes    <- numformat(Taxes)
    AfterTax <- numformat(AfterTax)
    df <- data.frame(Names, Taxes, Released, "After_Tax"=AfterTax)
    cat("<h4>Comparison of Taxes</h4>")
    cat("<pre>")
    #cat(paste0(filing,", ",input$children," children, ",input$otherdep," dependents, ",input$wages," in wages\n\n"))
    cat(paste0(Title, "\n\n"))
    print(df)
    eitc1 <- calcEITC(taxdef1[["EITC"]], input$wages, input$children, input$filing)
    eitc2 <- calcEITC(taxdef2[["EITC"]], input$wages, input$children, input$filing)
    if (eitc1 > 0 | eitc2 > 0){
      cat(paste0("\nNote: Taxes for plans include EITCs of $", eitc1, " and $", eitc2, ", respectively.\n"))
    }
    cat("</pre>")
  })
  taxdata <- reactive({
    filing <- input$filing
    if (filing == "Head of Household") filing = "Household"
    if (filing == "Married filing jointly") filing = "Married"
    taxname1 <- getMidTaxName(input$taxname1)
    taxname2 <- getMidTaxName(input$taxname2)
    wages <- seq(chknumeric(input$wagemin), chknumeric(input$wagemax,200000), chknumeric(input$wagestep,1000))
    df <- data.frame(wages)
    df$taxes1 <- 0
    df$taxes2 <- 0
    taxdef2 <- getTaxdef(taxname2, filing)
    taxdef1 <- getTaxdefAdj(taxname1, filing, taxdef2)
    incdef  <- getIncdef()
    for (i in 1:length(df$wages)){
      df$taxes1[i] <- calcTax(taxdef1, incdef, wages[i])
      df$taxes2[i] <- calcTax(taxdef2, incdef, wages[i])
    }
    if (chknumeric(input$wages) != 0){
      cat(file=stderr(), paste0(input$taxname1,"|",input$taxname2,"|",input$examples,"#",
                                input$filing,"|", input$children,"|",input$otherdep,"|",input$wages,"#",
                                input$medical,"|",input$stateloc,"|",input$property,"|",input$mortgage,"|",input$charity,"|",input$repealed,"#",
                                input$wagemin,"|",input$wagemax, "|",input$wagestep,"|",input$taxcutmin,"|",input$taxcutmax,"\n"))
    }
    genTitle(taxdef1, taxdef2, incdef)
    # if (last_example == input$examples){
    # } else {
    #   last_example <<- input$examples # skip since this will rerun after example sets parameters
    # }
    return(df)
  })
  output$taxPlot <- renderPlot({
    df <- taxdata()
    df$taxcut <- 100 * (df$taxes1 - df$taxes2) / df$taxes1
    df$taxcut[df$taxes == 0] <- NA
    df$taxcut[df$taxes1 <= 0 | df$taxes2 <= 0] <- NA
    df$taxcut[df$taxcut < input$taxcutmin | df$taxcut > input$taxcutmax] <- NA
    plot(df$wages, df$taxcut, xlab = "Wages", ylab = "Taxcut (percent)")
    title(main = paste0(Title,parenTaxName2()))
    grid(col = "lightgray")
    abline(v = input$wages, col = "red")
  })
  output$taxPlotDollars <- renderPlot({
    df <- taxdata()
    df$taxcut <- df$taxes1 - df$taxes2
    #df$taxcut[df$taxes1 <= 0 | df$taxes2 <= 0] <- NA
    #df$taxcut[df$taxcut < input$taxcutmin | df$taxcut > input$taxcutmax] <- NA
    plot(df$wages, df$taxcut, xlab = "Wages", ylab = "Taxcut (dollars)")
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
    if (filing == "Head of Household") filing = "Household"
    if (filing == "Married filing jointly") filing = "Married"
    taxname1 <- getMidTaxName(input$taxname1)
    taxname2 <- getMidTaxName(input$taxname2)
    td2 <- getTaxdef(taxname2, filing)
    td1 <- getTaxdefAdj(taxname1, filing, td2)
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
    df <- data.frame(ss, rr1, rr2, Diff)
    colnames(df) = c("Start", getShortTaxName(taxname1), getShortTaxName(taxname2), "Change")
    cat("<h4>Comparison of Brackets</h4>")
    cat("<pre>")
    print(df)
    cat("</pre>")
    Rules  <- c("Exemption Amount",
                "Standard Deduction",
                "Child Credit",
                "Dependent Credit",
                "Parent Credit",
                "CTC Refundable",
                "CTC Phase In",
                "CTC Phase Out",
                "Max State Deduction",
                "SS Wage Ceiling")
    Rule1 <- c(td1$Exempt, td1$StdDeduct, td1$ChildCredit, td1$DepCredit, td1$ParCredit, td1$CCRef, td1$CCMin, td1$CCMax, td1$StMax, td1$SSMax)
    Rule2 <- c(td2$Exempt, td2$StdDeduct, td2$ChildCredit, td2$DepCredit, td2$ParCredit, td2$CCRef, td2$CCMin, td2$CCMax, td2$StMax, td2$SSMax)
    RDiff <- Rule2 - Rule1
    rdf <- data.frame(Rules, Rule1, Rule2, RDiff)
    colnames(rdf) = c("Tax_Rule", getShortTaxName(taxname1), getShortTaxName(taxname2), "Change")
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
    ddf <- data.frame(Deducts, Deduct1, Deduct2)
    colnames(ddf) = c("Deduction", getShortTaxName(taxname1), getShortTaxName(taxname2))
    cat("<h4>Comparison of Deductions (0=not deductible, 1=deductible)</h4>")
    cat("<pre>")
    print(ddf)
    cat("</pre>")
    EITCdesc <- c("Wage1", "Wage2", "Wage3", "Slope1", "Slope2", "Maximum_Credit")
    eechildren <- min(input$children, 3)
    ee1 <- eitcdefs[eitcdefs$Name == as.character(td1["EITC"]) & as.numeric(eitcdefs$C) == eechildren,]
    ee2 <- eitcdefs[eitcdefs$Name == as.character(td2["EITC"]) & as.numeric(eitcdefs$C) == eechildren,]
    EITCval1 <- c(ee1$Wage1, ee1$Wage2 + ee1$Madd, ee1$Wage3 + ee1$Madd,
                  ee1$Per1, ee1$Per2, ee1$Ymax)
    EITCval2 <- c(ee2$Wage1, ee2$Wage2 + ee2$Madd, ee2$Wage3 + ee2$Madd,
                  ee2$Per1, ee2$Per2, ee2$Ymax)
    ddf2 <- data.frame(EITCdesc, EITCval1, EITCval2)
    colnames(ddf2) = c("Description", getShortTaxName(taxname1), getShortTaxName(taxname2))
    cat("<h4>Comparison of Earned Income Tax Credit Parameters</h4>")
    cat("<pre>")
    print(ddf2)
    cat("\n")
    cat("    0 <= wage <  Wage1: EITC = Slope1 * wage\n")
    cat("Wage1 <= wage <= Wage2: EITC = Maximum_Credit\n")
    cat("Wage2 <  wage <= Wage3: EITC = Slope2 * (Wage3 - wage)")
    cat("</pre>")
    cat("<pre>")
    cat("Sources: <A HREF=\"https://waysandmeans.house.gov/wp-content/uploads/2017/11/WM_TCJA_TaxPayerExamples.pdf\">Taxpayer Examples from the Committee on Ways and Means</A> (House).\n")
    cat("         <A HREF=\"https://waysandmeans.house.gov/wp-content/uploads/2017/10/WM_TCJA_PolicyOnePagers.pdf\">The Tax Cuts & Jobs Act - Communications and Policy Details</A> (House).\n")
    cat("         <A HREF=\"https://www.finance.senate.gov/imo/media/doc/11.9.17%20Chairman's%20Mark.pdf\">Description of the Chairman's Mark of the \"Tax Cuts and Jobs Act\"</A> (Senate).\n")
    cat("<br>")
    cat("Notes: Taxes in this application are calculated using the above values.\n")
    cat("       Any items not shown above (such as the phase out of the child)\n")
    cat("       credit are not included in the calculations.\n")
    cat("       Blog post on this application can be found at <A HREF=\"http://usbudget.blogspot.com/2017/11/the-problems-with-taxpayer-examples.html\">this link</A>.")
    cat("</pre>")
  })
  output$taxCalcPrint <- renderPrint({
    filing <- input$filing
    if (filing == "Head of Household") filing = "Household"
    if (filing == "Married filing jointly") filing = "Married"
    taxname1 <- getMidTaxName(input$taxname1)
    taxname2 <- getMidTaxName(input$taxname2)
    taxdef2 <- getTaxdef(taxname2, filing)
    taxdef1 <- getTaxdefAdj(taxname1, filing, taxdef2)
    incdef  <- getIncdef()
    genTitle(taxdef1, taxdef2, incdef)
    taxItemNames <- c(
      "---------------------------",
      "Wages, salaries, tips, etc.",
      "Tax-deferred contributions",
      "Exemptions",
      "Standard deductions",
      "Itemized deductions",
      "---------------------------",
      "Medical",
      "State and local taxes",
      "Real estate taxes",
      "Home mortgage interest",
      "Charity",
      "Misc. repealed deductions",
      "---------------------------",
      "Taxable income",
      "---------------------------",
      "Tax on taxable income",
      "Child credit",
      "Other dependent credit",
      "Parent credit",
      "Earned income tax credit",
      "---------------------------",
      "Income tax",
      "Payroll tax",
      "---------------------------",
      "Total tax"
    )
    getTaxItems1 <- getTaxItems(taxdef1, incdef, -1)
    getTaxItems2 <- getTaxItems(taxdef2, incdef, -1)
    getTaxChange <- getTaxItems2 - getTaxItems1
    taxItems1 <- numformat(getTaxItems1)
    taxItems2 <- numformat(getTaxItems2)
    taxChange <- numformat(getTaxChange)
    taxItems1[c(1,7,14,16,22,25)] <- "--------"
    taxItems2[c(1,7,14,16,22,25)] <- "--------"
    taxChange[c(1,7,14,16,22,25)] <- "--------"
    df <- data.frame(taxItemNames, taxItems1, taxItems2, taxChange)
    if (df$taxItems1[24] == 0){
      df <- df[1:23,]
    }
    colnames(df) = c("Tax Plan", getShortTaxName(taxname1), getShortTaxName(taxname2), "Change")
    cat("<pre>")
    cat(paste0(Title,"\n\n"))
    print(df)
    cat("</pre>")
  })
})
