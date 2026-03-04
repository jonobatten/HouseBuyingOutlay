library(shiny)
library(bslib)
library(shinyWidgets)
library(scales) 

ui <- page_sidebar(
  title = "UK House Purchase Affordability Ballparker",
  theme = bs_theme(version = 5, preset = "flatly"),
  
  sidebar = sidebar(
    width = 350,
    
    # 1.Header 1 - type
    h5("Buyer Type"),
    selectInput("buyer_type", label = NULL, 
                choices = c("First Time Buyer" = "ftb", 
                            "Moving Home (Main Residence)" = "mover", 
                            "Second Home / Investment" = "second")),
    
    hr(),
    
    # Header 2 - mortgage
    h5("Mortgage Estimate Variables"),
    autonumericInput("sav", "Current Savings / Equity", value = 110000, currencySymbol = "£", currencySymbolPlacement = "p", decimalPlaces = 0),
    autonumericInput("max_loan", "Maximum Loan Available", value = 400000, currencySymbol = "£", currencySymbolPlacement = "p", decimalPlaces = 0),
    
    layout_columns(
      numericInput("interest", "Interest (%)", value = 4.5, step = 0.1),
      numericInput("term", "Term (Yrs)", value = 25, step = 1)
    ),
    
    hr(),
    
    # 3. Header 3  - legals
    h5("Legal & Other Fees"),
    radioButtons("fee_type", label = NULL, 
                 choices = c("Realistic Estimate (Scales with Price)" = "dynamic",
                             "I have exact quotes" = "custom")),
    
    # Custom fee inputs
    conditionalPanel(
      condition = "input.fee_type == 'custom'",
      autonumericInput("conv", "Conveyancing", value = 1500, currencySymbol = "£", currencySymbolPlacement = "p", decimalPlaces = 0),
      autonumericInput("surv", "Survey", value = 900, currencySymbol = "£", currencySymbolPlacement = "p", decimalPlaces = 0),
      autonumericInput("move", "Moving", value = 800, currencySymbol = "£", currencySymbolPlacement = "p", decimalPlaces = 0)
    )
  ),
  
  # Dashboard Main Panel
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Maximum Affordable Property",
      value = textOutput("max_house_price"),
      theme = "success"
    ),
    value_box(
      title = "Estimated Monthly Repayment",
      value = textOutput("monthly_payment"),
      theme = "primary"
    ),
    value_box(
      title = "Actual Loan-to-Value (LTV)",
      value = textOutput("ltv_display"),
      theme = "info"
    )
  ),
  
  card(
    card_header("Affordability Breakdown"),
    tableOutput("breakdown_table"),
    card_footer("Disclaimer: This is a ballpark estimate and does not constitute financial advice. Dynamic fees scale against property value to reflect UK market realities.")
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Vectorised SDLT Calculator (2026 Rules)
  calc_sdlt <- function(p, type) {
    if (type == "ftb") {
      ifelse(p <= 500000,
             pmax(0, p - 300000) * 0.05,
             pmax(0, pmin(p, 250000) - 125000) * 0.02 +
               pmax(0, pmin(p, 925000) - 250000) * 0.05 +
               pmax(0, pmin(p, 1500000) - 925000) * 0.10 +
               pmax(0, p - 1500000) * 0.12)
    } else if (type == "mover") {
      pmax(0, pmin(p, 250000) - 125000) * 0.02 +
        pmax(0, pmin(p, 925000) - 250000) * 0.05 +
        pmax(0, pmin(p, 1500000) - 925000) * 0.10 +
        pmax(0, p - 1500000) * 0.12
    } else {
      # 5% Surcharge for Second Homes/Investments
      pmax(0, pmin(p, 125000) - 0) * 0.05 +
        pmax(0, pmin(p, 250000) - 125000) * 0.07 +
        pmax(0, pmin(p, 925000) - 250000) * 0.10 +
        pmax(0, pmin(p, 1500000) - 925000) * 0.15 +
        pmax(0, p - 1500000) * 0.17
    }
  }
  
  calculations <- reactive({
    
    # 1. Create a massive vector of property prices to test (£50k to £3m in £500 increments)
    p <- seq(50000, 3000000, by = 500)
    
    # 2. Calculate dynamic or fixed fees for the entire vector
    if (input$fee_type == "dynamic") {
      # Value-based scaling formulas
      # Conveyancing: Base £800 + £1.50 per £1k of property value
      conv_fees <- 800 + (p * 0.0015) 
      
      # Surveys (e.g. RICS Home Survey): Base £400 + £1.00 per £1k of property value
      surv_fees <- 400 + (p * 0.0010)
      
      # Moving: Base £500 + £0.50 per £1k of property value (assuming bigger house = more stuff)
      move_fees <- 500 + (p * 0.0005)
      
    } else {
      # Custom exact quotes
      conv_fees <- rep(input$conv, length(p))
      surv_fees <- rep(input$surv, length(p))
      move_fees <- rep(input$move, length(p))
    }
    
    total_fees <- conv_fees + surv_fees + move_fees
    
    # 3. Apply rules to the vector
    actual_loan <- pmin(p, input$max_loan)
    sdlt <- calc_sdlt(p, input$buyer_type)
    
    # Total savings a buyer must have to afford property 'p'
    required_savings <- (p - actual_loan) + sdlt + total_fees
    
    # 4. Find the highest property price where required savings <= actual savings
    valid_indices <- which(required_savings <= input$sav)
    
    if (length(valid_indices) == 0) return(NULL) 
    
    best_idx <- tail(valid_indices, 1)
    
    # 5. Extract the winning numbers
    best_p <- p[best_idx]
    best_loan <- actual_loan[best_idx]
    
    # 6. Mortgage Repayment Calc (Standard Amortisation)
    r <- (input$interest / 100) / 12
    n <- input$term * 12
    monthly <- if (r > 0) {
      best_loan * (r * (1 + r)^n) / ((1 + r)^n - 1)
    } else {
      best_loan / n
    }
    
    # 7. LTV Calc
    ltv <- (best_loan / best_p) * 100
    
    list(
      price = best_p,
      loan = best_loan,
      deposit = best_p - best_loan,
      ltv = ltv,
      sdlt = sdlt[best_idx],
      conv = conv_fees[best_idx],
      surv = surv_fees[best_idx],
      move = move_fees[best_idx],
      monthly = monthly,
      leftover = input$sav - required_savings[best_idx]
    )
  })
  
  # Render Outputs
  output$max_house_price <- renderText({
    res <- calculations()
    if (is.null(res)) return("Insufficient Funds")
    label_comma(prefix = "£")(res$price)
  })
  
  output$monthly_payment <- renderText({
    res <- calculations()
    if (is.null(res) || res$loan == 0) return("£0")
    label_comma(prefix = "£", accuracy = 1)(res$monthly)
  })
  
  output$ltv_display <- renderText({
    res <- calculations()
    if (is.null(res)) return("N/A")
    paste0(round(res$ltv, 1), "%")
  })
  
  output$breakdown_table <- renderTable({
    res <- calculations()
    
    # FIX: Make the fallback message a 2-column data frame to match align = "lr"
    if (is.null(res)) {
      return(data.frame(
        `Financial Breakdown` = "Status",
        Amount = "Current savings cannot cover minimum deposit and fees.",
        check.names = FALSE
      ))
    }
    
    data.frame(
      `Financial Breakdown` = c("Property Price", "Mortgage Loan Utilised", "Deposit Contributed", 
                                "Stamp Duty (SDLT)", "Conveyancing & Legal", "Survey Costs", 
                                "Moving Costs", "Savings Left Over"),
      Amount = c(
        label_comma(prefix = "£")(res$price),
        label_comma(prefix = "£")(res$loan),
        label_comma(prefix = "£")(res$deposit),
        label_comma(prefix = "£")(res$sdlt),
        label_comma(prefix = "£", accuracy = 1)(res$conv),
        label_comma(prefix = "£", accuracy = 1)(res$surv),
        label_comma(prefix = "£", accuracy = 1)(res$move),
        label_comma(prefix = "£", accuracy = 1)(res$leftover)
      ),
      check.names = FALSE
    )
  }, align = "lr")
}

shinyApp(ui = ui, server = server)