#-----------------
# User Inputs 

ScorecardInputs_ui <- function() {
  # NavList containing all the Quantitative Inputs
  #navlistPanel(
  bs4Dash::tabsetPanel(id = "inputs", type = "tabs", vertical = FALSE,
    #"Quantitative Inputs",
    # Tab #1
    tabPanel(
      "Loan Inputs",
      fluid = TRUE,
      tags$br(),
      fluidRow(
      col_6(
        # Subhead
        tags$h4(strong("Loan Amount Details:")),
        tags$br(),
        shinyWidgets::autonumericInput(
          inputId = "current_bal",
          label = "Current Balance:",
          value = 2000000,
          currencySymbol = "$",
          currencySymbolPlacement = "p",
          decimalPlaces = 2,
          digitGroupSeparator = ",",
          decimalCharacter = ".",
          align = "left"
        ),
        tags$br(),
        shinyWidgets::autonumericInput(
          inputId = "tot_commitment",
          label = "Total Commitment:",
          value = 3000000,
          currencySymbol = "$",
          currencySymbolPlacement = "p",
          decimalPlaces = 2,
          digitGroupSeparator = ",",
          decimalCharacter = ".",
          align = "left"
        ),
        tags$br(),
        shinyWidgets::prettyRadioButtons(
          inputId = "participation_y_n",
          label = "Participation? (Does the Institution hold less than 100% of Total Project Debt?)",
          choices = c(
            "Yes" = 1,
            "No" = 0
          ),
          selected = "No"
        ),
        tags$br(),
        conditionalPanel(
          condition = "input.participation_y_n == 1",
          shinyWidgets::autonumericInput(
            inputId = "senior_debt",
            label = "Participations Total Commitment:",
            value = 0,
            currencySymbol = "$",
            currencySymbolPlacement = "p",
            decimalPlaces = 2,
            digitGroupSeparator = ",",
            decimalCharacter = ".",
            align = "left"
          )
        ),
      ),
      col_6(
        tags$h4(strong(" Loan Term Details:")),
        tags$br(),
        dateRangeInput(
          inputId = "loan_term",
          end = Sys.Date() + 365 * 4,
          label = "Origination Date to Maturity Date:"
        ),
        tags$br(),
        dateInput(
          inputId = "assessment_date",
          label = "Assessment Date (As-of date):"
        ),
        tags$br(),
        sliderInput(
          inputId = "ramp_up",
          label = "Expected number of months from current rating date when loan will reach its highest outstanding balance:",
          max = 36,
          min = 1,
          value = 6
        )
      )
      )
    ),
    # end of tabPanel #1
    
    # Tab #2
    tabPanel(
      "Project Inputs",
      fluid = TRUE,
      tags$br(),
      fluidRow(
      col_6(
        tags$h4(strong(" Project & Collateral Details:")),
        tags$br(),
        shinyWidgets::autonumericInput(
          inputId = "prop_val",
          label = "Estimated Completed Property Value:",
          value = 4000000,
          currencySymbol = "$",
          currencySymbolPlacement = "p",
          decimalPlaces = 2,
          digitGroupSeparator = ",",
          decimalCharacter = ".",
          align = "left"
        ),
        tags$br(),
        selectInput(
          inputId = "prop_type",
          label = "Property Type:",
          choices = list(
            "Multifamily" = 1,
            "Retail" = 2,
            "Office" = 3,
            "Industrial" = 4,
            "Other" = 5
          ),
          selected = 1
        ),
        tags$br(),
        selectInput(
          inputId = "region",
          label = "Property Region:",
          choices = list(
            "Manhattan" = 1,
            "Queens" = 2,
            "Brooklyn" = 3,
            "Bronx" = 4,
            "Staten Island" = 5
          ),
          selected = 1
        )
      ),
      col_6(
        tags$h4(strong("Additional Pledged Collateral Detail:")),
        tags$br(),
        shinyWidgets::prettyRadioButtons(
          inputId = "add_coll_type",
          label = "Additional Collateral?:",
          choices = c(
            "None" = 3,
            "Cash" = 0,
            "Real Estate" = 1,
            "Other" = 2
          ),
          selected = 3
        ),
        tags$br(),
        
        
        ####
        conditionalPanel(
          condition = "input.add_coll_type < 3",
          shinyWidgets::autonumericInput(
            inputId = "add_coll_value",
            label = "Additional Collateral Amount:",
            value = 500000,
            currencySymbol = "$",
            currencySymbolPlacement = "p",
            decimalPlaces = 2,
            digitGroupSeparator = ",",
            decimalCharacter = ".",
            align = "left"
          )
        )
      )
      )
    ),
    # end of tab Panel2
    
    # Tab #3
    tabPanel(
      "Budget / Costs Details",
      fluid = TRUE,
      tags$br(),
      fluidRow(
      col_6(
        tags$h4(strong("Budget & Cost Details:")),
        tags$br(),
        shinyWidgets::autonumericInput(
          inputId = "const_cost",
          label = "Estimated Construction Costs:",
          value = 3500000,
          currencySymbol = "$",
          currencySymbolPlacement = "p",
          decimalPlaces = 2,
          digitGroupSeparator = ",",
          decimalCharacter = ".",
          align = "left"
        ),
        tags$br(),
        shinyWidgets::autonumericInput(
          inputId = "equity",
          label = "Borrower's Equity in Project:",
          value = 750000,
          currencySymbol = "$",
          currencySymbolPlacement = "p",
          decimalPlaces = 2,
          digitGroupSeparator = ",",
          decimalCharacter = ".",
          align = "left"
        )
      ),
      col_6(
             tags$h4(strong("Interest Reserve Details:")),
             tags$br(),
             shinyWidgets::prettyRadioButtons(
               inputId = "escrow_y_n",
               label = "Has the borrower funded an escrow account to cover interest expense?",
               choices = c(
                 "Yes" = 1,
                 "No" = 0
               ),
               selected = "0"
             ),
             tags$br(),
             conditionalPanel(
               condition = "input.escrow_y_n == 1",
               shinyWidgets::autonumericInput(
                 inputId = "bor_reserve",
                 label = "Interest Reserve funded / to be funded by Borrower:",
                 value = 0,
                 currencySymbol = "$",
                 currencySymbolPlacement = "p",
                 decimalPlaces = 2,
                 digitGroupSeparator = ",",
                 decimalCharacter = ".",
                 align = "left"
               ),
               tags$br()
             ),
             
             shinyWidgets::autonumericInput(
               inputId = "bank_reserve",
               label = "Unfunded (current) Interest Reserve allocated by Institution:",
               value = 0,
               currencySymbol = "$",
               currencySymbolPlacement = "p",
               decimalPlaces = 2,
               digitGroupSeparator = ",",
               decimalCharacter = ".",
               align = "left"
             )
      )
      )
    ),
    # end of tab Panel3
    
    # Tab Panel #4
    tabPanel(
      "Interest Rate Details",
      fluid = TRUE,
      tags$br(),
      fluidRow(
        col_6(
             tags$h4(strong("Interest Rate Type Details:")),
             tags$br(),
             shinyWidgets::prettyRadioButtons(
               inputId = "ir_type",
               label = "Fixed or Floating Rate?:",
               choices = c(
                 "Floating" = 1,
                 "Fixed" = 0
               ),
               selected = 0
             ),
             tags$br(),
             
             # if Floating
             conditionalPanel(
               condition = "input.ir_type == 1",
               shinyWidgets::prettyRadioButtons(
                 inputId = "index",
                 label = "Interest Rate Index:",
                 choices = c(
                   "Libor 1M" = 0,
                   "Libor 3M" = 1,
                   "Treasury" = 2,
                   "Prime"    = 3
                 ),
                 selected = 0
               ),
             )
      ),
      
      # If Fixed
      col_6(
             tags$h4(strong("Interest Rate Details:")),
             tags$br(),
             
             conditionalPanel(
               condition = "input.ir_type == 0",
               shinyWidgets::autonumericInput(
                 inputId = "fixed_rate",
                 label = "Fixed Interest Rate:",
                 value = 0.035,
                 currencySymbol = "",
                 currencySymbolPlacement = "p",
                 decimalPlaces = 2,
                 digitGroupSeparator = ",",
                 decimalCharacter = ".",
                 align = "left"
               )
             ),
             conditionalPanel(
               condition = "input.ir_type == 1", 
               
               shinyWidgets::autonumericInput(
                 inputId = "index_rate",
                 label = "Current Index Rate:",
                 value = 0.01,
                 currencySymbol = "",
                 currencySymbolPlacement = "p",
                 decimalPlaces = 2,
                 digitGroupSeparator = ",",
                 decimalCharacter = ".",
                 align = "left"
               ),
               tags$br(),
               
               shinyWidgets::autonumericInput(
                 inputId = "ir_spread",
                 label = "Interest Rate Spread:",
                 value = 0.035,
                 currencySymbol = "",
                 currencySymbolPlacement = "p",
                 decimalPlaces = 2,
                 digitGroupSeparator = ",",
                 decimalCharacter = ".",
                 align = "left"
               )
             )
      )
      )
    ),
    # Tab Panel 5
    tabPanel(
      bs4Dash::actionButton(inputId = "AB", label = "Calculate PD (%)", status = "primary")
    )
  )
}