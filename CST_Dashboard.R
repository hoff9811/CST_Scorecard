# Dashboard UI

gauge_css <- HTML("
.html-widget.gauge svg {
  height: 100%;
  width: 100%;
}")

Dashboard_ui <- function() {
  tagList(
    fluidRow(h1(strong(textOutput("customer_name")))),
    h6(textOutput("loan_officer")),
    h6(textOutput("assessment_date")),
    tags$br(),
  fluidRow(
    bs4ValueBoxOutput("model_score"),
    bs4ValueBoxOutput("rating"),
    bs4ValueBoxOutput("pass_fail")
  ),
  fluidRow(h4(strong("Risk Rating: Summary"))),
  
  fluidRow(
    bs4Card(title = "Customer Overview:",
                               solidHeader = T,
                               width = 12,
                               #height = "400px",
                               collapsible = F,
            fluidRow(
                     #h5(strong("ABC Construction Co.")),
                     col_6(uiOutput("summary_table")),
                     bs4Card(title = "Risk Rating",
                             width = 6,
                             collapsible = F,
                             fluidRow(
                               tags$head(tags$style(gauge_css)),
                             flexdashboard::gaugeOutput("rating_gauge")
                             )
                     )
                   )
                   )
  ),
  fluidRow(h4(strong("Risk Factor: Loan-to-Value"))),
  h6(em("Property Values are simulated across the life of loan and divided by the Loan Balance + Accrued Interest.
                The scorecard compares the simulated LTVs to the default threshold, measuring the proportion of scenarios where the borrower enters default.")),
  
  fluidRow(
    bs4Card(title = "Simulated LTVs",
                        solidHeader = T,
                        width = 5,
                        height = "350px",
                        collapsible = TRUE,
                        div(plotlyOutput("ltv_dist"))
                 ),
    bs4Card(title = "Simulated Property Values",
                        solidHeader = T,
                        width = 7,
                        height = "350px",
                        collapsible = TRUE,
                        fluidRow(col_6(div(plotlyOutput("prop_val_sim"))),
                                 col_6(div(plotlyOutput("prop_val_dist")))
                        )
                 )
  ), # row
  
  fluidRow(h4(strong("Risk Factor: Loan-to-Cost"))),
  h6(em("Construction Costs are simulated across the life of loan and divided by the Loan Balance + Accrued Interest.
                The scorecard compares the simulated LTCs to the default threshold, measuring the proportion of scenarios where the borrower enters default.")),
  
  
  fluidRow(
    bs4Card(title = "Simulated LTCs",
                        solidHeader = T,
                        width = 5,
                        height = "350px",
                        collapsible = TRUE,
                        div(plotlyOutput("ltc_dist"))
                 ),
    bs4Card(title = "Simulated Construction Costs",
                        solidHeader = T,
                        width = 7,
                        height = "350px",
                        collapsible = TRUE,
                        fluidRow(col_6(div(plotlyOutput("const_cost_sim"))),
                                 col_6(div(plotlyOutput("const_cost_dist")))
                        )
                 )
  ), # row
  tags$br()
  )
}