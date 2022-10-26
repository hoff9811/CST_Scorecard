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
  tags$h4(strong("Risk Rating Summary:")),
  
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
  
  tags$h4(strong("Risk Factor: Loan-to-Value")),
  
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
  
  tags$h4(strong("Risk Factor: Cost-to-Value")),
  
  fluidRow(
    bs4Card(title = "Simulated CTVs",
                        solidHeader = T,
                        width = 5,
                        height = "350px",
                        collapsible = TRUE,
                        div(plotlyOutput("ctv_dist"))
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