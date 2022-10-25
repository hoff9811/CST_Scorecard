# Dashboard UI

Dashboard_ui <- function() {
  tagList(
  fluidRow(
    bs4ValueBoxOutput("model_score"),
    bs4ValueBoxOutput("rating"),
    bs4ValueBoxOutput("pass_fail")
  ),
  fluidRow(bs4Card(title = "Other info",
                               solidHeader = T,
                               width = 12,
                               height = "50%",
                               collapsible = TRUE,
                               style = "font-size: 70%;")
  ),
  
  tags$h4(strong("Risk Factor: Loan-to-Value")),
  
  fluidRow(
    bs4Card(title = "Simulated LTVs",
                        solidHeader = T,
                        width = 4,
                        height = "350px",
                        collapsible = TRUE,
                        div(plotlyOutput("ltv_dist"))
                 ),
    bs4Card(title = "Simulated Property Values",
                        solidHeader = T,
                        width = 8,
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
                        width = 4,
                        height = "350px",
                        collapsible = TRUE,
                        div(plotlyOutput("ctv_dist"))
                 ),
    bs4Card(title = "Simulated Construction Costs",
                        solidHeader = T,
                        width = 8,
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