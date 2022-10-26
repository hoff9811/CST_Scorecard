#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyWidgets)
require(flexdashboard)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(bs4Dash)
library(shinyLP)
library(fresh)
library(lubridate)

source("CST_welcome_page.R")
source("CST_Parameters.R")
source("CST_Simulation.R")
source("CST_Commercial.R")
source("CST_Output.R")
source("CST_ScorecardInputs.R")
source("CST_Dashboard.R")
source("CST_AboutUs.R")
source("GaugeOutput.R")


default_triggers <- rbind(rep(1, 10), rep(1, 10))
param_input <- read.csv("CST_Correlation_Matrix.csv", header = TRUE)

#### Code
##
##
##
##

# Define UI for application that draws a histogram


app_ui <- function() {
  theme <- create_theme(
    bs4dash_status(
      primary = "#3c8dbc", secondary = "#D2D6DE", success = "#01ff70", warning = "#ff851b", danger = "#dc3545"
    )
  )

  tagList(
    bs4DashPage(
      freshTheme = theme,
      title = "Construction & Development Risk Scorecard",
      #sidebar_collapsed = FALSE,
      
      # navigation bar
      header = bs4DashNavbar(
        title = NULL,
        skin = "light",
        status = "grey"
      ),
      
      # left sidebar
      sidebar = bs4DashSidebar(
        skin = "dark",
        status = "grey",
        title = "Risk Rating",
        elevation = 4,
        collapsed = FALSE,
        minified = TRUE,
        expandOnHover = TRUE,
        fixed = TRUE,
        
        # left sidebar menu
        bs4SidebarMenu(id="tabs",
          bs4SidebarMenuItem(
            "Home",
            tabName = "home",
            icon = icon('globe')
          ),
          bs4SidebarMenuItem(
            "Calculate Risk Rating",
            icon = icon("keyboard"),
            tabName = "inputs"
          ),
          
          bs4Dash::menuItemOutput("dashboard"),
          # bs4SidebarMenuItem("Risk Assessment",
          #                    tabName = "dashboard",
          #                    icon = icon("chart-simple")),
          # 
        
          bs4SidebarMenuItem(
            "Scorecard Overview",
            icon = icon("question"),
            tabName = "aboutus"
          )
        )
      ),
      
      # main body
      body = bs4DashBody(
        bs4TabItems(
          # Home ui ----
          bs4TabItem(
            tabName = "home",
            mod_welcome_ui()
          ),
          
          # measure uic ----
          bs4TabItem(
            tabName = "inputs",
            ScorecardInputs_ui()
          ),
          
          # ourproject module ----
          bs4TabItem(
            tabName = "dashboard",
            Dashboard_ui()
          ),
          bs4TabItem(
            tabName = "aboutus",
            AboutUs_ui()
          )
        )
      ),
      
      # footer
      footer = bs4DashFooter(
        left = a(
          target = "_blank",
          "CRE Construction Risk Rating Scorecard"
        ),
        right = "2022"
      )
    )
  )
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    v <- reactiveValues()
  
  observeEvent(input$AB, {
    withProgress(message = 'Making plot', value = 0, {
      n  <- 10

    # Loan Life
    v$loan_life <- getLoanLife(
      input$loan_term,
      input$assessment_date
    )
    incProgress(1/n, detail = paste("Doing part", 1))
    
    # Simulation Parameters
    v$sim_parameters <- data.frame(getParameters(
      as.numeric(input$prop_type),
      as.numeric(input$index),
      v$loan_life,
      param_input
    ))
    incProgress(1/n, detail = paste("Doing part", 2))
    

    # Generate Simulation
    v$sim_var <- data.frame(
      getSimulations(
        v$sim_parameters,
        input$index_rate,
        input$const_cost,
        input$prop_val
      )
    )
    incProgress(1/n, detail = paste("Doing part", 3))
    


    # Run Commercial Model
    v$sim_ratios <- Commercial_fixed(
      input$ramp_up,
      input$tot_commitment,
      input$current_bal,
      input$add_coll_type,
      input$add_coll_value,
      input$prop_val,
      input$bor_reserve,
      input$fixed_rate,
      v$sim_var,
      v$loan_life
    )
    incProgress(1/n, detail = paste("Doing part", 4))
    
    default_thresholds <- CalcDefaultThresholds(
      input$prop_val,
      input$add_coll_value,
      input$add_coll_type,
      input$equity,
      default_triggers)

    v$Final_PD <- Commercial_default(
      v$sim_ratios,
      default_thresholds,
      v$loan_life
    )
    
    incProgress(1/n, detail = paste("Doing part", 5))
    
    # Output the rating
    rating_output <- AssignRating(v$Final_PD)
    incProgress(1/n, detail = paste("Doing part", 6))
    

    values_dist_plot   <- plotSimValues(v$sim_var, input)
    ratios_dist_plot   <- plotSimRatios(v$sim_ratios, input, default_thresholds)
    incProgress(1/n, detail = paste("Doing part", 7))
    
    f_prop_val_sim <- 
      FudgeSimulations(input$prop_val, input$loan_term, v$loan_life, 0.045, 0.1) %>% 
      SimulationPlot() +
      labs(y = "Simulated Property Values") +
      geom_line(color = "#3c8dbc",   size = 0.2, alpha = 0.7) +
      geom_point(data = . %>% filter(date == min(date), sim_id == 1), 
                 y = input$prop_val, color = "#001f3f", size = 3) +
      coord_flip()
    
    incProgress(1/n, detail = paste("Doing part", 8))
    
    f_const_cost_sim <- 
      FudgeSimulations(input$const_cost, input$loan_term, v$loan_life, 0.045, 0.06) %>% 
      SimulationPlot() +
      labs(y = "Simulated Construction Costs") +
      geom_line(color = "#001f3f", size = 0.2, alpha = 0.7) +
      geom_point(data = . %>% filter(date == min(date), sim_id == 1),
                 color = "#3c8dbc", size = 3) +
      coord_flip()
    
    # Summary Table values
    output$customer_name = renderText({input$customer_name})
    output$loan_officer = renderText({paste("Rating Assessment prepared for", input$loan_officer)})
    output$assessment_date= renderText({paste("Assessment Date:", input$assessment_date)})

  
    output$summary_table <- renderUI({
      table_data <-
        tibble("Field" := 
               c("Total Bank Commitment:",
                 "Property Value at Completion:", 
                 "Estimated Cost of Construction:", 
                 "Current LTV:", 
                 "Current CTV:"),
             "User Input" := 
               c(scales::dollar(input$tot_commitment),
                 scales::dollar(input$prop_val),
                 scales::dollar(input$const_cost),
                 scales::percent(input$tot_commitment / input$prop_val),
                 scales::percent(input$const_cost / input$prop_val)
                 )
      )
      
      bs4Table(
        cardWrap = F,
        striped = T,
        table_data
      )
    })

      
      
    
    # Rating Guage
    output$rating_gauge <- 
      flexdashboard::renderGauge({
        flexdashboard::gauge(rating_output[1], min = 1, max = 15, 
                             flexdashboard::gaugeSectors(
                success = c(1, 10), warning = c(11, 13), danger = c(14, 15),
                colors = c("#01ff70", "#ff851b", "#dc3545"))
              )
      })
    
    
    
    value_box_color = ifelse(rating_output[[2]] == "Pass", "lime", 
                              ifelse(rating_output[[2]] == "Watch","orange", "danger"))
    
    
    output$model_score <- renderbs4ValueBox({
        bs4ValueBox(tags$h3(scales::percent(v$Final_PD, accuracy = 0.01)), strong("Model Score"), icon = icon("calculator"), color = value_box_color)
    })
    incProgress(1/n, detail = paste("Doing part", 9))
    
    output$rating <- renderbs4ValueBox({
        bs4ValueBox(tags$h3(rating_output[1]), strong("Risk Rating"), icon = icon("chart-simple"), color = value_box_color)
    })
    
    output$pass_fail <- renderbs4ValueBox({
        bs4ValueBox(tags$h3(rating_output[2]), strong("OCC Rating"), icon = icon("pen"), color = value_box_color)
    })

    output$ltv_dist <- renderPlotly({
      ggplotly(ratios_dist_plot[[1]], height = 300)
      })
      
    output$ctv_dist <- renderPlotly({
      ggplotly(ratios_dist_plot[[2]], height = 300)
    })
    
    output$prop_val_sim <- renderPlotly({
      ggplotly(f_prop_val_sim, height = 300)
    })
    
    output$const_cost_sim <- renderPlotly({
      ggplotly(f_const_cost_sim, height = 300)
    })
    
    output$prop_val_dist <- renderPlotly({
      ggplotly(values_dist_plot[[1]], height = 300)
    })
    
    output$const_cost_dist <- renderPlotly({
      ggplotly(values_dist_plot[[2]], height = 300)
    })
    
   
    incProgress(1/n, detail = paste("Doing part", 10))
    
    }) 
  })
  

  observeEvent(input$AB, {
    # Create the risk assessment tab
    output$dashboard <- bs4Dash::renderMenu({
      bs4SidebarMenuItem("Risk Assessment", tabName = "dashboard", icon = icon("chart-simple"))
  })

  # Go to the risk assessment tab
  bs4Dash::updateTabItems(session, inputId = "tabs", selected = "dashboard")
  })

  
  
}


# Run the application
shinyApp(ui = app_ui(), server = server)
