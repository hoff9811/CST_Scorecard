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
library(shinydashboard)
library(tidyverse)
library(plotly)
library(bs4Dash)
library(shinyLP)

source("CST_welcome_page.R")
source("CST_Parameters.R")
source("CST_Simulation.R")
source("CST_Commercial.R")
source("CST_Output.R")
source("CST_ScorecardInputs.R")
source("CST_Dashboard.R")


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
      primary = "#3c8dbc", secondary = "#D2D6DE"
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
        status = "lightblue"
      ),
      
      # left sidebar
      sidebar = bs4DashSidebar(
        skin = "light",
        status = "lightblue",
        title = "Risk Rating",
        #brandColor = "lightblue",
        #elevation = 3,
        #opacity = 0.8,
        
        # left sidebar menu
        bs4SidebarMenu(id="tabs",
          bs4SidebarMenuItem(
            "Home",
            tabName = "home",
            icon = icon('globe')
          ),
          bs4SidebarMenuItem(
            "Risk Rating Inputs",
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
      FudgeSimulations(input$prop_val, input$loan_term, 0.045, 0.1) %>% 
      SimulationPlot() +
      labs(y = "Simulated Property Values") +
      geom_line(color = "light blue",   size = 0.2, alpha = 0.7) +
      geom_point(data = . %>% filter(date == min(date), sim_id == 1), 
                 y = input$prop_val, color = "dark blue", size = 3) +
      coord_flip()
    
    incProgress(1/n, detail = paste("Doing part", 8))
    
    f_const_cost_sim <- 
      FudgeSimulations(input$const_cost, input$loan_term, 0.045, 0.06) %>% 
      SimulationPlot() +
      labs(y = "Simulated Construction Costs") +
      geom_line(color = "light green", size = 0.2, alpha = 0.7) +
      geom_point(data = . %>% filter(date == min(date), sim_id == 1),
                 color = "dark green", size = 3) +
      coord_flip()
    

    
    output$myValue <-
      renderPrint({
        scales::percent(v$Final_PD, accuracy = 0.01)
      })
    
    output$model_score <- renderbs4ValueBox({
      if(rating_output[[2]] == "Pass"){
        bs4ValueBox(tags$h3(scales::percent(v$Final_PD, accuracy = 0.01)), 
                    strong("Model Score"), icon = icon("calculator"), color = "lime")
      } else {
        bs4ValueBox(tags$h3(scales::percent(v$Final_PD, accuracy = 0.01)), 
                    strong("Model Score"), icon = icon("calculator"), color = "orange")
      }
      
     
    })
    incProgress(1/n, detail = paste("Doing part", 9))
    
    output$rating <- renderbs4ValueBox({
      if(rating_output[[2]] == "Pass"){
        bs4ValueBox(tags$h3(rating_output[1]), 
                    strong("Risk Rating"), icon = icon("chart-simple"), color = "lime")
      } else {
        bs4ValueBox(tags$h3(rating_output[1]), 
                    strong("Risk Rating"), icon = icon("chart-simple"), color = "orange")
      }
    })
    
    output$pass_fail <- renderbs4ValueBox({
      if(rating_output[[2]] == "Pass"){
        bs4ValueBox(tags$h3(rating_output[2]), 
                    strong("OCC Rating"), icon = icon("pen"), color = "lime")
      } else {
        bs4ValueBox(tags$h3(rating_output[2]), 
                    strong("OCC Rating"), icon = icon("pen"), color = "orange")
      }
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
      bs4SidebarMenuSubItem("Risk Assessment", tabName = "dashboard", icon = icon("chart-simple"))
  })

  # Go to the risk assessment tab
  bs4Dash::updateTabItems(session, inputId = "tabs", selected = "dashboard")
  })

  
  
}


# Run the application
shinyApp(ui = app_ui(), server = server)
