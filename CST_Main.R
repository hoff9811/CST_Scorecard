#main test


default_triggers <- rbind(rep(1,10), rep(1,10))
param_input      <-  read.csv("CST_Correlation_Matrix.csv", header = TRUE)

source("CST_Parameters.R")
source("CST_Simulation.R")
source("CST_Commercial.R")

#Loan Life
loan_life      <- getLoanLife(loan_term, 
                                input$assessment_date)

#Simulation Parameters
sim_parameters <- getParameters(
  as.numeric(input$prop_type),
  as.numeric(input$index),
  loan_life,
  param_input)

#Generate Simulation
sim_var       <- getSimulations(sim_parameters,
                                  input$index_rate,
                                  input$const_cost,
                                  input$prop_val)

#Run Commercial Model
sim_ratios <- Commercial_fixed(input$ramp_up,
                               input$tot_commitment,
                               input$current_bal,
                               input$add_coll_type,
                               input$add_coll_value,
                               input$prop_val,
                               input$bor_reserve,
                               input$fixed_rate,
                               sim_var,         
                               loan_life)

Final_PD <- Commercial_default(input$prop_val,
                               input$add_coll_value,
                               input$add_coll_type,
                               input$equity,
                               sim_ratios,
                               default_triggers)

sim_var %>% 
  ggplot(aes(x= index_rate)) +
  geom_bar(stat = "density") +
  xlim(0,0.05)