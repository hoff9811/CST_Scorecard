###########################
### Commercial Model     ##
###   Simulation Logic ####
###########################


Commercial_fixed <- function(ramp_up,
                       tot_commitment,
                       current_bal,
                       add_coll_flag,
                       add_coll_value,
                       prop_val,
                       bor_reserve,
                       fixed_rate,
                       sim_var,
                       loan_life
                       )
{
  
  #Loan Ramp Up Logic
  if(ramp_up <= 0 | (tot_commitment - current_bal) < 0)
    {bal_increase <- 0} else
    {bal_increase <- (tot_commitment - current_bal) / ramp_up}
  
  #  For for the sim() matrix:
  #  1 = Cost, 2 = Interest, 3 = Prop Value
  
  #Adjust Property Value for additional collateral
  adj_coll_val <- ifelse(add_coll_flag < 2, add_coll_value,0)
  adj_prop_val <- prop_val + adj_coll_val
  
  ####################
  ####
  #### Commercial loop
  ####################
    
  #Simulated Variables
  sim_balance  <- current_bal
  sim_reserve  <- bor_reserve
  ir_temp      <- fixed_rate
  prop_val_sim <- sim_var[,3] + adj_coll_val
  
  #Ramp Up Loop based on loan balance, ramp_up time, and interest reserve
  for(j in c(0:(loan_life-1)))
    {
    sim_balance <- ifelse(j < ramp_up,  
                          sim_balance + bal_increase,
                          sim_balance) 
    if(sim_reserve<=0)
      {sim_balance <- sim_balance + sim_balance * (ir_temp / 12)} else
      {sim_reserve <- sim_reserve - sim_balance * (ir_temp / 12)}
  }
  
  #Simulated Ratios
  sim_LTV <- sim_balance / prop_val_sim 
  sim_CTV <- sim_var[,1] / prop_val_sim
  
  sim_ratios <- data.frame(sim_LTV, sim_CTV, prop_val_sim, sim_balance)
 
  return(sim_ratios) 
  
}

CalcDefaultThresholds <- function(prop_val,
                              add_coll_value,
                              add_coll_flag,
                              equity,
                              default_triggers)  {
  #Default Triggers
  ltv_threshold <- default_triggers[1,1]
  ctv_threshold <- default_triggers[1,2]
  
  #Adjust Property Value for additional collateral
  adj_coll_val <- ifelse(add_coll_flag < 2, add_coll_value,0)
  adj_prop_val <- prop_val + adj_coll_val
  
  #Calculates the CTV threshold.
  ctv_threshold <- max(1, equity / adj_prop_val + 1)                            
  
  default_thresholds <- c(ltv_threshold, ctv_threshold)
  
  return(default_thresholds)
}


Commercial_default <- function(sim_ratios,
                               default_triggers,
                               loan_life) 
{
  num_sim <- nrow(sim_ratios)
  
  #Default Triggers
  ltv_threshold <- default_thresholds[1]
  ctv_threshold <- default_thresholds[2]
  
  #Default Criteria #1: Prop Value is below Zero
  default_1 <- ifelse(sim_ratios$prop_val_sim <= 0, 1, 0)
  default_LTV <- ifelse(sim_ratios$sim_LTV > ltv_threshold, 1, 0)
  default_CTV <- ifelse(sim_ratios$sim_CTV > ctv_threshold, 1, 0)
  
  default <- default_1 + default_LTV + default_CTV
  default[default>0] = 1
  default <- sum(default)
    
  Final_PD <- ifelse(loan_life <= 12, 
                     default/num_sim,
                     1 - ((1 - default / num_sim)^ (12/loan_life))
                     )
  
  return(Final_PD)
  
}
  

AssignRating <- function(Final_PD) {
  rating_scale <- c(0, 0.003, 0.005, 0.01, 0.015, 0.025, 0.0375, 0.05, 0.075, 0.1, 0.14, 0.19, 0.25, 0.35, 0.5)
  pd_rating = findInterval(Final_PD, rating_scale)
  occ_rating = ifelse(pd_rating <= 10, "Pass", 
                      ifelse(pd_rating %in% c(11, 12), "Watch",
                             ifelse(pd_rating > 12, "Substandard")))
  output <- list(pd_rating, occ_rating)
  
  return(output)
}
                      


