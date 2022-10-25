########################
##Simulation Functions##
########################

getSimulations <- function(sim_parameters,
                           index_rate,
                           const_cost,
                           prop_val)
  
{
  #Simulation Parameters
  num_var <- 3
  num_sim <- 10000
  drift   <- 0
  
  var_corr <- 
    sim_parameters %>% 
    select(-starts_with("scaled"))
    
  #Cholesky Decomp of Correlation Matrix
  var_corr_chol <-  t(chol(var_corr))
  
  set.seed(1)
  #Draws 10k normal random numbers for each variable
  rand_draw     <- matrix(rnorm(num_var*num_sim,0,1),
                          nrow=num_var,
                          ncol=num_sim)
  
  #Creates the correlated random variables
  corr_sim      <- t(var_corr_chol %*% rand_draw)
  
  #Converts the distribution from normal random #s, to uniform random #s
  corr_sim      <- pnorm(corr_sim, 0, 1)
  
  #Variable #1 is change in Construction Cost
  ### Mean = 0, sd = scaled volatility
  sim_var <- matrix(NA, nrow = num_sim, ncol = num_var)
  
  for( i in c(1:3))
  {
    #norm_sim represents the % change over life of loan
    norm_sim   <-   qnorm(corr_sim[,i], 
                          mean = sim_parameters$scaled_mean[i],
                          sd   = sim_parameters$scaled_vols[i])
    #index rate is log normal
    if(i == 2){
      sim_var[,i]  <- exp(norm_sim) * index_rate
    }
    #Constrution costs and prop val are normally distributed
    else if (i == 1) {
      sim_var[,i]  <- exp(norm_sim) * const_cost 
      #sim_var[,i]  <- (1+norm_sim) * const_cost
    }
    else if (i == 3) {
      sim_var[,i]  <- exp(norm_sim) * prop_val 
      # sim_var[,i]  <- (1+norm_sim) * prop_val
    }
    
  }
  sim_var <- data.frame(sim_var)
  colnames(sim_var) <- c("const_cost", "index_rate", "prop_val")
  return(sim_var)
}





