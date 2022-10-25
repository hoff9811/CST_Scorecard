#######################
##Parameter Functions##
#######################

getLoanLife   <- function(loan_term,
                          assessment_date)
{  #Loan Life
  maturity_date <- loan_term[2]
  loan_life <- seq(as.Date(assessment_date), as.Date(maturity_date),  by = "month")
  loan_life <- length(loan_life) -1 #counting the # of months (exclude current month)
  loan_life <- max(1, loan_life) #floor of one month on loan life

  return(loan_life)
}


getParameters <- function(prop_type,
                          index,
                          loan_life,
                          param_input)
  
  {
  #index & prop type mapping
  index_string <- c("Libor_1M",
                    "Libor_3M",
                    "Treasury",
                    "Prime")
  index_string <- index_string[index+1]
  
  prop_type_string <- c("Multihousing", 
                           "Retail", 
                           "Office",
                           "Industrial",
                           "Other")
  
  prop_type_string <- prop_type_string[prop_type]
  
  #Sell Type Flag for volatility calc
  sell_type_flag <- "Partial_releases"
  
  #Correlations & Volatilities
  sim_parameters <- 
    param_input %>% 
    filter(VarName == "CCI" 
           |VarName == index_string 
           | VarName == prop_type_string) %>% 
    
    #orginal_vol * sell type scaling factor * time-adjustment
    mutate(scaled_vols := 
             Volatility * get(!!sell_type_flag) * sqrt((loan_life-1)/12)
           ) %>%   
    
    #####
    # Add in scaled_mean
    mutate(scaled_mean = ifelse(VarName %in% c(prop_type_string, "CCI"),
                                0.0475231 * sqrt((loan_life-1)/12),
                                0)) %>% 
    #####
  
    select(CCI, !!index_string, !!prop_type_string, scaled_vols, scaled_mean)
  

  return(sim_parameters)
}


