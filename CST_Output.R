##################################
# Output Display for Shiny App
##################################

# Variables Plot
plotSimValues <- function(sim_var, input) {
  #Property Value
  p1 <-
    sim_var %>% 
    mutate(prop_val = scales::squish(prop_val, quantile(prop_val, c(0.001, 0.999)))) %>% 
    ggplot(aes(x = prop_val)) +
    geom_area(stat = "density", fill = "#3c8dbc", color = "grey40", alpha = 0.3) +
    geom_vline(xintercept = input$prop_val, color = "midnight blue",  size = 1, linetype = "dashed") +
    scale_x_continuous(name = paste0("Simulated Property Values"),
                       labels = scales::dollar) +
    theme_bw() +
    theme(
          plot.title = element_text(size = 9),
          axis.title.x = element_text(face = "bold", size = 9),
          axis.text.x = element_text(angle = 90),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          #panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()) #+
    #labs(title = paste0("Distribution at Maturity (", input$loan_term[2],")"))  
  
  # Construction Costs
  p2 <- 
    sim_var %>% 
    mutate(const_cost = scales::squish(const_cost, quantile(const_cost, c(0.001, 0.999)))) %>% 
    ggplot(aes(x = const_cost)) +
    geom_area(stat = "density", fill = "light green", color = "grey40", alpha = 0.3) +
    geom_vline(xintercept = input$const_cost, color = "dark green",  size = 1, linetype = "dashed") +
    scale_x_continuous(name = paste0("Simulated Construction Costs"),
                       labels = scales::dollar) +
    theme_bw() +
    theme(
          plot.title = element_text(size = 9),
          axis.title.x = element_text(face = "bold", size = 9),
          axis.text.x = element_text(angle = 90),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          #panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()) #+
    #labs(title = paste0("Distribution at Maturity (", input$loan_term[2],")"))
  
  
  # Interest Rates
  p3 <- 
    sim_var %>% 
    mutate(index_rate = scales::squish(index_rate, quantile(index_rate, c(0.001, 0.99)))) %>% 
    ggplot(aes(x = index_rate)) +
    geom_area(stat = "density", fill = "pink", color = "grey40", alpha = 0.3) +
    geom_vline(aes(xintercept = input$index_rate), color = "dark red",  size = 1, linetype = "dashed") +
    scale_x_continuous(name = paste0("Simulated Interest Rates"),
                       labels = scales::percent) +
    theme_bw() +
    theme(
          plot.title = element_text(size = 9),
          axis.title.x = element_text(face = "bold", size = 9),
          axis.text.x = element_text(angle = 90),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          #panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())# +
    #labs(title = paste0("Distribution at Maturity (", input$loan_term[2],")"))  
    
  varplots <- list(p1, p2, p3)
  return(varplots)
}


# Ratios Plot
plotSimRatios <- function(sim_ratios, input, default_thresholds) {
  # LTV
  p1 <-
    sim_ratios %>% 
    mutate(sim_LTV = scales::squish(sim_LTV, quantile(sim_LTV, c(0.001, 0.999)))) %>% 
    ggplot(aes(x = sim_LTV)) +
    geom_area(stat = "density", fill = "#3c8dbc", color = "grey40", alpha = 0.5) +
   # geom_vline(xintercept = input$tot_commitment / input$prop_val, color = "midnight blue", size = 1, linetype = "dashed") +
    geom_vline(xintercept = default_thresholds[1], color = "dark red", size = 1.5) +
    scale_x_continuous(name = paste0("Simulated Loan-to-Values"),
                       labels = scales::percent) +
    theme_bw() +
    theme(axis.title.x = element_text(face = "bold", size = 9),
          axis.text.x = element_text(angle = 90),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          #panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()) + 
    labs(caption = paste0("Values are simulated over full life of loan: Maturity Date = ", input$loan_term[2]))
  
  # CTV
  p2 <- 
    sim_ratios %>% 
    mutate(sim_CTV = scales::squish(sim_CTV, quantile(sim_CTV, c(0.001, 0.999)))) %>% 
    ggplot(aes(x = sim_CTV)) +
    geom_area(stat = "density", fill = "light green", color = "grey40", alpha = 0.5) +
   # geom_vline(xintercept = input$const_cost/ input$prop_val, color = "dark green", size = 1, linetype = "dashed") +
    geom_vline(xintercept = default_thresholds[2], color = "dark red", size = 1.5) +
    scale_x_continuous(name = paste0("Simulated Cost-to-Values"),
                       labels = scales::percent) +
    theme_bw() +
    theme(axis.title.x = element_text(face = "bold", size = 9),
          axis.text.x = element_text(angle = 90),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          #panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()) + 
    labs(caption = paste0("Values are simulated over full life of loan: Maturity Date = ", input$loan_term[2]))
  
  
  varplots = list(p1, p2)
  return(varplots)
}



# Simulation Plots

FudgeSimulations <- function(input_val, loan_term, loan_life, avg, vol) {
  #inputs
  prop_val_input = input_val
  date_start = as.Date(loan_term[1])
  date_end = as.Date(loan_term[2])
  
  loan_life = loan_life / 12
  num_years = ceiling(loan_life)
  remainder = loan_life - floor(loan_life)
  num_sim = 500
  
  
  # Create 10 different "jiggles" to give the appearance of random simulation across years
  sims <- 
    tibble(sim_id = c(1:num_sim)) %>% 
    expand(sim_id, i_year = c(1:num_years)) %>% 
    mutate(rand_draw = pnorm(rnorm(num_sim * num_years)),
           norm_sim  = qnorm(rand_draw, 
                            mean = avg,
                            sd   = vol)) %>% 
    
    # Adjusts the volatility in the last year or partial year
    mutate(norm_sim = ifelse(i_year == num_years & remainder > 0, 
                             qnorm(rand_draw,
                                   mean = (1+avg)^remainder - 1,
                                   sd   = vol * sqrt(remainder)),
                             norm_sim))
  
  # Loop inputs
  df <- tibble(prop_val_input = rep(prop_val_input, num_sim))
  i <- 1
  
  while(i <= num_years) {
    df <-
      df %>% 
      # Assign the Random number to a row
      mutate(i_year = i,
             sim_id = row_number()) %>% 
      left_join(sims, by = c("sim_id", "i_year"))
    
    # "Jiggle" the starting property value by the random number
      if(i == 1) {
        df <- mutate(df, !!paste0("prop_val_", i) := exp(norm_sim) * prop_val_input)
      } else {
        df <- mutate(df, !!paste0("prop_val_", i) := exp(norm_sim) * get(paste0("prop_val_", i - 1)))
      }
    
    # Add in a date id and remove old random number
    df <- df %>%  select(-rand_draw, -norm_sim, -i_year) 
    
    i <- i + 1
  }
  
  # Reshape the data for plotting
  df <- 
    df %>% 
    pivot_longer(starts_with("prop"), 
                 names_to = "series", 
                 values_to = "prop_val", 
                 names_prefix = "prop_val_") %>% 
    mutate(date = if_else(series == "input", date_start,
                         if_else(series == as.character(num_years), date_end, 
                                date_start + years(series))
                         )
    )
  
  return(df)
}


SimulationPlot <- function(df) {
  p1 <-
    df %>%
    mutate(prop_val = scales::squish(prop_val, quantile(prop_val, c(0.005, 0.995)))) %>% 
    
    ggplot(aes(x = date, y = prop_val, group = sim_id)) +
    #geom_line(color = "grey70", size = 0.5) +
    scale_y_continuous(labels = scales::dollar) +
    labs(x = "Life of Loan") +
    theme_bw() +
    theme(
          axis.title.x = element_text(face = "bold", size = 9),
          axis.title.y = element_text(face = "bold", size = 9),
          axis.text.x = element_text(angle = 90),
          panel.background=element_blank(),
          panel.border=element_blank(),
          #panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
  
  return(p1)  
}



