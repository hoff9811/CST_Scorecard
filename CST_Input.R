######################
######################
## CST Scorecard #####
######################

#####Section 1.0###################### 
##### Loading Screen Inputs ##########

# Loan Amount Inputs
input <- list(current_bal = 3000000)

input$tot_commitment    <- 3000000
input$participation_y_n <- 'No'
input$senior_debt       <- 0
input$junior_debt       <- 0
input$pari_debt         <- 0

#Loan Term 
input$loan_term      <- c("2021-9-22", "2025-10-22")
input$assessment_date<- "2021-9-22"
input$ramp_up        <- 6


#Project & Collateral Details
input$region        <- 'Manhattan'
input$prop_val      <- 4000000
input$prop_type     <- 5
#num_units     <- 34
#unit_val      <- 10000
input$add_coll_value<- 500000
input$add_coll_type <- 3


#Budget, Cost, Interest Reserve Details
input$const_cost <- 3500000
input$equity     <- 750000
input$bor_reserve <- 0
input$escrow_y_n  <- "Yes"
input$bank_reserve <- 0


#Interest Rate Details
input$ir_type   <- 0
input$fixed_rate<- 0.035
input$ir_spread <- 0.035 
input$index     <- 0
input$index_rate<- 0.0028

#Remove the NA from input

#####Section 2######################## 
##### Assumption Screen Inputs #######

default_triggers <- rbind(rep(1,10), rep(1,10))
param_input      <-  read.csv("CST_Correlation_Matrix.csv", header = TRUE)


