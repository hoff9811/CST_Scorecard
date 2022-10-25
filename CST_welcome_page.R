#' welcome UI Function
#'
#
col_6 <- function(...){
  shiny::column(6, ...)
}

mod_welcome_ui <- function(){
  tagList(
    bs4Jumbotron(
      title = "CRE Construction Risk Rating Scorecard.",
      lead = "This is a Shiny application that rates Construction & Development loans using a simulation-based model. The scorecard simulates property value, construction costs, and interest rate over the life of the loan based on historical volatilities.", #The risk rating uses a threshold-appraoch, where the rating is assigned based on the number of simulations where the simulated values cross a threshold. Once a rating is generated, the simulation can be drilled into using the risk assessment dashboard. ",
      #skin = "dark",
      btnName = "App GitHub Repository",
      href = "https://github.com/hoff9811/hoff9811.github.io"
    ),
    fluidRow(
      col_6(
        bs4UserCard(
          title = bs4UserDescription(
            title = "Ryan Hoffman",
            subtitle = "Application Developer",
            image = "https://raw.githubusercontent.com/hoff9811/hoff9811.github.io/master/images/IMG_1374-modified.png",
            type = 2
          ),
          status = "gray",
          width = 12,
          maximizable = TRUE,
          bs4ListGroup(
                      width = 12,
                      type = "action",
                      bs4ListGroupItem(
                      "Github Portfolio: @hoff9811",
                      href = "https://hoff9811.github.io/"
                      ),
                      bs4ListGroupItem(
                        "Email: hoff9811@gmail.com"
                      ),
                      bs4ListGroupItem(
                        "Linkedin",
                        href = "https://linkedin.com/in/ryanlhoffman"
                      )
          )
        )
      )
    )
  )
}


