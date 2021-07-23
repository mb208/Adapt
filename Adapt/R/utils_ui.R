
#### UI utilities ####
#### Creating list of UIs for distribution inputs ####
get_dist_ui <- function(dist) {
  #### Returns Shiny Input 
  switch(
    dist,
    "gaussian" = tags$div(
      h4(strong("Gaussian Parameterization")),
      numericInput("guass_mu", "Mean", value = 0),
      numericInput("guass_sd", "SD", value = 1)
    ),
    
    "bernoulli" = tags$div(
      h4(strong("Bernoulli Parameterization")),
      numericInput("bern_p", "Probability", value = .5)
    ),
    
    "binomial" = tags$div(
      h4(strong("Binomial Parameterization")),
      numericInput("bin_n", "Size", value = 5),
      numericInput("bin_p", "Probability", value = .5)
    ),
    "gamma"   =  tags$div(
      h4(strong("Gamma Parameterization")),
      numericInput("gamma_s", "Shape", value = 1),
      numericInput("gamma_r", "Rate", value =  1)
    )
    
  )
}


err_dist_ui <- function(dist) {
  
  switch (dist,
          "gaussian" = tags$div(
            h4(strong("Gaussian Error")),
            numericInput("guass_err_sd", 
                         "SD",
                         value = 1)
          ),
          "double exponential" = tags$div(
            h4(strong("Double Exponential Error")),
            numericInput("dexp_err_scale",
                         "Scale",
                         value = 1))
  )
}

create_btn <- function(x) {
  paste0(
    '<button class="btn btn-default action-button btn-info action_button" id="select_',
    x, 
    '" type="button" onclick=get_id(this.id)></button>'
  )
}

## Taken from: https://towardsdatascience.com/heres-how-to-spice-up-your-shiny-app-84866ccb69dd
set_html_breaks <- function(n) {
  HTML(strrep(br(),n))
}