
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

create_checkbox <- function(x) {
  paste0(
    '<input type="checkbox"  name="selected" id="select_',
    x, 
    '"onclick=checkboxProperties(this)>'
  )
}



## Taken from: https://towardsdatascience.com/heres-how-to-spice-up-your-shiny-app-84866ccb69dd
set_html_breaks <- function(n) {
  HTML(strrep(br(),n))
}

wght_sum_expr <- function(weights, names) {
  
  # weights <- case_when(weights == -1 ~ "-",
  #                      weights == 1 ~ "" ,
  #                      TRUE ~ as.character(weights))
  
  expr <-  paste0(paste0(weights, '*', names), collapse = " + ")
  expr <- str_replace(expr, "\\+\\s-", "- ")
  return(str_glue("({expr})"))
}


get_expr_mult <- function(f, X, weights=NULL) {
  
  switch(f,
         "multiply" = paste0(X, collapse = "*"),
         "divide"   = paste0(X, collapse = "/"),
         "weighted sum" = wght_sum_expr(weights, X))
  
}

get_expr_unary <- function(f, X, pow=NULL) {
  multi_expr <- str_detect(X,  "\\+|-")
  
  if (any(multi_expr)) {
    # If any of vars is a weighted sum we wrap it in parantheses
    vars[which(multi_expr)] <- str_c("\\left(", vars[which(multi_expr)], "\\right)")
  }
  
  if (f == "^") {
    str_c(X, "^", pow)
  } else if (any(multi_expr)) {
    str_c(f, X)
  } else {
    str_c(f,"(", X, ")")
  }
}


f_to_str <- function(f, X, pow=NULL, weights=NULL){
  
  if (length(X) == 1) {
    str_replace(str_replace(get_expr_unary(f, X, pow), "\\(\\(", "\\("),
                "\\)\\)", "\\)")
    
  } else if (length(X) > 1) {
    get_expr_mult(f, X, weights)
  }
}


laplace_str <- function(location, scale){
  str_interp("Laplace(${location},  ${scale})")
}

guass_str <- function(mean, sd) {
  var <- sd**2
  str_interp("Normal(${mean}, ${var})")
}

bern_str <- function(p) {
  str_interp("Bernoulli(p = ${p})")
}

binomial_str <- function(size, p) {
  str_interp("Binomial(${size}, ${p})")
}

gamma_str <- function(rate, shape) {
  str_interp("Gamma(${rate}, ${shape})")
}





# Generate accordion list element ---

accordion_item <- function(id, label, ...) {
  tagList(tags$div(id = id, class="accordion", label),
          tags$div(class = "panel", ...))
}

# Template to apply JS to new accordion item
run_accordion_js <- function(acc_id) {
  return(str_interp(
    "
              $(document).ready(function() {
              $('#${acc_id}').on('click',  function(e) {
              e.preventDefault();
              $(this).next('.panel').toggle();
                
              }) ;
            })
                 "  
  ) )
}