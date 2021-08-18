#### Function to Apply different operators to input variable(s) ####


# Location-Scale model computations ----
n_ary_operator <- function(operator, var) {
  ## Make sure there are at least 2 variables
  stopifnot(length(var) > 1)
  
  ## Make sure operator is in list of operators
  # stopifnot(operator %in% c("+", "-", "x", "/"))
  
  switch(operator,
         # "+" = reduce(var, `+`),
         # "-" = reduce(var, `-`),
         "multiply" = reduce(var, `*`),
         "divide" = reduce(var, `/`))
}

unary_operator <- function(operator, var, pow=1) {
  ## Coniditions for power to work
  stopifnot((operator == "^" & exists("pow")) || operator != "^")
  stopifnot((operator == "^" & is.numeric(pow)) || operator != "^")
  
  switch(operator,
         "exp" = exp(var),
         "ln" = log(var),
         "^" = var^pow,
         "None" = I(var)
  )
}


# Sample Input distribution  ----
sample_dist <-  function(n, params) {
  switch(
    input$data_dist,
    "Gaussian"  = rnorm(n,
                        mean = params$guass_mu,
                        sd = params$guass_sd), 
    "Bernoulli" =  as.integer(rbernoulli(n,
                                         p = params$bern_p)), 
    
    "Binomial"  = rbinom(n,
                         size = params$bin_n,
                         p    = params$bin_p),
    
    "Gamma"     = rgamma(n,
                         shape = input$gamma_s,
                         rate = input$gamma_r)
    )
}


# Update Simulate Parameters ----

update_sim_params <- function(sim_params, cond_dist_step, num_vars, sim_data, mean=NULL, log_variance=NULL, error=NULL) {
  sim_params$num_vars <- num_vars
  sim_params$cond_dist_step <- cond_dist_step
  sim_params$sim_data <- sim_data
  sim_params$param_data <- sim_data # Used for calculating conditional mean / var
  sim_params$var_cnt <- 1
  sim_params$var_choices <- names(sim_params$sim_data)
  names(sim_params$var_choices) <- sim_params$var_choices
  sim_params$expression_tbl = tibble(Select = character(0),
                                     # Variables = character(0),
                                     Expression = character(0),
                                     # Arguments = character(0),
                                     Name = character(0))
  
  sim_params$sim_mean <- mean
  sim_params$log_sim_variance <- log_variance
  sim_params$sim_error <- error
  
  return(sim_params)
  
}


# Check variable name meets requirements ----
validate_variable_name <- function(name, df_names) {
  condition =  name == "" | 
    stringr::str_detect(name, "^\\d") |
    stringr::str_detect(name, "^_") |
    stringr::str_detect(name, "[[:space:]]") |
    stringr::str_detect(name, "[^_[:^punct:]]") |
    stringr::str_detect(name, "[A-Z]") |
    (name %in% names(df_names))
  
  return(condition)
}

#### List mapping input arguments to functions ####

expit <- function(x) {
  exp(x) / (1 + exp(x))
}

arctan <- function(x) {
  (1 / pi) * atan(x) + 1 / 2
}

adj_tanh <- function(x) {
  .5 * (tanh(x) + 1)
}


data_agg <- list("sum" = sum,
                 "average" = mean,
                 "weighted average" = weighted.mean)

prob_maps <- list("expit" = expit,
                  "arctan" = arctan,
                  "tanh" = adj_tanh)


# Upate UI functions ----
updateConditionalVars <- function(session, names) {
  updateSelectInput(session,
                    inputId = 'conditional_vars',
                    label = "Choose Variables",
                    names)
}


# Reset reactive values ----
reset_summary_tables <- function(summary_tables) {
  
  summary_tables$mean_tables = list()
  summary_tables$variance_tables = list()
  summary_tables$error_dist = list()
  summary_tables$data_dict = tibble(Variable = character(0), Independent = character(0))
  summary_tables$param_dists = list()
  
}

# Reset expr_params ---
reset_expr_params <- function(expr_params, var_names) {
  expr_params$var_names <- var_names
  expr_params$expr_list <- c()
  
  return(expr_params)
}

# Update accordion ----

insert_accordion <- function(selector, where, acc_id, label, item){
  insertUI(
    selector,
    where = where,
    immediate = TRUE,
    ui = tags$div(accordion_item(
      id = acc_id,
      label = h4(label),
      item
    ))
  )
}
