
visibility_init_simulation <- function(sim_params)
  observe({
    # When number of variables in greater then 1
    # Give option to choose if new variable is dependent on previous
    
    if (sim_params$num_vars > 1) {
      shinyjs::disable("n_participants")
      shinyjs::disable("decision_pts")
      shinyjs::disable("n_days")
      shinyjs::show("independ_dist")
      shinyjs::hide("data_dist")
      shinyjs::hide("dist_params")
      
    } else {
      shinyjs::hide("loc_scale_column")
      
    }
    
  })


visibility_variabile_options <- function(input) {
  observe({
    req(input$independ_dist)
    
    if (input$independ_dist == "Yes") {
      shinyjs::show("data_dist")
      shinyjs::show("dist_params")
      shinyjs::show("constant_time")
      shinyjs::hide("loc_scale_column")
      
    } else {
      shinyjs::hide("data_dist")
      shinyjs::hide("dist_params")
      shinyjs::hide("constant_time")
      shinyjs::show("loc_scale_column")
      
    }
  })
}


# logic for showing equation panels ----
visibility_location_scale_steps <- function(sim_params) {
  observe({
    if (sim_params$cond_dist_step == 3) {
      shinyjs::hide("cond_operations")
      shinyjs::show("error_calc")
    } else if (any(sim_params$cond_dist_step == c(1, 2))) {
      shinyjs::hide("error_calc")
      shinyjs::show("cond_operations")
    }
    
    if (sim_params$cond_dist_step == 1) {
      shinyjs::show("calc_mean")
      shinyjs::hide("calc_variance")
    } else if (sim_params$cond_dist_step == 2) {
      shinyjs::hide("calc_mean")
      shinyjs::show("calc_variance")
    }
  })
}

visibility_operation_choices <- function(input) {
  observe({
    req(input$independ_dist)
    if (!is.null(input$conditional_vars)) {
      if (input$conditional_vars[1] == "") {
        shinyjs::hide("unary_operation")
        shinyjs::hide("multi_operation")
        shinyjs::hide("power_val")
      } else if (length(input$conditional_vars) == 1) {
        shinyjs::show("unary_operation")
        shinyjs::hide("multi_operation")
        shinyjs::reset("multi_operation")
        # Need to specify because we don't want this to capture case where no variables are selected
      } else {
        shinyjs::hide("unary_operation")
        shinyjs::show("multi_operation")
        shinyjs::reset("unary_operation")
      }
      
      if (input$unary_operation == "^") {
        shinyjs::show("power_val")
      } else {
        shinyjs::hide("power_val")
      }
    } else {
      shinyjs::hide("unary_operation")
      shinyjs::hide("multi_operation")
      shinyjs::hide("power_val")
    }
    
  })
}

visibility_weighted_sum <- function(input) {
  
  observe({
    req(input$multi_operation)
    if (input$multi_operation == "weighted sum") {
      shinyjs::show("weighted_sum_inputs")
    } else {
      shinyjs::hide("weighted_sum_inputs")
    }
  })
}
