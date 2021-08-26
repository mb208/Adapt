# Enable / disable gen_var action button -----------------------
source("R/utils_server.R")

toggle_data_dist <- function(input, sim_params) {
  
  observe({
    req(input$sim_var_name)
    
    cond_varname <- validate_variable_name(input$sim_var_name, names(sim_params$sim_data))
    if (!cond_varname) {
      shinyjs::enable("data_dist")
    } else {
      shinyjs::disable("data_dist")
    }
    })
}

toggle_genVar_btn_init <- function(input, sim_params) {
  observe({
    req(input$sim_var_name)
    invalid_name <- validate_variable_name(input$sim_var_name,
                                           names(sim_params$sim_data))
    
    empty_dist <- input$data_dist == ""
    if (sim_params$num_vars == 1) {
      if (!invalid_name & !empty_dist) {
        shinyjs::enable("gen_var")
      } else {
        shinyjs::disable("gen_var")
      }
    }
    
    
  })
}

# toggle gen_var button for independent variable generation ----
toggle_genVar_btn_IV <- function(input, sim_params) {
  observe({
    req(input$independ_dist)
    invalid_name <- validate_variable_name(input$sim_var_name,
                                           names(sim_params$sim_data))
    
    empty_dist <- input$data_dist == ""
    if (input$independ_dist == "Yes") {
      if (!invalid_name & !empty_dist) {
        shinyjs::enable("gen_var")
      } else {
        shinyjs::disable("gen_var")
      }
    }
  })
}

# toggle gen_var button for dependent variable generation ----
toggle_genVar_btn_DV <- function(input, sim_params) {
  observe({
    req(input$independ_dist)
    invalid_name <- validate_variable_name(input$sim_var_name,
                                           names(sim_params$sim_data))
    
    missing_params = any(map_lgl(
      c("sim_mean", "log_sim_variance", "sim_error"),
      ~ is.null(sim_params[[.]])
    ))
    if (input$independ_dist == "No") {
      if (!invalid_name & !missing_params) {
        shinyjs::enable("gen_var")
      } else {
        shinyjs::disable("gen_var")
      }
    }
  })
}

# Enable / disable apply action button -----------------------

toggle_apply_op_btn <- function(input, sim_params) {
  observe({
    req(input$conditional_vars)
    var_selected <- input$conditional_vars
    unary <- input$unary_operation == ""
    multi <- input$multi_operation == ""
    
    if (is.null(var_selected)) {
      shinyjs::disable("apply_operation")
    } else if (length(var_selected) == 1 & !unary) {
      shinyjs::enable("apply_operation")
    } else if (length(var_selected) > 1 & !multi) {
      shinyjs::enable("apply_operation")
    } else {
      shinyjs::disable("apply_operation")
    }
  })
}

# Enable / disable gen mean / variance  -----------------------

toggle_mean_variance_btn <- function(input, sim_params) {
  observeEvent(input$expr_row, {
    if (sim_params$cond_dist_step == 1) {
      shinyjs::toggleState("calc_mean")
    } else if (sim_params$cond_dist_step == 2) {
      shinyjs::toggleState("calc_variance")
    }
  })
}

# Enable / disable gen error -----------------------
toggle_err_btn <- function(input, sim_params) {
  observe({
    req(input$independ_dist)
    if (input$error_dist == "") {
      shinyjs::disable("calc_error")
    } else {
      shinyjs::enable("calc_error")
    }
  })
}

toggle_indpendence_btn <- function(input, sim_params) {
  observe({
   
    if (sim_params$num_vars > 1) {
    
      cond_varname <- validate_variable_name(input$sim_var_name, names(sim_params$sim_data))
      
      if (!cond_varname) {
        
        shinyjs::enable("independ_dist")
       
        
      } else {
        
        shinyjs::disable("independ_dist")
        
      }
    }
  })
  
}


toggle_name_btn1 <- function(input) {
  observeEvent(req(input$independ_dist=="No"), {
      shinyjs::disable("sim_var_name")
    
  })
}

toggle_name_btn2 <- function(input) {
  observeEvent(input$data_dist, {
    req(input$sim_var_name)
    shinyjs::disable("sim_var_name")
    
  })
}

