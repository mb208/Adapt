# Enable / disable gen_var action button -----------------------

toggle_genVar_btn <- function(input, sim_params) {
  observe({
    req(input$sim_var_name)
    req(input$independ_dist)
    sim_var_name <-
      stringr::str_trim(input$sim_var_name) # Remove leading / trailing white space
    cond_varname =  input$sim_var_name == "" |
      stringr::str_detect(sim_var_name, "^\\d") |
      stringr::str_detect(sim_var_name, "^_") |
      stringr::str_detect(sim_var_name, "[[:space:]]") |
      stringr::str_detect(sim_var_name, "[^_[:^punct:]]") |
      stringr::str_detect(sim_var_name, "[A-Z]") |
      (sim_var_name %in% names(sim_params$sim_data))
    
    cond_ind = input$data_dist == ""
    cond_dep = any(map_lgl(
      c("sim_mean", "log_sim_variance", "sim_error"),
      ~ is.null(sim_params[[.]])
    ))
    
    independ = input$independ_dist == "Yes"
    if (input$independ_dist == "Yes") {
      if (!cond_varname & !cond_ind) {
        shinyjs::enable("gen_var")
        
      } else {
        shinyjs::disable("gen_var")
      }
    } else {
      if (!cond_varname & !cond_dep) {
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
