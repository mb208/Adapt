tex_var_name <- function(name) {
  return(str_interp("X_{\\text{${name}}}"))
}

handle_sum_tex <- function(vars) {
  multi_expr <- str_detect(vars,  "\\+|-")
  
  if (any(multi_expr)) {
    # If any of vars is a weighted sum we wrap it in parantheses
    if (length(var) > 1) {
      vars[which(multi_expr)] <- str_c("\\left(", vars[which(multi_expr)], "\\right)")
    } else {
     str_c("\\left(", vars, "\\right)")
    }
  } else {
    vars
  }
}

tex_multiply <- function(vars) {
  vars <- handle_sum_tex(vars)
  str_c(vars, collapse = " \\cdot ")
}

tex_divide_simp <- function(X1,X2) {
  
  str_interp("\\frac{${X1}}{${X2}}")
  
}

tex_divide <- function(X) {
  
  if (length(X) == 2) {
    return(tex_divide_simp(X[1],X[2]))
  } else {
    n <- length(X) 
    temp <- tex_divide_simp(X[n-1],X[n])
    n <- n - 2
    while (n > 0) {
      temp <- tex_divide_simp(X[n], temp)
      n <- n - 1
    }
  }
  return(temp)
}



tex_function <- function(f, x) {
  if (f == "exp") {
    str_interp("e^{${x}}")
  } else if (f == "None") {
    x
  } else {
    str_interp("\\text{${f}}(${x})")
  }
}

tex_wght_sum <- function(weights, names) {

  weights <- as.character(weights)
  weights <- case_when(weights == "1" ~ "",
                       weights == "-1" ~ "-",
                       TRUE ~ weights)
  expr <-  paste0(paste0(weights, '', names), collapse = " + ") %>% 
    str_replace_all("\\+\\s-", "- ")
  
  return(str_glue("{expr}"))
}


tex_power <- function(x, pow) {
  if (any(str_detect(x,  "\\+|-"))) { 
    x <- handle_sum_tex(x)
  } else if (str_detect(x,  "^")) {
    x <- str_interp("\\left(${x}\\right)")
  }
   
    str_interp("${x}^{${pow}}")
  
}

tex_lag <- function(x, n, unit) {
  
  str_interp("${x}^{t_{\\text{ ${unit}} } - ${n}}")
  
}

tex_expr_mult <- function(f, X, weights=NULL) {
  
  switch(f,
         "multiply" = tex_multiply(X),
         "divide"   = tex_divide(X),
         "weighted sum" = tex_wght_sum(weights, X)
         )
}

tex_expr_unary <- function(f, x, pow=NULL, n = NULL, unit=NULL) {
  
  if (f == "^") {
    tex_power(x, pow)
  } else if (f=="lag") {
    tex_lag(x, n = n, unit = unit)
  } else {
    tex_function(f, x)
  }
  
}

func_to_tex <- function(f, X, pow=NULL, n=NULL, unit=NULL, weights=NULL){
  
  if (length(X) == 1) {
    tex_expr_unary(f, X, pow=pow, n=n, unit=unit)
    
  } else if (length(X) > 1) {
    tex_expr_mult(f, X, weights)
  }
}


expr_to_tex <- function(f, X, var_names, pow=NULL, weights=NULL) {
  
  orig_name <- X %in% var_names
  X[which(orig_name)] <- map_chr(X[which(orig_name)], tex_var_name)
  func_to_tex(f, X,  pow, weights)
}

render_tex <- function(tex_syntax) {
  withMathJax(helpText(str_c("$$", tex_syntax, "$$", collapse = "")))
}

render_tex_inline <- function(tex_syntax) {
  withMathJax(helpText(str_c("\\(", tex_syntax, "\\)", collapse = "")))
}

laplace_tex <- function(location, scale){
  str_interp("\\text{Laplace}( \\mu = ${location}, b = ${scale})")
}

guass_tex <- function(mean, sd) {
  var <- sd**2
  str_interp("\\text{Normal}( \\mu = ${mean}, \\sigma^2 = ${var})")
}

bern_tex <- function(p) {
  str_interp("\\text{Bernoulli}(p = ${p})")
}

unif_tex <- function(Min, Max) {
  str_interp("\\text{Uniform}(min = ${Min}, max = ${Max})")
}

binomial_tex <- function(size, p) {
  str_interp("\\text{Binomial}(n = ${size}, p = ${p})")
}

gamma_tex <- function(rate, shape) {
  str_interp("\\text{Gamma}(\\alpha = ${rate}, \\beta = ${shape})")
}

