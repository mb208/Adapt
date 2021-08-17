tex_var_name <- function(name) {
  return(str_interp("X_{${name}}"))
}

tex_multiply <- function(vars) {
  multi_expr <- str_detect(vars,  "\\+|-")
  
  if (any(multi_expr)) {
    # If any of vars is a weighted sum we wrap it in parantheses
    vars[which(multi_expr)] <- str_c("\\left(", vars[which(multi_expr)], "\\right)")
  } else {
    
    str_c(vars, collapse = "\\cdot")
    
  }
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
  
  str_glue("{f}({x})")
  
}

tex_wght_sum <- function(weights, names) {

  expr <-  paste0(paste0(weights, '', names), collapse = " + ") %>% 
    str_replace("\\+\\s-", "- ") %>% 
    str_replace("1", "")
  
  return(str_glue("{expr}"))
}


tex_power <- function(x, pow) {
 
   str_glue("{x}^{pow}")
  
}

tex_expr_mult <- function(f, X, weights=NULL) {
  
  switch(f,
         "multiply" = tex_multiply(X),
         "divide"   = tex_divide(X),
         "weighted sum" = tex_wght_sum(weights, X)
         )
}

tex_expr_unary <- function(f, x, pow=NULL) {
  
  if (f == "^") {
    tex_power(x, pow)
  } else {
    tex_function(f, x)
  }
}

func_to_tex <- function(f, X, pow=NULL, weights=NULL){
  
  if (length(X) == 1) {
    tex_expr_unary(f, X, pow)
    
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

laplace_tex <- function(loc, scale){
  str_interp("\\text{Laplace}( \\mu = ${loc}, b = ${scale})")
}

guass_tex <- function(mean, sd) {
  var <- sd**2
  str_interp("\\text{Normal}( \\mu = ${mean}, \\sigma^2 = ${var})")
}

bern_tex <- function(p) {
  str_interp("\\text{Bernoulli}(p = ${p})")
}

binomial_tex <- function(size, p) {
  str_interp("\\text{Binomial}(n = ${size}, p = ${p})")
}

gamma_tex <- function(rate, shape) {
  str_interp("\\text{Gammaa}(\\alpha = ${rate}, \\beta = ${shape})")
}

