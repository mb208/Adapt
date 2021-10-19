parse_variable <- function(node) {
  
  # Check if required names are included
  attrs <- names(node) # values of variable
  stopifnot(all(c("threshold", "relation") %in% attrs))
  stopifnot(any(c("name", "index") %in% attrs))
  
  stopifnot(any(c("leq", "geq") %in% node["relation"]))
  stopifnot(class(node[["threshold"]]) %in% c("numeric", "integer"))
  

  operation <- switch (node[["relation"]],
                       "leq" = -1,
                       "geq" = 1)
  
  threshold <- node[["threshold"]]
  
  id <- ifelse(("index" %in% attrs), node[["index"]], node[["name"]])
  
  return(list(id, threshold, operation))

}

parse_clause <- function(node) {
  # Check if required names are included
  attrs <- names(node) # values of variable
  stopifnot(all(c("variables", "agg", "actions", "probs", "adaptive") %in% attrs))
  stopifnot(any(c("and", "or") %in% node[["agg"]] | is.null(node[["agg"]])))
    

  variables <- purrr::map(node$variables, parse_variable) %>%  unname() 
  extract <- list("variables" = variables,
                  "agg" = node$agg,
                  "actions" =  node$action,
                  "probs" = node$probs,
                  "adaptive" = node$adaptive
                  )
  return(extract)
  
}

parse_rules <- function(tree) {
  clauses <- names(tree) %>%
    keep(function(x) str_detect(x, "clause")) %>% # filter out nodes that aren't clauses
    purrr::map(., ~ parse_clause(tree[[.]]))
  
  names(clauses) <- paste0("c", seq_along(clauses))
  
  return(clauses)
  
}

eval_variable <- function(X, tau, b) {
  # Check if variable meets threshold
  
  b*X >= b*tau
  
}

or_and <- function(A,B, d) {
  # If d is identity (I) this returns A and B, if it is negation (!) it returns A or B
  
  return(d(d(A) & d(B)))
}

eval_clause <- function(clause, data) {
  
  attrs <- names(clause) # values of variable
  stopifnot(all(c("variables", "agg", "actions") %in% attrs))
  stopifnot(any(c("matrix", "data.frame") %in% class(data)))
  
  logic = list("and"=`I`, "or" = `!`)
  
  
  if (length(clause$variables) == 1) {
    rule <- clause$variables[[1]]
    
    return(eval_variable(data[, rule[[1]]], rule[[2]], rule[[3]]))
    
  } else {
    aggs = clause$agg
    
    tuples  <- lapply(clause$variables, function(x) {
      eval_variable(data[, x[[1]]], x[[2]], x[[3]])
    })
    
    # Initialize nested logic operations
    n <- length(tuples)
    truth_values <- or_and(tuples[[1]],tuples[[2]], logic[[aggs[[1]]]])
    
    if (n == 2) {
      
      return(truth_values)
      
    } else {
      for (i in 2:(length(tuples)-1)) {
        truth_values <- or_and(truth_values, tuples[[i+1]], logic[[aggs[[i]]]])
      }
      return(truth_values)
    }
  }
}

eval_warm_start <- function(d_list, data) {
  
  # get number of observations
  n_obs <- nrow(data)
  
  action_list <- d_list$actions # all available actions
  clauses  <- parse_rules(d_list)
  actions  <- purrr::map(clauses, ~.x$actions)
  probs    <- purrr::map(clauses, ~.x$probs)
  adaptive <- purrr::map(clauses, ~.x$adaptive)
  
  
  
  # get default actions 
  d_actions <- d_list$default$actions
  d_probs <- d_list$default$probs
  
  # Initialize actions
  treatment <- character(n_obs)
  nest_clause <- logical(n_obs)
  
  prev_clause <- eval_clause(clauses[[1]], data)
  
  if (sum(prev_clause) > 0) {
    treatment[prev_clause] <- replicate(sum(prev_clause), 
                                        sample(actions[[1]], size=1, prob = probs[[1]])
    )
  }
  for (i in 2:length(clauses)) {
    curr_clause <- eval_clause(clauses[[i]], data)
    # if prev clause was true don't do anything
    # if prev clause was false check current clause 
    nest_clause <- ((!prev_clause) & curr_clause)
    if (sum(nest_clause) > 0) {
      treatment[nest_clause] <- replicate(sum(nest_clause), 
                                         sample(actions[[i]], 
                                                size=1, 
                                                prob = probs[[i]]
                                                )
                                         )
       
    }
    # set new prev_clause to curr_clause but retain true values from prev clause
    prev_clause <- ifelse(prev_clause, prev_clause, curr_clause)
  }
  
  if (sum(prev_clause) < n_obs) {
    treatment[!prev_clause] <- replicate(sum(!prev_clause), 
                                         sample(x = d_actions, size = 1, prob = d_probs))
  }
  return(treatment)
}

