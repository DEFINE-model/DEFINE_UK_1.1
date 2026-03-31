estimate <- function(data, dummy = NULL, start, end, max_order = 4, 
                          selection = "BIC", alpha = 0.05) {
  
  # Load required libraries
  library(ARDL)
  library(tseries)
  library(lmtest)
  library(dplyr)
  
  # Input validation
  validate_inputs <- function(data, start, end) {
    if (!is.data.frame(data)) stop("Data must be a data.frame")
    if (start < 1 || end > nrow(data)) stop("Invalid start/end range")
    if (any(is.na(data[start:end, ]))) {
      warning("Missing values detected in selected range")
      print(data[start:end, ])
      return(FALSE)
    }
    return(TRUE)
  }
  
  if (!validate_inputs(data, start, end)) return(NULL)
  
  # Extract working data
  work_data <- data[start:end, ]
  
  # Print variable information
  cat("Dependent variable:", names(data)[1], "\n")
  cat("Independent variables:", paste(names(data)[-1], collapse = ", "), "\n\n")
  
  # Step 1: Test stationarity of all variables
  test_stationarity <- function(x, var_name) {
    adf_result <- adf.test(x, alternative = "stationary")
    cat("ADF test for", var_name, ":\n")
    cat("  Statistic:", round(adf_result$statistic, 4), "\n")
    cat("  P-value:", round(adf_result$p.value, 4), "\n")
    cat("  Stationary:", ifelse(adf_result$p.value < alpha, "Yes", "No"), "\n\n")
    return(adf_result$p.value < alpha)
  }
  
  # Test dependent variable stationarity
  dep_var <- work_data[, 1]
  dep_stationary <- test_stationarity(dep_var, names(data)[1])
  
  # Test independent variables stationarity
  indep_stationary <- sapply(2:ncol(work_data), function(i) {
    test_stationarity(work_data[, i], names(data)[i])
  })
  
  # Build formula helper
  build_formula <- function(dep_var, indep_vars, dummy_vars = NULL) {
    rhs <- paste(indep_vars, collapse = " + ")
    if (!is.null(dummy_vars)) {
      rhs <- paste(rhs, "|", paste(dummy_vars, collapse = " + "))
    }
    as.formula(paste(dep_var, "~", rhs))
  }
  
  # Create base formula
  dep_name <- names(data)[1]
  indep_names <- names(data)[-1]
  dummy_names <- if (!is.null(dummy)) names(dummy) else NULL
  
  # Combine data if dummy variables exist
  if (!is.null(dummy)) {
    combined_data <- cbind(work_data, dummy[start:end, , drop = FALSE])
  } else {
    combined_data <- work_data
  }
  
  formula_obj <- build_formula(dep_name, indep_names, dummy_names)
  
  # Case 1: All variables are stationary - estimate ARDL in levels
  if (dep_stationary && all(indep_stationary)) {
    cat("All variables are stationary - fitting ARDL in levels\n\n")
    
    # Use auto_ardl for model selection
    ardl_model <- auto_ardl(
      formula = formula_obj,
      data = combined_data,
      max_order = rep(max_order, ncol(work_data)),
      selection = selection,
      grid = TRUE
    )
    
    cat("Selected ARDL model:\n")
    print(summary(ardl_model$best_model))
    
    # Test for autocorrelation
    dw_test <- dwtest(ardl_model$best_model)
    cat("\nDurbin-Watson test for autocorrelation:\n")
    cat("DW statistic:", round(dw_test$statistic, 4), "\n")
    cat("P-value:", round(dw_test$p.value, 4), "\n")
    
    if (dw_test$p.value < alpha) {
      warning("Autocorrelation detected. Consider adding more lags.")
    }
    
    cat("\n=== FINAL MODEL SUMMARY ===\n")
    print(summary(ardl_model$best_model))
    
    return(list(
      model = ardl_model$best_model,
      type = "ARDL_levels",
      autocorr_test = dw_test
    ))
  }
  
  # Case 2: Variables are I(1) - test for cointegration
  if (!dep_stationary && !all(indep_stationary)) {
    cat("Variables appear to be I(1) - testing for cointegration\n\n")
    
    # Fit ARDL model for bounds testing
    ardl_model <- auto_ardl(
      formula = formula_obj,
      data = combined_data,
      max_order = rep(max_order, ncol(work_data)),
      selection = selection,
      grid = TRUE
    )
    
    cat("Selected ARDL model for bounds testing:\n")
    print(summary(ardl_model$best_model))
    
    # Estimate UECM
    uecm_model <- uecm(ardl_model$best_model, data = work_data)
    
    # Bounds test for cointegration
    bounds_result <- bounds_f_test(uecm_model, alpha = alpha, case = 3)
    
    cat("\nBounds Test Results:\n")
    cat("F-statistic:", round(bounds_result$statistic, 4), "\n")
    cat("Critical values (I0/I1):", 
        round(bounds_result$PSS2001parameters[1], 4), "/", 
        round(bounds_result$PSS2001parameters[2], 4), "\n")
    
    # Decision based on bounds test
    if (bounds_result$statistic > bounds_result$PSS2001parameters[2]) {
      cat("Cointegration detected - using ECM\n\n")
      
      result <- list(
        model = uecm_model,
        ardl_model = ardl_model$best_model,
        bounds_test = bounds_result,
        type = "ECM"
      )
      
      cat("=== FINAL MODEL SUMMARY ===\n")
      print(summary(uecm_model))
      
      return(result)
      
    } else if (bounds_result$statistic < bounds_result$PSS2001parameters[1]) {
      cat("No cointegration - using ARDL in differences\n\n")
      
      # Create differenced data
      diff_data <- data.frame(
        diff_dep = c(NA, diff(dep_var))
      )
      names(diff_data)[1] <- paste0("D_", dep_name)
      
      # Add lagged level of dependent variable
      diff_data[[paste0(dep_name, "_L1")]] <- c(NA, dep_var[-length(dep_var)])
      
      # Add independent variables in levels
      for (i in 2:ncol(work_data)) {
        diff_data[[names(work_data)[i]]] <- work_data[[i]]
      }
      
      # Remove first row (NA from differencing)
      diff_data <- diff_data[-1, ]
      
      # Fit model
      diff_formula <- as.formula(paste(names(diff_data)[1], "~", 
                                       paste(names(diff_data)[-1], collapse = " + ")))
      diff_model <- lm(diff_formula, data = diff_data)
      
      result <- list(
        model = diff_model,
        bounds_test = bounds_result,
        type = "ARDL_differences"
      )
      
      cat("=== FINAL MODEL SUMMARY ===\n")
      print(summary(diff_model))
      
      return(result)
      
    } else {
      cat("Bounds test inconclusive\n")
      cat("F-statistic falls between critical values\n")
      
      result <- list(
        ardl_model = ardl_model$best_model,
        uecm_model = uecm_model,
        bounds_test = bounds_result,
        type = "inconclusive"
      )
      
      cat("\n=== ARDL MODEL SUMMARY ===\n")
      print(summary(ardl_model$best_model))
      cat("\n=== UECM MODEL SUMMARY ===\n") 
      print(summary(uecm_model))
      
      return(result)
    }
  }
  
  # Case 3: Mixed integration orders - handle appropriately
  cat("Mixed integration orders detected\n")
  cat("Consider transforming non-stationary variables to achieve stationarity\n")
  
  # Transform non-stationary variables
  transformed_data <- work_data
  for (i in 2:ncol(work_data)) {
    if (!indep_stationary[i-1]) {
      transformed_data[, i] <- c(NA, diff(work_data[, i]))
      names(transformed_data)[i] <- paste0("D_", names(work_data)[i])
    }
  }
  
  # Remove rows with NA
  transformed_data <- na.omit(transformed_data)
  
  # Fit model with transformed data
  trans_formula <- build_formula(names(transformed_data)[1], 
                                 names(transformed_data)[-1])
  trans_model <- lm(trans_formula, data = transformed_data)
  
  result <- list(
    model = trans_model,
    type = "transformed",
    transformations = names(transformed_data)
  )
  
  cat("=== FINAL MODEL SUMMARY ===\n")
  print(summary(trans_model))
  
  return(result)
  
  
  
  
  
}

# Example usage:
# result <- estimate_ardl(data = your_data, start = 1, end = 100)
# summary(result$model)