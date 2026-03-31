econometrics_table <- function(result, end_period) {
  library(xtable)
  
  # --- Compute start period from end_period ---
  ep <- strsplit(end_period, "Q")[[1]]
  end_year <- as.numeric(ep[1])
  end_quarter <- as.numeric(ep[2])
  
  n_obs <- nobs(result$model)
  
  # Compute start period
  total_quarters <- (end_year * 4 + end_quarter) - n_obs + 1
  start_year <- (total_quarters - 1) %/% 4
  start_quarter <- ((total_quarters - 1) %% 4) + 1
  start_period <- paste0(start_year, "Q", start_quarter)
  
  # --- Extract coefficients ---
  summ <- summary(result$model)
  coef_df <- as.data.frame(coef(summ))
  coef_df$Variable <- rownames(coef_df)
  rownames(coef_df) <- NULL
  coef_df <- coef_df[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
  
  # Significance stars
  coef_df$Signif <- ""
  coef_df$Signif[coef_df$`Pr(>|t|)` < 0.1]  <- "."
  coef_df$Signif[coef_df$`Pr(>|t|)` < 0.05] <- "*"
  coef_df$Signif[coef_df$`Pr(>|t|)` < 0.01] <- "**"
  coef_df$Signif[coef_df$`Pr(>|t|)` < 0.001] <- "***"
  
  coef_df$Estimate <- paste0(round(coef_df$Estimate, 4), coef_df$Signif)
  coef_df$Signif <- NULL
  colnames(coef_df) <- c("Variable", "Estimate", "Std. Error", "t-statistic", "p-value")
  
  # --- Format variable names for LaTeX ---
  coef_df$Variable <- gsub("^d\\(", "$\\\\Delta$\\( ", coef_df$Variable)  #coef_df$Variable <- gsub("Log", "\\\\textbf{L}", coef_df$Variable)
  # Escape underscores for LaTeX
  coef_df$Variable <- gsub("_", "\\\\_", coef_df$Variable)
  
  
  # --- Bounds test info ---
  if (!is.null(result$bounds_test)) {
    bounds_stat <- result$bounds_test$statistic
    crit_low <- result$bounds_test$PSS2001parameters[1]
    crit_high <- result$bounds_test$PSS2001parameters[2]
    if (bounds_stat > crit_high) {
      coint_result <- "Cointegration"
    } else if (bounds_stat < crit_low) {
      coint_result <- "No cointegration"
    } else {
      coint_result <- "Inconclusive"
    }
  } else {
    bounds_stat <- NA
    coint_result <- "Not applicable"
    crit_low <- crit_high <- NA
  }
  
  # --- Model stats ---
  r2 <- summ$r.squared
  adj_r2 <- summ$adj.r.squared
  fstat <- summ$fstatistic[1]
  df1 <- summ$fstatistic[2]
  df2 <- summ$fstatistic[3]
  pval <- pf(fstat, df1, df2, lower.tail = FALSE)
  
  # --- Dependent variable ---
  dep_var <- all.vars(formula(result$model))[1]
  
  # --- Build LaTeX table ---
  xt <- xtable(coef_df, 
               caption = " ",  # blank caption at the top
               label = paste0("etab_", dep_var),
               align = c("l", "l", "c", "c", "c", "c"))
  
  addtorow <- list()
  k <- ncol(coef_df)
  
  # Custom header rows (appear below caption)
  header_rows <- c(
    paste0("\\multicolumn{", k, "}{l}{Dependent variable: ", dep_var, "}\\\\"),
    paste0("\\multicolumn{", k, "}{l}{Sample: ", start_period, " - ", end_period, "}\\\\"),
    paste0("\\multicolumn{", k, "}{l}{Observations: ", n_obs, "}\\\\"),
    paste0("\\multicolumn{", k, "}{l}{Bounds F-statistic: ", 
           ifelse(is.na(bounds_stat), "NA", round(bounds_stat, 4)), 
           " (", coint_result, 
           ", Lower bound = ", round(crit_low,4), 
           ", Upper bound = ", round(crit_high,4), ")}\\\\"),
    paste0("\\multicolumn{", k, "}{l}{Model type: ", result$type, "}\\\\"),
    "\\hline"
  )
  
  col_header <- paste0("\\textbf{Variable} & \\textbf{Estimate} & \\textbf{Std. Error} & \\textbf{t-statistic} & \\textbf{p-value} \\\\")
  
  footer_rows <- c(
    "\\hline",
    paste0("\\multicolumn{", k, "}{l}{R-squared: ", round(r2,4), "}\\\\"),
    paste0("\\multicolumn{", k, "}{l}{Adjusted R-squared: ", round(adj_r2,4), "}\\\\"),
    paste0("\\multicolumn{", k, "}{l}{F-statistic: ", round(fstat,4),
           " (df=", df1,",",df2,")}\\\\"),
    paste0("\\multicolumn{", k, "}{l}{p-value: ", signif(pval,4), "}\\\\"),
    paste0("\\multicolumn{", k, "}{l}{Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1}\\\\")
  )
  
  addtorow$pos <- list(0, nrow(coef_df))
  addtorow$command <- c(
    paste0(paste(header_rows, collapse="\n"), "\n", col_header, "\n"),
    paste0(paste(footer_rows, collapse="\n"), "\n")
  )
  
  print(xt, include.rownames = FALSE, include.colnames = FALSE, add.to.row = addtorow,
        sanitize.text.function = identity,
        caption.placement = "top",  # ensure caption appears at top
        tabular.environment = "tabular*", width = "\\textwidth")
}
