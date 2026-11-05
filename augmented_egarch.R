library(xts)
library(rugarch)
library(readxl)
library(parallel)
library(writexl)

# ==============================================================================
# 1. DATA LOADER WITH DYNAMIC PATH RESOLUTION
# ==============================================================================
clean_bloomberg_data <- function(df) {
  df$Date <- as.Date(df$Date)
  df$PX_LAST <- as.numeric(as.character(df$PX_LAST))
  df <- df[order(df$Date), ]
  return(na.omit(df[, c("Date", "PX_LAST")]))
}

data_dir <- if (dir.exists("datas")) "datas" else if (dir.exists("../datas")) "../datas" else if (dir.exists("data")) "data" else if (dir.exists("../data")) "../data" else "."

spx_raw     <- read_excel(file.path(data_dir, "spx.xlsx"))
vix_raw     <- read_excel(file.path(data_dir, "vix.xlsx"))
ukx_raw     <- read_excel(file.path(data_dir, "ukx.xlsx"))
ivukx30_raw <- read_excel(file.path(data_dir, "ivukx30.xlsx"))

spx_clean     <- clean_bloomberg_data(spx_raw)
vix_clean     <- clean_bloomberg_data(vix_raw)
ukx_clean     <- clean_bloomberg_data(ukx_raw)
ivukx30_clean <- clean_bloomberg_data(ivukx30_raw)

# ==============================================================================
# 2. MULTIPROCESSING EGARCH-X ROLLING ENGINE + EXCEL SUMMARY EXTRACTOR
# ==============================================================================
run_egarch_x_parallel <- function(price_clean, iv_clean, asset_name = "Asset", W = 1000) {
  
  price_xts <- xts(price_clean$PX_LAST, order.by = price_clean$Date)
  iv_xts    <- xts(iv_clean$PX_LAST,    order.by = iv_clean$Date)
  
  # Synchronize dates
  merged <- na.omit(merge(price_xts, iv_xts))
  
  # 1. Percentage returns: r_t = (ln(P_t) - ln(P_{t-1})) * 100
  returns_xts <- diff(log(merged[, 1])) * 100
  
  # 2. Exogenous variable: Strictly lagged log(IV) -> ln(IV_{t-1})
  log_iv_lag_xts <- lag(log(merged[, 2]), k = 1)
  
  # Synchronize and drop initial NA
  dataset <- na.omit(merge(returns_xts, log_iv_lag_xts))
  
  ret_data  <- dataset[, 1]
  xreg_data <- matrix(as.numeric(dataset[, 2]), ncol = 1)
  
  T_total <- length(ret_data)
  out_sample_len <- T_total - W
  
  # Detect CPU cores and initialize parallel cluster
  num_cores <- max(1, detectCores() - 1)
  cl <- makeCluster(num_cores)
  
  cat("\n=======================================================\n")
  cat("  ESTIMATING PARALLEL ROLLING EGARCH-X(1,1):", toupper(asset_name), "\n")
  cat("  Total Observations:", T_total, "| In-Sample (W):", W, "| Out-of-Sample:", out_sample_len, "\n")
  cat("  Allocated CPU Cores:", num_cores, "\n")
  cat("=======================================================\n")
  
  # Spec with exogenous regressor in conditional log-variance
  spec <- ugarchspec(
    variance.model = list(
      model = "eGARCH",
      garchOrder = c(1, 1),
      external.regressors = xreg_data
    ),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std"
  )
  
  # Execute rolling backtest across parallel cluster
  roll <- ugarchroll(
    spec = spec,
    data = ret_data,
    n.ahead = 1,
    forecast.length = out_sample_len,
    refit.every = 1,
    refit.window = "moving",
    window.size = W,
    solver = "hybrid",
    cluster = cl,
    VaR.alpha = c(0.05, 0.01),
    calculate.VaR = TRUE
  )
  
  stopCluster(cl)
  
  # Extract aligned VaR forecasts and realized returns
  var_matrix <- as.data.frame(roll@forecast$VaR)
  
  actual_returns <- var_matrix$realized
  var_95         <- var_matrix$`alpha(5%)`
  var_99         <- var_matrix$`alpha(1%)`
  
  # Standard regulatory backtests
  bt_95 <- VaRTest(alpha = 0.05, actual = actual_returns, VaR = var_95)
  bt_99 <- VaRTest(alpha = 0.01, actual = actual_returns, VaR = var_99)
  
  cat("\n=======================================================\n")
  cat("  EGARCH-X(1,1) REGULATORY BACKTEST:", toupper(asset_name), "\n")
  cat("=======================================================\n")
  
  cat("\n--- 95% Confidence Level (alpha = 0.05) ---\n")
  print(bt_95)
  
  cat("\n--- 99% Confidence Level (alpha = 0.01) ---\n")
  print(bt_99)
  
  # Build structured result row
  df_result <- data.frame(
    Market          = c(asset_name, asset_name),
    VaR_Level       = c("95% VaR", "99% VaR"),
    Out_of_Sample_T = c(out_sample_len, out_sample_len),
    Expected_Hits   = c(round(bt_95$expected.exceed, 1), round(bt_99$expected.exceed, 1)),
    Actual_Hits     = c(bt_95$actual.exceed, bt_99$actual.exceed),
    Failure_Rate    = c(sprintf("%.2f%%", (bt_95$actual.exceed / out_sample_len) * 100),
                        sprintf("%.2f%%", (bt_99$actual.exceed / out_sample_len) * 100)),
    Kupiec_p_val    = c(round(bt_95$uc.LRp, 4), round(bt_99$uc.LRp, 4)),
    Kupiec_Decision = c(bt_95$uc.Decision, bt_99$uc.Decision),
    CC_p_val        = c(round(bt_95$cc.LRp, 4), round(bt_99$cc.LRp, 4)),
    CC_Decision     = c(bt_95$cc.Decision, bt_99$cc.Decision),
    stringsAsFactors = FALSE
  )
  
  return(list(
    roll  = roll,
    table = df_result
  ))
}

# ==============================================================================
# 3. RUN ESTIMATIONS & COMBINE INTO TABLE 5.5
# ==============================================================================
results_egarchx_spx <- run_egarch_x_parallel(spx_clean, vix_clean, "S&P 500 (with VIX)", W = 1000)
results_egarchx_ukx <- run_egarch_x_parallel(ukx_clean, ivukx30_clean, "FTSE 100 (with VFTSE)", W = 1000)

table_5_5_egarchx <- rbind(results_egarchx_spx$table, results_egarchx_ukx$table)
print(table_5_5_egarchx)

# ==============================================================================
# 4. EXPORT TO TABLES DIRECTORY
# ==============================================================================
table_dir <- if (dir.exists("tables")) "tables" else if (dir.exists("../tables")) "../tables" else "tables"

if (!dir.exists(table_dir)) {
  dir.create(table_dir, recursive = TRUE)
}

output_file <- file.path(table_dir, "Table_5_5_EGARCH_X.xlsx")

write_xlsx(list("Table_5_5_EGARCH_X" = table_5_5_egarchx), path = output_file)
cat(sprintf("\nSuccessfully exported %s!\n", output_file))