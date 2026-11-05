library(xts)
library(rugarch)
library(readxl)
library(parallel)
library(writexl)

# ==============================================================================
# 1. LOAD DATASETS WITH DYNAMIC PATH RESOLUTION
# ==============================================================================
data_dir <- if (dir.exists("datas")) "datas" else if (dir.exists("../datas")) "../datas" else if (dir.exists("data")) "data" else if (dir.exists("../data")) "../data" else "."

spx <- read_excel(file.path(data_dir, "spx.xlsx"))
ukx <- read_excel(file.path(data_dir, "ukx.xlsx"))

# ==============================================================================
# 2. FUNCTION: Parallel Rolling Symmetric GARCH(1,1) + Excel Summary Extractor
# ==============================================================================
run_garch_backtest_parallel <- function(data, asset_name = "Asset", W = 1000, refit_freq = 1) {
  
  # Clean data
  data$Date <- as.Date(data$Date)
  data$PX_LAST <- as.numeric(as.character(data$PX_LAST))
  data <- data[order(data$Date), ]
  data_clean <- na.omit(data[, c("Date", "PX_LAST")])
  
  # Percentage log returns
  price_xts <- xts(data_clean$PX_LAST, order.by = data_clean$Date)
  returns_xts <- na.omit(diff(log(price_xts))) * 100
  
  # Symmetric GARCH(1,1) specification with Student-t innovations
  garch_spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std"
  )
  
  # Initialize parallel cluster
  n_out <- length(returns_xts) - W
  num_cores <- max(1, detectCores() - 1)
  cl <- makeCluster(num_cores)
  
  cat("\n=======================================================\n")
  cat("  ESTIMATING PARALLEL SYMMETRIC GARCH(1,1):", toupper(asset_name), "\n")
  cat("  Total Obs:", length(returns_xts), "| Window (W):", W, "| Out-of-Sample:", n_out, "\n")
  cat("  Refit Frequency:", refit_freq, "| Cores Allocated:", num_cores, "\n")
  cat("=======================================================\n")
  
  # Out-of-sample rolling estimation across CPU cores
  garch_roll <- ugarchroll(
    spec = garch_spec,
    data = returns_xts,
    n.ahead = 1,
    forecast.length = n_out,
    refit.every = refit_freq,
    refit.window = "moving",
    window.size = W,
    solver = "hybrid",
    cluster = cl,
    VaR.alpha = c(0.05, 0.01),
    calculate.VaR = TRUE
  )
  
  stopCluster(cl)
  
  # Extract aligned VaR forecasts and realized returns
  var_matrix <- as.data.frame(garch_roll@forecast$VaR)
  
  actual_returns <- var_matrix$realized
  var_95         <- var_matrix$`alpha(5%)`
  var_99         <- var_matrix$`alpha(1%)`
  
  # Regulatory backtests
  bt_95 <- VaRTest(alpha = 0.05, actual = actual_returns, VaR = var_95)
  bt_99 <- VaRTest(alpha = 0.01, actual = actual_returns, VaR = var_99)
  
  cat("\n=======================================================\n")
  cat("  SYMMETRIC GARCH(1,1) BACKTEST:", toupper(asset_name), "\n")
  cat("=======================================================\n")
  
  cat("\n--- 95% Confidence Level (alpha = 0.05) ---\n")
  print(bt_95)
  
  cat("\n--- 99% Confidence Level (alpha = 0.01) ---\n")
  print(bt_99)
  
  # Build structured result row
  df_result <- data.frame(
    Market          = c(asset_name, asset_name),
    VaR_Level       = c("95% VaR", "99% VaR"),
    Out_of_Sample_T = c(n_out, n_out),
    Expected_Hits   = c(round(bt_95$expected.exceed, 1), round(bt_99$expected.exceed, 1)),
    Actual_Hits     = c(bt_95$actual.exceed, bt_99$actual.exceed),
    Failure_Rate    = c(sprintf("%.2f%%", (bt_95$actual.exceed / n_out) * 100),
                        sprintf("%.2f%%", (bt_99$actual.exceed / n_out) * 100)),
    Kupiec_p_val    = c(round(bt_95$uc.LRp, 4), round(bt_99$uc.LRp, 4)),
    Kupiec_Decision = c(bt_95$uc.Decision, bt_99$uc.Decision),
    CC_p_val        = c(round(bt_95$cc.LRp, 4), round(bt_99$cc.LRp, 4)),
    CC_Decision     = c(bt_95$cc.Decision, bt_99$cc.Decision),
    stringsAsFactors = FALSE
  )
  
  return(list(
    roll    = garch_roll,
    test_95 = bt_95,
    test_99 = bt_99,
    table   = df_result
  ))
}

# ==============================================================================
# 3. RUN ESTIMATIONS & COMBINE INTO TABLE
# ==============================================================================
results_garch_spx <- run_garch_backtest_parallel(spx, asset_name = "S&P 500", W = 1000, refit_freq = 1)
results_garch_ukx <- run_garch_backtest_parallel(ukx, asset_name = "FTSE 100", W = 1000, refit_freq = 1)

table_symmetric_garch <- rbind(results_garch_spx$table, results_garch_ukx$table)
print(table_symmetric_garch)

# ==============================================================================
# 4. EXPORT TO TABLES DIRECTORY
# ==============================================================================
table_dir <- if (dir.exists("tables")) "tables" else if (dir.exists("../tables")) "../tables" else "tables"

if (!dir.exists(table_dir)) {
  dir.create(table_dir, recursive = TRUE)
}

output_path <- file.path(table_dir, "Table_Symmetric_GARCH.xlsx")

write_xlsx(list("Symmetric_GARCH" = table_symmetric_garch), path = output_path)
cat(sprintf("\nSuccessfully exported %s!\n", output_path))