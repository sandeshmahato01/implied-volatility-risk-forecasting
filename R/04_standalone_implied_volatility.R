library(xts)
library(rugarch)
library(readxl)
library(writexl)

# ==============================================================================
# 1. LOAD & CLEAN DATA FROM DATA FOLDER
# ==============================================================================
clean_bloomberg_data <- function(df) {
  df$Date <- as.Date(df$Date)
  df$PX_LAST <- as.numeric(as.character(df$PX_LAST))
  df <- df[order(df$Date), ]
  df_clean <- na.omit(df[, c("Date", "PX_LAST")])
  return(df_clean)
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
# 2. STANDALONE IMPLIED VOLATILITY BACKTEST FUNCTION + EXCEL SUMMARY EXTRACTOR
# ==============================================================================
run_standalone_iv_backtest <- function(price_clean, iv_clean, asset_name = "Asset", W = 1000) {
  
  price_xts <- xts(price_clean$PX_LAST, order.by = price_clean$Date)
  iv_xts    <- xts(iv_clean$PX_LAST,    order.by = iv_clean$Date)
  
  merged <- na.omit(merge(price = price_xts, iv = iv_xts))
  
  # Daily percentage log returns
  returns_vec <- as.numeric(diff(log(merged$price)) * 100)
  
  # Scaled daily implied volatility (strictly lagged ln(IV_{t-1}))
  iv_daily_sd <- as.numeric(lag(merged$iv, k = 1)) / sqrt(252)
  
  returns_vec <- returns_vec[-1]
  iv_daily_sd <- iv_daily_sd[-1]
  
  T_total <- length(returns_vec)
  out_sample_len <- T_total - W
  
  actual_returns <- returns_vec[(W + 1):T_total]
  out_iv_sd      <- iv_daily_sd[(W + 1):T_total]
  
  # Rolling mean of historical window
  mu_roll <- numeric(out_sample_len)
  for (i in 1:out_sample_len) {
    mu_roll[i] <- mean(returns_vec[i:(i + W - 1)])
  }
  
  # Parametric VaR Forecasts (Gaussian Quantiles)
  var_95 <- mu_roll + out_iv_sd * qnorm(0.05)
  var_99 <- mu_roll + out_iv_sd * qnorm(0.01)
  
  cat("\n=======================================================\n")
  cat("  STANDALONE IMPLIED VOLATILITY BACKTEST:", toupper(asset_name), "\n")
  cat("  Total Observations:", T_total, "| In-Sample (W):", W, "| Out-of-Sample:", out_sample_len, "\n")
  cat("=======================================================\n")
  
  # Regulatory backtests
  bt_95 <- VaRTest(alpha = 0.05, actual = actual_returns, VaR = var_95)
  bt_99 <- VaRTest(alpha = 0.01, actual = actual_returns, VaR = var_99)
  
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
    actual = actual_returns,
    var_95 = var_95,
    var_99 = var_99,
    table  = df_result
  ))
}

# ==============================================================================
# 3. RUN MODEL BACKTESTS & COMBINE INTO TABLE 5.4
# ==============================================================================
results_iv_spx <- run_standalone_iv_backtest(
  price_clean = spx_clean, 
  iv_clean    = vix_clean, 
  asset_name  = "S&P 500 (VIX)", 
  W = 1000
)

results_iv_ukx <- run_standalone_iv_backtest(
  price_clean = ukx_clean, 
  iv_clean    = ivukx30_clean, 
  asset_name  = "FTSE 100 (VFTSE)", 
  W = 1000
)

table_5_4_iv <- rbind(results_iv_spx$table, results_iv_ukx$table)
print(table_5_4_iv)

# ==============================================================================
# 4. EXPORT TO TABLES DIRECTORY
# ==============================================================================
table_dir <- if (dir.exists("tables")) "tables" else if (dir.exists("../tables")) "../tables" else "tables"

if (!dir.exists(table_dir)) {
  dir.create(table_dir, recursive = TRUE)
}

output_path <- file.path(table_dir, "Table_5_4_Standalone_IV.xlsx")

write_xlsx(list("Table_5_4_IV" = table_5_4_iv), path = output_path)
cat(sprintf("\nSuccessfully exported %s!\n", output_path))
