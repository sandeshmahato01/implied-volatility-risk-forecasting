library(xts)
library(rugarch)
library(readxl)
library(writexl)

# ==============================================================================
# 1. LOAD DATASETS WITH DYNAMIC PATH RESOLUTION
# ==============================================================================
data_dir <- if (dir.exists("datas")) "datas" else if (dir.exists("../datas")) "../datas" else if (dir.exists("data")) "data" else if (dir.exists("../data")) "../data" else "."

spx <- read_excel(file.path(data_dir, "spx.xlsx"))
ukx <- read_excel(file.path(data_dir, "ukx.xlsx"))

# ==============================================================================
# 2. FUNCTION: Rolling Historical Simulation VaR + Result Extractor
# ==============================================================================
run_hs_sim <- function(data, asset_name = "Asset", W = 1000) {
  
  # Clean data
  data$Date <- as.Date(data$Date)
  data$PX_LAST <- as.numeric(as.character(data$PX_LAST))
  data <- data[order(data$Date), ]
  data_clean <- na.omit(data[, c("Date", "PX_LAST")])
  
  # Percentage log returns
  price_xts <- xts(data_clean$PX_LAST, order.by = data_clean$Date)
  returns_xts <- na.omit(diff(log(price_xts))) * 100
  
  T_total <- length(returns_xts)
  out_sample_len <- T_total - W
  actual_returns <- as.numeric(returns_xts[(W + 1):T_total])
  
  hs_var_95 <- numeric(out_sample_len)
  hs_var_99 <- numeric(out_sample_len)
  
  # Rolling window estimation
  for (i in 1:out_sample_len) {
    window_data <- as.numeric(returns_xts[i:(i + W - 1)])
    hs_var_95[i] <- quantile(window_data, probs = 0.05, type = 1)
    hs_var_99[i] <- quantile(window_data, probs = 0.01, type = 1)
  }
  
  # Run Kupiec & Christoffersen backtests
  bt_95 <- VaRTest(alpha = 0.05, actual = actual_returns, VaR = hs_var_95)
  bt_99 <- VaRTest(alpha = 0.01, actual = actual_returns, VaR = hs_var_99)
  
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
  
  return(df_result)
}

# ==============================================================================
# 3. RUN ESTIMATIONS & COMBINE INTO TABLE 5.1
# ==============================================================================
res_spx <- run_hs_sim(spx, asset_name = "S&P 500", W = 1000)
res_ukx <- run_hs_sim(ukx, asset_name = "FTSE 100", W = 1000)

table_5_1_hs <- rbind(res_spx, res_ukx)
print(table_5_1_hs)

# ==============================================================================
# 4. EXPORT TO TABLES DIRECTORY
# ==============================================================================
table_dir <- if (dir.exists("tables")) "tables" else if (dir.exists("../tables")) "../tables" else "tables"

if (!dir.exists(table_dir)) {
  dir.create(table_dir, recursive = TRUE)
}

output_path <- file.path(table_dir, "Table_5_1_Historical_Simulation.xlsx")

write_xlsx(list("Table_5_1_HS" = table_5_1_hs), path = output_path)
cat(sprintf("\nSuccessfully exported %s!\n", output_path))
