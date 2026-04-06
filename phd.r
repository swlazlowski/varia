rm(list = ls())
# Load necessary packages
# Run install.packages(c("xts", "urca", "tsDyn")) if you don't have them
library(xts)
library(urca)
library(tsDyn)

# 1. Load CSV files
csv_files <- list.files(path = "data", pattern = "\\.csv$", full.names = TRUE)
data_list <- lapply(setNames(csv_files, tools::file_path_sans_ext(basename(csv_files))), read.csv)

# A) Treat all variables as time series
# We use 'xts' to handle daily irregular time series appropriately.
to_xts <- function(df) {
    # Assumes first column is the Date, second column is the value
    dates <- as.Date(df[[1]])
    values <- df[[2]]
    xts(values, order.by = dates)
}

# Example: Extract series for DIESEL
usp <- to_xts(data_list[["brent"]]) # Brent crude (USP)
msp <- to_xts(data_list[["MSP_DIESEL"]]) # Mid-stream Diesel
dsp <- to_xts(data_list[["DSP_GB_DIESEL"]]) # Downstream Diesel
fex <- to_xts(data_list[["FEX_GB"]]) # Exchange Rate

# Merge the series for Diesel into a single multivariate time series
# 'inner' join keeps only dates where all data is available
diesel_data <- na.omit(merge(usp, msp, dsp, fex, join = "inner"))
colnames(diesel_data) <- c("USP", "MSP", "DSP", "FEX")


# B) Test for cointegration
cat("\n--- B.1) Cointegration: USP -> MSP (w/ forex) ---\n")
# Johansen cointegration test for USP, MSP, FEX
sys_usp_msp <- diesel_data[, c("MSP", "USP", "FEX")]
jo_usp_msp <- ca.jo(sys_usp_msp, type = "trace", ecdet = "const", K = 2)
# --- INTERPRETATION GUIDE: Johansen Test (Trace) ---
# 1. Look at the 'Values of teststatistic and critical values of test' section.
# 2. Read top-down from r = 0 (no cointegration) to higher ranks.
# 3. If the 'test' statistic is GREATER than the critical value (e.g., 5pct) for r = 0,
#    you reject the null hypothesis of no cointegration. 
# 4. The first rank 'r' where you CANNOT reject the null (i.e., test < 5pct) 
#    indicates the number of cointegrating relationships.
print(summary(jo_usp_msp))

cat("\n--- B.2) Cointegration: MSP -> DSP (w/o forex) ---\n")
# Johansen cointegration test for MSP and DSP (without FEX)
sys_msp_dsp <- diesel_data[, c("DSP", "MSP")]
jo_msp_dsp <- ca.jo(sys_msp_dsp, type = "trace", ecdet = "const", K = 2)
# --- INTERPRETATION GUIDE: Johansen Test (Trace) ---
# Evaluate the trace statistic similarly. If r = 0 is rejected but r <= 1 is not,
# it implies there is exactly 1 stable long-run cointegrating relationship between MSP and DSP.
print(summary(jo_msp_dsp))


# C) Test for non-linear cointegration using LSTAR and ESTAR
cat("\n--- C) Non-linear cointegration (LSTAR / ESTAR) ---\n")

# To test non-linear cointegration, we can compute the cointegrating residuals
# (equilibrium error) and check for non-linear mean reversion using STAR models.
coint_eq <- lm(DSP ~ MSP, data = as.data.frame(sys_msp_dsp))
z_t <- ts(residuals(coint_eq)) # extracting the error correction term

# Estimate an LSTAR model on the residuals
cat("Fitting LSTAR model on cointegrating residuals:\n")
lstar_mod <- star(z_t, m = 1, d = 1, thDelay = 1, trace = FALSE)
# --- INTERPRETATION GUIDE: LSTAR Model ---
# 1. 'Gamma': Indicates the smoothness of the transition between regimes. 
#    A high gamma means the transition is abrupt (similar to a strict threshold).
#    A low gamma means the transition is very smooth.
# 2. 'Threshold' (c): The value of the transition variable (the residual here) where the regime shifts.
# 3. In the summary, look at how the coefficients change between the linear and non-linear 
#    parts to see if adjustment speed differs when the price spread is above/below the threshold.
print(summary(lstar_mod))

# Additionally, the tsDyn package allows us to test Smooth Transition (STAR) directly
# You can also use TVECM for Threshold VECM directly on the relationships:
tvecm_mod <- TVECM(sys_msp_dsp, lag = 1, nthresh = 1, trim = 0.1, trace = FALSE)
# --- INTERPRETATION GUIDE: TVECM (Threshold VECM) ---
# 1. Look at 'Threshold': This splits the Error Correction Mechanism into two regimes 
#    (e.g., small price deviations vs large price deviations).
# 2. 'Bdown' and 'Bup' are the Error Correction (speed of adjustment) coefficients for each regime.
# 3. If Bdown and Bup differ significantly, it means price transmission adjusts at 
#    different speeds depending on the direction of the shock (asymmetric price transmission).
print(summary(tvecm_mod))
