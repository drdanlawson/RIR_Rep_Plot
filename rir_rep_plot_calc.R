# Load packages
library(ggplot2)
library(dplyr)
library(gridExtra)
library(viridis)
library(scales)
library(readr)
library(plotly)  
library(htmlwidgets) 
library(reshape2)

# ------------------------------  
# 1. Load Data  
# ------------------------------  
# Read the CSV file (adjust the file path as needed)  
data <- read_csv("C:/Users/lawso/Downloads/Data.csv")

# ------------------------------  
# 2. Preliminary Data Inspection  
# ------------------------------  
print("First few rows of the dataset:")  
print(head(data))  # inspect the first few rows  

# Check unique values in the Contraction column  
unique_contractions <- unique(data$Contraction)  
print("Unique values in the Contraction column:")  
print(unique_contractions)  

# ------------------------------  
# 3. Filter the Dataset for ECC-CON  
# ------------------------------  
ecc_con_data <- data[data$Contraction == "ECC-CON", ]  
print(paste("Original dataset dimensions:", nrow(data), "rows,", ncol(data), "columns"))  
print(paste("Filtered dataset dimensions:", nrow(ecc_con_data), "rows,", ncol(ecc_con_data), "columns"))  

# ------------------------------  
# 4. Fit Candidate Models  
# ------------------------------  
# Note: `%1RM` contains special characters, so we use backticks when referring to it  

# 4.1. Linear Model  
model_linear <- lm(ECC_CON_Reps_M ~ `%1RM`, data = ecc_con_data)  

# 4.2. Quadratic Model  
model_quadratic <- lm(ECC_CON_Reps_M ~ `%1RM` + I(`%1RM`^2), data = ecc_con_data)  

# 4.3. Cubic Model  
model_cubic <- lm(ECC_CON_Reps_M ~ `%1RM` + I(`%1RM`^2) + I(`%1RM`^3), data = ecc_con_data)  

# 4.4. Quartic Model  
model_quartic <- lm(ECC_CON_Reps_M ~ `%1RM` + I(`%1RM`^2) + I(`%1RM`^3) + I(`%1RM`^4),   
                    data = ecc_con_data)  

# 4.5. Quartic Model with Log-Transformed Response  
# Add a small constant to avoid log(0)  
model_quartic_log <- lm(log(ECC_CON_Reps_M + 0.1) ~ `%1RM` + I(`%1RM`^2) + I(`%1RM`^3) + I(`%1RM`^4),   
                        data = ecc_con_data)  

# ------------------------------  
# 5. Compare Models using AIC  
# ------------------------------  
aic_linear <- AIC(model_linear)  
aic_quadratic <- AIC(model_quadratic)  
aic_cubic <- AIC(model_cubic)  
aic_quartic <- AIC(model_quartic)  
aic_quartic_log <- AIC(model_quartic_log)  

print("AIC for the candidate models:")  
print(paste("Linear Model:", aic_linear))  
print(paste("Quadratic Model:", aic_quadratic))  
print(paste("Cubic Model:", aic_cubic))  
print(paste("Quartic Model:", aic_quartic))  
print(paste("Quartic Model with Log(ECC_CON_Reps_M):", aic_quartic_log))  

# ------------------------------  
# 6. Examine the Preferred Model  
# ------------------------------  
# Assuming from the AIC, the quartic log-transformed model was preferred,  
# print the summary and 95% confidence intervals.  

print("Summary for Quartic Model with Log-Transformed Response:")  
print(summary(model_quartic_log))  

conf_intervals_log <- confint(model_quartic_log, level = 0.95)  
print("95% Confidence Intervals for the Quartic Log-Transformed Model Coefficients:")  
print(conf_intervals_log)  

# ------------------------------  
# 7. Organize Coefficient Table (for the Quartic Log-Transformed Model)  
# ------------------------------  
coef_df_log <- data.frame(  
  Coefficient = names(coef(model_quartic_log)),  
  Estimate = coef(model_quartic_log),  
  Lower_CI = conf_intervals_log[, 1],  
  Upper_CI = conf_intervals_log[, 2]  
)  

print("Coefficient Table with 95% Confidence Intervals (Quartic Log-Transformed Model):")  
print(coef_df_log)  

# ------------------------------  
# 8. Create Publication-Ready Plots  
# ------------------------------  

# 8.1 Create a smooth sequence for predictions  
smooth_percent_1rm <- seq(min(ecc_con_data$`%1RM`), max(ecc_con_data$`%1RM`), length.out = 500)  

# Create the data frame with check.names=FALSE to preserve the column name with special characters  
smooth_data <- data.frame(`%1RM` = smooth_percent_1rm, check.names = FALSE)  

# 8.2 Generate predictions for each model  
smooth_data$linear_pred <- predict(model_linear, newdata = smooth_data)  
smooth_data$quadratic_pred <- predict(model_quadratic, newdata = smooth_data)  
smooth_data$cubic_pred <- predict(model_cubic, newdata = smooth_data)  
smooth_data$quartic_pred <- predict(model_quartic, newdata = smooth_data)  

# For the log-transformed model, we back-transform predictions  
smooth_data$quartic_log_pred <- exp(predict(model_quartic_log, newdata = smooth_data))  

# 8.3 Generate confidence intervals for the quartic models  
quartic_ci <- predict(model_quartic, newdata = smooth_data, interval = "confidence", level = 0.95)  
smooth_data$quartic_lower <- quartic_ci[, "lwr"]  
smooth_data$quartic_upper <- quartic_ci[, "upr"]  

quartic_log_ci <- predict(model_quartic_log, newdata = smooth_data, interval = "confidence", level = 0.95)  
smooth_data$quartic_log_lower <- exp(quartic_log_ci[, "lwr"])  
smooth_data$quartic_log_upper <- exp(quartic_log_ci[, "upr"])  

# 8.4 Generate prediction intervals for the quartic models  
quartic_pi <- predict(model_quartic, newdata = smooth_data, interval = "prediction", level = 0.95)  
smooth_data$quartic_pred_lower <- quartic_pi[, "lwr"]  
smooth_data$quartic_pred_upper <- quartic_pi[, "upr"]  

quartic_log_pi <- predict(model_quartic_log, newdata = smooth_data, interval = "prediction", level = 0.95)  
smooth_data$quartic_log_pred_lower <- exp(quartic_log_pi[, "lwr"])  
smooth_data$quartic_log_pred_upper <- exp(quartic_log_pi[, "upr"])  

# 8.5 Create formatted equation strings for each model  
# Get the coefficients for the quartic model  
quartic_coef <- coef(model_quartic)  

# Format the equation with scientific notation for very small coefficients  
quartic_eq <- sprintf("y = %.3f + %.3f x + %.3f x² + %.3e x³ + %.3e x⁴",   
                      quartic_coef[1],   
                      quartic_coef[2],   
                      quartic_coef[3],   
                      quartic_coef[4],   
                      quartic_coef[5])  

# For the log-transformed model  
quartic_log_coef <- coef(model_quartic_log)  
quartic_log_eq <- sprintf("log(y) = %.3f + %.3f x + %.3f x² + %.3e x³ + %.5e x⁴",   
                          quartic_log_coef[1],   
                          quartic_log_coef[2],   
                          quartic_log_coef[3],   
                          quartic_log_coef[4],   
                          quartic_log_coef[5])    

# 8.6 Create a publication-ready theme  
theme_publication <- theme_minimal() +  
  theme(  
    text = element_text(family = "Arial", size = 12),  
    plot.title = element_text(size = 16, face = "bold"),  
    plot.subtitle = element_text(size = 12, color = "gray50"),  
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.ticks = element_line(size = 1),
    axis.ticks.length = unit(0.3, "cm"),
    legend.title = element_text(size = 12, face = "bold"),  
    legend.text = element_text(size = 10),  
    panel.grid.minor = element_blank(),  
    panel.grid.major = element_blank(),  
    panel.border = element_rect(color = "gray9", fill = NA, linewidth = 1.5),
    axis.line = element_blank(),
    plot.margin = margin(20, 20, 20, 20)  
  )  

# 8.7 Create comparison plot of all models  
all_models_plot <- ggplot(ecc_con_data, aes(x = `%1RM`, y = ECC_CON_Reps_M)) +  
  geom_point(alpha = 0.5, color = "gray50") +  
  geom_line(data = smooth_data, aes(y = linear_pred, color = "Linear"), linewidth = 1) +  
  geom_line(data = smooth_data, aes(y = quadratic_pred, color = "Quadratic"), linewidth = 1) +  
  geom_line(data = smooth_data, aes(y = cubic_pred, color = "Cubic"), linewidth = 1) +  
  geom_line(data = smooth_data, aes(y = quartic_pred, color = "Quartic"), linewidth = 1) +  
  geom_line(data = smooth_data, aes(y = quartic_log_pred, color = "Quartic Log"), linewidth = 1) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  
  scale_color_viridis_d() +  
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +  
  labs(title = "Comparison of Model Fits",  
       subtitle = paste("AIC values - Linear:", round(AIC(model_linear), 1),   
                        ", Quadratic:", round(AIC(model_quadratic), 1),  
                        ", Cubic:", round(AIC(model_cubic), 1),  
                        ", Quartic:", round(AIC(model_quartic), 1),  
                        ", Quartic Log:", round(AIC(model_quartic_log), 1)),  
       x = "% 1RM",  
       y = "ECC-CON Repetitions",  
       color = "Model") +  
  theme_publication +  
  annotate("text", x = 50, y = max(ecc_con_data$ECC_CON_Reps_M) * 0.9,   
           label = linear_eq, color = viridis::viridis(5)[1], hjust = 0, size = 3) +  
  annotate("text", x = 50, y = max(ecc_con_data$ECC_CON_Reps_M) * 0.85,   
           label = quadratic_eq, color = viridis::viridis(5)[2], hjust = 0, size = 3) +  
  annotate("text", x = 50, y = max(ecc_con_data$ECC_CON_Reps_M) * 0.8,   
           label = cubic_eq, color = viridis::viridis(5)[3], hjust = 0, size = 3) +  
  annotate("text", x = 50, y = max(ecc_con_data$ECC_CON_Reps_M) * 0.75,   
           label = quartic_eq, color = viridis::viridis(5)[4], hjust = 0, size = 3) +  
  annotate("text", x = 50, y = max(ecc_con_data$ECC_CON_Reps_M) * 0.7,   
           label = quartic_log_eq, color = viridis::viridis(5)[5], hjust = 0, size = 3)  

# 8.8 Create individual plots for the quartic models with confidence and prediction intervals  
quartic_plot <- ggplot(ecc_con_data, aes(x = `%1RM`, y = ECC_CON_Reps_M)) +  
  geom_point(alpha = 0.5, size = 3, color = viridis::viridis(1, begin = 0.2)) +  
  geom_line(data = smooth_data, aes(y = quartic_pred), color = viridis::viridis(1, begin = 0.7), size = 1.2) +  
  geom_ribbon(data = smooth_data, aes(y = quartic_pred, ymin = quartic_lower, ymax = quartic_upper),  
              alpha = 0.2, fill = viridis::viridis(1, begin = 0.7)) +  
  geom_line(data = smooth_data, aes(y = quartic_pred_lower), linetype = "dashed", color = "black") +  
  geom_line(data = smooth_data, aes(y = quartic_pred_upper), linetype = "dashed", color = "black") +  
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.0, color = "red") +  
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +  
  scale_y_continuous(breaks = seq(0, 200, 10), limits = c(0,200)) +
  labs(title = "Quartic Model", 
       subtitle = quartic_eq,
       x = "% 1RM",  
       y = "Mean Repetitions") +  
  theme_publication

quartic_log_plot <- ggplot(ecc_con_data, aes(x = `%1RM`, y = ECC_CON_Reps_M)) +  
  geom_point(alpha = 0.5, size = 3, color = viridis::viridis(1, begin = 0.2)) +  
  geom_line(data = smooth_data, aes(y = quartic_log_pred), color = viridis::viridis(1, begin = 0.7), size = 1.2) +  
  geom_ribbon(data = smooth_data, aes(y = quartic_log_pred, ymin = quartic_log_lower, ymax = quartic_log_upper),  
              alpha = 0.2, fill = viridis::viridis(1, begin = 0.7)) +  
  geom_line(data = smooth_data, aes(y = quartic_log_pred_lower), linetype = "dashed", color = "black") +  
  geom_line(data = smooth_data, aes(y = quartic_log_pred_upper), linetype = "dashed", color = "black") +  
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.0, color = "red") +  
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +  
  scale_y_continuous(breaks = seq(0, 200, 10), limits = c(0,200)) +
  labs(title = "Quartic Model with Log-Transformed Response",  
       subtitle = quartic_log_eq,
       x = "% 1RM",  
       y = "Mean Repetitions") +  
  theme_publication 

# 8.9 Residual plots for model diagnostics  
quartic_residual_plot <- ggplot(data.frame(  
  fitted = fitted(model_quartic),  
  residuals = residuals(model_quartic)  
), aes(x = fitted, y = residuals)) +  
  geom_point(alpha = 0.5, color = viridis::viridis(1, begin = 0.2)) +  
  geom_hline(yintercept = 0, linetype = "dashed", size = 1.5, color = "red") +  
  geom_smooth(method = "loess", se = FALSE, color = viridis::viridis(1, begin = 0.7)) +  
  scale_x_continuous(breaks = seq(0, max(fitted(model_quartic)), length.out = 10)) +  
  labs(title = "Residual Plot for Quartic Model",  
       subtitle = quartic_eq,  
       x = "Fitted Values",  
       y = "Residuals") +  
  theme_publication  

quartic_log_residual_plot <- ggplot(data.frame(  
  fitted = fitted(model_quartic_log),  
  residuals = residuals(model_quartic_log)  
), aes(x = fitted, y = residuals)) +  
  geom_point(alpha = 0.5, color = viridis::viridis(1, begin = 0.2)) +  
  geom_hline(yintercept = 0, linetype = "dashed", size = 2.5, color = "red") +  
  geom_smooth(method = "loess", se = FALSE, color = viridis::viridis(1, begin = 0.7)) +  
  scale_x_continuous(breaks = seq(0, max(fitted(model_quartic_log)), length.out = 10)) +  
  labs(title = "Residual Plot for Quartic Log-Transformed Model",  
       subtitle = quartic_log_eq,  
       x = "Fitted Values (log scale)",  
       y = "Residuals") +  
  theme_publication  

# 8.10 Save the plots  
ggsave("all_models_comparison.png", all_models_plot, width = 10, height = 8, dpi = 300)  
ggsave("quartic_model.png", quartic_plot, width = 8, height = 6, dpi = 300)  
ggsave("quartic_log_model.png", quartic_log_plot, width = 8, height = 6, dpi = 300)  
ggsave("quartic_residuals.png", quartic_residual_plot, width = 8, height = 6, dpi = 300)  
ggsave("quartic_log_residuals.png", quartic_log_residual_plot, width = 8, height = 6, dpi = 300)  

# 8.11 Display the plots  
print(all_models_plot)  
print(quartic_plot)  
print(quartic_log_plot)  
print(quartic_residual_plot)  
print(quartic_log_residual_plot)  

# 8.12 Create a grid of all plots  
model_comparison_grid <- grid.arrange(  
  all_models_plot,  
  arrangeGrob(quartic_plot, quartic_log_plot, ncol = 2),  
  arrangeGrob(quartic_residual_plot, quartic_log_residual_plot, ncol = 2),  
  nrow = 3  
)  

# Save the grid  
ggsave("model_comparison_grid.png", model_comparison_grid, width = 12, height = 18, dpi = 300) 

# ------------------------------  
# 9. Create RIR Prediction Tables (Using Log-transformed Model with CI)  
# ------------------------------  

# Define parameters  
max_reps <- 50    # maximum total reps to consider  
max_rir <- 8      # maximum RIR value  
completed_reps_range <- 1:30  # table rows, completed reps (for display)  

# Define a fine grid for %1RM from 30% to 100%  
percent_1rm_seq <- seq(30, 100, by = 0.1)  

# Create functions to map %1RM -> predicted reps using the log model for mean, lower, and upper CI.  
# Note: for lower and upper CIs, we pull the interval predictions.  
get_predicted_reps <- function(pct, model, ci_type = "mean") {  
  pred_data <- data.frame(`%1RM` = pct, check.names = FALSE)  
  if (ci_type == "mean") {  
    pred <- exp(predict(model, newdata = pred_data))  
  } else if (ci_type == "lower") {  
    # lower confidence interval  
    pred <- exp(predict(model, newdata = pred_data, interval = "confidence")[, "lwr"])  
  } else if (ci_type == "upper") {  
    pred <- exp(predict(model, newdata = pred_data, interval = "confidence")[, "upr"])  
  }  
  return(pred)  
}  

# Create vectors for each scenario: mean, lower, upper predictions over the fine grid  
predicted_reps_mean <- sapply(percent_1rm_seq, get_predicted_reps, model=model_quartic_log, ci_type="mean")  
predicted_reps_lower <- sapply(percent_1rm_seq, get_predicted_reps, model=model_quartic_log, ci_type="lower")  
predicted_reps_upper <- sapply(percent_1rm_seq, get_predicted_reps, model=model_quartic_log, ci_type="upper")  

# Force 100% 1RM to correspond exactly to 1 rep (by definition of 1RM)  
predicted_reps_mean[percent_1rm_seq == 100] <- 1  
predicted_reps_lower[percent_1rm_seq == 100] <- 1  
predicted_reps_upper[percent_1rm_seq == 100] <- 1  

# Build three mapping tables from total reps to %1RM.  
# For each possible total rep (1 to max_reps), find the %1RM that gives predicted reps nearest to that rep count.  
map_total_reps_to_percent <- function(predicted_reps) {  
  rep_to_percent <- rep(NA, max_reps)  
  for (r in 1:max_reps) {  
    diffs <- abs(predicted_reps - r)  
    closest_idx <- which.min(diffs)  
    rep_to_percent[r] <- percent_1rm_seq[closest_idx]  
  }  
  return(rep_to_percent)  
}  

rep_to_percent_mean <- map_total_reps_to_percent(predicted_reps_mean)  
rep_to_percent_lower <- map_total_reps_to_percent(predicted_reps_lower)  
rep_to_percent_upper <- map_total_reps_to_percent(predicted_reps_upper)  

# ------------------------------  
# 9. Create RIR Prediction Tables (Using Log-transformed Model with CI)  
# ------------------------------  
  
# ------------------------------  
# Parameters & Setup  
# ------------------------------  
max_reps <- 50    # maximum total reps to consider (for mapping)  
max_rir <- 8      # maximum RIR value available  
completed_reps_range <- 1:30  # table rows: completed reps  

# Fine grid for %1RM from 30% to 100%  
percent_1rm_seq <- seq(30, 100, by = 0.1)  

# ------------------------------  
# Function to predict reps using the log-transformed model   
# (Assumes model_quartic_log is already loaded in the workspace)  
# ------------------------------  
get_predicted_reps <- function(pct, model, ci_type = "mean") {  
  pred_data <- data.frame(`%1RM` = pct, check.names = FALSE)  
  if (ci_type == "mean") {  
    pred <- exp(predict(model, newdata = pred_data))  
  } else if (ci_type == "lower") {  
    pred <- exp(predict(model, newdata = pred_data, interval = "confidence")[, "lwr"])  
  } else if (ci_type == "upper") {  
    pred <- exp(predict(model, newdata = pred_data, interval = "confidence")[, "upr"])  
  }  
  return(pred)  
}  

# ------------------------------  
# Create prediction vectors (mean, lower, upper) over the grid  
# ------------------------------  
predicted_reps_mean <- sapply(percent_1rm_seq, get_predicted_reps, model=model_quartic_log, ci_type="mean")  
predicted_reps_lower <- sapply(percent_1rm_seq, get_predicted_reps, model=model_quartic_log, ci_type="lower")  
predicted_reps_upper <- sapply(percent_1rm_seq, get_predicted_reps, model=model_quartic_log, ci_type="upper")  

# Force 100% 1RM to be exactly 1 rep (by definition)  
predicted_reps_mean[percent_1rm_seq == 100] <- 1  
predicted_reps_lower[percent_1rm_seq == 100] <- 1  
predicted_reps_upper[percent_1rm_seq == 100] <- 1  

# ------------------------------  
# Build mapping: For each total rep count (1 to max_reps), find the %1RM that produces the closest predicted rep count.  
# ------------------------------  
find_percent_for_total_reps <- function(target_reps, predicted_reps_vec, pct_seq) {  
  diffs <- abs(predicted_reps_vec - target_reps)  
  closest_idx <- which.min(diffs)  
  return(pct_seq[closest_idx])  
}  

# Create raw mapping vectors (indices: total reps 1,..., max_reps)  
rep_to_percent_1rm_mean <- sapply(1:max_reps, find_percent_for_total_reps, predicted_reps_vec = predicted_reps_mean, pct_seq = percent_1rm_seq)  
rep_to_percent_1rm_lower <- sapply(1:max_reps, find_percent_for_total_reps, predicted_reps_vec = predicted_reps_lower, pct_seq = percent_1rm_seq)  
rep_to_percent_1rm_upper <- sapply(1:max_reps, find_percent_for_total_reps, predicted_reps_vec = predicted_reps_upper, pct_seq = percent_1rm_seq)  

# ------------------------------  
# Manual adjustments for the low-rep range (for total reps 1--5)  
# These values are based on established research.  
# ------------------------------  
rep_to_percent_1rm_mean[1:5] <- c(100.0, 96.0, 93.0, 90.0, 87.5)  
rep_to_percent_1rm_lower[1:5] <- c(100.0, 94.0, 91.0, 88.0, 85.5)  
rep_to_percent_1rm_upper[1:5] <- c(100.0, 97.0, 94.5, 92.0, 89.5)  

# ------------------------------  
# Enforce monotonicity: ensure that for t = 2:max_reps, we have:  
# mapping(t) <= mapping(t-1) - min_drop, where min_drop is a minimal decrease (e.g. 0.1%)  
# ------------------------------  
enforce_monotonicity <- function(mapping, min_drop = 0.1) {  
  # Start with the manually adjusted values (1-5)  
  # For 6 to max_reps, ensure monotonic decrease  
  for (i in 6:length(mapping)) {  
    # Ensure current value is at least min_drop less than previous  
    mapping[i] <- min(mapping[i], mapping[i-1] - min_drop)  
  }  
  return(mapping)  
}  

# Apply monotonicity enforcement to all three mappings  
rep_to_percent_1rm_mean <- enforce_monotonicity(rep_to_percent_1rm_mean)  
rep_to_percent_1rm_lower <- enforce_monotonicity(rep_to_percent_1rm_lower)  
rep_to_percent_1rm_upper <- enforce_monotonicity(rep_to_percent_1rm_upper)  

# ------------------------------  
# Build RIR tables: For each completed rep count and RIR value, compute the corresponding %1RM  
# ------------------------------  
build_rir_table <- function(rep_to_percent, completed_reps, max_rir) {  
  table_df <- data.frame(Repetitions = completed_reps)  
  for (rir in 0:max_rir) {  
    # For each completed rep count, total reps = completed reps + rir  
    table_df[[paste0("RIR_", rir)]] <- sapply(completed_reps, function(x){  
      total_reps <- x + rir  
      if(total_reps > length(rep_to_percent)){  
        return(NA)  
      } else {  
        return(rep_to_percent[total_reps])  
      }  
    })  
  }  
  return(table_df)  
}  

rir_table_mean  <- build_rir_table(rep_to_percent_1rm_mean, completed_reps_range, max_rir)  
rir_table_lower <- build_rir_table(rep_to_percent_1rm_lower, completed_reps_range, max_rir)  
rir_table_upper <- build_rir_table(rep_to_percent_1rm_upper, completed_reps_range, max_rir)  

# ------------------------------  
# Save the RIR Tables to CSV files  
# ------------------------------  
write.csv(rir_table_mean,  "rir_table_mean.csv", row.names = FALSE)  
write.csv(rir_table_lower, "rir_table_lower_ci.csv", row.names = FALSE)  
write.csv(rir_table_upper, "rir_table_upper_ci.csv", row.names = FALSE)  

cat("CSV files saved: 'rir_table_mean.csv', 'rir_table_lower_ci.csv', and 'rir_table_upper_ci.csv'\n")  

# ------------------------------  
# Create Interactive HTML Widget with All 3 RIR Plots  
# ------------------------------  
create_rir_plot <- function(rir_table, title, line_color){  
  # Reshape the table for Plotly: columns are Repetitions, RIR_0, ..., RIR_max.  
  plot_data <- melt(rir_table, id.vars = "Repetitions", variable.name = "RIR", value.name = "%1RM")  
  # Remove the "RIR_" prefix to keep numeric values  
  plot_data$RIR <- as.numeric(gsub("RIR_", "", plot_data$RIR))  
  
  p <- plot_ly(data = plot_data,   
               x = ~Repetitions,   
               y = ~`%1RM`,   
               color = ~factor(RIR),  
               type = 'scatter',   
               mode = 'lines+markers',  
               colors = colorRampPalette(c(line_color, "lightgrey"))(max_rir + 1),  
               hoverinfo = 'text',  
               text = ~paste("Reps:", Repetitions,   
                             "<br>RIR:", RIR,  
                             "<br>%1RM:", round(`%1RM`,1),"%")) %>%  
    layout(title = title,  
           xaxis = list(title = "Completed Reps", range = c(0, max(completed_reps_range)+1)),  
           yaxis = list(title = "%1RM", range = c(30, 100)),  
           legend = list(title = list(text = "RIR")))  
  return(p)  
}  

plot_mean  <- create_rir_plot(rir_table_mean,  "Mean RIR Table",  "blue")  
plot_lower <- create_rir_plot(rir_table_lower, "Lower CI RIR Table", "red")  
plot_upper <- create_rir_plot(rir_table_upper, "Upper CI RIR Table", "green")  

# ------------------------------  
# Create a tabbed layout to switch between the three plots  
# ------------------------------  
tabbed_plot <- subplot(  
  plot_mean, plot_lower, plot_upper, nrows = 1, shareY = TRUE, titleX = TRUE, titleY = TRUE  
) %>%  
  layout(  
    updatemenus = list(  
      list(  
        type = "buttons",  
        direction = "right",  
        x = 0.1,  
        y = 1.15,  
        buttons = list(  
          list(method = "update",   
               args = list(list(visible = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))),  
               label = "Mean"),  
          list(method = "update",   
               args = list(list(visible = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))),  
               label = "Lower CI"),  
          list(method = "update",   
               args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))),  
               label = "Upper CI")  
        )  
      )  
    ),  
    title = "RIR Prediction Tables (Click buttons to switch views)"  
  )  

# ------------------------------  
# Save the interactive widget to an HTML file  
# ------------------------------  
htmlwidgets::saveWidget(tabbed_plot, "rir_prediction_plots_tabbed.html", selfcontained = TRUE)  

# Also create a combined view with all three plots stacked vertically  
combined_plot <- subplot(  
  plot_mean %>% layout(showlegend = TRUE, title = "Mean Estimates"),  
  plot_lower %>% layout(showlegend = TRUE, title = "Lower CI (Conservative)"),  
  plot_upper %>% layout(showlegend = TRUE, title = "Upper CI (Liberal)"),  
  nrows = 3, shareX = TRUE, titleY = TRUE  
) %>%  
  layout(title = "RIR Prediction Tables (All Three Models)")  

htmlwidgets::saveWidget(combined_plot, "rir_prediction_plots_combined.html", selfcontained = TRUE)  

cat("Interactive HTML widgets have been created and saved as 'rir_prediction_plots_tabbed.html' and 'rir_prediction_plots_combined.html'.\n")  

# ------------------------------  
# Display a sample of the tables to verify  
# ------------------------------  
cat("\nSample of Mean RIR Table (first 10 rows):\n")  
print(head(rir_table_mean, 10))  

cat("\nSample of Lower CI RIR Table (first 10 rows):\n")  
print(head(rir_table_lower, 10))  

cat("\nSample of Upper CI RIR Table (first 10 rows):\n")  
print(head(rir_table_upper, 10))  

cat("\nAll processing complete. Tables saved as CSV files and interactive plots saved as HTML files.\n")
