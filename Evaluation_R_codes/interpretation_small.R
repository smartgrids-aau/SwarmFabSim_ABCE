# clean up
rm(list = ls())
gc()

cat("\014")

# Get the directory of the currently running script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory to the directory of the script
setwd(script_dir)

library(ggplot2)

# import csv file from the results folder where the R script is located
df <- read.csv("SFAB_Baseline_ABCE.csv", skip = 6)
# set column names
colnames(df) <- c("RunNumber", "algorithm", "strategy", "config_file", "config_file_name", "DEBUG", "VIS", "steps", "avg_q", "max_q", "min_q", "active_lots", "FF", "TRD", "RPT")

df_agg <- aggregate(cbind(max_q_length,mean_q_length) ~ steps + config_file_name, data = df, FUN = mean)

# ggplot xaxis steps vs max_q_length, color by config_file_name
ggplot(df_agg, aes(x = steps, y = max_q_length, color = config_file_name)) +
  geom_line() +
  geom_point() +
  labs(title = "Max Q Length vs Steps", x = "Steps", y = "Max Q Length") +
  theme_minimal()

# same for mean_q_length
ggplot(df_agg, aes(x = steps, y = mean_q_length, color = config_file_name)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Q Length vs Steps", x = "Steps", y = "Mean Q Length") +
  theme_minimal()


