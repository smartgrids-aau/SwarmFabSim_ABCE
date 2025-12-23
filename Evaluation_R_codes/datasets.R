# clean up
rm(list = ls())
gc()

cat("\014")

# Get the directory of the currently running script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory to the directory of the script
setwd(script_dir)

library(ggplot2)
library(dplyr)
library(purrr)

# import csv file from the results folder where the R script is located
df <- read.csv("SwarmFab-Simulation SMLFAB_Baseline_ABCE_all_steps-table.csv", skip = 6)
colnames(df) <- c("RunNumber", "algorithm", "strategy", "config_file", "config_file_name", "DEBUG", "VIS", "steps", "end_time", "RPT", "entropies", "syntropies", "q_lengths", "wt_machines")

df_agg <- aggregate(
  cbind(avg_q_lengths, max_q_lengths, avg_entropies, min_entropies, wt_machines) ~ algorithm + steps,
  data = df,
  FUN = mean
)
