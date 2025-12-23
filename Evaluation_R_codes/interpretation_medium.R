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
df <- read.csv("MFAB_Baseline_ABCE.csv", skip = 6)
# set column names
colnames(df) <- c("RunNumber", "algorithm", "strategy", "config_file", "config_file_name", "DEBUG", "VIS", "steps", "avg_q", "max_q", "min_q", "active_lots", "FF", "TRD", "RPT")

df_agg <- aggregate(cbind(steps,FF,TRD,max_q) ~ algorithm, data = df, FUN = mean)

# exoport the aggregated data csv
write.csv(df_agg, "ABC_E_new_MFAB.csv")

# compare basic algorithm with ABC-E (%)
steps_basic <- df_agg$steps[df_agg$algorithm == "baseline"]
steps_ABC_E <- df_agg$steps[df_agg$algorithm == "ABC-E"]
steps_diff <- (steps_basic - steps_ABC_E) / steps_basic * 100
# same for FF
FF_basic <- df_agg$FF[df_agg$algorithm == "baseline"]
FF_ABC_E <- df_agg$FF[df_agg$algorithm == "ABC-E"]
FF_diff <- (FF_basic - FF_ABC_E) / FF_basic * 100
# same for TRD
TRD_basic <- df_agg$TRD[df_agg$algorithm == "baseline"]
TRD_ABC_E <- df_agg$TRD[df_agg$algorithm == "ABC-E"]
TRD_diff <- (TRD_basic - TRD_ABC_E) / TRD_basic * 100
print(df_agg)
# print upto 2 decimal places
print(paste("steps diff (%) = ", round(steps_diff, 2)))
print(paste("FF diff (%) = ", round(FF_diff, 2)))
print(paste("TRD diff (%) = ", round(TRD_diff, 2)))



