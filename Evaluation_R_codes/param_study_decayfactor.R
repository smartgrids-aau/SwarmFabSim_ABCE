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
df <- read.csv("SwarmFab-Simulation SFAB_DF_ABCE-table.csv", skip = 6)
# set column names
colnames(df) <- c("RunNumber", "algorithm", "strategy", "config_file", "config_file_name", "DEBUG", "VIS", "decayFactor", "steps", "FF", "TRD")

df_agg <- aggregate(cbind(steps,FF,TRD) ~ algorithm+decayFactor, data = df, FUN = mean)

print(df_agg)


# import csv file from the results folder where the R script is located
baseline <- read.csv("SFAB_Baseline_ABCE.csv", skip = 6)
colnames(baseline) <- c("RunNumber", "algorithm", "strategy", "config_file", "config_file_name", "DEBUG", "VIS", "steps", "avg_q", "max_q", "min_q", "active_lots", "FF", "TRD", "RPT")
baseline_agg <- aggregate(cbind(steps,FF,TRD) ~ algorithm, data = baseline, FUN = mean)

# box plots for each decayFactor
ggplot(df, aes(x = as.factor(decayFactor), y = steps, fill = as.factor(decayFactor))) +
  geom_boxplot() +
  # center
  labs(x = "Discount factor", y = "Steps",  ) +
  # add baseline line
  geom_hline(data = baseline_agg, aes(yintercept = steps), linetype = "dashed", color = "darkgray") +
  # add "Baseline algorithm" text gray and under the line on the very left
  annotate("text", x = 1, y = baseline_agg$steps, label = "Baseline algorithm", vjust = -1, color = "darkgray") +
  # leged
  scale_fill_discrete() +
  theme_minimal()

# box plots for each decayFactor
ggplot(df, aes(x = as.factor(decayFactor), y = FF, fill = as.factor(decayFactor))) +
  geom_boxplot() +
  # center
  labs(x = "Discount factor", y = "Flow Factor",  ) +
  # add baseline line
  geom_hline(data = baseline_agg, aes(yintercept = FF), linetype = "dashed", color = "darkgray") +
  # add "Baseline algorithm" text gray and under the line on the very left
  annotate("text", x = 1, y = baseline_agg$FF, label = "Baseline algorithm", vjust = -1, color = "darkgray") +
  # leged
  scale_fill_discrete() +
  theme_minimal()

# box plots for each decayFactor
ggplot(df, aes(x = as.factor(decayFactor), y = TRD, fill = as.factor(decayFactor))) +
  geom_boxplot() +
  # center
  labs(x = "Discount Factor", y = "Tardiness",  ) +
  
  scale_fill_discrete(guide="none") +
  theme_minimal()

ggsave("param_study_df.jpg", units = "cm")
