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
df <- read.csv("SwarmFab-Simulation SFAB_LA_ABCE-table.csv", skip = 6)
# set column names
colnames(df) <- c("RunNumber", "algorithm", "strategy", "config_file", "config_file_name", "DEBUG", "VIS", "lookahead", "steps", "FF", "TRD")

df_agg <- aggregate(cbind(steps,FF,TRD) ~ algorithm+lookahead, data = df, FUN = mean)

print(df_agg)


# import csv file from the results folder where the R script is located
baseline <- read.csv("SFAB_Baseline_ABCE.csv", skip = 6)
colnames(baseline) <- c("RunNumber", "algorithm", "strategy", "config_file", "config_file_name", "DEBUG", "VIS", "steps", "avg_q", "max_q", "min_q", "active_lots", "FF", "TRD", "RPT")

baseline_agg <- aggregate(cbind(steps,FF,TRD) ~ algorithm, data = baseline, FUN = mean)

# box plots for each lookahead
ggplot(df, aes(x = as.factor(lookahead), y = steps, fill = as.factor(lookahead))) +
  geom_boxplot() +
  # center
  labs(x = "Lookahead", y = "Steps",  ) +
  # add baseline line
  geom_hline(data = baseline_agg, aes(yintercept = steps), linetype = "dashed", color = "darkgray") +
  # add "Baseline algorithm" text gray and under the line on the very left
  annotate("text", x = 1, y = baseline_agg$steps, label = "Baseline algorithm", vjust = -1, color = "darkgray") +
  # leged
  scale_fill_discrete(name = "Lookahead window") +
  theme_minimal()

# box plots for each lookahead
ggplot(df, aes(x = as.factor(lookahead), y = FF, fill = as.factor(lookahead))) +
  geom_boxplot() +
  # center
  labs(x = "Lookahead", y = "Flow Factor",  ) +
  # add baseline line
  geom_hline(data = baseline_agg, aes(yintercept = FF), linetype = "dashed", color = "darkgray") +
  # add "Baseline algorithm" text gray and under the line on the very left
  annotate("text", x = 1, y = baseline_agg$FF, label = "Baseline algorithm", vjust = -1, color = "darkgray") +
  # leged
  scale_fill_discrete(name = "Lookahead window") +
  theme_minimal()

# box plots for each lookahead
ggplot(df, aes(x = as.factor(lookahead), y = TRD, fill = as.factor(lookahead))) +
  geom_boxplot() +
  # center
  labs(x = "Lookahead window", y = "Tardiness",  ) +
  # add smaller text

  # no legend
  scale_fill_discrete(guide="none") +
  theme_minimal()
# save png

ggsave("param_study_lookahead.jpg", units = "cm")

