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
df <- read.csv("SwarmFab-Simulation SFAB_Baseline_ABCE_all_steps-table.csv", skip = 6)
colnames(df) <- c("RunNumber", "algorithm", "strategy", "config_file", "config_file_name", "DEBUG", "VIS", "steps", "end_time", "RPT", "entropies", "syntropies", "q_lengths", "wt_machines")

convert_to_vec <- function(cell) {
  cell <- gsub("\\[|\\]", "", cell)
  as.numeric(strsplit(trimws(cell), "\\s+")[[1]])
}

df <- df %>%
  mutate(
    q_lengths2 = map(q_lengths, convert_to_vec),
    avg_q_lengths = map_dbl(q_lengths2, ~ mean(.x, na.rm = TRUE)),
    max_q_lengths = map_dbl(q_lengths2, ~ max(.x, na.rm = TRUE)),
    entropies2 = map(entropies, convert_to_vec),
    avg_entropies = map_dbl(entropies2, ~ mean(.x, na.rm = TRUE)),
    min_entropies = map_dbl(entropies2, ~ min(.x, na.rm = TRUE)),
  )

df_agg <- aggregate(
  cbind(avg_q_lengths, max_q_lengths, avg_entropies, min_entropies, wt_machines) ~ algorithm + steps,
  data = df,
  FUN = mean
)

library(dplyr)
library(ggplot2)

# -------------------------
# Normalize algorithm names
# -------------------------
df_agg <- df_agg %>%
  mutate(
    algorithm = recode(
      as.character(algorithm),
      "baseline" = "Baseline",
      "basic"    = "Baseline",
      "rw"       = "Baseline",
      "ABC-E"    = "ABC-E",
      "abce"     = "ABC-E",
      "abc-e"    = "ABC-E"
    ),
    algorithm = factor(algorithm, levels = c("Baseline", "ABC-E"))
  )

# -------------------------
# Common theme (journal style)
# -------------------------
base_size <- 14

base_theme <- theme_minimal(base_size = base_size) +
  theme(
    legend.position = "bottom",
    plot.title = element_blank()
  )

# -------------------------
# Journal-quality save helper
# -------------------------
save_journal <- function(p, name_no_ext, width = 7, height = 5, dpi = 600) {
  ggsave(
    paste0(name_no_ext, ".pdf"),
    plot = p,
    device = cairo_pdf,
    width = width,
    height = height,
    units = "in"
  )
  ggsave(
    paste0(name_no_ext, ".png"),
    plot = p,
    dpi = dpi,
    width = width,
    height = height,
    units = "in"
  )
  ggsave(
    paste0(name_no_ext, ".tiff"),
    plot = p,
    dpi = dpi,
    compression = "lzw",
    width = width,
    height = height,
    units = "in"
  )
}

# -------------------------
# p1: avg_q_lengths
# -------------------------
p1 <- ggplot(df_agg, aes(x = steps, y = avg_q_lengths, color = algorithm)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Steps",
    y = "Average queue lengths",
    color = "Algorithm"
  ) +
  base_theme
print(p1)
save_journal(p1, "all_steps_avg_q_lengths")

# -------------------------
# p2: max_q_lengths
# -------------------------
p2 <- ggplot(df_agg, aes(x = steps, y = max_q_lengths, color = algorithm)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Steps",
    y = "Maximum queue lengths",
    color = "Algorithm"
  ) +
  base_theme
print(p2)
save_journal(p2, "all_steps_max_q_lengths")

# -------------------------
# p3: avg_entropies (skip first 100 steps)
# -------------------------
p3 <- ggplot(df_agg %>% filter(steps >= 100),
             aes(x = steps, y = avg_entropies, color = algorithm)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Steps",
    y = "Average entropy",
    color = "Algorithm"
  ) +
  base_theme
print(p3)
save_journal(p3, "all_steps_avg_entropies")

# -------------------------
# p4: min_entropies (skip first 100 steps)
# -------------------------
p4 <- ggplot(df_agg %>% filter(steps >= 100),
             aes(x = steps, y = min_entropies, color = algorithm)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Steps",
    y = "Minimum entropy",
    color = "Algorithm"
  ) +
  base_theme
print(p4)
save_journal(p4, "all_steps_min_entropies")

# -------------------------
# p5: wt_machines (skip first 100 steps)
# -------------------------
p5 <- ggplot(df_agg %>% filter(steps >= 100),
             aes(x = steps, y = wt_machines, color = algorithm)) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Steps",
    y = "Machines waiting for a full batch",
    color = "Algorithm"
  ) +
  base_theme
print(p5)
save_journal(p5, "all_steps_wt_machines")
