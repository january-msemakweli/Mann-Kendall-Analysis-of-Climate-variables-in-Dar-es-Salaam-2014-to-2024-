knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(dplyr)
library(Kendall)
library(trend)
library(knitr)
library(kableExtra)
library(ggplot2)
library(tidyr)

df <- read_csv("CLIMATE DAR DATASET.csv")
df$date <- as.Date(paste(df$Year, df$Month, "01", sep = "-"), format="%Y-%b-%d")

vars <- c("rainfall_mm",
          "daytime_temperature_celcius",
          "nighttime_temperature_celcius",
          "relative_humidity_%")

var_labels <- c("Rainfall (mm)",
                "Daytime Temperature (°C)",
                "Nighttime Temperature (°C)",
                "Relative Humidity (%)")

run_mk <- function(x) {
  mk <- MannKendall(x)
  sen <- sens.slope(x)$estimates
  S <- mk$S
  varS <- mk$varS
  
  if (varS > 0) {
    if (S > 0) {
      Z <- (S - 1) / sqrt(varS)
    } else if (S == 0) {
      Z <- 0
    } else {
      Z <- (S + 1) / sqrt(varS)
    }
  } else {
    Z <- 0
  }
  
  return(list(Z = Z, p = mk$sl, slope = sen))
}

results <- data.frame()

for (s in unique(df$Season)) {
  sub_data <- df %>% filter(Season == s) %>% arrange(date)
  
  for (i in seq_along(vars)) {
    v <- vars[i]
    label <- var_labels[i]
    x <- sub_data[[v]]
    
    if (length(x) >= 8) {
      mkout <- run_mk(x)
      results <- rbind(results, data.frame(
        Season = s,
        Variable = label,
        Z = round(mkout$Z, 4),
        p_value = round(mkout$p, 5),
        Sen_slope = round(mkout$slope, 5)
      ))
    }
  }
}

results %>%
  kable(
    format = "html",
    caption = "Mann–Kendall Trend Test Results for Seasonal Climatic Variables (2014–2024)",
    col.names = c("Season", "Variable", "Z-Score", "p-value", "Sen's slope"),
    digits = 5
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

## Panel Plot of Significant Trends
df_long <- df %>%
  select(date, Season, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "var", values_to = "value") %>%
  mutate(Variable = var_labels[match(var, vars)])

sig_results <- results %>%
  filter(p_value < 0.05) %>%
  select(Season, Variable) %>%
  distinct()

df_long_sig <- df_long %>%
  inner_join(sig_results, by = c("Season", "Variable"))

df_panel <- df_long_sig %>%
  group_by(Season, Variable) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(t = row_number()) %>%
  group_modify(~ {
    sen <- sens.slope(.x$value)$estimates
    intercept <- median(.x$value - sen * .x$t, na.rm = TRUE)
    .x %>%
      mutate(
        sen_slope = sen,
        intercept = intercept,
        sen_line = intercept + sen * t
      )
  }) %>%
  ungroup()

p1 <- ggplot(df_panel, aes(x = date)) +
  geom_point(aes(y = value)) +
  geom_line(aes(y = sen_line), linewidth = 1) +
  facet_grid(Variable ~ Season, scales = "free_y") +
  labs(x = "Year", y = "Value")

print(p1)
ggsave("panel_significant_trends.png", p1, width = 10, height = 8, dpi = 300)

### Complete Time Series for All Variables and Seasons
df_all_panel <- df_long %>%
  group_by(Season, Variable) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(t = row_number()) %>%
  group_modify(~ {
    if (length(.x$value) >= 8) {
      sen <- sens.slope(.x$value)$estimates
      intercept <- median(.x$value - sen * .x$t, na.rm = TRUE)
      .x %>%
        mutate(
          sen_slope = sen,
          intercept = intercept,
          sen_line = intercept + sen * t
        )
    } else {
      .x %>%
        mutate(
          sen_slope = NA,
          intercept = NA,
          sen_line = NA
        )
    }
  }) %>%
  ungroup() %>%
  left_join(results, by = c("Season", "Variable")) %>%
  mutate(Significance = if_else(p_value < 0.05, "Significant (p < 0.05)", "Not Significant"))

p2 <- ggplot(df_all_panel, aes(x = date, y = value)) +
  geom_point(aes(color = Significance), alpha = 0.6, size = 1) +
  geom_line(aes(y = sen_line, linetype = Significance), linewidth = 1.2, na.rm = TRUE) +
  scale_color_manual(values = c("Significant (p < 0.05)" = "#d62728", 
                                 "Not Significant" = "#636363")) +
  scale_linetype_manual(values = c("Significant (p < 0.05)" = "solid", 
                                   "Not Significant" = "dashed")) +
  facet_grid(Variable ~ Season, scales = "free_y") +
  labs(
    x = "Year",
    y = "Value",
    color = "Trend Significance",
    linetype = "Trend Significance",
    subtitle = "Solid lines indicate statistically significant trends (p < 0.05); dashed lines indicate non-significant trends"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(size = 10),
    legend.position = "bottom"
  )

print(p2)
ggsave("complete_time_series.png", p2, width = 12, height = 10, dpi = 300)

### Seasonal Box Plots
p3 <- ggplot(df_long, aes(x = Season, y = value)) +
  stat_boxplot(geom = "errorbar", width = 0.6, linewidth = 0.7, color = "black", 
               coef = 1.5) +
  geom_boxplot(fill = "white", color = "black", outlier.color = "black", 
               linewidth = 0.7, fatten = 1, coef = 1.5, 
               width = 0.6, outlier.size = 1.5, show.legend = FALSE) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  labs(x = "Season", y = "Value") +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "white", linewidth = 0.5),
    panel.grid.minor = element_line(color = "white", linewidth = 0.25),
    panel.background = element_rect(fill = "lightgrey"),
    panel.border = element_rect(color = "black", fill = NA),
    strip.background = element_rect(fill = "grey", color = "black"),
    strip.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "none"
  )

print(p3)
ggsave("seasonal_boxplots.png", p3, width = 10, height = 8, dpi = 300)

### Sen's Slope Magnitude Visualization
results_viz <- results %>%
  mutate(
    Significance = if_else(p_value < 0.05, "Significant", "Not Significant"),
    Trend_Direction = if_else(Sen_slope > 0, "Increasing", "Decreasing")
  )

p4 <- ggplot(results_viz, aes(x = reorder(paste(Season, Variable, sep = " - "), Sen_slope), 
                        y = Sen_slope, 
                        fill = Significance)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("Significant" = "#d62728", 
                                "Not Significant" = "#95a5a6")) +
  labs(
    x = "Season - Variable",
    y = "Sen's Slope (units/year)",
    fill = "Statistical Significance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

print(p4)
ggsave("sens_slope_magnitude.png", p4, width = 10, height = 8, dpi = 300)

### Annual Trends in Climatic Variables by Season (2014-2024)
#### Dashed lines show linear trend fits with 95% confidence intervals
df_annual <- df %>%
  group_by(Year, Season) %>%
  summarise(
    Rainfall = mean(rainfall_mm, na.rm = TRUE),
    Daytime_Temp = mean(daytime_temperature_celcius, na.rm = TRUE),
    Nighttime_Temp = mean(nighttime_temperature_celcius, na.rm = TRUE),
    Humidity = mean(`relative_humidity_%`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Rainfall, Daytime_Temp, Nighttime_Temp, Humidity),
    names_to = "Variable",
    values_to = "Annual_Mean"
  ) %>%
  mutate(
    Variable = case_when(
      Variable == "Rainfall" ~ "Rainfall (mm)",
      Variable == "Daytime_Temp" ~ "Daytime Temperature (°C)",
      Variable == "Nighttime_Temp" ~ "Nighttime Temperature (°C)",
      Variable == "Humidity" ~ "Relative Humidity (%)"
    )
  )

p5 <- ggplot(df_annual, aes(x = Year, y = Annual_Mean, color = Season)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_line(linewidth = 1, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", linewidth = 1, alpha = 0.3) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  labs(
    x = "Year",
    y = "Annual Mean Value",
    color = "Season"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(size = 10),
    legend.position = "bottom"
  ) +
  scale_color_brewer(palette = "Set2")

print(p5)
ggsave("annual_trends.png", p5, width = 10, height = 8, dpi = 300)

### Monthly Climatology of Climatic Variables (2014-2024)
#### Average monthly values showing typical annual cycles

df_monthly <- df %>%
  mutate(Month_Num = as.numeric(format(date, "%m"))) %>%
  group_by(Month_Num, Season) %>%
  summarise(
    Rainfall = mean(rainfall_mm, na.rm = TRUE),
    Daytime_Temp = mean(daytime_temperature_celcius, na.rm = TRUE),
    Nighttime_Temp = mean(nighttime_temperature_celcius, na.rm = TRUE),
    Humidity = mean(`relative_humidity_%`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Rainfall, Daytime_Temp, Nighttime_Temp, Humidity),
    names_to = "Variable",
    values_to = "Monthly_Mean"
  ) %>%
  mutate(
    Variable = case_when(
      Variable == "Rainfall" ~ "Rainfall (mm)",
      Variable == "Daytime_Temp" ~ "Daytime Temperature (°C)",
      Variable == "Nighttime_Temp" ~ "Nighttime Temperature (°C)",
      Variable == "Humidity" ~ "Relative Humidity (%)"
    ),
    Month_Name = month.abb[Month_Num]
  ) %>%
  mutate(Month_Name = factor(Month_Name, levels = month.abb))

p6 <- ggplot(df_monthly, aes(x = Month_Name, y = Monthly_Mean, color = Season, group = Season)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  labs(
    x = "Month",
    y = "Average Value",
    color = "Season"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "white", linewidth = 0.5),
    panel.grid.minor = element_line(color = "white", linewidth = 0.25),
    panel.background = element_rect(fill = "lightgrey"),
    panel.border = element_rect(color = "black", fill = NA),
    strip.background = element_rect(fill = "grey", color = "black"),
    strip.text = element_text(color = "black", size = 10),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black")
  ) +
  scale_color_brewer(palette = "Set2")

print(p6)
ggsave("monthly_climatology.png", p6, width = 10, height = 6, dpi = 300)

### Mann-Kendall Z-Scores by Season and Variable
#### Horizontal dashed lines indicate critical values (±1.96) for α = 0.05
results_viz2 <- results %>%
  mutate(
    Significance = if_else(p_value < 0.05, "Significant", "Not Significant"),
    Z_abs = abs(Z)
  )

p7 <- ggplot(results_viz2, aes(x = reorder(paste(Season, Variable, sep = " - "), Z), 
                         y = Z, 
                         fill = Significance)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_hline(yintercept = c(-1.96, 0, 1.96), 
             linetype = c("dashed", "solid", "dashed"), 
             color = c("red", "black", "red"), 
             linewidth = 0.7) +
  annotate("text", x = 1, 
           y = c(-1.96, 1.96), 
           label = c("Z = -1.96\n(α = 0.05)", "Z = 1.96\n(α = 0.05)"), 
           vjust = c(1.2, -0.2), hjust = 0, size = 3, color = "red") +
  coord_flip() +
  scale_fill_manual(values = c("Significant" = "#d62728", 
                               "Not Significant" = "#95a5a6")) +
  labs(
    x = "Season - Variable",
    y = "Z-Score",
    fill = "Statistical Significance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 8),
    legend.position = "bottom"
  )

print(p7)
ggsave("zscore_visualization.png", p7, width = 10, height = 8, dpi = 300)

### Trend Magnitudes by Variable Across Seasons
#### Positive values indicate increasing trends; negative values indicate decreasing trends
p8 <- ggplot(results_viz, aes(x = Season, y = Sen_slope, fill = Trend_Direction)) +
  geom_bar(stat = "identity", alpha = 0.8, position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("Increasing" = "#d62728", 
                               "Decreasing" = "#2ca02c")) +
  labs(
    x = "Season",
    y = "Sen's Slope (units/year)",
    fill = "Trend Direction"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    strip.text = element_text(size = 10),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p8)
ggsave("trend_magnitudes_by_variable.png", p8, width = 10, height = 8, dpi = 300)