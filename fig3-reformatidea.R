library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggpubr)

## Carmen - I tried to redo this from your raw data, but had trouble figuring out which file to source
## So I used Claude, gave it the original and told it what I wanted it to look like
## It used simulated data from the figure, so obviously needs to be redone with the actual data, but - 
## This has some redundancy with the earlier graph (in the lambdas) but i think its worth it for the comparison
## specifically, I think it makes the fact that legumes compete with brome more than facilitate at low water
## but facilitate more than compete at high water levels, super clear
## it also makes an interesting point that, even though sterilized soils aren't good for the legume
## they have a bigger competitive effect of brome under them (because the facilitation, the benefits both brome and them, isn't there)
## lastly, i think it makes the fact that symbionts are essential for legumes to ever invade quite clearly


# Create a sample dataframe based on values from the figure
set.seed(123)

# Function to create random points around a mean with error
create_points <- function(mean_val, n_points = 30, sd_val = mean_val * 0.05) {
  return(rnorm(n_points, mean = mean_val, sd = sd_val))
}

# Water levels and soil treatments
water_levels <- c("Low", "Intermediate", "High")
water_numeric <- c(1, 2, 3)  # Numeric representation for plotting on x-axis
soil_treatments <- c("Live", "Sterilized")
growth_types <- c("λi", "ri")

# Estimated mean values from the figure (log scale)
# Format: legume_lambda, legume_r for each water/soil combination
# Live Low, Sterilized Low, Live Int, Sterilized Int, Live High, Sterilized High
legume_lambda <- c(2.7, 2.45, 3.1, 2.55, 3.1, 2.6)
legume_r <- c(-2.2, -2.4, -1.8, -2.2, 0.5, -0.9)

grass_lambda <- c(6.1, 6.1, 6.3, 6.3, 6.3, 6.3)
grass_r <- c(5.5, 4.8, 6.1, 4.9, 6.7, 5.8)

# Create empty dataframe
n_points <- 30
df <- data.frame()

# Generate data points
counter <- 1
for (i in 1:length(water_levels)) {
  for (j in 1:length(soil_treatments)) {
    # Legume data points
    legume_lambda_points <- create_points(legume_lambda[counter], sd_val = 0.05)
    legume_r_points <- create_points(legume_r[counter], sd_val = 0.1)
    
    # Grass data points
    grass_lambda_points <- create_points(grass_lambda[counter], sd_val = 0.05)
    grass_r_points <- create_points(grass_r[counter], sd_val = 0.1)
    
    # Add lambda points to dataframe
    temp_df_lambda <- data.frame(
      WaterLevel = rep(water_levels[i], n_points),
      WaterNumeric = rep(water_numeric[i], n_points),
      SoilTreatment = rep(soil_treatments[j], n_points),
      GrowthType = rep("λi", n_points),
      LegumeGrowth_log = legume_lambda_points,
      GrassGrowth_log = grass_lambda_points,
      LegumeGrowth_raw = exp(legume_lambda_points),
      GrassGrowth_raw = exp(grass_lambda_points)
    )
    
    # Add r points to dataframe
    temp_df_r <- data.frame(
      WaterLevel = rep(water_levels[i], n_points),
      WaterNumeric = rep(water_numeric[i], n_points),
      SoilTreatment = rep(soil_treatments[j], n_points),
      GrowthType = rep("ri", n_points),
      LegumeGrowth_log = legume_r_points,
      GrassGrowth_log = grass_r_points,
      LegumeGrowth_raw = exp(legume_r_points),
      GrassGrowth_raw = exp(grass_r_points)
    )
    
    # Combine and add to main dataframe
    df <- rbind(df, temp_df_lambda, temp_df_r)
    counter <- counter + 1
  }
}

# Calculate means and standard errors for summary points
df_summary <- df %>%
  group_by(WaterLevel, WaterNumeric, SoilTreatment, GrowthType) %>%
  summarize(
    LegumeGrowth_log_mean = mean(LegumeGrowth_log),
    LegumeGrowth_log_se = sd(LegumeGrowth_log) / sqrt(n()),
    GrassGrowth_log_mean = mean(GrassGrowth_log),
    GrassGrowth_log_se = sd(GrassGrowth_log) / sqrt(n()),
    LegumeGrowth_raw_mean = mean(LegumeGrowth_raw),
    LegumeGrowth_raw_se = sd(LegumeGrowth_raw) / sqrt(n()),
    GrassGrowth_raw_mean = mean(GrassGrowth_raw),
    GrassGrowth_raw_se = sd(GrassGrowth_raw) / sqrt(n())
  )

# Function to create a consistent plotting theme
my_theme <- function() {
  theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
}

# Calculate differences between live and sterilized for annotations
soil_diffs <- df_summary %>%
  select(WaterLevel, GrowthType, SoilTreatment, 
         LegumeGrowth_log_mean, GrassGrowth_log_mean,
         LegumeGrowth_raw_mean, GrassGrowth_raw_mean) %>%
  pivot_wider(
    names_from = SoilTreatment,
    values_from = c(LegumeGrowth_log_mean, GrassGrowth_log_mean, 
                    LegumeGrowth_raw_mean, GrassGrowth_raw_mean)
  ) %>%
  mutate(
    legume_log_diff = LegumeGrowth_log_mean_Live - LegumeGrowth_log_mean_Sterilized,
    grass_log_diff = GrassGrowth_log_mean_Live - GrassGrowth_log_mean_Sterilized,
    legume_raw_diff = LegumeGrowth_raw_mean_Live - LegumeGrowth_raw_mean_Sterilized,
    grass_raw_diff = GrassGrowth_raw_mean_Live - GrassGrowth_raw_mean_Sterilized
  )

# Function to add annotations for notable differences
add_annotations <- function(plot, soil_diffs, species, scale_type) {
  diff_col <- paste0(species, "_", scale_type, "_diff")
  threshold <- if(scale_type == "log") 0.5 else 10
  
  notable_diffs <- soil_diffs %>%
    filter(abs(!!sym(diff_col)) > threshold)
  
  if(nrow(notable_diffs) > 0) {
    for(i in 1:nrow(notable_diffs)) {
      row <- notable_diffs[i,]
      x_pos <- which(growth_types == row$GrowthType)
      y_pos <- if(scale_type == "log") {
        max(df_summary[[paste0(species, "Growth_log_mean")]]) * 0.9
      } else {
        max(df_summary[[paste0(species, "Growth_raw_mean")]]) * 0.9
      }
      
      plot <- plot + 
        annotate("text", 
                 x = x_pos, 
                 y = y_pos, 
                 label = sprintf("Δ = %.1f", row[[diff_col]]), 
                 size = 3,
                 fontface = "bold",
                 color = if(row[[diff_col]] > 0) "blue" else "red")
    }
  }
  return(plot)
}

# ########################
# # OPTION 1: Swap x-axis and coloring
# ########################
# 
# # Legume Growth Rate (Log Scale) - Option 1
# p1_log <- ggplot() +
#   geom_boxplot(data = df, 
#                aes(x = GrowthType, y = LegumeGrowth_log, fill = SoilTreatment),
#                position = position_dodge(width = 0.8), width = 0.7, alpha = 0.7) +
#   geom_point(data = df, 
#              aes(x = GrowthType, y = LegumeGrowth_log, color = SoilTreatment),
#              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
#              alpha = 0.3, size = 1) +
#   facet_grid(. ~ WaterLevel) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   scale_fill_manual(values = c("Live" = "#4B0082", "Sterilized" = "#FFC0CB")) +
#   scale_color_manual(values = c("Live" = "#4B0082", "Sterilized" = "#FFC0CB")) +
#   labs(x = "Type of Growth Rate", y = "Legume Growth Rate (log scale)", title = "A: Legume Growth Rate") +
#   my_theme()
# 
# # Grass Growth Rate (Log Scale) - Option 1
# p2_log <- ggplot() +
#   geom_boxplot(data = df, 
#                aes(x = GrowthType, y = GrassGrowth_log, fill = SoilTreatment),
#                position = position_dodge(width = 0.8), width = 0.7, alpha = 0.7) +
#   geom_point(data = df, 
#              aes(x = GrowthType, y = GrassGrowth_log, color = SoilTreatment),
#              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
#              alpha = 0.3, size = 1) +
#   facet_grid(. ~ WaterLevel) +
#   scale_fill_manual(values = c("Live" = "#006400", "Sterilized" = "#90EE90")) +
#   scale_color_manual(values = c("Live" = "#006400", "Sterilized" = "#90EE90")) +
#   labs(x = "Type of Growth Rate", y = "Grass Growth Rate (log scale)", title = "B: Grass Growth Rate") +
#   my_theme()
# 
# # Raw data versions
# p1_raw <- ggplot() +
#   geom_boxplot(data = df, 
#                aes(x = GrowthType, y = LegumeGrowth_raw, fill = SoilTreatment),
#                position = position_dodge(width = 0.8), width = 0.7, alpha = 0.7) +
#   geom_point(data = df, 
#              aes(x = GrowthType, y = LegumeGrowth_raw, color = SoilTreatment),
#              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
#              alpha = 0.3, size = 1) +
#   facet_grid(. ~ WaterLevel) +
#   scale_fill_manual(values = c("Live" = "#4B0082", "Sterilized" = "#FFC0CB")) +
#   scale_color_manual(values = c("Live" = "#4B0082", "Sterilized" = "#FFC0CB")) +
#   labs(x = "Type of Growth Rate", y = "Legume Growth Rate (raw)", title = "A: Legume Growth Rate") +
#   my_theme()
# 
# 
# p2_raw <- ggplot() +
#   geom_boxplot(data = df, 
#                aes(x = GrowthType, y = GrassGrowth_raw, fill = SoilTreatment),
#                position = position_dodge(width = 0.8), width = 0.7, alpha = 0.7) +
#   geom_point(data = df, 
#              aes(x = GrowthType, y = GrassGrowth_raw, color = SoilTreatment),
#              position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8), 
#              alpha = 0.3, size = 1) +
#   facet_grid(. ~ WaterLevel) +
#   scale_fill_manual(values = c("Live" = "#006400", "Sterilized" = "#90EE90")) +
#   scale_color_manual(values = c("Live" = "#006400", "Sterilized" = "#90EE90")) +
#   labs(x = "Type of Growth Rate", y = "Grass Growth Rate (raw)", title = "B: Grass Growth Rate") +
#   my_theme()
# 
# 
# # Arrange plots - Option 1 (Log scale)
# option1_log <- ggarrange(p1_log, p2_log, ncol = 1, common.legend = TRUE, legend = "bottom")
# 
# # Arrange plots - Option 1 (Raw scale)
# option1_raw <- ggarrange(p1_raw, p2_raw, ncol = 1, common.legend = TRUE, legend = "bottom")

########################
# OPTION 2: Water on x-axis with line types for growth types
########################

# Prepare data for line plots
df_summary$GrowthTypeNum <- ifelse(df_summary$GrowthType == "λi", 1, 2)

# Legume Growth Rate (Log Scale) - Option 2
p3_log <- ggplot() +
  geom_point(data = df, 
             aes(x = WaterNumeric, y = LegumeGrowth_log, color = SoilTreatment, shape = GrowthType),
             position = position_jitter(width = 0.1), alpha = 0.2, size = 1) +
  geom_errorbar(data = df_summary, 
                aes(x = WaterNumeric, 
                    ymin = LegumeGrowth_log_mean - LegumeGrowth_log_se, 
                    ymax = LegumeGrowth_log_mean + LegumeGrowth_log_se,
                    color = SoilTreatment,
                    linetype = GrowthType), 
                width = 0.1, position = position_dodge(width = 0.2)) +
  geom_line(data = df_summary, 
            aes(x = WaterNumeric, y = LegumeGrowth_log_mean, 
                color = SoilTreatment, linetype = GrowthType, group = interaction(SoilTreatment, GrowthType)), 
            size = 1, position = position_dodge(width = 0.2)) +
  geom_point(data = df_summary, 
             aes(x = WaterNumeric, y = LegumeGrowth_log_mean, 
                 color = SoilTreatment, shape = GrowthType), 
             size = 3, position = position_dodge(width = 0.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 1:3, labels = c("Low", "Intermediate", "High")) +
  scale_color_manual(values = c("Live" = "#4B0082", "Sterilized" = "#FFC0CB")) +
  labs(x = "Water Level", y = "Legume Growth Rate (log scale)", title = "A: Legume Growth Rate") +
  my_theme() +
  theme(legend.position = "bottom")

# Grass Growth Rate (Log Scale) - Option 2
p4_log <- ggplot() +
  geom_point(data = df, 
             aes(x = WaterNumeric, y = GrassGrowth_log, color = SoilTreatment, shape = GrowthType),
             position = position_jitter(width = 0.1), alpha = 0.2, size = 1) +
  geom_errorbar(data = df_summary, 
                aes(x = WaterNumeric, 
                    ymin = GrassGrowth_log_mean - GrassGrowth_log_se, 
                    ymax = GrassGrowth_log_mean + GrassGrowth_log_se,
                    color = SoilTreatment,
                    linetype = GrowthType), 
                width = 0.1, position = position_dodge(width = 0.2)) +
  geom_line(data = df_summary, 
            aes(x = WaterNumeric, y = GrassGrowth_log_mean, 
                color = SoilTreatment, linetype = GrowthType, group = interaction(SoilTreatment, GrowthType)), 
            size = 1, position = position_dodge(width = 0.2)) +
  geom_point(data = df_summary, 
             aes(x = WaterNumeric, y = GrassGrowth_log_mean, 
                 color = SoilTreatment, shape = GrowthType), 
             size = 3, position = position_dodge(width = 0.2)) +
  scale_x_continuous(breaks = 1:3, labels = c("Low", "Intermediate", "High")) +
  scale_color_manual(values = c("Live" = "#006400", "Sterilized" = "#90EE90")) +
  labs(x = "Water Level", y = "Grass Growth Rate (log scale)", title = "B: Grass Growth Rate") +
  my_theme() +
  theme(legend.position = "bottom")

# Raw scale versions for Option 2
p3_raw <- ggplot() +
  geom_point(data = df, 
             aes(x = WaterNumeric, y = LegumeGrowth_raw, color = SoilTreatment, shape = GrowthType),
             position = position_jitter(width = 0.1), alpha = 0.2, size = 1) +
  geom_errorbar(data = df_summary, 
                aes(x = WaterNumeric, 
                    ymin = LegumeGrowth_raw_mean - LegumeGrowth_raw_se, 
                    ymax = LegumeGrowth_raw_mean + LegumeGrowth_raw_se,
                    color = SoilTreatment,
                    linetype = GrowthType), 
                width = 0.1, position = position_dodge(width = 0.2)) +
  geom_line(data = df_summary, 
            aes(x = WaterNumeric, y = LegumeGrowth_raw_mean, 
                color = SoilTreatment, linetype = GrowthType, group = interaction(SoilTreatment, GrowthType)), 
            size = 1, position = position_dodge(width = 0.2)) +
  geom_point(data = df_summary, 
             aes(x = WaterNumeric, y = LegumeGrowth_raw_mean, 
                 color = SoilTreatment, shape = GrowthType), 
             size = 3, position = position_dodge(width = 0.2)) +
  geom_hline(yintercept = 1, linetype = "dashed") +  # 0 on log scale is 1 on raw scale
  annotate("text", x = 1.2, y = 1.5, label = "positive growth", size = 3) +
  annotate("text", x = 1.2, y = 0.5, label = "negative growth", size = 3) +
  scale_x_continuous(breaks = 1:3, labels = c("Low", "Intermediate", "High")) +
  scale_color_manual(values = c("Live" = "#4B0082", "Sterilized" = "#FFC0CB")) +
  labs(x = "Water Level", y = "Legume Growth Rate (raw)", title = "A: Legume Growth Rate") +
  my_theme() +
  theme(legend.position = "bottom")

p4_raw <- ggplot() +
  geom_point(data = df, 
             aes(x = WaterNumeric, y = GrassGrowth_raw, color = SoilTreatment, shape = GrowthType),
             position = position_jitter(width = 0.1), alpha = 0.2, size = 1) +
  geom_errorbar(data = df_summary, 
                aes(x = WaterNumeric, 
                    ymin = GrassGrowth_raw_mean - GrassGrowth_raw_se, 
                    ymax = GrassGrowth_raw_mean + GrassGrowth_raw_se,
                    color = SoilTreatment,
                    linetype = GrowthType), 
                width = 0.1, position = position_dodge(width = 0.2)) +
  geom_line(data = df_summary, 
            aes(x = WaterNumeric, y = GrassGrowth_raw_mean, 
                color = SoilTreatment, linetype = GrowthType, group = interaction(SoilTreatment, GrowthType)), 
            size = 1, position = position_dodge(width = 0.2)) +
  geom_point(data = df_summary, 
             aes(x = WaterNumeric, y = GrassGrowth_raw_mean, 
                 color = SoilTreatment, shape = GrowthType), 
             size = 3, position = position_dodge(width = 0.2)) +
  scale_x_continuous(breaks = 1:3, labels = c("Low", "Intermediate", "High")) +
  scale_color_manual(values = c("Live" = "#006400", "Sterilized" = "#90EE90")) +
  labs(x = "Water Level", y = "Grass Growth Rate (raw)", title = "B: Grass Growth Rate") +
  my_theme() +
  theme(legend.position = "bottom")

# Arrange plots - Option 2 (Log scale)
option2_log <- ggarrange(p3_log, p4_log, ncol = 1, common.legend = TRUE, legend = "bottom")

# Arrange plots - Option 2 (Raw scale)
option2_raw <- ggarrange(p3_raw, p4_raw, ncol = 1, common.legend = TRUE, legend = "bottom")

# # Print each option
# # Option 1 - Log scale
# print(option1_log)
# 
# # Option 1 - Raw scale
# print(option1_raw)

# Option 2 - Log scale
print(option2_log)

# # Option 2 - Raw scale
# print(option2_raw)
