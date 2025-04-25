library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

## Carmen - I tried to redo this from your raw data, but had trouble figuring out which file to source
## So I used Claude, gave it the original and told it what I wanted it to look like
## It used simulated data from the figure, so obviously needs to be redone with the actual data, but - 
## I think this format does a good job of highlighting the two main points:
## a) the legume lambda is very responsive to water and innoculation - but there's not necessarily an interaction
## b) grass lambda doesn't care so much about innoculation, but it does increase the legume facilitation effect 
## (AND - the biggest differential owing to live soil occuring at intermediate rainfall)
## Finally - I think it would be best to log transform this figure, as you used log transformed in the other figures (final option below)


# Create a sample dataframe based on estimated values
# You would replace this with your actual data
set.seed(123)

# Function to create random points around a mean with error
create_points <- function(mean_val, n_points = 30, sd_val = mean_val * 0.15) {
  return(rnorm(n_points, mean = mean_val, sd = sd_val))
}

# Create data frame with all combinations
water_levels <- c("Low", "Intermediate", "High")
water_numeric <- c(1, 2, 3)  # Numeric representation for plotting on x-axis
soil_treatments <- c("Live", "Sterilized")

# Estimated mean values from the figure
grass_growth <- c(28, 10, 52, 18, 65, 20)  # Live Low, Steril Low, Live Int, etc.
legume_growth <- c(470, 460, 550, 540, 590, 570)
grass_alpha <- c(-0.06, -0.02, -0.07, -0.03, -0.05, -0.03)
legume_alpha <- c(-0.012, -0.017, 0.001, -0.020, 0.005, -0.007)

# Create empty dataframe
n_points <- 30
df <- data.frame()

# Generate data points
counter <- 1
for (i in 1:length(water_levels)) {
  for (j in 1:length(soil_treatments)) {
    # Generate points for each water level and soil treatment combination
    grass_growth_points <- create_points(grass_growth[counter])
    legume_growth_points <- create_points(legume_growth[counter])
    grass_alpha_points <- create_points(grass_alpha[counter], sd_val = 0.01)
    legume_alpha_points <- create_points(legume_alpha[counter], sd_val = 0.008)
    
    # Add to dataframe
    temp_df <- data.frame(
      WaterLevel = rep(water_levels[i], n_points),
      WaterNumeric = rep(water_numeric[i], n_points),
      SoilTreatment = rep(soil_treatments[j], n_points),
      GrassGrowth = grass_growth_points,
      LegumeGrowth = legume_growth_points,
      GrassAlpha = grass_alpha_points,
      LegumeAlpha = legume_alpha_points
    )
    df <- rbind(df, temp_df)
    counter <- counter + 1
  }
}

# Calculate means and standard errors for summary points
df_summary <- df %>%
  group_by(WaterLevel, WaterNumeric, SoilTreatment) %>%
  summarize(
    GrassGrowth_mean = mean(GrassGrowth),
    GrassGrowth_se = sd(GrassGrowth) / sqrt(n()),
    LegumeGrowth_mean = mean(LegumeGrowth),
    LegumeGrowth_se = sd(LegumeGrowth) / sqrt(n()),
    GrassAlpha_mean = mean(GrassAlpha),
    GrassAlpha_se = sd(GrassAlpha) / sqrt(n()),
    LegumeAlpha_mean = mean(LegumeAlpha),
    LegumeAlpha_se = sd(LegumeAlpha) / sqrt(n())
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

# Create individual plots
# Grass Growth Rate Plot
p1 <- ggplot() +
  geom_point(data = df, aes(x = WaterNumeric, y = GrassGrowth, color = SoilTreatment), 
             alpha = 0.2, position = position_jitter(width = 0.1)) +
  geom_errorbar(data = df_summary, 
                aes(x = WaterNumeric, 
                    ymin = GrassGrowth_mean - GrassGrowth_se, 
                    ymax = GrassGrowth_mean + GrassGrowth_se,
                    color = SoilTreatment), 
                width = 0.1) +
  geom_point(data = df_summary, 
             aes(x = WaterNumeric, y = GrassGrowth_mean, color = SoilTreatment), 
             size = 3) +
  geom_line(data = df_summary, 
            aes(x = WaterNumeric, y = GrassGrowth_mean, color = SoilTreatment, 
                group = SoilTreatment), 
            size = 1) +
  scale_x_continuous(breaks = 1:3, labels = c("Low", "Intermediate", "High")) +
  scale_color_manual(values = c("Live" = "#4B0082", "Sterilized" = "#FFC0CB")) +
  labs(x = "Water Level", y = "Intrinsic Growth Rate", title = "A: Legume Intrinsic Growth ") +
  my_theme()

# Legume Growth Rate Plot
p2 <- ggplot() +
  geom_point(data = df, aes(x = WaterNumeric, y = LegumeGrowth, color = SoilTreatment), 
             alpha = 0.2, position = position_jitter(width = 0.1)) +
  geom_errorbar(data = df_summary, 
                aes(x = WaterNumeric, 
                    ymin = LegumeGrowth_mean - LegumeGrowth_se, 
                    ymax = LegumeGrowth_mean + LegumeGrowth_se,
                    color = SoilTreatment), 
                width = 0.1) +
  geom_point(data = df_summary, 
             aes(x = WaterNumeric, y = LegumeGrowth_mean, color = SoilTreatment), 
             size = 3) +
  geom_line(data = df_summary, 
            aes(x = WaterNumeric, y = LegumeGrowth_mean, color = SoilTreatment, 
                group = SoilTreatment), 
            size = 1) +
  scale_x_continuous(breaks = 1:3, labels = c("Low", "Intermediate", "High")) +
  scale_color_manual(values = c("Live" = "#006400", "Sterilized" = "#90EE90")) +
  labs(x = "Water Level", y = "Intrinsic Growth Rate", title = "B: Grass Intrinsic Growth") +
  my_theme()

# Grass Alpha Plot
p3 <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("text", x = 1.2, y = 0.01, label = "facilitation", size = 3) +
  annotate("text", x = 1.2, y = -0.01, label = "competition", size = 3) +
  geom_point(data = df, aes(x = WaterNumeric, y = GrassAlpha, color = SoilTreatment), 
             alpha = 0.2, position = position_jitter(width = 0.1)) +
  geom_errorbar(data = df_summary, 
                aes(x = WaterNumeric, 
                    ymin = GrassAlpha_mean - GrassAlpha_se, 
                    ymax = GrassAlpha_mean + GrassAlpha_se,
                    color = SoilTreatment), 
                width = 0.1) +
  geom_point(data = df_summary, 
             aes(x = WaterNumeric, y = GrassAlpha_mean, color = SoilTreatment), 
             size = 3) +
  geom_line(data = df_summary, 
            aes(x = WaterNumeric, y = GrassAlpha_mean, color = SoilTreatment, 
                group = SoilTreatment), 
            size = 1) +
  scale_x_continuous(breaks = 1:3, labels = c("Low", "Intermediate", "High")) +
  scale_color_manual(values = c("Live" = "#4B0082", "Sterilized" = "#FFC0CB")) +
  labs(x = "Water Level", y = "INTRA specific alpha", title = "C: Legume on Legume") +
  my_theme()

# Legume Alpha Plot
p4 <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  annotate("text", x = 1.2, y = 0.01, label = "facilitation", size = 3) +
  annotate("text", x = 1.2, y = -0.01, label = "competition", size = 3) +
  geom_point(data = df, aes(x = WaterNumeric, y = LegumeAlpha, color = SoilTreatment), 
             alpha = 0.2, position = position_jitter(width = 0.1)) +
  geom_errorbar(data = df_summary, 
                aes(x = WaterNumeric, 
                    ymin = LegumeAlpha_mean - LegumeAlpha_se, 
                    ymax = LegumeAlpha_mean + LegumeAlpha_se,
                    color = SoilTreatment), 
                width = 0.1) +
  geom_point(data = df_summary, 
             aes(x = WaterNumeric, y = LegumeAlpha_mean, color = SoilTreatment), 
             size = 3) +
  geom_line(data = df_summary, 
            aes(x = WaterNumeric, y = LegumeAlpha_mean, color = SoilTreatment, 
                group = SoilTreatment), 
            size = 1) +
  scale_x_continuous(breaks = 1:3, labels = c("Low", "Intermediate", "High")) +
  scale_color_manual(values = c("Live" = "#006400", "Sterilized" = "#90EE90")) +
  labs(x = "Water Level", y = "INTER specific alpha", title = "D: Legume on Grass") +
  my_theme()

# Arrange plots
grid.arrange(p1, p2, p3, p4, ncol = 2)

# Growth Rate on Log Scale
p1_log <- p1 + scale_y_log10() + labs(title = "A: Legume Growth Rate (Log Scale)")
p2_log <- p2 + scale_y_log10() + labs(title = "B: Grass Growth Rate (Log Scale)")


# For log scale version
grid.arrange(p1_log, p2_log, p3, p4, ncol = 2)
