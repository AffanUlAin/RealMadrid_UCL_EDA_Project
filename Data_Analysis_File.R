#This package for join functions
#install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(forcats)
library(reshape2) 

# Load all the datasets
attacking <- read.csv("attacking.csv", header = TRUE)
attempts <- read.csv("attempts.csv", header = TRUE)
defending <- read.csv("defending.csv", header = TRUE)
disciplinary <- read.csv("disciplinary.csv", header = TRUE)
distribution <- read.csv("distributon.csv", header = TRUE)
goalkeeping <- read.csv("goalkeeping.csv", header = TRUE)
goals <- read.csv("goals.csv", header = TRUE)
key_stats <- read.csv("key_stats.csv", header = TRUE)


# Merge all the excel files into one table, replacing all NA values with zero, and removing duplicate columns
merged_data <- key_stats %>%
  left_join(attacking, by = c("player_name", "club", "position", "match_played")) %>%
  left_join(attempts, by = c("player_name", "club", "position", "match_played")) %>%
  left_join(defending, by = c("player_name", "club", "position", "match_played")) %>%
  left_join(disciplinary, by = c("player_name", "club", "position", "match_played")) %>%
  left_join(distribution, by = c("player_name", "club", "position", "match_played")) %>%
  left_join(goalkeeping, by = c("player_name", "club", "position", "match_played")) %>%
  left_join(goals, by = c("player_name", "club", "position", "match_played")) %>%
  select(-starts_with("serial"), -matches("\\.y$")) %>%   
  mutate_all(~replace(., is.na(.), 0))

str(merged_data)

# 1. Univariate Analysis

# Attacking Players

# a) Top 10 Goals+ Assists
merged_data <- merged_data %>%
  mutate(total_goals_assists = goals.x + assists.x)

sorted_data <- merged_data %>%
  arrange(desc(total_goals_assists))

top_players <- head(sorted_data, 10)

club_counts <- top_players %>%
  group_by(club) %>%
  summarise(player_count = n())

fill_colors <- ifelse(club_counts$club == "Real Madrid", "darkblue", "maroon")

# Bar chart for clubs
ggplot(club_counts, aes(x = reorder(club, -player_count), y = player_count, fill = club)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = fill_colors) +
  labs(title = "Clubs with most Goals + Assists Providers (Top 10 players)",
       x = "Club",
       y = "Number of Players") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  
  guides(fill = "none") 

# Bar Chart for names of players
top_10_players <- top_players %>%
  mutate(real_madrid_indicator = ifelse(club == "Real Madrid", "Real Madrid", "Other"))

ggplot(top_10_players, aes(x = reorder(player_name, -total_goals_assists), y = total_goals_assists, fill = real_madrid_indicator)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("maroon", "darkblue")) +
  labs(title = "Top 10 Players with Most Goals + Assists",
       x = "Player Name",
       y = "Total Goals + Assists") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  
  guides(fill = "none") 

# b) Best Dribblers

forward_players <- merged_data %>%
  filter(position == "Forward")

fill_colors <- ifelse(forward_players$club == "Real Madrid", "darkblue", "maroon")

# Plot the scatter plot
ggplot(forward_players, aes(x = minutes_played.x, y = dribbles, color = club, label = player_name)) +
  geom_point() +
  geom_text(check_overlap = TRUE, vjust = 1.5, size = 3) +
  scale_color_manual(values = c("Real Madrid" = "darkblue", "Other" = "maroon")) +
  labs(title = "Top Dribblers (Forwards)",
       x = "Number of Minutes",
       y = "Number of Dribbles") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +  
  guides(fill = "none")

# Midfield Players

#a) Top Passing and Crossing Accuracies

midfield_players <- merged_data %>%
  filter(position == "Midfielder")

# Analyze clubs with the most top 10 players with pass completed
top_pass_completed <- midfield_players %>%
  top_n(10, pass_completed) %>%
  group_by(club) %>%
  summarise(player_count = n())

pass_fill_colors <- ifelse(top_pass_completed$club == "Real Madrid", "darkblue", "maroon")

ggplot(top_pass_completed, aes(x = reorder(club, -player_count), y = player_count, fill = club)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = pass_fill_colors) +
  labs(title = "Clubs with Most Top 10 Players by Pass Completed",
       x = "Club",
       y = "Number of Players") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill = "none") 

#Bar chart of top 10 players with highest pass completed and cross completed
top_pass_cross_completed <- midfield_players %>%
  top_n(10, pass_completed + cross_complted) %>%
  mutate(avg_completed = (pass_completed + cross_complted)) %>%
  arrange(desc(avg_completed)) %>%
  mutate(player_name = factor(player_name, levels = player_name))

fill_colors <- ifelse(top_pass_cross_completed$club == "Real Madrid", "darkblue", "maroon")

ggplot(top_pass_cross_completed, aes(x = player_name)) +
  geom_bar(aes(y = pass_completed, fill = fill_colors), position = "dodge", stat = "identity", width = 0.4) +
  geom_bar(aes(y = cross_complted, fill = fill_colors), position = "dodge", stat = "identity", width = 0.4) +
  geom_text(aes(y = pass_completed, label = pass_completed), position = position_dodge(width = 0.4), vjust = -0.5, size = 3, color = "black") +
  geom_text(aes(y = cross_complted, label = cross_complted), position = position_dodge(width = 0.4), vjust = -0.5, size = 3, color = "black") +
  labs(title = "Top 10 Players with Highest Pass and Cross Completed",
       x = "Player Name",
       y = "Completed") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),  
        legend.position = "none") +  
  scale_fill_manual(values = c("maroon" = "maroon", "darkblue" = "darkblue"))

# Defending Players

defender_players <- merged_data %>%
  filter(position == "Defender")

defender_players <- defender_players %>%
  mutate(sum_defending = balls_recoverd + t_won)

top_defending_players <- head(arrange(defender_players, desc(sum_defending)), 10)

fill_colors_defenders <- ifelse(top_defending_players$club == "Real Madrid", "darkblue", "maroon")

# Plot the bar chart for top 10 defenders with highest sum of defending attributes
ggplot(top_defending_players, aes(x = player_name)) +
  geom_bar(aes(y = balls_recoverd, fill = fill_colors_defenders), position = "dodge", stat = "identity", width = 0.4) +
  geom_bar(aes(y = t_won, fill = fill_colors_defenders), position = "dodge", stat = "identity", width = 0.4) +
  geom_bar(aes(y = clearance_attempted, fill = fill_colors_defenders), position = "dodge", stat = "identity", width = 0.4) +
  labs(title = "Top 10 Defenders with the Highest Balls Recovered and Tackles Won",
       x = "Player Name",
       y = "Sum of Defending Attributes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5), 
        legend.position = "none") +  
  scale_fill_manual(values = c("maroon" = "maroon", "darkblue" = "darkblue"))

# Goalkeepers

goalkeeper_players <- merged_data %>%
  filter(position == "Goalkeeper")

top_goalkeeper_players <- head(arrange(goalkeeper_players, desc(saved)), 10)

fill_colors_goalkeepers <- ifelse(top_goalkeeper_players$club == "Real Madrid", "darkblue", "maroon")

ggplot(top_goalkeeper_players, aes(x = reorder(player_name, -saved), y = saved, fill = fill_colors_goalkeepers)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = saved), vjust = -0.5, size = 3, color = "black") +  # Add numbers on top of bars
  labs(title = "Top 10 Goalkeepers with the most Saves",
       x = "Player Name",
       y = "Saved") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),  # Center-align the plot title
        legend.position = "none") +  # Remove the legend
  scale_fill_manual(values = c("maroon" = "maroon", "darkblue" = "darkblue"))

#--------------------------------------------------------------------------------------------------------------

# 2. Bivariate Analysis

# Select the relevant columns from merged_data
correlation_data <- merged_data[, c("goals.x", "assists.x", "dribbles", "pass_completed", "cross_accuracy", "cross_attempted", "pass_accuracy","t_won", "balls_recoverd", "clearance_attempted", "saved", "conceded")]

# Calculate pairwise correlations using Pearson method
correlation_matrix <- cor(correlation_data, method = "pearson")

correlation_df <- melt(correlation_matrix)
ggplot(correlation_df, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "right") +
  labs(title = "Pairwise Pearson Correlation between Performance Metrics",
       x = "Performance Metrics",
       y = "Performance Metrics")

#-------------------------------------------------------------------------------------------------

# 3. Multivariate Analysis (PCA)

dim(merged_data)
str(merged_data)
merged_data$distance_covered <- as.numeric(merged_data$distance_covered)
str(merged_data)
summary(merged_data)

#PCA IMPLEMENTATION FOR OVERALL DATA
data_for_mva <- merged_data[, c("goals.x", "assists.x", "dribbles", "pass_completed", "cross_accuracy", "cross_attempted", "pass_accuracy","t_won", "balls_recoverd", "clearance_attempted", "saved", "conceded")]
data_pca <- princomp(data_for_mva, cor = TRUE)

summary(data_pca)

vars <- data_pca$sdev^2
var_prop <- vars / sum(vars)
var_prop_cum <- cumsum(var_prop)

plot(var_prop_cum, type = "b", pch = 21, lty = 3, bg = "skyblue", cex = 1.5,
     ylim = c(0, 1), xlab = "Principal component",
     ylab = "Cumulative proportion of variance explained",
     xaxt = "n", yaxt = "n")
axis(1, at = 1:12)
axis(2, at = 0:10 / 10, las = 2)
text(x = 1:12, y = var_prop_cum, labels = paste0(round(var_prop_cum * 100), "%"), pos = 3, cex = 0.8)

round(data_pca$loadings[, 1:7], 2)

pc12 <- data_pca$scores[, 1:2]
load12 <- data_pca$loadings[, 1:2]
pc_axis <- c(-max(abs(pc12)), max(abs(pc12)))
ld_axis <- c(-1, 1)

plot(pc12, xlim = pc_axis, ylim = pc_axis, pch = 21, bg = 8, cex = 1.25,
     xlab = paste0("PC 1 (", round(100 * var_prop[1], 2), "%)"),
     ylab = paste0("PC 2 (", round(100 * var_prop[2], 2), "%)"))
par(new = TRUE)
plot(load12, axes = FALSE, type = "n", xlab = "", ylab = "", xlim = ld_axis,
     ylim = ld_axis)
axis(3, col = 2)
axis(4, col = 2)
arrows(0, 0, load12[, 1], load12[, 2], length = 0.1, col = 2)
text(load12[, 1], load12[, 2], rownames(load12), pos = 3)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)

#PCA IMPLEMENTATION FOR REAL MADRID PLAYERS
real_madrid_data <- merged_data[merged_data$club == "Real Madrid", ]

data_for_mva_rma <- real_madrid_data[, c("goals.x", "assists.x", "dribbles", "pass_completed", "cross_accuracy", 
                                     "cross_attempted", "pass_accuracy", "t_won", "balls_recoverd", 
                                     "clearance_attempted", "saved", "conceded")]

# Perform PCA
data_pca_rma <- princomp(data_for_mva_rma, cor = TRUE)

# Summary of PCA results
summary(data_pca_rma)

# How much variation is explained by the first k principal components
vars_rma <- data_pca_rma$sdev^2
var_prop_rma <- vars_rma / sum(vars_rma)
var_prop_cum_rma <- cumsum(var_prop_rma)

plot(var_prop_cum_rma, type = "b", pch = 21, lty = 3, bg = "skyblue", cex = 1.5,
     ylim = c(0, 1), xlab = "Principal component",
     ylab = "Cumulative proportion of variance",
     xaxt = "n", yaxt = "n", main = "Cumulative Proportion of Variance Explained (For Real Madrid)")

axis(1, at = 1:12)
axis(2, at = 0:10 / 10, las = 2)
text(x = 1:12, y = var_prop_cum_rma, labels = paste0(round(var_prop_cum_rma * 100), "%"), pos = 3, cex = 0.8)

round(data_pca_rma$loadings[, 1:7], 2)

# Biplot
pc12_rma <- data_pca_rma$scores[, 1:2]
load12_rma <- data_pca_rma$loadings[, 1:2]
pc_axis_rma <- c(-max(abs(pc12_rma)), max(abs(pc12_rma)))
ld_axis_rma <- c(-1, 1)

plot(pc12_rma, xlim = pc_axis_rma, ylim = pc_axis_rma, pch = 21, bg = 8, cex = 1.25,
     xlab = paste0("PC 1 (", round(100 * var_prop_rma[1], 2), "%)"),
     ylab = paste0("PC 2 (", round(100 * var_prop_rma[2], 2), "%)"), 
     main = "Biplot of First Two Principal Components (For Real Madrid)")
par(new = TRUE)
plot(load12_rma, axes = FALSE, type = "n", xlab = "", ylab = "", xlim = ld_axis_rma,
     ylim = ld_axis_rma)
axis(3, col = 2)
axis(4, col = 2)
arrows(0, 0, load12_rma[, 1], load12_rma[, 2], length = 0.1, col = 2)
text(load12[, 1], load12_rma[, 2], rownames(load12_rma), pos = 3)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)

#PCA IMPLEMENTATION FOR LIVERPOOL PLAYERS
# Filter data for Liverpool FC
liverpool_data <- merged_data[merged_data$club == "Liverpool", ]

# Select numeric variables for PCA
data_for_mva_liv <- liverpool_data[, c("goals.x", "assists.x", "dribbles", "pass_completed", "cross_accuracy", 
                                   "cross_attempted", "pass_accuracy", "t_won", "balls_recoverd", 
                                   "clearance_attempted", "saved", "conceded")]

# Perform PCA
data_pca_liv <- princomp(data_for_mva_liv, cor = TRUE)


# Summary of PCA results
summary(data_pca_liv)

# How much variation is explained by the first k principal components
vars_liv <- data_pca_liv$sdev^2
var_prop_liv <- vars_liv / sum(vars_liv)
var_prop_cum_liv <- cumsum(var_prop_liv)

plot(var_prop_cum_liv, type = "b", pch = 21, lty = 3, bg = "skyblue", cex = 1.5,
     ylim = c(0, 1), xlab = "Principal component",
     ylab = "Cumulative proportion of variance",
     xaxt = "n", yaxt = "n", main = "Cumulative Proportion of Variance Explained (For Liverpool FC)")

axis(1, at = 1:12)
axis(2, at = 0:10 / 10, las = 2)
text(x = 1:12, y = var_prop_cum_liv, labels = paste0(round(var_prop_cum_liv * 100), "%"), pos = 3, cex = 0.8)

round(data_pca_liv$loadings[, 1:7], 2)

# Biplot
pc12_liv <- data_pca_liv$scores[, 1:2]
load12_liv <- data_pca_liv$loadings[, 1:2]
pc_axis_liv <- c(-max(abs(pc12_liv)), max(abs(pc12_liv)))
ld_axis_liv <- c(-1, 1)

plot(pc12_liv, xlim = pc_axis_liv, ylim = pc_axis_liv, pch = 21, bg = 8, cex = 1.25,
     xlab = paste0("PC 1 (", round(100 * var_prop_liv[1], 2), "%)"),
     ylab = paste0("PC 2 (", round(100 * var_prop_liv[2], 2), "%)"), 
     main = "Biplot of First Two Principal Components (For Liverpool FC)")
par(new = TRUE)
plot(load12_liv, axes = FALSE, type = "n", xlab = "", ylab = "", xlim = ld_axis_liv,
     ylim = ld_axis_liv)
axis(3, col = 2)
axis(4, col = 2)
arrows(0, 0, load12_liv[, 1], load12_liv[, 2], length = 0.1, col = 2)
text(load12_liv[, 1], load12_liv[, 2], rownames(load12_liv), pos = 3)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)


#PCA IMPLEMENTATION FOR MACHESTER CITY PLAYERS

man_city_data <- merged_data[merged_data$club == "Man. City", ]

# Select numeric variables for PCA
data_for_mva_city <- man_city_data[, c("goals.x", "assists.x", "dribbles", "pass_completed", "cross_accuracy", 
                                  "cross_attempted", "pass_accuracy", "t_won", "balls_recoverd", 
                                  "clearance_attempted", "saved", "conceded")]

# Perform PCA
data_pca_city <- princomp(data_for_mva_city, cor = TRUE)

# Summary of PCA results
summary(data_pca_city)

# How much variation is explained by the first k principal components
vars_city <- data_pca_city$sdev^2
var_prop_city <- vars_city / sum(vars_city)
var_prop_cum_city <- cumsum(var_prop_city)

plot(var_prop_cum_city, type = "b", pch = 21, lty = 3, bg = "skyblue", cex = 1.5,
     ylim = c(0, 1), xlab = "Principal component",
     ylab = "Cumulative proportion of variance",
     xaxt = "n", yaxt = "n", main = "Cumulative Proportion of Variance Explained (For Manchester City)")

axis(1, at = 1:12)
axis(2, at = 0:10 / 10, las = 2)
text(x = 1:12, y = var_prop_cum_city, labels = paste0(round(var_prop_cum_city * 100), "%"), pos = 3, cex = 0.8)

round(data_pca_city$loadings[, 1:7], 2)

# Biplot
  pc12_city <- data_pca_city$scores[, 1:2]
load12_city <- data_pca_city$loadings[, 1:2]
pc_axis_city <- c(-max(abs(pc12_city)), max(abs(pc12_city)))
ld_axis_city <- c(-1, 1)

plot(pc12_city, xlim = pc_axis_city, ylim = pc_axis_city, pch = 21, bg = 8, cex = 0.8, 
     xlab = paste0("PC 1 (", round(100 * var_prop_city[1], 2), "%)"),
     ylab = paste0("PC 2 (", round(100 * var_prop_city[2], 2), "%)"), 
     main = "Biplot of First Two Principal Components (For Manchester City)")
par(new = TRUE)
plot(load12_city, axes = FALSE, type = "n", xlab = "", ylab = "", xlim = ld_axis_city,
     ylim = ld_axis_city)
axis(3, col = 2)
axis(4, col = 2)
arrows(0, 0, load12_city[, 1], load12_city[, 2], length = 0.1, col = 2)

text(load12_city[, 1], load12_city[, 2], rownames(load12_city), pos = 3, cex = 1)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)


#PCA IMPLEMENTATION FOR VILLARREAL PLAYERS
# Filter data for Villarreal
villarreal_data <- merged_data[merged_data$club == "Villarreal", ]

# Select numeric variables for PCA
data_for_mva_vll <- villarreal_data[, c("goals.x", "assists.x", "dribbles", "pass_completed", "cross_accuracy", 
                                    "cross_attempted", "pass_accuracy", "t_won", "balls_recoverd", 
                                    "clearance_attempted", "saved", "conceded")]

# Perform PCA
data_pca_vll <- princomp(data_for_mva_vll, cor = TRUE)

# Summary of PCA results
summary(data_pca_vll)

# How much variation is explained by the first k principal components
vars_vll <- data_pca_vll$sdev^2
var_prop_vll <- vars_vll / sum(vars_vll)
var_prop_cum_vll <- cumsum(var_prop_vll)

plot(var_prop_cum_vll, type = "b", pch = 21, lty = 3, bg = "skyblue", cex = 1.5,
     ylim = c(0, 1), xlab = "Principal component",
     ylab = "Cumulative proportion of variance",
     xaxt = "n", yaxt = "n", main = "Cumulative Proportion of Variance Explained (For Villarreal)")

axis(1, at = 1:12)
axis(2, at = 0:10 / 10, las = 2)
text(x = 1:12, y = var_prop_cum_vll, labels = paste0(round(var_prop_cum_vll * 100), "%"), pos = 3, cex = 0.8)

round(data_pca_vll$loadings[, 1:7], 2)

# Biplot
pc12_vll <- data_pca_vll$scores[, 1:2]
load12_vll <- data_pca_vll$loadings[, 1:2]
pc_axis_vll <- c(-max(abs(pc12_vll)), max(abs(pc12_vll)))
ld_axis_vll <- c(-1, 1)

plot(pc12_vll, xlim = pc_axis_vll, ylim = pc_axis_vll, pch = 21, bg = 8, cex = 1.25,
     xlab = paste0("PC 1 (", round(100 * var_prop_vll[1], 2), "%)"),
     ylab = paste0("PC 2 (", round(100 * var_prop_vll[2], 2), "%)"), 
     main = "Biplot of First Two Principal Components (For Villarreal)")
par(new = TRUE)
plot(load12_vll, axes = FALSE, type = "n", xlab = "", ylab = "", xlim = ld_axis_vll,
     ylim = ld_axis_vll)
axis(3, col = 2)
axis(4, col = 2)
arrows(0, 0, load12_vll[, 1], load12_vll[, 2], length = 0.1, col = 2)
text(load12_vll[, 1], load12_vll[, 2], rownames(load12_vll), pos = 3, cex = 1)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)

