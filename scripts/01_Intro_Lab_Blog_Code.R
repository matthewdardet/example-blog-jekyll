#' @title GOV 1347: Introductory Blog Post/Laboratory Session
#' @author Matthew E. Dardet
#' @date August 29, 2024

####----------------------------------------------------------#
#### Preamble
####----------------------------------------------------------#

# Load libraries.
## install via `install.packages("name")`
library(ggplot2)
library(maps)
library(tidyverse)

## set working directory here
# setwd("~")

####----------------------------------------------------------#
#### Read and clean presidential popular vote.
####----------------------------------------------------------#

# Read presidential popular vote. 
d_popvote <- read_csv("data/popvote_1948-2020.csv")

# Subset data to most recent past election year. 
d_popvote |> 
  filter(year == 2020) |> 
  select(party, candidate, pv2p)

# Pivot data to wide format with party names as columns and two-party vote share as values.
(d_popvote_wide <- d_popvote |>
  select(year, party, pv2p) |>
  pivot_wider(names_from = party, values_from = pv2p))

# Modify winner column to show "D" if Democrats win and "R" if Republicans win. 
(d_popvote_wide <- d_popvote_wide |> 
  mutate(winner = case_when(democrat > republican ~ "D",
                            TRUE ~ "R")))

# Summarize data with respect to winners. 
d_popvote_wide |> 
  group_by(winner) |>
  summarise(races = n())

####----------------------------------------------------------#
#### Visualize trends in national presidential popular vote. 
####----------------------------------------------------------#

# Example: bad histogram; unpleasant bin width and fill!
ggplot(d_popvote, aes(x = pv2p)) + 
    geom_histogram() + 
    labs(x = "Two-Party Vote Share", 
         y = "Count", 
         title = "Distribution of Two-Party Vote Share (1948-2020)")

# Example: better histogram.
ggplot(d_popvote, aes(x = pv2p)) + 
  geom_histogram(bins = 10, color = "black", fill = "goldenrod2") + 
  theme_bw() +
  labs(x = "Two-Party Vote Share", 
       y = "Count", 
       title = "Distribution of Two-Party Vote Share (1948-2020)")

# Example: barplot (+ custom colors).
ggplot(d_popvote, aes(x = year, y = pv2p, fill = party)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("dodgerblue4", "firebrick1")) 

# Example: lineplot (+ custom colors + nicer theme).
ggplot(d_popvote, aes(x = year, y = pv2p, colour = party)) +
    geom_line() +
    scale_color_manual(values = c("dodgerblue4", "firebrick1")) + 
    theme_bw()

# Example: bad line plot; opaque background, "too much ink", no legend, small font. Yuck! 
ggplot(d_popvote, aes(x = year, y = pv2p, colour = party)) +
    geom_line(stat = "identity") + 
    theme_dark() +
    theme(legend.position = "none", axis.title = element_text(size = 5))

# Example: good line plot: high contrast, "minimal ink", legend, detailed x-ticks, larger font.
ggplot(d_popvote, aes(x = year, y = pv2p, colour = party)) +
    geom_line(stat = "identity") + 
    scale_x_continuous(breaks = seq(1948, 2020, 4)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45))

# Example: excellent plot; "pretty" customized theme.
my_pretty_theme <- theme_bw() + 
    theme(panel.border = element_blank(),
          plot.title = element_text(size = 15, hjust = 0.5), 
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 12),
          strip.text = element_text(size = 18),
          axis.line = element_line(colour = "black"),
          legend.position = "top",
          legend.text = element_text(size = 12))

ggplot(d_popvote, aes(x = year, y = pv2p, colour = party)) +
    geom_line(stat = "identity") +
    scale_color_manual(values = c("dodgerblue4", "firebrick1"), name = "") +
    xlab("") + ## no need to label an obvious axis
    ylab("Two-Party National Popular Vote (%)") +
    ggtitle("Presidential Vote Share (1948-2020)") + 
    scale_x_continuous(breaks = seq(1948, 2020, 4)) +
    my_pretty_theme

# Save last displayed plot.
ggsave("figures/PV_national_historical.png", height = 4, width = 8)

####----------------------------------------------------------#
#### State-by-state map of presidential popular votes.
####----------------------------------------------------------#

# Sequester shapefile of states from `maps` library.
states_map <- map_data("state")
unique(states_map$region)

# Read/clean/merge state popular vote. 
# Create wide version of dataset that can be used to compare candidate votes with one another. 
d_pvstate <- read_csv("data/state_2pv_1948_2020.csv")
colnames(d_pvstate)

d_pvstate_wide <- d_pvstate |> 
  select(-c(candidate, state_fips)) |> 
  rename(pv = vote_share,
         pv2p = two_party_vote_share) |>
  mutate(party_simple = case_when(party == "Democrat" ~ "D", 
                                  .default = "R")) |>
  pivot_wider(names_from = party_simple, values_from = c(pv, pv2p)) |> 
  select(-c(party, candidatevotes, totalvotes, two_party_votes)) |> 
  group_by(year, state) |> 
  summarize_all(function(x) x[!is.na(x)]) |> 
  rename(D_pv = pv_D, R_pv = pv_R, D_pv2p = pv2p_D, R_pv2p = pv2p_R) |> 
  arrange(year) |> 
  group_by(state) |> 
  mutate(D_pv_lag1 = lag(D_pv, 1),
         R_pv_lag1 = lag(R_pv, 1),
         D_pv2p_lag1 = lag(D_pv2p, 1),
         R_pv2p_lag1 = lag(R_pv2p, 1), 
         D_pv_lag2 = lag(D_pv, 2), 
         R_pv_lag2 = lag(R_pv, 2),
         D_pv2p_lag2 = lag(D_pv2p, 2),
         R_pv2p_lag2 = lag(R_pv2p, 2)) 

write_csv(d_pvstate_wide, 
          "data/clean_wide_state_2pv_1948_2020.csv") # Output widened dataset for use by the class. 

d_pvstate_wide$region <- tolower(d_pvstate_wide$state)

pv_map <- d_pvstate_wide |>
  filter(year == 2020) |>
  left_join(states_map, by = "region")

# Generate map of two-party vote share for Republican candidates. 
pv_map |> # N.B. You can pipe (|>) data directly into `ggplot()`!
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = R_pv2p), color = "white")+
  scale_fill_gradient(low = "white", high = "firebrick1") +
  theme_void()

# Generate map of state winners using their vote count. 
pv_win_map <- d_pvstate_wide |>
    filter(year == 2020) |>
    left_join(states_map, by = "region") |>
    mutate(winner = ifelse(R_pv > D_pv, "republican", "democrat"))

ggplot(pv_win_map, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = winner), color = "black") +
    scale_fill_manual(values=c("dodgerblue4", "firebrick1")) +
    theme_void()

# Generate map of win-margins.
pv_margins_map <- d_pvstate_wide |>
    filter(year == 2020) |>
    left_join(states_map, by = "region") |>
    mutate(win_margin = (R_pv2p-D_pv2p))

ggplot(pv_margins_map, aes(long, lat, group = group)) +
       geom_polygon(aes(fill = win_margin), color = "black") +
       scale_fill_gradient2(high = "firebrick1", 
                            mid = "white",
                            name = "win margin",
                            low = "dodgerblue4", 
                            breaks = c(-50,-25,0,25,50), 
                            limits=c(-50,50)) +
    theme_void()

# Make grid version of maps. 
d_pvstate_wide |>
    filter(year >= 1980) |>
    left_join(states_map, by = "region") |>
    mutate(winner = ifelse(R_pv > D_pv, "republican", "democrat")) |>
    
    ggplot(aes(long, lat, group = group)) +
    facet_wrap(facets = year ~.) + ## specify a grid by year
    geom_polygon(aes(fill = winner), color = "white") +
    scale_fill_manual(values = c("dodgerblue4", "firebrick1")) +
    theme_void() +
    ggtitle("Presidential Vote Share by State (1980-2020)") + 
    theme(strip.text = element_text(size = 12),
          aspect.ratio=1)

ggsave("figures/PV_states_historical.png")

####----------------------------------------------------------#
#### Forecast: simplified electoral cycle model. 
####----------------------------------------------------------#

# Create prediction (pv2p and margin) based on simplified electoral cycle model: 
# vote_2024 = 3/4*vote_2020 + 1/4*vote_2016 (lag1, lag2, respectively). 
pv2p_2024_states <- d_pvstate_wide |> 
    filter(year == 2020) |>
    group_by(state) |> 
  summarise(D_pv2p_2024 = 0.75*D_pv2p + 0.25*D_pv2p_lag1,
            R_pv2p_2024 = 0.75*R_pv2p + 0.25*R_pv2p_lag1) |>
  mutate(pv2p_2024_margin = R_pv2p_2024 - D_pv2p_2024,
         region = tolower(state))

# Plot the margin of victory in a U.S. state map.
pv2p_2024_states |>
    left_join(states_map, by = "region") |>
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(aes(fill = pv2p_2024_margin), color = "black") +
    ggtitle("2024 Presidential Forecast (Simplified Electoral Cycle Model)") + 
    scale_fill_gradient2(high = "firebrick1", 
                         mid = "white",
                         name = "win margin",
                         low = "dodgerblue4", 
                         breaks = c(-50,-25,0,25,50), 
                         limits=c(-50,50)) + 
  theme_void()

ggsave("figures/PV2024_simple_forecast.png")

# Generate projected state winners and merge with electoral college votes to make 
# summary of electoral college vote distributions. 
pv2p_2024_states <- pv2p_2024_states |>
    mutate(winner = ifelse(R_pv2p_2024 > D_pv2p_2024, "R", "D"),
           year = 2024)

ec <- read_csv("data/ec_full.csv")
ec[which(ec$state == "D.C."),]$state <- "District Of Columbia"
write_csv(ec, "data/ec_full.csv")

pv2p_2024_states <- pv2p_2024_states |>
    left_join(ec, by = c("state", "year"))

pv2p_2024_states |> 
    group_by(winner) |>
    summarise(electoral_votes = sum(electors))

## Harris: 276
## Trump: 262

####----------------------------------------------------------#
#### Forecast: past vote OLS models. 
####----------------------------------------------------------#

# OLS Model 1:
# Estimate simple/bivariate OLS models using previous cycle's vote share.
ols.D.1 <- lm(D_pv2p ~ D_pv2p_lag1, 
              data = d_pvstate_wide)
ols.R.1 <- lm(R_pv2p ~ R_pv2p_lag1, 
              data = d_pvstate_wide)
summary(ols.D.1); summary(ols.R.1)

# Make updated pv2p and ec predictions based on this model using 2020 (last cycle's) vote share.
ols.pred.data <- d_pvstate_wide |> 
  filter(year == 2020) |> 
  select(state, D_pv2p, R_pv2p, D_pv2p_lag1, R_pv2p_lag1) |> 
  rename(D_pv2p_lag2 = D_pv2p_lag1,
         R_pv2p_lag2 = R_pv2p_lag1, 
         D_pv2p_lag1 = D_pv2p, 
         R_pv2p_lag1 = R_pv2p) |> 
  arrange(state)

pred.ols.D.1 <- predict(ols.D.1, newdata = data.frame("D_pv2p_lag1" =  ols.pred.data$D_pv2p_lag1))
pred.ols.R.1 <- predict(ols.R.1, newdata = data.frame("R_pv2p_lag1" =  ols.pred.data$R_pv2p_lag1))

ols.pred.1 <- data.frame("state" = pv2p_2024_states$state, 
                         "year" = 2024,
                         "D_pv2p_2024" = pred.ols.D.1, 
                         "R_pv2p_2024" = pred.ols.R.1) |> 
  mutate(pv2p_2024_margin = R_pv2p_2024 - D_pv2p_2024, 
         winner = ifelse(R_pv2p_2024 > D_pv2p_2024, "R", "D")) |> 
  left_join(ec, by = c("state", "year"))

ols.pred.1 |>
  group_by(winner) |>
  summarise(electoral_votes = sum(electors))

# Harris: 226
# Trump: 312

# OLS Model 2:
# Estimate multivariate OLS models using one and two-cycle-lagged vote share variables. 
ols.D.2 <- lm(D_pv2p ~ D_pv2p_lag1 + D_pv2p_lag2, 
              data = d_pvstate_wide)
ols.R.2 <- lm(R_pv2p ~ R_pv2p_lag1 + R_pv2p_lag2,
              data = d_pvstate_wide)
summary(ols.D.2); summary(ols.R.2)

# Make updated pv2p and ec based on this model using 2020 (last cycle's) and 2016 (two cycle's ago) vote share. 
pred.ols.D.2 <- predict(ols.D.2, newdata = data.frame("D_pv2p_lag1" =  ols.pred.data$D_pv2p_lag1, 
                                                      "D_pv2p_lag2" =  ols.pred.data$D_pv2p_lag2))
pred.ols.R.2 <- predict(ols.R.2, newdata = data.frame("R_pv2p_lag1" =  ols.pred.data$R_pv2p_lag1,
                                                      "R_pv2p_lag2" =  ols.pred.data$R_pv2p_lag2))

ols.pred.2 <- data.frame("state" = pv2p_2024_states$state,
                         "year" = 2024,
                         "D_pv2p_2024" = pred.ols.D.2, 
                         "R_pv2p_2024" = pred.ols.R.2) |> 
  mutate(pv2p_2024_margin = R_pv2p_2024 - D_pv2p_2024, 
         winner = ifelse(R_pv2p_2024 > D_pv2p_2024, "R", "D")) |> 
  left_join(ec, by = c("state", "year"))

ols.pred.2 |>
  group_by(winner) |>
  summarise(electoral_votes = sum(electors))

# Harris: 247
# Trump: 291

# Extras: 
# These results seem pretty different than the electoral cycle model. 
# What would happen if we ran a different prediction model for each state without an intercept? 
# This is known as "regression through the origin," and may be appropriate when we expect that Y = 0 when X = 0. 
# See, e.g., https://onlinelibrary-wiley-com.ezp-prod1.hul.harvard.edu/doi/full/10.1111/1467-9639.00136. 
# For example, if the GOP in a previous election cycle received 0 votes last cycle, why would we expect them to have more votes this cycle? 
state.vec <- unique(d_pvstate_wide$state) |> sort()
state.mod.preds <- data.frame("state" = state.vec,
                              "year" = 2024, 
                              "D_pv2p_2024" = NA,
                              "R_pv2p_2024" = NA)

for (i in 1:length(state.vec)) {
  d_sub <- d_pvstate_wide[d_pvstate_wide$state == state.vec[i],]
  state.ols.D <- lm(D_pv2p ~ 0 + D_pv2p_lag1 + D_pv2p_lag2, 
                      data = d_sub)
  state.ols.R <- lm(R_pv2p ~ 0 + R_pv2p_lag1 + R_pv2p_lag2, 
                      data = d_sub)
  
  state.pred.data <- d_sub |> 
    filter(year == 2020) |> 
    ungroup() |> 
    select(D_pv2p, R_pv2p, D_pv2p_lag1, R_pv2p_lag1) |> 
    rename(D_pv2p_lag2 = D_pv2p_lag1,
           R_pv2p_lag2 = R_pv2p_lag1, 
           D_pv2p_lag1 = D_pv2p, 
           R_pv2p_lag1 = R_pv2p)
  
  state.mod.preds[i,]$D_pv2p_2024 <- predict(state.ols.D, newdata = data.frame("D_pv2p_lag1" =  state.pred.data$D_pv2p_lag1, 
                                                                               "D_pv2p_lag2" =  state.pred.data$D_pv2p_lag2))
  state.mod.preds[i,]$R_pv2p_2024 <- predict(state.ols.R, newdata = data.frame("R_pv2p_lag1" =  state.pred.data$R_pv2p_lag1,
                                                                               "R_pv2p_lag2" =  state.pred.data$R_pv2p_lag2))
}

state.mod.preds <- state.mod.preds |> 
  mutate(pv2p_2024_margin = R_pv2p_2024 - D_pv2p_2024, 
         winner = ifelse(R_pv2p_2024 > D_pv2p_2024, "R", "D")) |> 
  left_join(ec, by = c("state", "year"))

state.mod.preds |>
  group_by(winner) |>
  summarise(electoral_votes = sum(electors))

# Harris: 287
# Trump: 251

# Why might these models produce different results?
state.vec[which((pv2p_2024_states |> select(winner)) != 
                  (state.mod.preds |> select(winner)))]

# Models disagree about one state: the swing state of Arizona! 
pv2p_2024_states |> filter(state %in% c("Arizona"))
state.mod.preds |> filter(state %in% c("Arizona"))

# Influence of time in each estimator/form --> different weights on lagged votes. 
# May not want to use every year in subsequent analyses. 

# If you're up for a challenge, try plotting the margins from state model as a hexbin map! 
# https://r-graph-gallery.com/328-hexbin-map-of-the-usa.html
# (If not, don't worry about it!)
library(sf)
library(RColorBrewer)
library(viridis)

# Read US hexgrid data. 
us_hexgrid <- read_sf("data/us_states_hexgrid.geojson") |> 
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
us_hexgrid[us_hexgrid$google_name == "District of Columbia",]$google_name <- "District Of Columbia"

# Merge state predictions with hexgrid data.
state.mod.preds.hex <- us_hexgrid |> 
  left_join(state.mod.preds, by = c("google_name" = "state"))

state.mod.preds.hex[state.mod.preds.hex$google_name == "District Of Columbia",]$pv2p_2024_margin <- -50 # "Hack" so that DC can be included within normal margin bounds. 

ggplot(state.mod.preds.hex) +
  geom_sf(aes(fill = pv2p_2024_margin)) +
  geom_sf_text(aes(label = iso3166_2), color = "black", size = 3, alpha = 0.6) +
  scale_fill_gradient2(high = "firebrick1", 
                       mid = "white",
                       name = "win margin",
                       low = "dodgerblue4", 
                       breaks = c(-50,-25,0,25,50), 
                       limits=c(-50,50)) +
  ggtitle("2024 Presidential Forecast (Cycle-Lagged Vote Shares Model)") + 
  theme_void() + 
  theme(
    legend.position = c(0.07, 0.55),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(
      size = 22, hjust = 0.5, color = "#4e4d47",
      margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")
    )
  )

ggsave("figures/PV2024_OLS_forecast.png")

####----------------------------------------------------------#




