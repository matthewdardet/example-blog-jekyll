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
    scale_fill_manual(values = c("blue", "red")) 

# Example: lineplot (+ custom colors + nicer theme).
ggplot(d_popvote, aes(x = year, y = pv2p, colour = party)) +
    geom_line() +
    scale_color_manual(values = c("blue", "red")) + 
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
    scale_color_manual(values = c("blue", "red"), name = "") +
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
  rename(D_pv = pv_D, R_pv = pv_R, D_pv2p = pv2p_D, R_pv2p = pv2p_R)

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
  scale_fill_gradient(low = "white", high = "red") +
  theme_void()

# Generate map of state winners using their vote count. 
pv_win_map <- d_pvstate_wide |>
    filter(year == 2000) |>
    left_join(states_map, by = "region") |>
    mutate(winner = ifelse(R_pv > D_pv, "republican", "democrat"))

ggplot(pv_win_map, aes(long, lat, group = group)) +
    geom_polygon(aes(fill = winner), color = "black") +
    scale_fill_manual(values=c("blue", "red")) +
    theme_void()

# Generate map of win-margins.
pv_margins_map <- d_pvstate_wide |>
    filter(year == 2000) |>
    left_join(states_map, by = "region") |>
    mutate(win_margin = (R_pv2p-D_pv2p))

ggplot(pv_margins_map, aes(long, lat, group = group)) +
       geom_polygon(aes(fill = win_margin), color = "black") +
       scale_fill_gradient2(high = "red", 
                            #mid = scales::muted("purple"), ##TODO: purple or white better?
                            mid = "white",
                            name = "win margin",
                            low = "blue", 
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
    scale_fill_manual(values = c("blue", "red")) +
    theme_void() +
    ggtitle("Presidential Vote Share by State (1980-2020)") + 
    theme(strip.text = element_text(size = 12),
          aspect.ratio=1)

ggsave("figures/PV_states_historical.png")

####----------------------------------------------------------#
#### Forecast: simplified electoral cycle model. 
####----------------------------------------------------------#

pv2p_2024_states <- d_pvstate_wide |>
    filter(year == 2016 | year == 2020) |>
    group_by(state) |>
    arrange(-year) |>
    summarise(D_pv2p_2024 = 0.75*first(D_pv2p) + 0.25*last(D_pv2p),
              R_pv2p_2024 = 0.75*first(R_pv2p) + 0.25*last(R_pv2p)) |>
    mutate(pv2p_2024_margin = R_pv2p_2024 - D_pv2p_2024,
           region = tolower(state))

pv2p_2024_states |>
    left_join(states_map, by = "region") |>
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(aes(fill = pv2p_2024_margin), color = "black") +
    ggtitle("2024 Presidential Forecast (Simplified Electoral Cycle Model)") + 
    scale_fill_gradient2(high = "red", 
                         mid = "white",
                         name = "win margin",
                         low = "blue", 
                         breaks = c(-50,-25,0,25,50), 
                         limits=c(-50,50)) + 
  theme_void()
ggsave("figures/PV2024_simple_forecast.png")

# Generate projected state winners and merge with electoral college votes 
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

