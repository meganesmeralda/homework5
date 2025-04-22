# Meta --------------------------------------------------------------------
# Author:        Megan Zheng
# Date Created:  4/21/2025
# Date Edited:   4/21/2025
# Homework 5-1

# Preliminaries -----------------------------------------------------------
# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, knitr, modelsummary, broom)

final.data <- read_tsv('data/output/acs_medicaid.txt')

# Question 1: Adult population with direct purchase health insurance
## Calculate share of direct purchase insurance
final.data <- final.data %>%
  mutate(direct_share = ins_direct / adult_pop)
## Group by year to get national average
avg_direct_by_year <- final.data %>%
  group_by(year) %>%
  summarize(avg_direct_share = mean(direct_share, na.rm = TRUE))
## Plot just the national average line
q1 <- ggplot(avg_direct_by_year, aes(x = year, y = avg_direct_share)) +
  geom_line(color = "darkslategrey", size = 2) +
  geom_point(color = "darkslategrey", size = 2.5) +
  labs(title = "Average Share of Adult Population with Direct Purchase Health Insurance",
       x = "Year",
       y = "Direct Purchase Share") +
  theme_minimal()
print(q1)

# Question 2: written response

# Question 3: Adult population with Medicaid
## Compute the national average Medicaid share per year
medicaid_by_year <- final.data %>%
  group_by(year) %>%
  summarise(medicaid_share = sum(ins_medicaid) / sum(adult_pop))
## Plot
q3 <- ggplot(medicaid_by_year, aes(x = year, y = medicaid_share)) +
  geom_line(color = "darkslategrey", size = 2) +
  geom_point(color = "darkslategrey", size = 2.5) +
  labs(title = "Share of Adult Population with Medicaid Over Time",
       x = "Year",
       y = "Medicaid Share") +
  theme_minimal()
print(q3)

# Question 4: 
## Adding a new column for filtering states that expanded in 2014 
expanded <- final.data %>%
  group_by(State) %>%
  summarize(first_expand_year = unique(year(date_adopted))) %>%
  mutate(
    expand_group = case_when(
      is.na(first_expand_year) ~ "Never Expanded",
      first_expand_year == 2014 ~ "Expanded in 2014",
      TRUE ~ NA_character_  
    )
  ) %>%
  filter(!is.na(expand_group))

final.data.exp <- final.data %>%
  inner_join(expanded, by = "State")
## Calculate uninsured share by year and expansion group
uninsured.share <- final.data.exp %>%
  group_by(year, expand_group) %>%
  summarize(
    total_uninsured = sum(uninsured, na.rm = TRUE),
    total_adult_pop = sum(adult_pop, na.rm = TRUE),
    share_uninsured = total_uninsured / total_adult_pop,
    .groups = "drop"
  )
## Plot
q4 <- ggplot(uninsured.share, aes(x = year, y = share_uninsured, color = expand_group)) +
    geom_line(size = 2) +
    geom_point() +
    geom_vline(xintercept = 2014, linetype = "dashed", color = "black") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = c("darkslategrey", "lightslategrey")) +
    labs(
        title = "Uninsured Rate by Medicaid Expansion Status (2012–2019)",
        x = "Year",
        y = "Share Uninsured",
        color = "Expansion Status"
    ) +
    theme_minimal()
print(q4)

# Estimating ATEs

# Question 5: Average percent of uninsured individuals in 2012 and 2015 (expansion vs not), DD table

## Compute mean uninsured share by year & group
dd.table <- final.data.exp %>%
  filter(year %in% c(2012, 2015)) %>%
  group_by(expand_group, year) %>%
  summarize(
    avg_uninsured = sum(uninsured, na.rm = TRUE) / sum(adult_pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = year, values_from = avg_uninsured)

## DID Table 
q5 <- dd.table %>%
  mutate(
    diff = `2015` - `2012`
  )
 print(q5)

# Question 6: 

rm(list=c("final.data"))
save.image("submission1/results/hwk5_workspace.Rdata")
