# Meta --------------------------------------------------------------------
# Author:        Megan Zheng
# Date Created:  4/28/2025
# Date Edited:   4/28/2025
# Homework 5-1

# Preliminaries -----------------------------------------------------------
# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, knitr, modelsummary, broom, fixest, gt)

final.data <- read_tsv('data/output/acs_medicaid.txt')

final.data <- final.data %>%
  mutate(perc_private = (ins_employer + ins_direct)/adult_pop,
         perc_public = (ins_medicare + ins_medicaid)/adult_pop,
         perc_ins = (adult_pop - uninsured)/adult_pop,
         perc_unins = uninsured/adult_pop,
         perc_employer = ins_employer/adult_pop,
         perc_medicaid = ins_medicaid/adult_pop,
         perc_medicare = ins_medicare/adult_pop,
         perc_direct = ins_direct/adult_pop) %>%
  filter(! State %in% c("Puerto Rico", "District of Columbia"))


# Question 1: Adult population with direct purchase health insurance
## Calculate share of direct purchase insurance
q1 <- direct.plot <- final.data %>% group_by(year) %>% summarize(mean=mean(perc_direct)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_bw() +
  labs(
    x="Year",
    y="Fraction with Direct Purchase",
    title="Share of Direct Purchase Insurance over Time") +
  geom_vline(xintercept=2013.5, color="black")

# Question 2: written response

# Question 3: Adult population with Medicaid
q3 <- final.data %>% group_by(year) %>% summarize(mean=mean(perc_medicaid)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_bw() +
  labs(
    x="Year",
    y="Fraction with Medicaid",
    title="Share of Medicaid Insurance over Time"
  ) +
  geom_vline(xintercept=2013.5, color="red")

# Question 4: 
ins.plot.dat <- final.data %>% filter(is.na(expand_year) | expand_year==2014) %>%
  group_by(expand_ever, year) %>% summarize(mean=mean(perc_unins))

q4 <- ggplot(data=ins.plot.dat, aes(x=year,y=mean,group=expand_ever,linetype=expand_ever)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=2013.5, color="red") +
  geom_text(data = ins.plot.dat %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype="none") +
  labs(
    x="Year",
    y="Fraction Uninsured",
    title="Share of Uninsured over Time"
  )


# Estimating ATEs ------------------------------------------------------------

# Question 5, ATE: Average percent of uninsured individuals in 2012 and 2015 (expansion vs not), DD table
dd.table <- final.data %>% 
  filter(is.na(expand_year) | expand_year==2014) %>%
  filter(year %in% c(2012, 2015)) %>%  
  group_by(expand_ever, year) %>%
  summarize(uninsured=mean(perc_unins))

dd.table <- pivot_wider(dd.table, names_from="year", names_prefix="year", values_from="uninsured") %>% 
  ungroup() %>%
  mutate(expand_ever=case_when(
    expand_ever==FALSE ~ 'Non-expansion',
    expand_ever==TRUE ~ 'Expansion')
  ) %>%
  rename(Group=expand_ever,
         Pre=year2012,
         Post=year2015)

# Question 6/7, ATE: DID regression for Medicaid expansion effect, with and without fixed effects
final.data <- final.data %>%
  mutate(
    expand_ever = if_else(is.na(expand_year), FALSE, TRUE),
    post = year >= 2014
  )
reg.data <- final.data %>%
  filter(is.na(expand_year) | expand_year == 2014)

dd.est <- lm(perc_unins ~ post + expand_ever + post:expand_ever, data = reg.data)
fe.est <- feols(perc_unins ~ i(post, expand_ever, ref=FALSE) | State + year,
                cluster = ~State,
                data = reg.data)

q6 <- modelsummary(dd.est,
              output = "gt",
             coef_rename=c("postTRUE" = "Post 2014","expand_everTRUE"="Expand",
                           "treat" = "Post x Expand"),
             gof_omit='DF|F|Lik|AIC|BIC|Adj')
gtsave(q6, "submission3/results/q6.png")

models <- list("Standard DD" = dd.est, 
               "TWFE" = fe.est)
q7 <- modelsummary(models,
             output = "gt",
             coef_rename=c("postTRUE" = "Post 2014","expand_everTRUE"="Expand",
                           "treat" = "Post x Expand"),
             gof_omit='DF|F|Lik|AIC|BIC|Adj')
gtsave(q7, "submission3/results/q7.png")

## Question 8, ATE - DD with time varying treatment
reg.data2 <- final.data %>% 
  mutate(treat=case_when(
    year>=expand_year & !is.na(expand_year) ~ 1,
    is.na(expand_year) ~ 0,
    year<expand_year & !is.na(expand_year) ~ 0)
  )
fe.est2 <- feols(perc_unins~treat | State + year, data=reg.data2)

models <- list("Standard DD" = dd.est, 
               "TWFE" = fe.est,
               "Time-varying Treatment" = fe.est2)
q8 <- modelsummary(models,
             output = "gt",
             coef_rename=c("postTRUE" = "Post 2014","expand_everTRUE"="Expand",
                           "treat" = "Post x Expand"),
             gof_omit='DF|F|Lik|AIC|BIC|Adj')
gtsave(q8, "submission3/results/q8.png")

# Question 9, ATE - Event study with constant treatment
mod.twfe <- feols(perc_unins~i(year, expand_ever, ref=2013) | State + year,
                  cluster=~State,
                  data=reg.data)



# Question 10, ATE - Event study with time varying treatment
reg.data2 <- reg.data2 %>%
  mutate(time_to_treat=ifelse(expand_ever==TRUE, year-expand_year, -1),
         time_to_treat=ifelse(time_to_treat<=-4, -4, time_to_treat))

mod.twfe2 <- feols(perc_unins~i(time_to_treat, expand_ever, ref=-1) | State + year,
                  cluster=~State,
                  data=reg.data2)

rm(list=c("final.data"))
save.image("submission3/results/hwk5_workspace.Rdata")
