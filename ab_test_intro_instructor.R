#' ab_test_instructor.R
#' 
#' Code accompanying Lecture 4: A/B Tests: Introduction
#' of Social Media and Web Analytics
#' at TiSEM in 2024
#' 
#' Instructor Version

# --- Libraries --- #
library(readr)
library(dplyr)
library(ggplot2)
library(broom)
library(infer)
library(vtable)
library(car)

# --- Load and Inspect Data --- #
df <- read_csv("data/test_data.csv")

glimpse(df)

# --- Balance Testing --- #
df %>%
    select(group, days_since, visits, past_purch, 
           chard, sav_blanc, syrah, cab) %>%
    group_by(group) %>%
    summarize(across(everything(), list(mean = mean)))


df %>%
    select(group, days_since, visits, past_purch,
           chard, sav_blanc, syrah, cab) %>%
    st(group = 'group', group.test = TRUE)

# --- Treatment Effects --- #
# Mean diffs
df %>%
    select(group, open, click, purch) %>%
    group_by(group) %>%
    summarize(across(everything(), list(mean = mean)))

# Example Plot
df %>%
    filter(group != 'ctrl') %>%
    mutate(click = as.factor(click)) %>%
    ggplot() + 
    geom_bar(aes(fill = click, y=as.numeric(click), x=group),
             position="fill", stat="identity") +
    theme_bw()

# Test of proportions
df %>%
    filter(group != "ctrl") %>%
    mutate(open = as.logical(open)) %>%
    prop_test(open ~ group,
              alternative = "greater")

# Linear Regression Based analaysis
mod1 <- lm(open ~ group, 
          data = df %>% filter(group != "ctrl")
          )
tidy(mod1)

mod2 <- lm(click ~ group, 
          data = df %>% filter(group != "ctrl")
          )
tidy(mod2)

mod3 <- lm(purch ~ group, 
          data = df %>% filter(group != "ctrl")
          )
tidy(mod3)

mod4 <- lm(purch ~ group, 
          data = df
          )
tidy(mod4)

linearHypothesis(mod4, c("groupemail_A = groupemail_B"))

# --- Sample Size Planning --- # 
power.t.test(sd = sd(df$purch), # ideally using
                                # pre-experiment data!
             delta = 1, # minimum detectable effect
             sig.level = 0.95, # alpha: industry standard
             power=0.80 # 1 - beta: industry standard
             )

power.prop.test(p1 = 0.07,
                p2 = 0.07 + 0.01, # d = 0.01
                sig.level = 0.05,
                power = 0.80
                )


