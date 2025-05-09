library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(lme4)
library(lmerTest)
library(janitor)
library(here)
library(readr)


# Read your CSV with semicolon delimiter
data <- read_delim("/Users/diegomedina/Desktop/final_paper/raw_data/measurements.csv", 
                   delim = ";")

################################################################################
#General Stats
# Summary stats per participant
stats_summary <- data %>%
  group_by(participant) %>%
  summarise(
    F3_mid_mean = mean(`F3 Midpoint`, na.rm = TRUE),
    F3_avg_mean = mean(`F3 Average`, na.rm = TRUE),
    F4_mid_mean = mean(`F4 Midpoint`, na.rm = TRUE),
    F4_avg_mean = mean(`F4 Average`, na.rm = TRUE)
  )

#Summary stats (overall - accross participant)
stats_summary2 <- data %>%
  summarise(
    F3_mid_mean = mean(`F3 Midpoint`, na.rm = TRUE),
    F3_avg_mean = mean(`F3 Average`, na.rm = TRUE),
    F4_mid_mean = mean(`F4 Midpoint`, na.rm = TRUE),
    F4_avg_mean = mean(`F4 Average`, na.rm = TRUE),
    F3_mid_sd = sd(`F3 Midpoint`, na.rm = TRUE),
    F3_avg_sd = sd(`F3 Average`, na.rm = TRUE),
    F4_mid_sd = sd(`F4 Midpoint`, na.rm = TRUE),
    F4_avg_sd = sd(`F4 Average`, na.rm = TRUE)
  )
#plot of general distribution of F3 and F4 across participants
 data |> 
  pivot_longer(
    cols = `F3 Midpoint`:`F4 Average`, 
    names_to = "estimate", 
    values_to = "vals"
  ) |> 
  ggplot() +
  aes(x = estimate, y = vals) + 
  geom_point(alpha = 0.03, position = position_nudge(x = 0.1)) + 
  stat_summary(
    fun.data = mean_sdl, geom = "pointrange", 
    fun.args = list(mult = 1)
  )
 
#Plot of specific distribution of F3 and F4 per participant.
data |> 
  filter(participant != "pr15") |> 
  pivot_longer(
    cols = `F3 Midpoint`:`F4 Average`, 
    names_to = "estimate", 
    values_to = "vals"
  ) |> 
  ggplot() +
  aes(x = estimate, y = vals) + 
  facet_wrap(~ participant) +
  geom_point(alpha = 0.1, position = position_nudge(x = 0.2)) + 
  stat_summary(
    fun.data = mean_sdl, geom = "pointrange", 
    fun.args = list(mult = 1)
  )

###############################################################################

#How linguistic factors influence f3 and f4 average
data_clean <- data %>%
  janitor::clean_names() %>%
  mutate(
    participant = as.factor(participant),
    word = as.factor(word),
    position_inside_the_word = as.factor(position_inside_the_word),
    preceding_vowel = as.factor(preceding_vowel),
    word_category = as.factor(word_category)
  )

#f3_average as a function of the other categories

# Null Model - F3 with random intercepts only
m0_f3 <- lmer(f3_average ~ 1 +
                (1 + position_inside_the_word | participant) +  # Random slope for position_inside_the_word
                (1 | word),
              data = data_clean)

summary(m0_f3)

# F3 as a function of position_inside_the_word
m1_f3 <- lmer(f3_average ~ 1 + position_inside_the_word +
                (1 + position_inside_the_word | participant) +  # Random slope for position_inside_the_word
                (1 | word),
              data = data_clean)

summary(m1_f3)

anova(m0_f3, m1_f3)

# F3 as a function of position_inside_the_word and word_category
m2_f3 <- lmer(f3_average ~ 1 + position_inside_the_word + word_category +
                (1 + position_inside_the_word | participant) +  # Random slope for position_inside_the_word
                (1 | word),
              data = data_clean)

summary(m2_f3)

anova(m0_f3, m1_f3, m2_f3)

# F3 as a function of position_inside_the_word, word_category was taken out, and preceding_vowel
m3_f3 <- lmer(f3_average ~ 1 + position_inside_the_word + preceding_vowel +
                (1 + position_inside_the_word | participant) +  # Random slope for position_inside_the_word
                (1 | word),
              data = data_clean)

summary(m3_f3)

anova(m0_f3, m1_f3, m3_f3)

# F3 as a function of position_inside_the_word, preceding_vowel, and gender
m4_f3 <- lmer(f3_average ~ 1 + position_inside_the_word + preceding_vowel + gender +
                (1 + position_inside_the_word | participant) +  # Random slope for position_inside_the_word
                (1 | word),
              data = data_clean)

summary(m4_f3)

anova(m0_f3, m1_f3, m3_f3, m4_f3)

# F3 as a function of position_inside_the_word, preceding_vowel, gender, and interaction
m5_f3 <- lmer(f3_average ~ 1 + position_inside_the_word + preceding_vowel * gender + # Interaction between preceding vowel and gender
                (1 + position_inside_the_word | participant) +  # Random slope for position_inside_the_word
                (1 | word),
              data = data_clean)

summary(m5_f3)

anova(m0_f3, m1_f3, m3_f3, m4_f3, m5_f3)


#plotting
#This one shows the two categories as (man and woman)

data |> 
  pivot_longer(
    cols = `F3 Midpoint`:`F4 Average`, 
    names_to = "estimate", 
    values_to = "vals"
  ) |> 
  filter(estimate %in% c("F3 Average", "F4 Average")) |> 
  ggplot() +
  aes(x = estimate, y = vals, color = preceding_vowel) + 
  geom_point(alpha = 0.1, position = position_jitter(width = 0.2, height = 0)) + 
  stat_summary(
    fun.data = mean_sdl, geom = "pointrange", 
    fun.args = list(mult = 1)
  ) + 
  facet_wrap(~gender)

# So I changed it to Men AND Women
data |> 
  pivot_longer(
    cols = `F3 Midpoint`:`F4 Average`, 
    names_to = "estimate", 
    values_to = "vals"
  ) |> 
  filter(estimate %in% c("F3 Average", "F4 Average")) |> 
  ggplot() +
  aes(x = estimate, y = vals, color = preceding_vowel) + 
  geom_point(alpha = 0.1, position = position_jitter(width = 0.2, height = 0)) + 
  stat_summary(
    fun.data = mean_sdl, geom = "pointrange", 
    fun.args = list(mult = 1)
  ) + 
  facet_wrap(~gender, labeller = labeller(gender = c("man" = "Men", "woman" = "Women")))

# For Educational Bakcground

stats_summary_edu <- data_clean %>%
  group_by(educational_background) %>%
  summarise(
    f3_mid_mean = mean(f3_midpoint, na.rm = TRUE),
    f3_mid_sd   = sd(f3_midpoint, na.rm = TRUE),
    f3_avg_mean = mean(f3_average, na.rm = TRUE),
    f3_avg_sd   = sd(f3_average, na.rm = TRUE)
  )


###############################################################################
#f4

data_clean <- data %>%
  janitor::clean_names() %>%
  mutate(
    participant = as.factor(participant),
    word = as.factor(word),
    position_inside_the_word = as.factor(position_inside_the_word),
    preceding_vowel = as.factor(preceding_vowel),
    word_category = as.factor(word_category)
  )

# Null Model - F4 with random intercepts only
m0_f4 <- lmer(f4_average ~ 1 +
                (1 + position_inside_the_word | participant) +  # Random slope for position_inside_the_word
                (1 | word),
              data = data_clean)

summary(m0_f4)

# F4 as a function of position_inside_the_word
m1_f4 <- lmer(f4_average ~ 1 + position_inside_the_word +
                (1 + position_inside_the_word | participant) +  # Random slope for position_inside_the_word
                (1 | word),
              data = data_clean)

summary(m1_f4)

anova(m0_f4, m1_f4)

# F4 as a function of position_inside_the_word and word_category
m2_f4 <- lmer(f4_average ~ 1 + position_inside_the_word + word_category +
                (1 + position_inside_the_word | participant) +  # Random slope for position_inside_the_word
                (1 | word),
              data = data_clean)

summary(m2_f4)

anova(m0_f4, m1_f4, m2_f4)

# F4 as a function of position_inside_the_word, word_category, and preceding_vowel
m3_f4 <- lmer(f4_average ~ 1 + position_inside_the_word + preceding_vowel +
                (1 + position_inside_the_word | participant) +  # Random slope for position_inside_the_word
                (1 | word),
              data = data_clean)

summary(m3_f4)

anova(m0_f4, m1_f4, m2_f4, m3_f4)

# F4 as a function of position_inside_the_word, preceding_vowel, and gender
m4_f4 <- lmer(f4_average ~ 1 + position_inside_the_word + preceding_vowel + gender +
                (1 + position_inside_the_word | participant) +  # Random slope for position_inside_the_word
                (1 | word),
              data = data_clean)

summary(m4_f4)

anova(m0_f4, m1_f4, m3_f4, m4_f4)

# F4 as a function of position_inside_the_word, preceding_vowel, gender, and interaction
m5_f4 <- lmer(f4_average ~ 1 + position_inside_the_word + preceding_vowel * gender + # Interaction between preceding vowel and gender
                (1 + position_inside_the_word | participant) +  # Random slope for position_inside_the_word
                (1 | word),
              data = data_clean)

summary(m5_f4)

anova(m0_f4, m1_f4, m2_f4, m3_f4, m4_f4, m5_f4)


# For Educational Bakcground

stats_summary_edu1 <- data_clean %>%
  group_by(educational_background) %>%
  summarise(
    f4_mid_mean = mean(f4_midpoint, na.rm = TRUE),
    f4_mid_sd   = sd(f4_midpoint, na.rm = TRUE),
    f4_avg_mean = mean(f4_average, na.rm = TRUE),
    f4_avg_sd   = sd(f4_average, na.rm = TRUE)
  )

