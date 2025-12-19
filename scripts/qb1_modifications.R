# This script has two purposes: 
# 1) Apply standard filtering and normalisation, following Hurring et al. (Under review)
# 2) Get word frequency information.

# Code is borrowed from a project in which the same filters were applied
# separately to multiple datasets. For that reason, general functions were
# defined. In this script, we apply the steps to a single data set. Nonetheless,
# we keep the functions.

library(tidyverse)
library(here)
library(nzilbb.vowels)
library(nzilbb.labbcat)

labbcat.url <- "https://labbcat.canterbury.ac.nz/quakestudies"

QB1_anon <- read_rds(here('data', 'QB1_anon.rds'))

# Filter first, then get the additional frequency information (therefore:
# smaller requirest to LaBB-CAT).

wells_code <- function(in_data) {
  in_data %>% 
    mutate(
      vowel = fct_recode(
        factor(vowel),
        FLEECE = "i",
        KIT = "I",
        DRESS = "E",
        TRAP = "{",
        START = "#",
        LOT = "Q",
        THOUGHT = "$",
        NURSE = "3",
        STRUT = "V",
        FOOT = "U",
        GOOSE = "u"
      )
    )
}

QB1_anon <- QB1_anon %>%
  wells_code() %>% 
  mutate(
    vowel_end = as.numeric(vowel_end),
    vowel_start = as.numeric(vowel_start),
    vowel_dur = vowel_end - vowel_start
  )

error_filter <- function(in_data) {
  out_data <- in_data %>%
    filter(
      between(vowel_dur, 0.01, 2), #filter tokens with very short or long vowel 
      # durations
      F1_50 < 1100, # Remove all tokens with F1 at or above 1100hz.
      !is.na(gender), #filter speakers with missing gender
      !gender == "",
      !is.na(age), # Filter speakers with missing age.
      age != 'na', # Filter speakers with missing age.
      !is.na(F1_50), # Filter missing formant values.
      !is.na(F2_50)
    )
}

QB1_anon <- error_filter(QB1_anon)
# 508774 -> 482797

QB1_anon <- QB1_anon %>% 
  mutate(
    unstressed = stress != "0" #label any unstressed tokens
  )

liquid_filter <- function(in_data) {
  out_data <- in_data %>% mutate(
    following_segment_category = fct_collapse(
      as_factor(following_segment),
      labial = c('m', 'p', 'b', 'f', 'w'),
      velar = c('k', 'g', 'N'),
      liquid = c('r', 'l'),
      other_level = "other"
    )
  ) %>%
    filter(
      !following_segment_category == 'liquid'
    )
}

QB1_anon <- QB1_anon %>%
  liquid_filter()
# 482797 -> 453075

# Apply standard deviation filter with limit of 2.5
sd_filter <- function(in_data, sd_limit = 2.5) {
  vowels_all_summary <- in_data %>%
    # Remove tokens at +/- sd limit (default 2.5 sd)
    group_by(speaker, vowel) %>%
    summarise(
      #calculate the summary statistics required for the outlier removal.
      n = n(),
      mean_F1 = mean(F1_50, na.rm = TRUE),
      mean_F2 = mean(F2_50, na.rm = TRUE),
      sd_F1 = sd(F1_50, na.rm = TRUE),
      sd_F2 = sd(F2_50, na.rm = TRUE),
      # Calculate cut off values.
      max_F1 = mean(F1_50) + sd_limit*(sd(F1_50)),
      min_F1 = mean(F1_50) - sd_limit*(sd(F1_50)),
      max_F2 = mean(F2_50) + sd_limit*(sd(F2_50)),
      min_F2 = mean(F2_50) - sd_limit*(sd(F2_50))
    )
  
  #this is the main outlier filtering step.
  out_data <- in_data %>%
    inner_join(vowels_all_summary) %>%
    mutate(
      outlier = ifelse(
        F1_50 > min_F1 &
          F1_50 < max_F1 &
          F2_50 > min_F2 &
          F2_50 < max_F2, 
        FALSE, 
        TRUE
      )
    ) %>%
    group_by(speaker, vowel) %>%
    filter(outlier == FALSE) %>%
    ungroup() %>% 
    select(
      -c(
        outlier, n, mean_F1, mean_F2, sd_F1,
        sd_F2, max_F1, min_F1, max_F2, min_F2,
      )
    )
}


QB1_anon <- QB1_anon %>%
  sd_filter(sd_limit = 2.5)
# 453075 -> 436788

# remove FOOT
QB1_anon <- QB1_anon %>%
  filter(
    vowel != "FOOT"
  )
# 453075 -> 429667

# Filter speakers with low token counts. 
low_n_filter <- function(in_data) {
  low_speakers <- in_data %>%
    mutate(
      vowel = factor(vowel)
    ) %>% 
    # .drop is required to get situations in which a speaker has NO tokens for
    # a given vowel. It ensures a group for all levels of the vowel factor 
    # (including those not present in the data for a given speaker)
    group_by(speaker, vowel, .drop = FALSE) %>% 
    count() %>%
    ungroup() %>%
    filter(n < 5) %>%
    pull(speaker)
  
  out_data <- in_data %>%
    ungroup() %>%
    filter(!speaker %in% low_speakers)
}

QB1_anon <- QB1_anon %>%
  low_n_filter()
# 429667 -> 423735

# apply normalisation. We change name of dataframe here. This would be slightly
# clearer if done above. We will save this data as `QB1.rds` at the end of the
# script.
QB1 <- QB1_anon %>%
  # Reorder the data into the column order required by the Lobanov 2 function.
  relocate(speaker, vowel, F1_50, F2_50) %>%
  lobanov_2()

# Get frequency
celex_frequency <- getMatchLabels(
  labbcat.url,
  QB1$MatchId,
  "celex frequency"
)

QB1 <- QB1 |> 
  mutate(
    celex_frequency = celex_frequency
  )

# Output
write_rds(QB1, here('data', 'QB1.rds'), , compress = "gz")

