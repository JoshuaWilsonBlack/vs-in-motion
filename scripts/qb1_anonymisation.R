# This script is slightly modified from one available in supplementary material
# to Hurring et al. (Under review)

# Script to anonymise data. This requires correspondence tables which are not
# made public.

library(tidyverse)
library(glue)
library(here)

QB1 <- read_rds(here('data', 'private_qb1_data.rds'))


# Remove speakers who did not grow up in NZ
QB1 <- QB1 %>%
  filter(
    participant_grew_up != "other",
    participant_grew_up != "",
    !is.na(participant_grew_up)
  )

# Data loss: 
# QB1: 707287 -> 511649


# Anonymise speakers
correspondence_table <- read_rds(
  here('data', 'private_correspondence_table.rds')
)

# Add anonymous speaker codes to data:
QB1 <- QB1 %>%
  mutate(
    code = str_extract(Participant, '[A-Z]+[0-9]+')
  ) %>%
  left_join(
    correspondence_table %>%
      mutate(
        code = str_extract(speaker, '[A-Z]+[0-9]+')
      ) %>%
      select(
        code, anonymised
      ),
    by = "code"
  )

# Any missing data in the anonymised column?
QB1 %>%
  filter(
    is.na(anonymised)
  )
# Nope!


# Stopword identification
# Modified to match stopwords used for kids.
stopwords <- c(
  # List from Brand 2021.
  'a', 'ah', 'ahh', 'am', 'an', 'and', 'are', "aren't", 'as', 'at',
  'aw', 'because', 'but', 'could', 'do', "don't", 'eh', 'for', 'from', 'gonna',
  'had', 'has', 'have', 'he', "he's", 'her', 'high', 'him', 'huh', 'i', "i'll",
  "i'm", "i've", "i'd", 'in', 'into', 'is', 'it', "it's", 'its', 'just', 'mean',
  'my', 'nah', 'not', 'of', 'oh', 'on', 'or', 'our', 'says', 'she', "she's",
  'should', 'so', 'than', 'that', "that's", 'the', 'them', 'there', "there's",
  'they', 'this', 'to', 'uh', 'um', 'up', 'was', "wasn't", 'we', 'were', 'what',
  'when', 'which', 'who', 'with', 'would', 'yeah', 'you', "you've",
  # Additional identified stop words.
  "then", "me", "too", "his",  "off", "onto", "can't", "can", "cos", "said", 
  "where")


word_filter <- function(in_data) {
  in_data %>%
    rename(word = orthography) %>%
    filter(
      !grepl("~", word), #filter tokens with hesitations
      !is.na(word), #filter tokens which do not have the word transcribed
    ) |> 
    mutate(stopword = word %in% stopwords)
}

QB1 <- word_filter(QB1)
# Data loss: 511649 -> 208774

# Anonymise words 
word_correspondence_table <- read_rds(
  here('data', 'private_word_correspondence_table.rds')
)

# Join word anonymisation codes.
QB1 <- QB1 %>%
  left_join(
    word_correspondence_table %>%
      rename(word_anon = anonymised),
    by = "word"
  )

# Modifed to remove unnecesary demographic info for this study.
desired_columns <- c(
  "anonymised", "participant_gender", "participant_age_category",
  "participant_nz_ethnic",
  "MatchId", "word_anon", "Target.segment",
  "Target.segment.start", "Target.segment.end", 
  "stress", "time_0_5", "Token.minus.1.segment",
  "Token.plus.1.segment", "utterance.articulation.rate", "utterance.speech.rate",
  "f1_time_0_5", "f2_time_0_5", "stopword"
)


QB1_out <- QB1 %>%
  select(
    any_of(desired_columns)
  ) %>%
  # We'll rename the variables as well to reduce tidying in the script.
  rename(
    speaker = anonymised,
    word = word_anon,
    gender = participant_gender,
    age = participant_age_category,
    ethnicity = participant_nz_ethnic,
    vowel = Target.segment,
    vowel_start = Target.segment.start,
    vowel_end = Target.segment.end,
    vowel_mid = time_0_5,
    previous_segment = Token.minus.1.segment,
    following_segment = Token.plus.1.segment,
    articulation_rate = utterance.articulation.rate,
    speech_rate = utterance.speech.rate,
    F1_50 = `f1_time_0_5`,
    F2_50 = `f2_time_0_5`
  )

# Export data
write_rds(QB1_out, here("data", "QB1_anon.rds"))
