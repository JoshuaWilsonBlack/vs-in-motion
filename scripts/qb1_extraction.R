# This script is taken from Hurring et al. (Under review).

# This script searches for all vowel tokens in the QB1 corpus, collects relevant
# attributes, and interfaces with Praat (through LaBB-CAT) in order to extract
# formants.

# This version of the script handles each vowel separately to avoid problems
# with very large LaBB-CAT queries. A data frame is made with a row for each
# vowel type. The mapping functions from 'purrr' (within the core tidyverse
# packages) are then used to apply the 'nzilbb.labbcat' functions to each vowel.

library(tidyverse)
library(glue)
library(here)
library(nzilbb.labbcat)

labbcat.url <- "https://labbcat.canterbury.ac.nz/quakestudies"

labbcatTimeout(seconds=120)

patterns = c(
  "I", "E", "V", "Q", "U", "{", "i", "#", "3", "\\$", "u"
)

matches <- tibble(
  pattern = patterns 
)

matches <- matches %>% 
  mutate(
    data = map(
      pattern, 
      ~ getMatches(
          labbcat.url,
          pattern = list(segment = .x),
          aligned = TRUE,
          transcript.expression =  expressionFromTranscriptTypes(
            c('Quake Story')
          ),
          page.length = 500,
          main.participant = TRUE
      )
    )
  )

# Collect QB1 and QB2 participant information.

desired_part_attributes <- c(
  'participant_gender', 'participant_age_category', 'participant_ethnicity',
  'participant_nz_ethnic', 'participant_grew_up', 'participant_grew_up_region',
  'participant_grew_up_town', 'participant_languages_spoken', 
  'participant_duration', 'participant_speaker articulation rate', 
  'participant_speaker speech rate', 'participant_word count',
  'participant_qb2_age_category', 'participant_qb2_gender',
  'participant_qb2_ethnicity', 'participant_qb2_iwi_hapū',
  'participant_qb2_secondary_school', 'participant_qb2_areas_lived_since_2012',
  'participant_qb2_area_lived_2020', 'participant_qb2_occupation_2012',
  'participant_qb2_occupation_2020', 'participant_qb2_household_2012',
  'participant_qb2_household_2020', 'participant_qb2_recently_seen_original'
)

matches <- matches %>% 
  mutate(
    participant_attributes = map(
      data, 
      ~ getParticipantAttributes(
        labbcat.url,
        .x$Participant %>% unique(),
        desired_part_attributes
      )
    )
  )



matches <- matches %>%
  mutate(
    data = map2(
      data,
      participant_attributes,
      ~ left_join(
        .x,
        .y %>% 
          rename(
            Participant = participant
          )
      )
    )
  )



# Collect transcript attributes 
matches <- matches %>% 
  mutate(
    transcript_attributes = map(
      data, 
      ~ getTranscriptAttributes(
        labbcat.url,
        .x$Transcript %>% unique(),
        c(
          'transcript_duration', 
          'transcript_word count', 
          "transcript_speaker articulation rate"
        )
      )
    )
  )

matches <- matches %>%
  mutate(
    data = map2(
      data,
      transcript_attributes,
      ~ left_join(
        .x,
        .y %>% 
          rename(
            Transcript = transcript
          )
      )
    )
  )

### Continue

# Collect formants

matches <- matches %>% 
  mutate(
    formants = map(
      data,
      ~ processWithPraat(
        labbcat.url,
        .x$MatchId, .x$Target.segment.start, .x$Target.segment.end,
        praatScriptFormants(
          formants = c(1, 2),
          sample.points = c(0.5),
          max.number.formants = 5,
          max.formant = 5500,
          max.formant.male = 5000,
          gender.attribute = "participant_gender",
          value.for.male = "M",
          window.length = 0.025,
          preemphasis.from = 50,
          time.step=0.0025
        ),
        window.offset = 0.5 # process audio 0.5 seconds either side of target.
      )
    )
  )

matches <- matches %>% 
  mutate(
    data = map2(data, formants, bind_cols)
  )

# Get environment
get_environ <- function(in_data, offset) {
  preceeding <- getMatchAlignments(
    labbcat.url, 
    in_data$MatchId, 
    "segment",
    target.offset=offset, 
    anchor.confidence.min=50, #  automatically aligned following or preceeding
    include.match.ids = TRUE
  )
}

matches <- matches %>% 
  mutate(
    preceeding = map(data, ~ get_environ(.x, -1)),
    following = map(data, ~ get_environ(.x, 1))    
  )

matches <- matches %>% 
  mutate(
    data = map2(data, preceeding, ~ bind_cols(.x, select(.y, -x))),
    data = map2(data, following, ~ bind_cols(.x, select(.y, -x))),
  )


# Get additional useful layers.
matches <- matches %>% 
  mutate(
    additional_layers = map(
      data, 
      ~ getMatchLabels(
        labbcat.url, 
        .x$MatchId, 
        c(
          "orthography", 
          "stress"
        )
      )
    ),
    utterance_layers = map(
      data,
      ~ getMatchLabels(
        labbcat.url,
        .x$MatchId,
        c(
          "utterance articulation rate", 
          "utterance speech rate"
        )
      )
    )
  )

matches <- matches %>% 
  mutate(
    data = map2(data, additional_layers, bind_cols),
    data = map2(data, utterance_layers, bind_cols)
  )


# The extra material in the 'mutate' is to deal with differences in data type
# due to some queries returning error messages rather than numerical values.
# and the presence of numerical symbols in the DISC alphabet.
# Warnings produced in this data are all NAs introduced by coercion. This is
# note a problem. We leave the warnings in and recommend people look at them
# if using this code with their own data.

out_data <- matches %>% 
  select(
    pattern, data
  ) %>% 
  mutate(
    data = map(
      data,
      ~ .x %>% 
        mutate(
          across(contains('time_0_5'), as.numeric),
          across(contains('Target.segment'), as.character)
        )
    )
  ) %>% 
  unnest(data)

# Formants to numeric formant.
out_data <- out_data %>%
  mutate(
    # Change formant columns to numeric file type.
    across(contains('time_0_5'), as.numeric)
  )


# Convert to tibble.
out_data <- as_tibble(out_data)

# Output
write_rds(out_data, here('data', 'private_qb1_data.rds'))
