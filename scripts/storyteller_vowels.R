# Extract the story teller data from LaBB-CAT.
# This requires access to our LaBB-CAT instance.
library(tidyverse)
library(ggrepel)
library(here)
library(nzilbb.labbcat)
library(nzilbb.vowels)

labbcat.url <- "https://labbcat.canterbury.ac.nz/kids"

participant <- expressionFromIds("Storyteller")

pattern <- list(
  columns = list(
    list(
      layers = list(
        segment = list(pattern = "[I@EVQU{i#3\\$u]")
      )
    )  
  )
)

# Collect matching tokens.
matches <- getMatches(
  labbcat.url,
  pattern = pattern,
  page.length = 1000,
  anchor.confidence.min = 50,
  participant.expression = participant
)


# Collect match labels (additional information at the token level)
match_labels <- getMatchLabels(
  labbcat.url,
  match.ids = matches$MatchId,
  layer.ids = c(
    "celexSegment", "noise",
    "syllable", "orthography",
    "celex_frequency", 'corpus_frequency'
  )
)

# Collect the following segment
following_seg <- getMatchLabels(
  labbcat.url, 
  matches$MatchId, 
  "segment",
  target.offset=1
)

matches <- bind_cols(matches, match_labels, following_seg)

# Use Praat integration to collect pitch information
pitches <- processWithPraat(
  labbcat.url,
  matches$MatchId, matches$Target.segment.start, matches$Target.segment.end,
  window.offset = 0.025,
  praatScriptPitch(
    get.mean = TRUE,
    get.minimum = TRUE,
    get.maximum = TRUE,
    time.step = 0,
    pitch.floor = 150,
    max.number.of.candidates = 15,
    very.accurate = FALSE,
    silence.threshold = 0.03,
    voicing.threshold = 0.5,
    octave.cost = 0.01,
    octave.jump.cost = 0.35,
    voiced.unvoiced.cost = 0.35,
    pitch.ceiling = 1250, # On the high side, some children can reach almost 2000 Hz when yelling
    # https://www.fon.hum.uva.nl/praat/manual/Intro_4_2__Configuring_the_pitch_contour.html
    pitch.floor.male = NULL,
    voicing.threshold.male = 0.4,
    pitch.ceiling.male = NULL,
    gender.attribute = "participant_gender",
    value.for.male = "M",
    sample.points = NULL,
    interpolation = "linear",
    skip.errors = TRUE
  )
)

matches <- bind_cols(matches, pitches) |> select(-Error)

# Tidy names, generate useful columns, remove unnecessary columns.
matches <- matches |> 
  rename(
    start = Target.segment.start,
    end = Target.segment.end,
    word_start = Target.word.start,
    word_end = Target.word.end,
    text = Text,
    word = orthography,
    following_segment = Token.plus.1.segment
  ) 


matches <- matches |> 
  mutate(
    vowel = fct_recode(
      factor(Target.segment),
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
      GOOSE = "u",
      SCHWA = "@"
    ),
    # Remove consonants added by Celex
    celex_vowel = str_replace(
      celexSegment,
      "[^iIE{#\\$3QVUu@185276]",
      ""
    ),
    celex_vowel = fct_recode(
      factor(celex_vowel),
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
      GOOSE = "u",
      SCHWA = "@"
    ),
    stress = case_when(
      str_detect(syllable, '".*') ~ '"',
      str_detect(syllable, "'.*") ~ "'",
      .default = "0"
    )
  )

matches <- matches |> 
  mutate(
    participant_gender = "M"
  )

fasttrack_formants <- processWithPraat(
  labbcat.url,
  matches$MatchId, matches$start, matches$end,
  window.offset = 0.025,
  praatScriptFastTrack(
    formants = c(1, 2),
    sample.points = c(0.5),
    lowest.analysis.frequency = 5000,
    lowest.analysis.frequency.male = 4500,
    highest.analysis.frequency = 7000,
    highest.analysis.frequency.male = 6500,
    gender.attribute = "participant_gender",
    value.for.male = "M",
    time.step = 0.002,
    tracking.method = "burg",
    number.of.formants = 3,
    maximum.f1.frequency = 1200,
    maximum.f1.bandwidth = NULL,
    maximum.f2.bandwidth = NULL,
    maximum.f3.bandwidth = NULL,
    minimum.f4.frequency = 2900,
    enable.rhotic.heuristic = TRUE,
    enable.f3.f4.proximity.heuristic = TRUE,
    number.of.steps = 20,
    number.of.coefficients = 5
  )
)

matches <- bind_cols(
  matches,
  fasttrack_formants
)

matches <- matches |> 
  rename(
    F1_50 = f1_time_0_5,
    F2_50 = f2_time_0_5
  ) |> 
  filter(
    celex_vowel %in% c(
      "DRESS",
      "FLEECE",
      "KIT",
      "STRUT",
      "START",
      "THOUGHT",
      "LOT",
      "NURSE",
      "GOOSE",
      "TRAP"
    )
  )

matches <- matches |> 
  filter(
    !following_segment %in% c('l', 'r')
  )

matches <- matches |> 
  relocate(
    Participant, celex_vowel, F1_50, F2_50
  ) |> 
  lobanov_2()


vowel_means <- matches |> 
  group_by(celex_vowel) |> 
  summarise(
    F1_lob2 = mean(F1_lob2),
    F2_lob2 = mean(F2_lob2)
  )

# Plot the storyteller vowel space.
matches |> 
  ggplot(
    aes(
      x = F2_lob2,
      y = F1_lob2,
      colour = celex_vowel,
      label = celex_vowel
    )
  ) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_point(size = 2, data = vowel_means) +
  geom_label_repel(size = 3, data = vowel_means) + 
  scale_x_reverse() + 
  scale_y_reverse()

# Save data
write_rds(matches, here('data', 'storyteller.rds'))
write_csv(matches, here('data', 'storyteller.csv'))
write_rds(vowel_means, here('data', 'storyteller_means.rds'))
write_csv(vowel_means, here('data', 'storyteller_means.csv'))
