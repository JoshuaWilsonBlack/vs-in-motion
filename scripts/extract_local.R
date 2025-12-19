# Extract audio for local Praat processing.

# Praat version 6.2.09, Feb 15, 2022.
# FastTrack version: most recent 4/5/23

# Formant bounds
# label	f1lower	f1upper	f2lower	f2upper	f3lower	f3upper
# START	350	1500	1200	3500	0	5000
# THOUGHT	350	1500	1200	2250	0	5000
# TRAP	350	1500	1200	3500	0	5000
# NURSE	350	1500	1200	3500	0	5000
# DRESS	350	1500	1500	4000	0	5000
# FLEECE	350	1500	1500	4000	0	5000
# KIT	350	1500	1250	3500	0	5000
# LOT	350	1500	1200	2500	0	5000
# GOOSE	350	1500	1000	3500	0	5000
# FOOT	350	1500	900	3500	0	5000
# STRUT	350	1500	1200	3500	0	5000

library(tidyverse)
library(here)

library(fasttrackr)

library(nzilbb.labbcat)

labbcat.url <- "https://labbcat.canterbury.ac.nz/kids/"

matches <- read_rds(here('data', 'untracked_vowels.rds'))

# Establish front, back and mid categories.
matches <- matches |> 
  filter(
    celex_vowel %in% c(
      "DRESS", "THOUGHT", "TRAP", "SCHWA", "START", "NURSE", "FLEECE", 
      "KIT", "LOT", "GOOSE", "FOOT", "STRUT"
    )
  ) |> 
  mutate(
    vowel_type = case_when(
      vowel %in% c('FLEECE', 'DRESS', 'NURSE', 'GOOSE', 'TRAP', 'KIT') ~ "Front",
      vowel %in% c('LOT', 'THOUGHT', 'FOOT') ~ "Back",
      .default = "Other"
    ),
    celex_vowel_type = case_when(
      celex_vowel %in% c('FLEECE', 'DRESS', 'NURSE', 'GOOSE', 'TRAP', 'KIT') ~ "Front",
      celex_vowel %in% c('LOT', 'THOUGHT', 'FOOT') ~ "Back",
      .default = "Other"
    )
  )

# tokens: 15974

# How many type changes?
matches |> 
  filter(
    vowel_type != celex_vowel_type
  ) |> 
  count(vowel_type, celex_vowel_type)
  
# Starting nzilbb.labbcat within map doesn't start the authentication so we 
# evaluate getLayerIds here.
getLayerIds(labbcat.url)

# Extract audio files.
match_extraction <- matches |> 
  group_by(celex_vowel_type) |> 
  nest() |> 
  mutate(
    extracted_files = map2(
      data, 
      celex_vowel_type,
      ~ getSoundFragments(
        labbcat.url,
        ids = .x$transcript,
        start.offsets = .x$start - 0.025,
        end.offsets = .x$end - 0.025,
        path = paste0("/home/jwb/Documents/kids_local_processing/", .y, "/sounds")
      )
    )
  )

match_extraction <- match_extraction |> 
  mutate(
    data = map2(
      data, 
      extracted_files, 
      ~ mutate(
        .x, 
        file = str_extract(.y, "[A-Za-z0-9_\\.\\-]*$")
      )
    ),
    file_info = map(
      celex_vowel_type, 
      ~ makefileinformation(paste0("/home/jwb/Documents/kids_local_processing/", .x))
    )
  )


match_extraction <- match_extraction |> 
  mutate(
    file_info = map2(
      file_info,
      data,
      ~ .x |>
        left_join(
          .y |> 
            select(file, vowel)
        ) |> 
        select(-label) |> 
        rename(
          label = vowel
        ) |> 
        relocate(number, file, label)
    )
  )

# save file info
walk2(
  match_extraction$file_info,
  match_extraction$celex_vowel_type, 
  ~write_csv(
    .x, 
    paste0(
      "/home/jwb/Documents/kids_local_processing/",
      .y,
      "/file_information.csv"
    )  
  )
)

names(match_extraction)
match_extraction$data[[1]]$file

# Need to link token and audio file.
token2file <- match_extraction |> 
  select(celex_vowel_type, data) |> 
  unnest(data) |> 
  select(MatchId, celex_vowel_type, file)

write_rds(token2file, here('data', 'token2file.rds'))
