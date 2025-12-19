# This script uses the `childesr` package to download frequency information from
# the English portion of the CHILDES corpus <https://childes.talkbank.org/>.
library(tidyverse)
library(here)
library(childesr)

# Get the age range from the extracted vowels. We only want words from within
# this age range. 
vowels <- read_rds(here('data', 'untracked_vowels.rds'))
min_age <- min(vowels$age)
max_age <- max(vowels$age)

# What collections are available?
collections <- get_collections()
View(collections)

# What corpora are there?
avail_corpora <- get_corpora()
View(avail_corpora)

# Each corpus we use data from needs to be cited separately. 
# A previous version of this analysis took _all_ corpora. This is not workable.
# We select corpora including our age range and more than five participants 
# which have more-or-less conversational data. We avoid story retell tasks and
# picture naming, etc, to get a better sense of the everyday lexical environment
# of the children. Two corpora are from the UK, four are from the US. 
corpora <- c(
  "Wells", # https://childes.talkbank.org/access/Eng-UK/Wells.html - Eng-UK
  "Fletcher", # https://childes.talkbank.org/access/Eng-UK/Fletcher.html - Eng-UK
  "Tommerdahl", # https://childes.talkbank.org/access/Eng-UK/Tommerdahl.html - Eng-UK
  "Sawyer", # https://childes.talkbank.org/access/Eng-NA/Sawyer.html - Eng-US
  "Garvey", # https://childes.talkbank.org/access/Eng-NA/Garvey.html - Eng-US
  "Gathercole", # https://childes.talkbank.org/access/Eng-NA/Gathercole.html, - Eng-US
  "NewmanRatner" # https://childes.talkbank.org/access/Eng-NA/NewmanRatner.html - Eng-US
)

types <- get_types(
  collection = c('Eng-NA', 'Eng-UK'),
  corpus = corpora,
  language = "eng", 
  age=c(min_age, max_age),
  role = "target_child"
)

tokens <- get_tokens(
  token = "*",
  collection = c('Eng-NA', 'Eng-UK'),
  corpus = corpora,
  language = "eng", 
  age=c(min_age, max_age),
  role = "target_child"
)

# Some corpora use '+' to separate constituent words, e.g. 'bath+room'. We'll
# just strip these.
types |> 
  filter(
    str_detect(gloss, '\\+')
  ) |> 
  select(
    gloss, corpus_id
  )

types <- types |> 
  mutate(
    gloss = str_replace_all(gloss, '\\+', '')
  )
  
tokens <- tokens |> 
  mutate(
    gloss = str_replace_all(gloss, '\\+', '')
  )


# Let's save these in case we need to check the raw data the frequency 
# information comes form.
write_csv(types, here('data', 'raw_CHILDES_types.csv'))
write_csv(tokens, here('data', 'raw_CHILDES_tokens.csv'))

# We now aggregate the counts for each of types and tokens. 
types_agg <- types |> 
  group_by(gloss) |> 
  summarise(
    count = sum(count)
  ) |> 
  arrange(desc(count))

tokens_agg <- tokens |> 
  mutate(
    total_count = nrow(tokens)
  ) |> 
  group_by(gloss) |> 
  summarise(
    count = n(),
    total_count = first(total_count),
    perc = count/total_count * 100
  ) |> 
  arrange(desc(count))

# Check if there's anything major that changes if we get a measure which
# controls for difference in corpus size.
tokens_agg_by_corpus <- tokens |> 
  group_by(corpus_name) |> 
  mutate(
    corpus_count = n()
  ) |> 
  group_by(corpus_name, gloss) |> 
  summarise(
    word_count = n(),
    corpus_count = first(corpus_count),
    corpus_rate = word_count/corpus_count * 100
  ) |> 
  group_by(gloss) |> 
  summarise(
    mean_rate = mean(corpus_rate)
  ) |> 
  arrange(desc(mean_rate))

# I was curious whether rate data and the raw counts are closely associated.
# This might not be the case, if one corpus massively dwarfed the others and 
# had quite different lexical items.
cor.test(tokens_agg_by_corpus$mean_rate, tokens_agg$count, method="spearman")
# rho: 0.9696, r: 0.998, All good.

# Output aggregated tokens.
write_rds(tokens_agg, here('data', 'CHILDES_tokens.rds'))

# We could use the aggregated types data, 
# but the aggregated tokens data also gives percentages.
