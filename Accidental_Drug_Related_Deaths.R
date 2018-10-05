# Accidental drug related deaths in Connecticut, 2012-2017
#
# Examples for a workshop on lubrdiate and string. 
#
# Author: James Henderson
# Updated: October 5, 2018

# Libraries: ------------------------------------------------------------------
library(tidyverse); library(lubridate); library(magrittr)

# Data: -----------------------------------------------------------------------
adr_deaths =
  readr::read_delim( 'https://data.ct.gov/api/views/rybz-nyjw/rows.csv?accessType=DOWNLOAD', 
                   delim = ',')
adr_deaths %<>% mutate(Date = mdy(Date)) %>% filter(!is.na(Date) & !is.na(Sex))

with(adr_deaths, range(Date, na.rm=TRUE) )

# Annual trends: --------------------------------------------------------------
adr_deaths %>% 
  mutate(year = year(Date) ) %>%
  group_by(year) %>%
  summarize( n = n() ) %>%
  ggplot( aes( x = year, y = n) ) +
  geom_point() +
  geom_line() + 
  theme_bw()

# Monthly trend: -------------------------------------------------------------
adr_deaths %>% 
  mutate(month = floor_date(Date, 'month') ) %>%
  group_by(month) %>%
  summarize( n = n() ) %>%
  ggplot( aes( x = month, y = n) ) +
  geom_point() +
  geom_line() + 
  theme_bw() + 
  geom_smooth()

# Day of week: ----------------------------------------------------------------
adr_deaths %>% 
  mutate(day = wday(Date, label = TRUE) ) %>%
  group_by(day) %>%
  summarize( n = n() ) %>%
  ggplot( aes( x = day, y = n) ) +
  geom_col() +
  theme_bw() + 
  xlab('')

# Full length names
adr_deaths %>% 
  mutate(day = wday(Date, label = TRUE, abbr = FALSE) ) %>%
  group_by(day) %>%
  summarize( n = n() ) %>%
  ggplot( aes( x = day, y = n) ) +
  geom_col() +
  theme_bw() + 
  xlab('')

## Part 2, strings: -----------------------------------------------------------
library(stringr)

# Explore InjuryPlace
adr_deaths %>% 
  group_by(InjuryPlace) %>% 
  summarize(n = n()) %>%
  arrange(desc(n))

# Use grepl to find words similar to 
place = {adr_deaths %>% filter( grepl('resid', InjuryPlace, ignore.case = TRUE))}$InjuryPlace
table(place)

# An alternative using "str_detect" 
adr_deaths %>% 
   filter( str_detect( str_to_lower(InjuryPlace), 'resid') ) %>%
   group_by(InjuryPlace) %>%
   summarize( n = n())

# Flag for residential
adr_deaths %<>% 
  mutate(residential = 
            str_detect( str_to_lower(InjuryPlace), 'resid') & 
           !str_detect( str_to_lower(InjuryPlace), 'instit')
  )
with(adr_deaths, table(residential) )

## Look for injuries in automobiles
#! Doesn't work as expected. Use "or" i.e. | instead.
adr_deaths %>% 
  filter( str_detect( str_to_lower(InjuryPlace), c('aut', 'veh', 'car') )) %>%
  group_by(InjuryPlace) %>% 
  summarize(n = n()) %>%
  arrange(desc(n))

adr_deaths %>%
  mutate( inj_place = str_to_lower(InjuryPlace)) %>%
  filter( str_detect( inj_place, 'aut') | 
          str_detect(inj_place, 'veh') |
          str_detect(inj_place, 'car')
   ) %>%
  group_by(InjuryPlace) %>% 
  summarize(n = n()) %>%
  arrange(desc(n))

# Put the "or" logic into the pattern
adr_deaths %>%
  mutate( inj_place = str_to_lower(InjuryPlace)) %>%
  filter( str_detect( inj_place, 'aut|veh|car ') ) %>% # Note the space!
  group_by(InjuryPlace) %>% 
  summarize(n = n()) %>%
  arrange(desc(n))

# Explore injury descriptions: ------------------------------------------------
adr_deaths %>% 
  group_by(DescriptionofInjury) %>%
  summarize(n=n()) %>%
  arrange(desc(n))

tab = 
  adr_deaths %>% 
  mutate( Desc = str_to_lower(DescriptionofInjury) ) %>%
  group_by(Desc) %>%
  summarize(n=n()) %>%
  arrange(desc(n))
tab

# Descriptions with "med" in them: 
med_desc = {tab %>% filter( grepl('med', Desc) )}$Desc

# Split these descriptions into words and extract the matches
med_desc_words = str_split(med_desc, " ")

# Pull out the matching words with grep
sapply(med_desc_words, function(x){ x[grep('med', x)] } )

# Use str_subset instead
sapply(med_desc_words, str_subset, pattern = 'med')

# Split words again, but use a regex to also split on punctuation
med_desc_words = str_split(med_desc, "[ .,/]")
sapply(med_desc_words, str_subset, pattern = 'med')
sapply(med_desc_words, str_subset, pattern = 'med.')
table( sapply(med_desc_words, str_subset, pattern = 'med.') )

#
adr_deaths %<>% 
  mutate( Medication = str_detect( DescriptionofInjury, 'medic'))

# Exercise: find all instances of "DescriptionofInjury" that refer to
# ethanol or alcohol. How do these compare to the EtOH column?
#alc_desc = {tab %>% filter( grepl('ethan', Desc) | grepl('alc', Desc))}$Desc


# String replace and string replace all: --------------------------------------
adr_deaths %>%
  group_by(ImmediateCauseA) %>%
  summarize(n=n()) %>%
  arrange( desc(n))

# Replace all Intoxication with "Toxicity" 
adr_deaths %>%
  mutate(causeA = str_replace(ImmediateCauseA, 'Intoxication', 'Toxicity') ) %>%
  group_by(causeA) %>%
  summarize(n=n()) %>%
  arrange( desc(n) )


# Exercise: replace instances of "Acute A" in causeA with an empty string
