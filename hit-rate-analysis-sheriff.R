# Libraries
library(jsonlite)
library(tibble)
library(tidyr)
library(beepr)
library(dplyr)
library(purrr)
library(stringr)
library(stringi)
library(readr)
library(chron)
library(lubridate)
library(suncalc)

# Set working directory
setwd("~/Desktop/hit-rate-analysis-sheriff")

###################################################################
######################## IMPORT 1ST DF ############################
###################################################################

# Import
library(jsonlite)
test1 <- stream_in(file("JUL_-_DEC_2018.txt"))

# Flatten
test1 <- jsonlite::flatten(test1)

# Create df
library(tibble)
test1_tbl <- as_data_frame(test1)

library(tidyr)
# Unnesting person stopped
test1_tbl <- test1_tbl %>% 
  # take Person_Stopped out of its nested structure and unfurl it
  # Change the data from one line per stop to one line per person in a stop 
  unnest(cols = c(ListPerson_Stopped.Person_Stopped)) %>% 
  # flatten what came out of it
  jsonlite::flatten() %>% 
  # convert back to tibble
  as_tibble()
## Row count == 40,515 == the number of people stopped

# Unnesting Action Taken
## This creates additional rows for multiple actions taken
test1_tbl <- test1_tbl %>% 
  # take Action Taken out of its nested structure and unfurl it
  # Change the data from one line per stop to one line action in a stop 
  unnest(cols = c(ListActTak.ActTak)) %>% 
  # flatten what came out of it
  jsonlite::flatten() %>% 
  # convert back to tibble
  as_tibble()
## New row count == 66,574, due to additional rows for multiple actions taken

# Unnesting Results (optional)
# test1_tbl <- test1_tbl %>% 
  # take results out of its nested structure and unfurl it
  # Change the data from one line per stop to one line results in a stop 
#  unnest(cols = c(ListResult.Result)) %>% 
  # flatten what came out of it
#  jsonlite::flatten() %>% 
  # convert back to tibble
#  as_tibble()

# Create distinct ID for each stop_person
test1_tbl$id <- paste0(test1_tbl$LEARecordID, "_", test1_tbl$PID)

# Rearrange
test1_tbl <- test1_tbl %>%
  select(id, everything())

# There can be multiple actions taken listed for each person
## Aggregate rows based on the id and collapse bases into one cell
actions <- test1_tbl %>% 
  group_by(id) %>%
  summarise(actions_taken = paste(Act_CD, collapse = "|"))

# Keep only disctinct rows for each id in test1tbl to merge with jd182
test1_tbl <- test1_tbl[!duplicated(test1_tbl$id),]

# Left join
jd18 <- left_join(test1_tbl, actions, by = "id")

library(beepr)
beepr::beep()

# check ethnicity, which is still a list
library(dplyr)
jd18 %>% 
  select(Perc.ListEthn.Ethn) %>%
  unnest(c(Perc.ListEthn.Ethn)) %>% 
  count(Perc.ListEthn.Ethn, sort = TRUE)

library(purrr)
library(stringr)
# all single digit codes, but many people have more than one ethnicity
# smash ethnicities together, sort the numbers and return a character
jd18 = jd18 %>% 
  mutate(Perc.ListEthn.Ethn = map_chr(Perc.ListEthn.Ethn, ~str_c(sort(.x), collapse = '|')))

# smash susp_t together, sort the numbers and return a character
jd18 = jd18 %>% 
  mutate(PrimaryReason.ListSusp_T.Susp_T = map_chr(PrimaryReason.ListSusp_T.Susp_T, ~str_c(sort(.x), collapse = '|')))

# smash search basis together, sort the numbers and return a character
jd18 = jd18 %>% 
  mutate(ListBasSearch.BasSearch = map_chr(ListBasSearch.BasSearch, ~str_c(sort(.x), collapse = '|')))

# smash seize basis together, sort the numbers and return a character
jd18 = jd18 %>% 
  mutate(ListBasSeiz.BasSeiz = map_chr(ListBasSeiz.BasSeiz, ~str_c(sort(.x), collapse = '|')))

# smash prop type together, sort the numbers and return a character
jd18 = jd18 %>% 
  mutate(ListPropType.PropType = map_chr(ListPropType.PropType, ~str_c(sort(.x), collapse = '|')))

# smash cb type together, sort the numbers and return a character
jd18 = jd18 %>% 
  mutate(ListCB.Cb = map_chr(ListCB.Cb, ~str_c(sort(.x), collapse = '|')))

nrow(jd18) # this should be the number of people stopped in the data 
# 40,515
n_distinct(jd18$LEARecordID) # this should match the number of lines in original df
# 36,829

# Select columns for analysis
names(jd18)
jd18 <- jd18 %>% 
  select(id, LEARecordID, SDate, 
         STime, SDur, 
         Is_ServCall, Location.Loc, 
         Location.City, PID,
         Is_Stud, BasSearch_N,
         Perc.Age, Perc.Is_LimEng,
         Perc.Gend, Perc.GendNC, 
         Perc.LGBT, Perc.ListEthn.Ethn,
         Perc.ListDisb.Disb, PrimaryReason.StReas, 
         PrimaryReason.StReas_N, PrimaryReason.Susp_O_CD,
         PrimaryReason.Tr_ID, PrimaryReason.Tr_O_CD, 
         PrimaryReason.ListSusp_T.Susp_T, 
         ListBasSearch.BasSearch, ListBasSeiz.BasSeiz,
         ListPropType.PropType, ListCB.Cb, actions_taken)

# Remove originals
remove(test1, test1_tbl, actions)

###################################################################
######################## IMPORT 2ND DF ############################
###################################################################

# Import
test2 <- stream_in(file("JAN_-_JUN_2019.txt"))

# Flatten
test2 <- jsonlite::flatten(test2)

# Create df
test2_tbl <- as_data_frame(test2)

# Unnesting person stopped
test2_tbl <- test2_tbl %>% 
  # take Person_Stopped out of its nested structure and unfurl it
  # Change the data from one line per stop to one line per person in a stop 
  unnest(cols = c(ListPerson_Stopped.Person_Stopped)) %>% 
  # flatten what came out of it
  jsonlite::flatten() %>% 
  # convert back to tibble
  as_tibble()
## Row count == 32,943 == the number of people stopped

# Unnesting Action Taken
## This creates additional rows for multiple actions taken
 test2_tbl <- test2_tbl %>% 
# take Action Taken out of its nested structure and unfurl it
# Change the data from one line per stop to one line action in a stop 
 unnest(cols = c(ListActTak.ActTak)) %>% 
# flatten what came out of it
  jsonlite::flatten() %>% 
# convert back to tibble
  as_tibble()
## New row count == 55,718

# Unnesting Results (optional)
# test2_tbl <- test2_tbl %>% 
# take results out of its nested structure and unfurl it
# Change the data from one line per stop to one line results in a stop 
#  unnest(cols = c(ListResult.Result)) %>% 
# flatten what came out of it
#  jsonlite::flatten() %>% 
# convert back to tibble
#  as_tibble()

# Create distinct ID for each stop_person
test2_tbl$id <- paste0(test2_tbl$LEARecordID, "_", test2_tbl$PID)
 
# Rearrange
test2_tbl <- test2_tbl %>%
  select(id, everything())
 
# There can be multiple actions taken listed for each person
## Aggregate rows based on the id and collapse bases into one cell
actions <- test2_tbl %>% 
  group_by(id) %>%
  summarise(actions_taken = paste(Act_CD, collapse = "|"))

# Keep only disctinct rows for each id in test1tbl to merge with jd182
test2_tbl <- test2_tbl[!duplicated(test2_tbl$id),]

# Left join
jj19 <- left_join(test2_tbl, actions, by = "id")

library(beepr)
beepr::beep()

# check ethnicity, which is still a list
jj19 %>% 
  select(Perc.ListEthn.Ethn) %>%
  unnest(c(Perc.ListEthn.Ethn)) %>% 
  count(Perc.ListEthn.Ethn, sort = TRUE)

# all single digit codes, but many people have more than one ethnicity
# smash ethnicities together, sort the numbers and return a character
jj19 = jj19 %>% 
  mutate(Perc.ListEthn.Ethn = map_chr(Perc.ListEthn.Ethn, ~str_c(sort(.x), collapse = '|')))

# smash susp_t together, sort the numbers and return a character
jj19 = jj19 %>% 
  mutate(PrimaryReason.ListSusp_T.Susp_T = map_chr(PrimaryReason.ListSusp_T.Susp_T, ~str_c(sort(.x), collapse = '|')))

# smash search basis together, sort the numbers and return a character
jj19 = jj19 %>% 
  mutate(ListBasSearch.BasSearch = map_chr(ListBasSearch.BasSearch, ~str_c(sort(.x), collapse = '|')))

# smash seize basis together, sort the numbers and return a character
jj19 = jj19 %>% 
  mutate(ListBasSeiz.BasSeiz = map_chr(ListBasSeiz.BasSeiz, ~str_c(sort(.x), collapse = '|')))

# smash prop type together, sort the numbers and return a character
jj19 = jj19 %>% 
  mutate(ListPropType.PropType = map_chr(ListPropType.PropType, ~str_c(sort(.x), collapse = '|')))

# smash cb type together, sort the numbers and return a character
jj19 = jj19 %>% 
  mutate(ListCB.Cb = map_chr(ListCB.Cb, ~str_c(sort(.x), collapse = '|')))

nrow(jj19) # this should be the number of people stopped in the data 
# 32,943
n_distinct(jj19$LEARecordID) # this should match the number of lines in original df
# 30,234

# Select columns we need for analysis
jj19 <- jj19 %>% 
  select(id, LEARecordID, SDate, 
         STime, SDur, 
         Is_ServCall, Location.Loc, 
         Location.City, PID,
         Is_Stud, BasSearch_N,
         Perc.Age, Perc.Is_LimEng,
         Perc.Gend, Perc.GendNC, 
         Perc.LGBT, Perc.ListEthn.Ethn,
         Perc.ListDisb.Disb, PrimaryReason.StReas, 
         PrimaryReason.StReas_N, PrimaryReason.Susp_O_CD,
         PrimaryReason.Tr_ID, PrimaryReason.Tr_O_CD, 
         PrimaryReason.ListSusp_T.Susp_T, 
         ListBasSearch.BasSearch, ListBasSeiz.BasSeiz,
         ListPropType.PropType, ListCB.Cb, actions_taken)

# Remove originals
remove(test2, test2_tbl, actions)

###################################################################
######################## IMPORT 3RD DF ############################
###################################################################

# Import
test3 <- stream_in(file("JUL-DEC_2019.txt"))

# Flatten
test3 <- jsonlite::flatten(test3)

# Create df
test3_tbl <- as_data_frame(test3)

test3_tbl <- test3_tbl %>% 
  # take Person_Stopped out of its nested structure and unfurl it
  # Change the data from one line per stop to one line per person in a stop 
  unnest(cols = c(ListPerson_Stopped.Person_Stopped)) %>% 
  # flatten what came out of it
  jsonlite::flatten() %>% 
  # convert back to tibble
  as_tibble()
## Row count == 33,405 == the number of people stopped

# Unnesting Action Taken
## This creates additional rows for multiple actions taken
test3_tbl <- test3_tbl %>% 
# take Action Taken out of its nested structure and unfurl it
# Change the data from one line per stop to one line action in a stop 
  unnest(cols = c(ListActTak.ActTak)) %>% 
# flatten what came out of it
  jsonlite::flatten() %>% 
# convert back to tibble
  as_tibble()
## New row count == 53,239, due to additional rows for multiple actions taken

# Unnesting Results (optional)
# test3_tbl <- test3_tbl %>% 
# take results out of its nested structure and unfurl it
# Change the data from one line per stop to one line results in a stop 
#  unnest(cols = c(ListResult.Result)) %>% 
# flatten what came out of it
#  jsonlite::flatten() %>% 
# convert back to tibble
#  as_tibble()

# Create distinct ID for each stop_person
test3_tbl$id <- paste0(test3_tbl$LEARecordID, "_", test3_tbl$PID)

# Rearrange
test3_tbl <- test3_tbl %>%
  select(id, everything())

# There can be multiple actions taken listed for each person
## Aggregate rows based on the id and collapse bases into one cell
actions <- test3_tbl %>% 
  group_by(id) %>%
  summarise(actions_taken = paste(Act_CD, collapse = "|"))

# Keep only disctinct rows for each id in test1tbl to merge with jd182
test3_tbl <- test3_tbl[!duplicated(test3_tbl$id),]

# Left join
jd19 <- left_join(test3_tbl, actions, by = "id")

library(beepr)
beepr::beep()

# check ethnicity, which is still a list
jd19 %>% 
  select(Perc.ListEthn.Ethn) %>%
  unnest(c(Perc.ListEthn.Ethn)) %>% 
  count(Perc.ListEthn.Ethn, sort = TRUE)

# all single digit codes, but many people have more than one ethnicity
# smash ethnicities together, sort the numbers and return a character
jd19 = jd19 %>% 
  mutate(Perc.ListEthn.Ethn = map_chr(Perc.ListEthn.Ethn, ~str_c(sort(.x), collapse = '|')))

# smash susp_t together, sort the numbers and return a character
jd19 = jd19 %>% 
  mutate(PrimaryReason.ListSusp_T.Susp_T = map_chr(PrimaryReason.ListSusp_T.Susp_T, ~str_c(sort(.x), collapse = '|')))

# smash search basis together, sort the numbers and return a character
jd19 = jd19 %>% 
  mutate(ListBasSearch.BasSearch = map_chr(ListBasSearch.BasSearch, ~str_c(sort(.x), collapse = '|')))

# smash seize basis together, sort the numbers and return a character
jd19 = jd19 %>% 
  mutate(ListBasSeiz.BasSeiz = map_chr(ListBasSeiz.BasSeiz, ~str_c(sort(.x), collapse = '|')))

# smash prop type together, sort the numbers and return a character
jd19 = jd19 %>% 
  mutate(ListPropType.PropType = map_chr(ListPropType.PropType, ~str_c(sort(.x), collapse = '|')))

# smash cb type together, sort the numbers and return a character
jd19 = jd19 %>% 
  mutate(ListCB.Cb = map_chr(ListCB.Cb, ~str_c(sort(.x), collapse = '|')))

nrow(jd19) # this should be the number of people stopped in the data 
# 33,405
n_distinct(jd19$LEARecordID) # this should match the number of lines in original df
# 31,020

# Select columns we need for analysis
jd19 <- jd19 %>% 
  select(id, LEARecordID, SDate, 
         STime, SDur, 
         Is_ServCall, Location.Loc, 
         Location.City, PID,
         Is_Stud, BasSearch_N,
         Perc.Age, Perc.Is_LimEng,
         Perc.Gend, Perc.GendNC, 
         Perc.LGBT, Perc.ListEthn.Ethn,
         Perc.ListDisb.Disb, PrimaryReason.StReas, 
         PrimaryReason.StReas_N, PrimaryReason.Susp_O_CD,
         PrimaryReason.Tr_ID, PrimaryReason.Tr_O_CD, 
         PrimaryReason.ListSusp_T.Susp_T, 
         ListBasSearch.BasSearch, ListBasSeiz.BasSeiz,
         ListPropType.PropType, ListCB.Cb, actions_taken)

# Remove originals
remove(test3, test3_tbl, actions)

###################################################################
######################## IMPORT 4TH DF ############################
###################################################################

# Import
test4 <- stream_in(file("JAN_-_JUN_2020.txt"))

# Flatten
test4 <- jsonlite::flatten(test4)

# Create df
test4_tbl <- as_data_frame(test4)

test4_tbl <- test4_tbl %>% 
  # take Person_Stopped out of its nested structure and unfurl it
  # Change the data from one line per stop to one line per person in a stop 
  unnest(cols = c(ListPerson_Stopped.Person_Stopped)) %>% 
  # flatten what came out of it
  jsonlite::flatten() %>% 
  # convert back to tibble
  as_tibble()
## Row count == 21,506 == the number of people stopped

# Unnesting Action Taken
## This creates additional rows for multiple actions taken
test4_tbl <- test4_tbl %>% 
# take Action Taken out of its nested structure and unfurl it
# Change the data from one line per stop to one line action in a stop 
  unnest(cols = c(ListActTak.ActTak)) %>% 
# flatten what came out of it
  jsonlite::flatten() %>% 
# convert back to tibble
  as_tibble()
## New row count == 33,338, due to additional rows for multiple actions taken

# Unnesting Results (optional)
# test4_tbl <- test4_tbl %>% 
# take results out of its nested structure and unfurl it
# Change the data from one line per stop to one line results in a stop 
#  unnest(cols = c(ListResult.Result)) %>% 
# flatten what came out of it
#  jsonlite::flatten() %>% 
# convert back to tibble
#  as_tibble()

# Create distinct ID for each stop_person
test4_tbl$id <- paste0(test4_tbl$LEARecordID, "_", test4_tbl$PID)

# Rearrange
test4_tbl <- test4_tbl %>%
  select(id, everything())

# There can be multiple actions taken listed for each person
## Aggregate rows based on the id and collapse bases into one cell
actions <- test4_tbl %>% 
  group_by(id) %>%
  summarise(actions_taken = paste(Act_CD, collapse = "|"))

# Keep only disctinct rows for each id in test1tbl to merge with jd182
test4_tbl <- test4_tbl[!duplicated(test4_tbl$id),]

# Left join
jj20 <- left_join(test4_tbl, actions, by = "id")

library(beepr)
beepr::beep()

# check ethnicity, which is still a list
jj20 %>% 
  select(Perc.ListEthn.Ethn) %>%
  unnest(c(Perc.ListEthn.Ethn)) %>% 
  count(Perc.ListEthn.Ethn, sort = TRUE)

# all single digit codes, but many people have more than one ethnicity
# smash ethnicities together, sort the numbers and return a character
jj20 = jj20 %>% 
  mutate(Perc.ListEthn.Ethn = map_chr(Perc.ListEthn.Ethn, ~str_c(sort(.x), collapse = '|')))

# smash susp_t together, sort the numbers and return a character
jj20 = jj20 %>% 
  mutate(PrimaryReason.ListSusp_T.Susp_T = map_chr(PrimaryReason.ListSusp_T.Susp_T, ~str_c(sort(.x), collapse = '|')))

# smash search basis together, sort the numbers and return a character
jj20 = jj20 %>% 
  mutate(ListBasSearch.BasSearch = map_chr(ListBasSearch.BasSearch, ~str_c(sort(.x), collapse = '|')))

# smash seize basis together, sort the numbers and return a character
jj20 = jj20 %>% 
  mutate(ListBasSeiz.BasSeiz = map_chr(ListBasSeiz.BasSeiz, ~str_c(sort(.x), collapse = '|')))

# smash prop type together, sort the numbers and return a character
jj20 = jj20 %>% 
  mutate(ListPropType.PropType = map_chr(ListPropType.PropType, ~str_c(sort(.x), collapse = '|')))

# smash cb type together, sort the numbers and return a character
jj20 = jj20 %>% 
  mutate(ListCB.Cb = map_chr(ListCB.Cb, ~str_c(sort(.x), collapse = '|')))

nrow(jj20) # this should be the number of people stopped in the data 
# 21,506
n_distinct(jj20$LEARecordID) # this should match the number of lines in original df
# 19,573

# Select columns we need for analysis
jj20 <- jj20 %>% 
  select(id, LEARecordID, SDate, 
         STime, SDur, 
         Is_ServCall, Location.Loc, 
         Location.City, PID,
         Is_Stud, BasSearch_N,
         Perc.Age, Perc.Is_LimEng,
         Perc.Gend, Perc.GendNC, 
         Perc.LGBT, Perc.ListEthn.Ethn,
         Perc.ListDisb.Disb, PrimaryReason.StReas, 
         PrimaryReason.StReas_N, PrimaryReason.Susp_O_CD,
         PrimaryReason.Tr_ID, PrimaryReason.Tr_O_CD, 
         PrimaryReason.ListSusp_T.Susp_T, 
         ListBasSearch.BasSearch, ListBasSeiz.BasSeiz,
         ListPropType.PropType, ListCB.Cb, actions_taken)

# Remove originals
remove(test4, test4_tbl, actions)

###################################################################
######################## CLEANING #################################
###################################################################

# Bind dfs together
master <- rbind(jd18, jj19, jd19, jj20)

# smash disability type together, sort the numbers and return a character
master = master %>% 
  mutate(Perc.ListDisb.Disb = map_chr(Perc.ListDisb.Disb, ~str_c(sort(.x), collapse = '|')))

# smash susp offense code together, sort the numbers and return a character
master = master %>% 
  mutate(PrimaryReason.Susp_O_CD = map_chr(PrimaryReason.Susp_O_CD, ~str_c(sort(.x), collapse = '|')))

# Rename columns
names(master)
names(master) <- c("id", "stop_id", "date", "time", "dur", "serv_call",
                   "location", "city", "pid", "stud", "search_bas_desc",
                   "age", "lim_eng", "gender", "gendernc",
                   "lgbt", "eth", "disb", "reasonid", "reason_desc", 
                   "susp_code", "tr_id", "tr_o", "susp_type", "search_basis",
                   "seiz_basis", "prop_seized", "cont_evid", "actions")

# Remove leading and trailing whitespace
master <- master %>% 
  mutate_if(is.character, str_trim)

# Remove all types of whitespace inside strings
master <- master %>% 
  mutate_if(is.character, str_squish)

# Remove floating commas
master$location <- gsub(" , ", ", ", master$location)
master$reason_desc <- gsub(" , ", ", ", master$reason_desc)

# Convert date into date format
master$date <- as.Date(master$date, "%m/%d/%Y")

# Time doesn't always have seconds / ending ":00")
## Count number of characters in string to insert :00
library(stringi)
master$count <- nchar(stri_escape_unicode(master$time))

# Paste ":00" at the end of times that have 5 characters in count column
master$time <- ifelse(master$count == 5, paste0(master$time, ":00"), master$time)

# Remove count column
master <- master %>% 
  select(-count)

# Create second time column that's in time format
library(chron)
master$time2 <- times(master$time)

###################################################################
####################### CLEAN RACE ################################
###################################################################

# Create new column for race, written out
master <- master %>% 
  mutate(race_words = str_replace_all(eth, "7", "white") %>% 
           str_replace_all("6", "pi") %>% 
           str_replace_all("5", "nam") %>% 
           str_replace_all("4", "me_sa") %>% 
           str_replace_all("3", "hisp") %>% 
           str_replace_all("2", "black") %>% 
           str_replace_all("1", "asian"))

# Create new race category for vod test
## Hispanic + other race == "hisp"
## More than one race (but not hisp) == "mixed"
master <- master %>% 
  mutate(race_condensed = case_when(str_detect(race_words, "hisp") ~ "hisp", # if contains "hisp", add to hispanic category
                                    str_detect(race_words, "\\|") ~ "mixed", # if contains "|", create mixed category
                                    TRUE ~ race_words)) # if neither above is true, paste original from race_words

# Remove race_words column
master <- master %>% 
  select(-race_words)

# Create race column descriptions
master$asian <- ifelse(grepl("asian", master$race_condensed), 1, 0)
master$black <- ifelse(grepl("black", master$race_condensed), 1, 0)
master$hisp <- ifelse(grepl("hisp", master$race_condensed), 1, 0)
master$me_sa <- ifelse(grepl("me_sa", master$race_condensed), 1, 0)
master$mixed <- ifelse(grepl("mixed", master$race_condensed), 1, 0)
master$nam <- ifelse(grepl("nam", master$race_condensed), 1, 0)
master$pi <- ifelse(grepl("pi", master$race_condensed), 1, 0)
master$white <- ifelse(grepl("white", master$race_condensed), 1, 0)

###################################################################
###################### CLEAN SUSP TYPE ############################
###################################################################

# Create new column for susp type, written out
master <- master %>% 
  mutate(susp_type_words = str_replace_all(susp_type, "1", "susp_comm_crime") %>% 
           str_replace_all("2", "susp_match") %>% 
           str_replace_all("3", "susp_wit_scene") %>% 
           str_replace_all("4", "susp_obj") %>% 
           str_replace_all("5", "susp_casing") %>% 
           str_replace_all("6", "susp_lookout") %>% 
           str_replace_all("7", "susp_drug_trans") %>% 
           str_replace_all("8", "susp_vc") %>% 
           str_replace_all("9", "susp_other"))

# Create columns for susp type descriptions
master$susp_comm_crime <- ifelse(grepl("1", master$susp_type), 1, 0)
master$susp_match <- ifelse(grepl("2", master$susp_type), 1, 0)
master$susp_wit_scene <- ifelse(grepl("3", master$susp_type), 1, 0)
master$susp_obj <- ifelse(grepl("4", master$susp_type), 1, 0)
master$susp_casing <- ifelse(grepl("5", master$susp_type), 1, 0)
master$susp_lookout <- ifelse(grepl("6", master$susp_type), 1, 0)
master$susp_drug_trans <- ifelse(grepl("7", master$susp_type), 1, 0)
master$susp_vc <- ifelse(grepl("8", master$susp_type), 1, 0)
master$susp_other <- ifelse(grepl("9", master$susp_type), 1, 0)

###################################################################
###################### CLEAN SEARCH BASIS #########################
###################################################################

# Change blanks in search_basis to NA
master$search_basis[master$search_basis == ""] = NA

# Create new column for search basis type, written out
master <- master %>% 
  mutate(search_basis_words = str_replace_all(search_basis, "13", "sch_school") %>% 
           str_replace_all("12", "sch_inventory") %>%
           str_replace_all("11", "sch_emerg") %>%
           str_replace_all("10", "sch_arrest") %>%
           str_replace_all("1", "sch_consent") %>%
           str_replace_all("2", "sch_safety") %>% 
           str_replace_all("3", "sch_warrant") %>% 
           str_replace_all("4", "sch_parole") %>% 
           str_replace_all("5", "sch_susp_weapons") %>% 
           str_replace_all("6", "sch_vis_cont") %>% 
           str_replace_all("7", "sch_od_cont") %>% 
           str_replace_all("8", "sch_k9") %>% 
           str_replace_all("9", "sch_crime"))

# Create columns for search basis descriptions
master$sch_consent <- ifelse(grepl("sch_consent", master$search_basis_words), 1, 0)
master$sch_safety <- ifelse(grepl("sch_safety", master$search_basis_words), 1, 0)
master$sch_warrant <- ifelse(grepl("sch_warrant", master$search_basis_words), 1, 0)
master$sch_parole <- ifelse(grepl("sch_parole", master$search_basis_words), 1, 0)
master$sch_susp_weapons <- ifelse(grepl("sch_susp_weapons", master$search_basis_words), 1, 0)
master$sch_vis_cont <- ifelse(grepl("sch_vis_cont", master$search_basis_words), 1, 0)
master$sch_od_cont <- ifelse(grepl("sch_od_cont", master$search_basis_words), 1, 0)
master$sch_k9 <- ifelse(grepl("sch_k9", master$search_basis_words), 1, 0)
master$sch_crime <- ifelse(grepl("sch_crime", master$search_basis_words), 1, 0)
master$sch_arrest <- ifelse(grepl("sch_arrest", master$search_basis_words), 1, 0)
master$sch_emerg <- ifelse(grepl("sch_emerg", master$search_basis_words), 1, 0)
master$sch_inventory <- ifelse(grepl("sch_inventory", master$search_basis_words), 1, 0)
master$sch_school <- ifelse(grepl("sch_school", master$search_basis_words), 1, 0)

###################################################################
######################## DISCRETION ###############################
###################################################################

# RIPA categorized search basis into higher discretion and lower discretion
## Experts suggested (in interviews) that the U-T categorize sheriff's bases 
## into "discretionary" and "non-discretionary"

# Create discretionary column
## Since there can be more than one "search basis", 
## create check if any of these reasons were selected
master$discretionary <- ifelse(master$sch_consent == 1, 1,
                              ifelse(master$sch_safety == 1, 1,
                                     ifelse(master$sch_parole == 1, 1,
                                      ifelse(master$sch_susp_weapons == 1, 1,
                                             ifelse(master$sch_vis_cont == 1, 1,
                                                    ifelse(master$sch_od_cont == 1, 1,
                                                           ifelse(master$sch_k9 == 1, 1,
                                                                  ifelse(master$sch_crime == 1, 1,
                                                                         ifelse(master$sch_emerg == 1, 1,
                                                                                ifelse(master$sch_school == 1, 1, 0))))))))))

# Create non_discretionary column
## Since there can be more than one "search basis",
## create check if any of these reasons were selected
master$non_discretionary <- ifelse(master$sch_warrant == 1, 1,
                                      ifelse(master$sch_arrest == 1, 1,
                                             ifelse(master$sch_inventory == 1, 1, 0)))

# There are some that are both discretionary and non_discretionary, due to multiple search bases
## Remove 1's from discretionary if there's also a non_discretionary reason for the search
### Since non_discretionary searches will always take place, regardless of other circumstances
master$discretionary <- ifelse(master$discretionary == 1 & master$non_discretionary == 1, 0, 
                                   master$discretionary)

# Create final is_discretion column for glm tests
master$is_discretion <- ifelse(master$discretionary == 1, TRUE, FALSE)

###################################################################
###################### CLEAN CONTRABAND ###########################
###################################################################

# Change blanks in contraband to NA
master$cont_evid[master$cont_evid == ""] = NA

# Create new column for contraband type found during search, written out
master <- master %>% 
  mutate(cont_evid_words = str_replace_all(cont_evid, "11", "cont_other") %>% 
           str_replace_all("10", "cont_cell") %>%
           str_replace_all("9", "cont_stolen_prop") %>%
           str_replace_all("8", "cont_para") %>%
           str_replace_all("7", "cont_money") %>%
           str_replace_all("6", "cont_alcohol") %>% 
           str_replace_all("5", "cont_drugs") %>% 
           str_replace_all("4", "cont_weapons") %>% 
           str_replace_all("3", "cont_ammu") %>% 
           str_replace_all("2", "cont_firearm") %>% 
           str_replace_all("1", "cont_none"))

# Create columns for contraband descriptions
master$cont_other <- ifelse(grepl("cont_other", master$cont_evid_words), 1, 0)
master$cont_cell <- ifelse(grepl("cont_cell", master$cont_evid_words), 1, 0)
master$cont_stolen_prop <- ifelse(grepl("cont_stolen_prop", master$cont_evid_words), 1, 0)
master$cont_para <- ifelse(grepl("cont_para", master$cont_evid_words), 1, 0)
master$cont_money <- ifelse(grepl("cont_money", master$cont_evid_words), 1, 0)
master$cont_alcohol <- ifelse(grepl("cont_alcohol", master$cont_evid_words), 1, 0)
master$cont_drugs <- ifelse(grepl("cont_drugs", master$cont_evid_words), 1, 0)
master$cont_weapons <- ifelse(grepl("cont_weapons", master$cont_evid_words), 1, 0)
master$cont_ammu <- ifelse(grepl("cont_ammu", master$cont_evid_words), 1, 0)
master$cont_firearm <- ifelse(grepl("cont_firearm", master$cont_evid_words), 1, 0)
master$cont_none <- ifelse(grepl("cont_none", master$cont_evid_words), 1, 0)

# Create column that aggregates drugs, weapons and ammunition
master$drugs_weapons <- ifelse(master$cont_drugs == 1, 1, 
                               ifelse(master$cont_weapons == 1, 1,
                                      ifelse(master$cont_firearm == 1, 1,
                                             ifelse(master$cont_ammu == 1, 1, 0))))

###################################################################
######################## CLEAN REASON #############################
###################################################################

# Create new column for reason of stop, written out
## Only one reason ID is listed in this column, no multiple entries
master <- master %>% 
  mutate(reason_words = str_replace_all(reasonid, "8", "rs_school") %>% 
           str_replace_all("7", "rs_ed") %>%
           str_replace_all("6", "rs_consent") %>%
           str_replace_all("5", "rs_truant") %>%
           str_replace_all("4", "rs_warrant") %>%
           str_replace_all("3", "rs_parole") %>% 
           str_replace_all("2", "rs_susp") %>% 
           str_replace_all("1", "rs_traff"))

# Create columns for search basis descriptions
master$rs_school <- ifelse(grepl("rs_school", master$reason_words), 1, 0)
master$rs_ed <- ifelse(grepl("rs_ed", master$reason_words), 1, 0)
master$rs_consent <- ifelse(grepl("rs_consent", master$reason_words), 1, 0)
master$rs_truant <- ifelse(grepl("rs_truant", master$reason_words), 1, 0)
master$rs_warrant <- ifelse(grepl("rs_warrant", master$reason_words), 1, 0)
master$rs_parole <- ifelse(grepl("rs_parole", master$reason_words), 1, 0)
master$rs_susp <- ifelse(grepl("rs_susp", master$reason_words), 1, 0)
master$rs_traff <- ifelse(grepl("rs_traff", master$reason_words), 1, 0)

# Create final "grouped" reason id column
master$reason_condensed <- ifelse(master$reasonid == 1, "TRAFFIC",
                                  ifelse(master$reasonid == 2, "SUSP",
                                         ifelse(master$reasonid == 3 | 4 | 5 | 6 | 7, "OTHER", "CHECK")))

###################################################################
######################## CLEAN ACTIONS ############################
###################################################################

# Create new column for actions, written out
master <- master %>% 
  mutate(act_words = str_replace_all(actions, "24", "act_none") %>% 
           str_replace_all("23", "act_student") %>%
           str_replace_all("22", "act_vi") %>%
           str_replace_all("21", "act_prop_seiz") %>%
           str_replace_all("20", "act_sch_prop") %>%
           str_replace_all("19", "act_req_sch_prop") %>% 
           str_replace_all("18", "act_sch_pers") %>% 
           str_replace_all("17", "act_req_sch_pers") %>% 
           str_replace_all("16", "act_photo") %>%
           str_replace_all("15", "act_physical") %>%
           str_replace_all("14", "act_chem") %>%
           str_replace_all("13", "act_baton") %>%
           str_replace_all("12", "act_k9_bit") %>%
           str_replace_all("11", "act_ip") %>%
           str_replace_all("10", "act_elect") %>%
           str_replace_all("9", "act_fad") %>%
           str_replace_all("8", "act_fp") %>%
           str_replace_all("7", "act_k9_rem") %>%
           str_replace_all("6", "act_car_det") %>%
           str_replace_all("5", "act_hc") %>%
           str_replace_all("4", "act_curb") %>%
           str_replace_all("3", "act_sober") %>%
           str_replace_all("2", "act_rem_cont") %>%
           str_replace_all("1", "act_rem_order"))

# Create separate column for action detained category
master$act_detained <- ifelse(grepl("act_car_det", master$act_words), 1, 
                              ifelse(grepl("act_hc", master$act_words), 1,
                                     ifelse(grepl("act_curb", master$act_words), 1, 0)))

# Create separate column for action force category
master$act_force <- ifelse(grepl("act_chem", master$act_words), 1, 
                              ifelse(grepl("act_baton", master$act_words), 1,
                                     ifelse(grepl("act_k9_bit", master$act_words), 1,
                                            ifelse(grepl("act_ip", master$act_words), 1, 
                                                   ifelse(grepl("act_elect", master$act_words), 1,
                                                          ifelse(grepl("act_fad", master$act_words), 1,
                                                                 ifelse(grepl("act_fp", master$act_words), 1,
                                                                        ifelse(grepl("act_rem_cont", master$act_words), 1,
                                                                               ifelse(grepl("act_physical", master$act_words), 1, 0)))))))))

# Create separate column for action none category
master$act_none <- ifelse(grepl("act_none", master$act_words), 1, 0)

# Create separate column for action other category
master$act_other <- ifelse(grepl("act_k9_rem", master$act_words), 1, 
                           ifelse(grepl("act_stud", master$act_words), 1,
                                  ifelse(grepl("act_photo", master$act_words), 1,
                                                ifelse(grepl("act_sober", master$act_words), 1,
                                                       ifelse(grepl("act_rem_order", master$act_words), 1, 0)))))

# Create separate column for action search requested category
master$act_req_search <- ifelse(grepl("act_req_sch_prop", master$act_words), 1, 
                           ifelse(grepl("act_req_sch_pers", master$act_words), 1, 0))

# Create separate column for action search conducted category
master$act_search <- ifelse(grepl("act_sch_prop", master$act_words), 1, 
                                ifelse(grepl("act_sch_pers", master$act_words), 1, 0))

# Create separate column for action seize category
master$act_seize <- ifelse(grepl("act_vi", master$act_words), 1, 
                                ifelse(grepl("act_prop_seize", master$act_words), 1, 0))

###################################################################
########################## ANALYSIS ###############################
###################################################################

# How many stops total?
n_distinct(master$stop_id)
# 117,656

# How many stops resulted in searches?
master %>% 
  filter(!is.na(master$search_basis)) %>%
  group_by(stop_id) %>% 
  summarise(total = n())
# 21,259
## This means 18.068 percent of stops resulted in a search of a person or property

# Create is_searched column check
master$is_searched <- ifelse(!is.na(master$search_basis),1,0)

# Create table of just searches
searches <- master %>% 
  filter(is_searched == 1)
## Row count == 24,073 people were searched in the 21,259 stops

###################################################################

# How many people were stopped because it was officer-initiated?
master %>% 
  count(serv_call)

# serv_call  count
# N         118222
# Y          10147
# TOTAL     128,369

# How many stops were officer-initiated?
master %>% 
  filter(master$serv_call == "N") %>%
  group_by(stop_id) %>% 
  summarise(total = n())
# 108,930 were not service calls out of 117,656 total stops
## 92.5 percent of stops were initiated by an officer, 
## or were not in response to a call for service, radio call or dispatch. 

###################################################################

# Calculate proportion of race of all people stopped
master %>% 
  count(race_condensed) %>% 
  arrange(desc(n))

# race_condensed  count
# white          68020
# hisp           39234
# black          10165
# asian           4690
# me_sa           3891
# pi              1220
# nam              887
# mixed            262

# Calculate percentages
master %>% 
  count(race_condensed) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# race_condensed  count                 percent
# white          68020                    53.0  
# hisp           39234                    30.6  
# black          10165                     7.92 
# asian           4690                     3.65 
# me_sa           3891                     3.03 
# pi              1220                     0.950
# nam              887                     0.691
# mixed            262                     0.204

###################################################################
# Reason for stop analysis

# Calculate proportion of race for each reason for stop
reasons <- master %>% 
  group_by(reason_condensed) %>% 
  count(race_condensed)

# Spread
reasons <- reasons %>%
  spread(key = reason_condensed, value = n, fill = 0)

# Add percentages
reasons %>%
  mutate(per_of_other = round((OTHER / sum(OTHER))*100,1),
         per_of_susp = round((SUSP / sum(SUSP))*100,1),
         per_of_traffic = round((TRAFFIC / sum(TRAFFIC))*100,1)) %>% 
  arrange(desc(per_of_susp))

# race_condensed OTHER  SUSP TRAFFIC per_of_other per_of_susp per_of_traffic
# white           6754 18108   43158         55.4        54.3           52.1
# hisp            3836  9614   25784         31.5        28.8           31.1
# black            959  3948    5258          7.9        11.8            6.3
# asian            201   615    3874          1.7         1.8            4.7
# me_sa            105   537    3249          0.9         1.6            3.9
# nam              192   225     470          1.6         0.7            0.6
# pi               113   231     876          0.9         0.7            1.1
# mixed             21    84     157          0.2         0.3            0.2
# TOTAL          12181 33362   82826

# Add percentages by race, as opposed to by the total
reasons %>% 
  mutate(other_per = round((OTHER / (OTHER + SUSP + TRAFFIC))*100,1),
         susp_per = round((SUSP / (OTHER + SUSP + TRAFFIC))*100,1),
         traffic_per = round((TRAFFIC / (OTHER + SUSP + TRAFFIC))*100,1),
         total_race = OTHER + SUSP + TRAFFIC) %>%
  arrange(desc(susp_per))

# race_condensed OTHER  SUSP TRAFFIC other_per susp_per traffic_per total_race
# black            959  3948    5258       9.4     38.8        51.7      10165
# mixed             21    84     157       8       32.1        59.9        262
# white           6754 18108   43158       9.9     26.6        63.4      68020
# nam              192   225     470      21.6     25.4        53          887
# hisp            3836  9614   25784       9.8     24.5        65.7      39234
# pi               113   231     876       9.3     18.9        71.8       1220
# me_sa            105   537    3249       2.7     13.8        83.5       3891
# asian            201   615    3874       4.3     13.1        82.6       4690

###################################################################
# Pedestrian vs Traffic Stops

# Change any blanks to NA in tr_id
master$tr_id[master$tr_id == ""] = NA

# Create is_traffic stop column
master$is_traffic <- ifelse(!is.na(master$tr_id),1,0)

sum(master$is_traffic)
# 82,826 were involved in traffic stops

# Calculate proportion of race stopped in traffic vs ped stops
trVped <- master %>% 
     group_by(is_traffic) %>% 
     count(race_condensed)

# Spread
trVped <- trVped %>%
  spread(key = is_traffic, value = n, fill = 0)

# Rename columns
names(trVped) <- c("race_condensed", "pedestrian", "traffic")

# Add percentages
trVped %>%
  mutate(per_of_pedestrian = round((pedestrian / sum(pedestrian))*100,1),
         per_of_traffic = round((traffic / sum(traffic))*100,1)) %>% 
  arrange(desc(per_of_pedestrian))

# race_condensed pedestrian traffic per_of_pedestrian per_of_traffic
# white               24862   43158              54.6           52.1
# hisp                13450   25784              29.5           31.1
# black                4907    5258              10.8            6.3
# asian                 816    3874               1.8            4.7
# me_sa                 642    3249               1.4            3.9
# nam                   417     470               0.9            0.6
# pi                    344     876               0.8            1.1
# mixed                 105     157               0.2            0.2
# TOTAL               45543   82826

# Add percentages by race, as opposed to by the total
trVped %>%
  mutate(pedestrian_per = round((pedestrian / (pedestrian + traffic))*100,1),
         traffic_per = round((traffic / (pedestrian + traffic))*100,1),
         total_race = pedestrian + traffic) %>% 
  arrange(desc(pedestrian_per))

# race_condensed pedestrian traffic pedestrian_per traffic_per total_race
# black                4907    5258           48.3        51.7      10165
# nam                   417     470           47          53          887
# mixed                 105     157           40.1        59.9        262
# white               24862   43158           36.6        63.4      68020
# hisp                13450   25784           34.3        65.7      39234
# pi                    344     876           28.2        71.8       1220
# asian                 816    3874           17.4        82.6       4690
# me_sa                 642    3249           16.5        83.5       3891

###################################################################
# How many people were searched?
sum(!is.na(master$search_basis))
# 24,073
## This means officers searched 24,073 people out of 128,369 (row count of master)
## They searched about 18.75 percent of people

###################################################################
# Searched vs Not Searched

# Calculate proportion of race searched and not searched
search_race <- master %>% 
  group_by(is_searched) %>% 
  count(race_condensed)

# Spread
search_race <- search_race %>%
  spread(key = is_searched, value = n, fill = 0)

# Rename columns
names(search_race) <- c("race_condensed", "no_search", "searched")

# Add percentages
search_race %>%
  mutate(per_of_no_search = round((no_search / sum(no_search))*100,1),
         per_of_searched = round((searched / sum(searched))*100,1)) %>%
  arrange(desc(per_of_searched))

# race_condensed no_search searched per_of_no_search per_of_searched
# white              55137    12883             52.9            53.5
# hisp               31546     7688             30.2            31.9
# black               7819     2346              7.5             9.7
# asian               4344      346              4.2             1.4
# nam                  607      280              0.6             1.2
# me_sa               3629      262              3.5             1.1
# pi                  1011      209              1               0.9
# mixed                203       59              0.2             0.2
# TOTAL             104296    24073

# Add percentages by race, as opposed to by the total
search_race %>%
  mutate(no_search_per = round((no_search / (no_search + searched))*100,1),
         searched_per = round((searched / (no_search + searched))*100,1),
         total_race = no_search + searched) %>%
  arrange(desc(searched_per))

# race_condensed no_search searched no_search_per searched_per total_race
# nam                  607      280          68.4         31.6        887
# black               7819     2346          76.9         23.1      10165
# mixed                203       59          77.5         22.5        262
# hisp               31546     7688          80.4         19.6      39234
# white              55137    12883          81.1         18.9      68020
# pi                  1011      209          82.9         17.1       1220
# asian               4344      346          92.6          7.4       4690
# me_sa               3629      262          93.3          6.7       3891

# GLM TESTS
# If native american
glmTest1 <- glm(is_searched ~ nam,
                data = master,
                family = "binomial")
summary(glmTest1)
exp(coef(glmTest1))

# (Intercept)     nam 
#  0.229465    2.010263
## Someone is 2 times more likely to be searched if they are Native American

# If black
glmTest2 <- glm(is_searched ~ black,
                data = master,
                family = "binomial")
summary(glmTest2)
exp(coef(glmTest2))

#(Intercept)  black 
# 0.2252039   1.3322963
## Someone is 1.3 times more likely to be searched if they are black

###################################################################
# Discretionary vs non-discretionary

# Calculate proportion of race searched when there's discretion or not
disc_race <- searches %>% 
  group_by(is_discretion) %>% 
  count(race_condensed)

# Spread
disc_race <- disc_race %>%
  spread(key = is_discretion, value = n, fill = 0)

# Rename columns
names(disc_race) <- c("race_condensed", "non_disc", "disc")

# Add percentages
disc_race %>%
  mutate(per_of_non_disc = round((non_disc / sum(non_disc))*100,1),
         per_of_disc = round((disc / sum(disc))*100,1)) %>% 
  arrange(desc(per_of_disc))

# race_condensed non_disc  disc per_of_non_disc per_of_disc
# white              4677  8206            51          55.1
# hisp               2987  4701            32.6        31.5
# black              1014  1332            11.1         8.9
# asian               162   184             1.8         1.2
# nam                 111   169             1.2         1.1
# pi                   65   144             0.7         1  
# me_sa               126   136             1.4         0.9
# mixed                30    29             0.3         0.2
# TOTAL              9172  14901

# Add percentages by race, as opposed to by the total
disc_race %>%
  mutate(non_disc_per = round((non_disc / (non_disc + disc))*100,1),
         disc_per = round((disc / (non_disc + disc))*100,1),
         total_race = non_disc + disc) %>% 
  arrange(desc(disc_per))

# race_condensed non_disc  disc non_disc_per disc_per total_race
# pi                   65   144         31.1     68.9        209
# white              4677  8206         36.3     63.7      12883
# hisp               2987  4701         38.9     61.1       7688
# nam                 111   169         39.6     60.4        280
# black              1014  1332         43.2     56.8       2346
# asian               162   184         46.8     53.2        346
# me_sa               126   136         48.1     51.9        262
# mixed                30    29         50.8     49.2         59

###################################################################
# Drugs, weapons or ammunition in contraband

# Calculate proportion of race when drugs, weapons or ammunition was found
## Of the people who were searched
dw_race <- searches %>% 
  group_by(drugs_weapons) %>% 
  count(race_condensed)

# Spread
dw_race <- dw_race %>%
  spread(key = drugs_weapons, value = n, fill = 0)

# Rename columns
names(dw_race) <- c("race_condensed", "none", "drugs_weapons")

# Add percentages
dw_race %>%
  mutate(per_of_none = round((none / sum(none))*100,1),
         per_of_drugs_weapons = round((drugs_weapons / sum(drugs_weapons))*100,1)) %>% 
  arrange(desc(per_of_drugs_weapons))

# race_condensed  none drugs_weapons per_of_none per_of_drugs_weapons
# white          10801          2082        52.8                 57.3
# hisp            6603          1085        32.3                 29.9
# black           2067           279        10.1                  7.7
# nam              227            53         1.1                  1.5
# me_sa            211            51         1                    1.4
# asian            303            43         1.5                  1.2
# pi               180            29         0.9                  0.8
# mixed             50             9         0.2                  0.2
# TOTAL          20442          3631

# Add percentages by race, as opposed to by the total
dw_race %>%
  mutate(none_per = round((none / (none + drugs_weapons))*100,1),
         drugs_weapons_per = round((drugs_weapons / (none + drugs_weapons))*100,1),
         total_race = none + drugs_weapons) %>% 
  arrange(desc(drugs_weapons_per))

# race_condensed  none drugs_weapons none_per drugs_weapons_per total_race
# me_sa            211            51     80.5              19.5        262
# nam              227            53     81.1              18.9        280
# white          10801          2082     83.8              16.2      12883
# mixed             50             9     84.7              15.3         59
# hisp            6603          1085     85.9              14.1       7688
# pi               180            29     86.1              13.9        209
# asian            303            43     87.6              12.4        346
# black           2067           279     88.1              11.9       2346

###################################################################
# Calculate search yield rates
## Proportion of individuals subject to a search where contraband / evidence was found
### search yield rate formula = 
### (number of searched people with contraband / total number searched) * 100

# Overall syr
searches %>% 
  summarise(syr = mean(cont_none))
# 0.763 ~ 76 percent of people searched had no contraband or evidence

# syr by race
syr <- searches %>% 
  group_by(cont_none) %>% 
  count(race_condensed)

# Spread
syr <- syr %>%
  spread(key = cont_none, value = n, fill = 0)

# Rename columns
## Unlike other 1/0 check columns, if cont_cont == 1, there was no contraband found
names(syr) <- c("race_condensed", "cont", "no_cont")

# Add percentages to calculate hit rate
syr %>%
  mutate(hit_rate = round((cont / (sum(cont) + sum(no_cont)))*100,1)) %>% 
  arrange(desc(hit_rate))

# race_condensed  cont no_cont hit_rate
# white           3187    9696     13.2
# hisp            1723    5965      7.2
# black            522    1824      2.2
# asian             73     273      0.3
# me_sa             69     193      0.3
# nam               64     216      0.3
# pi                42     167      0.2
# mixed             14      45      0.1
# TOTAL           5694   18379

# Add percentages by race, as opposed to by the total
syr %>%
  mutate(cont_per = round((cont / (cont + no_cont))*100,1),
         no_cont_per = round((no_cont / (cont + no_cont))*100,1),
         total_race = cont + no_cont) %>% 
  arrange(desc(cont_per))

# race_condensed  cont no_cont cont_per no_cont_per total_race
# me_sa             69     193     26.3        73.7        262
# white           3187    9696     24.7        75.3      12883
# mixed             14      45     23.7        76.3         59
# nam               64     216     22.9        77.1        280
# hisp            1723    5965     22.4        77.6       7688
# black            522    1824     22.3        77.7       2346
# asian             73     273     21.1        78.9        346
# pi                42     167     20.1        79.9        209

###################################################################
# SYRs by discretion or non-discretion searches

# syr by race
syr_disc <- searches %>% 
  group_by(cont_none, is_discretion) %>% 
  count(race_condensed)

# Spread
syr_disc <- syr_disc %>%
  spread(key = cont_none, value = n, fill = 0)

# Rearrange
syr_disc <- syr_disc %>%
  select(race_condensed, everything())

# Rename columns
## Unlike other 1/0 check columns, if cont_cont == 1, there was no contraband found
names(syr_disc) <- c("race_condensed", "is_discretion", "cont", "no_cont")

# Spread again to pull out discretionary or not
syr_disc <- pivot_wider(data = syr_disc, 
            id_cols = race_condensed, 
            names_from = is_discretion, 
            values_from = c("cont", "no_cont"))

# Rename columns
names(syr_disc) <- c("race", "cont_non_disc", "cont_disc", "no_cont_non_disc", "no_cont_disc")

# Add percentages
syr_disc %>%
  mutate(per_of_cont_non_disc = round((cont_non_disc / (sum(cont_non_disc) + sum(no_cont_non_disc)))*100,1),
         per_of_cont_disc = round((cont_disc / (sum(cont_disc) + sum(no_cont_disc)))*100,1),
         per_of_no_cont_non_disc = round((no_cont_non_disc / (sum(no_cont_non_disc) + sum(cont_non_disc)))*100,1),
         per_of_no_cont_disc = round((no_cont_disc / (sum(no_cont_disc) + sum(cont_disc)))*100,1)) %>% 
  arrange(desc(per_of_cont_disc)) %>%
  select(-cont_non_disc, -cont_disc, -no_cont_non_disc, -no_cont_disc)

# race  per_of_cont_non_disc per_of_cont_disc per_of_no_cont_non_disc per_of_no_cont_disc
# white                 13.9             12.8                    37                  42.3
# hisp                   7.5              7                      25.1                24.6
# black                  2.7              1.8                     8.3                 7.1
# asian                  0.4              0.3                     1.4                 1  
# me_sa                  0.3              0.2                     1                   0.7
# nam                    0.3              0.2                     0.9                 0.9
# pi                     0.1              0.2                     0.6                 0.8
# mixed                  0.1              0                       0.3                 0.1

# Add percentages by race, as opposed to by the total
syr_disc %>%
  mutate(cont_non_disc_per = round((cont_non_disc / (cont_non_disc + no_cont_non_disc))*100,1),
         cont_disc_per = round((cont_disc / (cont_disc + no_cont_disc))*100,1),
         no_cont_non_disc_per = round((no_cont_non_disc / (cont_non_disc + no_cont_non_disc))*100,1),
         no_cont_disc_per = round((no_cont_disc / (cont_disc + no_cont_disc))*100,1),
         total_race = cont_non_disc + cont_disc + no_cont_non_disc + no_cont_disc) %>% 
  arrange(desc(cont_disc_per)) %>%
  select(-cont_non_disc, -cont_disc, -no_cont_non_disc, -no_cont_disc)

# race  cont_non_disc_per cont_disc_per no_cont_non_disc_per no_cont_disc_per total_race
# me_sa              25.4          27.2                 74.6             72.8        262
# mixed              23.3          24.1                 76.7             75.9         59
# white              27.3          23.3                 72.7             76.7      12883
# hisp               22.9          22.1                 77.1             77.9       7688
# asian              20.4          21.7                 79.6             78.3        346
# pi                 16.9          21.5                 83.1             78.5        209
# nam                26.1          20.7                 73.9             79.3        280
# black              24.7          20.4                 75.3             79.6       2346

###################################################################
# SYRs by just discretion and contraband

# Create discretionary df
discretions <- master %>% 
  filter(is_discretion == 1)

# race breakdown of people invovled in discretionary searches
discretions %>% 
  count(race_condensed) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# race_condensed     count                 percent
# white           8206                    55.1  
# hisp            4701                    31.5  
# black           1332                     8.94 
# asian            184                     1.23 
# nam              169                     1.13 
# pi               144                     0.966
# me_sa            136                     0.913
# mixed             29                     0.195

# Calculate hit rate by races
syr_disc2 <- discretions %>% 
  group_by(cont_none) %>% 
  count(race_condensed)

# Spread
syr_disc2 <- syr_disc2 %>%
  spread(key = cont_none, value = n, fill = 0)

# Rename columns
## Unlike other 1/0 check columns, if cont_cont == 1, there was no contraband found
names(syr_disc2) <- c("race_condensed", "cont", "no_cont")

# Add percentages
syr_disc2 %>%
  mutate(per_of_cont = round((cont / (sum(cont) + sum(no_cont)))*100,1),
         per_of_no_cont = round((no_cont / (sum(cont) + sum(no_cont)))*100,1)) %>% 
  arrange(desc(per_of_cont))

# race_condensed  cont no_cont per_of_cont per_of_no_cont
# white           1908    6298        12.8           42.3
# hisp            1038    3663         7             24.6
# black            272    1060         1.8            7.1
# asian             40     144         0.3            1  
# me_sa             37      99         0.2            0.7
# nam               35     134         0.2            0.9
# pi                31     113         0.2            0.8
# mixed              7      22         0              0.1

# Add percentages by race, as opposed to by the total
syr_disc2 %>%
  mutate(cont_per = round((cont / (cont + no_cont))*100,1),
         no_cont_per = round((no_cont / (cont + no_cont))*100,1),
         total_race = cont + no_cont) %>% 
  arrange(desc(cont_per))

# race_condensed  cont no_cont cont_per no_cont_per total_race
# me_sa             37      99     27.2        72.8        136
# mixed              7      22     24.1        75.9         29
# white           1908    6298     23.3        76.7       8206
# hisp            1038    3663     22.1        77.9       4701
# asian             40     144     21.7        78.3        184
# pi                31     113     21.5        78.5        144
# nam               35     134     20.7        79.3        169
# black            272    1060     20.4        79.6       1332

###################################################################
# SYRs by consent and contraband

# Create consent df
## Filtering to searches where consent is the only reason
consent <- master %>% 
  filter(search_basis == 1)

# Race breakdown of people invovled in consent searches
consent %>% 
  count(race_condensed) %>% 
  mutate((prop = n / sum(n))*100) %>% 
  arrange(desc(`(prop = n/sum(n)) * 100`))

# race_condensed  COUNT                 PERCENT
# white           2225                    52.3  
# hisp            1430                    33.6  
# black            402                     9.45 
# asian             64                     1.50 
# me_sa             52                     1.22 
# pi                41                     0.964
# nam               34                     0.799
# mixed              6                     0.141

# Calculate syr among consent searches
syr_consent <- consent %>% 
  group_by(cont_none) %>% 
  count(race_condensed)

# Spread
syr_consent <- syr_consent %>%
  spread(key = cont_none, value = n, fill = 0)

# Rename columns
## Unlike other 1/0 check columns, if cont_cont == 1, there was no contraband found
names(syr_consent) <- c("race_condensed", "cont", "no_cont")

# Add percentages
syr_consent %>%
  mutate(per_of_cont = round((cont / (sum(cont) + sum(no_cont)))*100,1),
         per_of_no_cont = round((no_cont / (sum(cont) + sum(no_cont)))*100,1)) %>% 
  arrange(desc(per_of_cont))

# race_condensed  cont no_cont per_of_cont per_of_no_cont
# white            379    1846         8.9           43.4
# hisp             242    1188         5.7           27.9
# black             80     322         1.9            7.6
# asian             12      52         0.3            1.2
# me_sa              9      43         0.2            1  
# pi                 7      34         0.2            0.8
# nam                4      30         0.1            0.7
# mixed              1       5         0              0.1

# Add percentages by race, as opposed to by the total
syr_consent %>%
  mutate(cont_per = round((cont / (cont + no_cont))*100,1),
         no_cont_per = round((no_cont / (cont + no_cont))*100,1),
         total_race = cont + no_cont) %>% 
  arrange(desc(cont_per))

# race_condensed  cont no_cont cont_per no_cont_per total_race
# black             80     322     19.9        80.1        402
# asian             12      52     18.8        81.2         64
# me_sa              9      43     17.3        82.7         52
# pi                 7      34     17.1        82.9         41
# white            379    1846     17          83         2225
# hisp             242    1188     16.9        83.1       1430
# mixed              1       5     16.7        83.3          6
# nam                4      30     11.8        88.2         34

###################################################################
# Actions taken by race
# Calculate proportion of race by force being used
force_race <- master %>% 
  group_by(act_force) %>% 
  count(race_condensed)

# Spread
force_race <- force_race %>%
  spread(key = act_force, value = n, fill = 0)

# Rename columns
names(force_race) <- c("race_condensed", "no_force", "force")

# Add percentages
force_race %>%
  mutate(per_of_no_force = round((no_force / sum(no_force))*100,1),
         per_of_force = round((force / sum(force))*100,1)) %>% 
  arrange(desc(per_of_force))

# race_condensed no_force force per_of_no_force per_of_force
# white             67338   682            53.1         45.8
# hisp              38690   544            30.5         36.5
# black              9996   169             7.9         11.3
# asian              4664    26             3.7          1.7
# me_sa              3866    25             3            1.7
# nam                 863    24             0.7          1.6
# pi                 1207    13             1            0.9
# mixed               255     7             0.2          0.5
# TOTAL            126879  1490

# Add percentages by race, as opposed to by the total
force_race %>%
  mutate(no_force_per = round((no_force / (no_force + force))*100,1),
         force_per = round((force / (no_force + force))*100,1),
         total_race = no_force + force) %>% 
  arrange(desc(force_per))

# race_condensed no_force force no_force_per force_per total_race
# mixed               255     7         97.3       2.7        262
# nam                 863    24         97.3       2.7        887
# black              9996   169         98.3       1.7      10165
# hisp              38690   544         98.6       1.4      39234
# pi                 1207    13         98.9       1.1       1220
# white             67338   682         99         1        68020
# asian              4664    26         99.4       0.6       4690
# me_sa              3866    25         99.4       0.6       3891

# GLM TESTS
# If native american
glmTest3 <- glm(act_force ~ nam,
                data = master,
                family = "binomial")
summary(glmTest3)
exp(coef(glmTest3))

# (Intercept)     nam 
# 0.01163344  2.39051881
## Someone is 2.4 times more likely to have officers use force if they are Native American

# If mixed
glmTest4 <- glm(act_force ~ mixed,
                data = master,
                family = "binomial")
summary(glmTest4)
exp(coef(glmTest4))

# (Intercept)     mixed 
# 0.01171184  2.34386577
## Someone is 2.3 times more likely to have officers use force if they are two or more races

# If black
glmTest5 <- glm(act_force ~ black,
                data = master,
                family = "binomial")
summary(glmTest5)
exp(coef(glmTest5))

# (Intercept)     black 
# 0.0113019   1.4959221
## Someone is 1.5 times more likely to have officers use force if they are black

###################################################################
# Officer drawing weapon by race

# How many individuals had firearms discharged at them
master$is_fired <- ifelse(grepl("act_fad", master$act_words), 1, 0)
sum(master$is_fired)
# Deputies fired at 25 people

# Race of those fired at
master %>% 
  filter(is_fired == 1) %>% 
  group_by(is_fired) %>% 
  count(race_condensed) %>% 
  arrange(desc(n))

# race_condensed     n
# hisp              11
# white             11
# black              2
# nam                1

# Create tag for all types of "forceful actions"
## master$is_fired already created above
# master$is_fired <- ifelse(grepl("act_fad", master$act_words), 1, 0)
master$is_pointed <- ifelse(grepl("act_fp", master$act_words), 1, 0)
master$is_chem <- ifelse(grepl("act_chem", master$act_words), 1, 0)
master$is_baton <- ifelse(grepl("act_baton", master$act_words), 1, 0)
master$is_k9 <- ifelse(grepl("act_k9_bit", master$act_words), 1, 0)
master$is_ip <- ifelse(grepl("act_ip", master$act_words), 1, 0)
master$is_elect <- ifelse(grepl("act_elect", master$act_words), 1, 0)
master$is_rem <- ifelse(grepl("act_rem_cont", master$act_words), 1, 0)
master$is_physical <- ifelse(grepl("act_physical", master$act_words), 1, 0)

# Total forceful actions
sum(master$is_pointed) + sum(master$is_chem) + sum(master$is_baton) + sum(master$is_k9) + sum(master$is_ip) + sum(master$is_elect) + sum(master$is_fired) + sum(master$is_rem) + sum(master$is_physical)
# Forceful tactics were applied 1648 times on 1490 people
sum(master$is_pointed)
# police pointed firearms at 516 people
sum(master$act_force)
# Forceful tactics were used on 1490 people

# Calculate proportion of race by weapons pointed
pointed_race <- master %>% 
  group_by(is_pointed) %>% 
  count(race_condensed)

# Spread
pointed_race <- pointed_race %>%
  spread(key = is_pointed, value = n, fill = 0)

# Rename columns
names(pointed_race) <- c("race_condensed", "no_point", "point")

# Add percentages
pointed_race %>%
  mutate(per_of_no_point = round((no_point / sum(no_point))*100,1),
         per_of_point = round((point / sum(point))*100,1)) %>% 
  arrange(desc(per_of_point))

# race_condensed no_point point per_of_no_point per_of_point
# hisp              39010   224            30.5         43.4
# white             67843   177            53.1         34.3
# black             10091    74             7.9         14.3
# me_sa              3879    12             3            2.3
# nam                 875    12             0.7          2.3
# asian              4679    11             3.7          2.1
# mixed               259     3             0.2          0.6
# pi                 1217     3             1            0.6
# TOTAL              127853  516

# Add percentages by race, as opposed to by the total
pointed_race %>%
  mutate(no_point_per = round((no_point / (no_point + point))*100,1),
         point_per = round((point / (no_point + point))*100,1),
         total_race = no_point + point) %>% 
  arrange(desc(point_per))

# race_condensed no_point point no_point_per point_per total_race
# nam                 875    12         98.6       1.4        887
# mixed               259     3         98.9       1.1        262
# black             10091    74         99.3       0.7      10165
# hisp              39010   224         99.4       0.6      39234
# me_sa              3879    12         99.7       0.3       3891
# white             67843   177         99.7       0.3      68020
# asian              4679    11         99.8       0.2       4690
# pi                 1217     3         99.8       0.2       1220

# GLM TESTS
# If native american
glmTest6 <- glm(is_pointed ~ nam,
                data = master,
                family = "binomial")
summary(glmTest6)
exp(coef(glmTest6))

# (Intercept)     nam 
# 0.003969192 3.455183673
## Someone is 3.5 times more likely to have police firearms pointed at them if they are Native American

# If mixed
glmTest7 <- glm(is_pointed ~ mixed,
                data = master,
                family = "binomial")
summary(glmTest7)
exp(coef(glmTest7))

# (Intercept)     mixed 
# 0.004020565 2.880941091
## Someone is 2.9 times more likely to have officers point their firearms if they are two or more races

# If black
glmTest8 <- glm(is_pointed ~ black,
                data = master,
                family = "binomial")
summary(glmTest8)
exp(coef(glmTest8))

# (Intercept)     black 
# 0.003753333 1.953801402
## Someone is 2 times more likely to have officers point firearms if they are black

# If hisp
glmTest9 <- glm(is_pointed ~ hisp,
                 data = master,
                 family = "binomial")
summary(glmTest9)
exp(coef(glmTest9))

# (Intercept)     hisp 
# 0.003286697 1.747078548
## Someone is 1.7 times more likely to have officers point firearms if they are hispanic

###################################################################
# OTHER ACTIONS ANALYSIS

# Calculate proportion of race by detained
detained <- master %>% 
  group_by(act_detained) %>% 
  count(race_condensed)

# Spread
detained <- detained %>%
  spread(key = act_detained, value = n, fill = 0)

# Rename columns
names(detained) <- c("race_condensed", "no_det", "det")

# Add percentages
detained %>%
  mutate(per_of_no_det = round((no_det / sum(no_det))*100,1),
         per_of_det = round((det / sum(det))*100,1)) %>% 
  arrange(desc(per_of_det))

# race_condensed no_det   det per_of_no_det per_of_det
# white           50964 17056          52.8       53.5
# hisp            29410  9824          30.5       30.8
# black            6730  3435           7         10.8
# asian            4176   514           4.3        1.6
# me_sa            3491   400           3.6        1.3
# nam               567   320           0.6        1  
# pi                974   246           1          0.8
# mixed             186    76           0.2        0.2

# Add percentages by race, as opposed to by the total
detained %>%
  mutate(no_det_per = round((no_det / (no_det + det))*100,1),
         det_per = round((det / (no_det + det))*100,1),
         total_race = no_det + det) %>% 
  arrange(desc(det_per))

# race_condensed no_det   det no_det_per det_per total_race
# nam               567   320       63.9    36.1        887
# black            6730  3435       66.2    33.8      10165
# mixed             186    76       71      29          262
# white           50964 17056       74.9    25.1      68020
# hisp            29410  9824       75      25        39234
# pi                974   246       79.8    20.2       1220
# asian            4176   514       89      11         4690
# me_sa            3491   400       89.7    10.3       3891

# If black
glmTest10 <- glm(act_detained ~ black,
                data = master,
                family = "binomial")
summary(glmTest10)
exp(coef(glmTest10))

# (Intercept)       black 
# 0.3167721   1.6112566 
## Someone is 1.6 times more likely to be detained if they are black

# If nam
glmTest11 <- glm(act_detained ~ nam,
                data = master,
                family = "binomial")
summary(glmTest11)
exp(coef(glmTest11))

# (Intercept)         nam 
# 0.3288926   1.7159821
## Someone is 1.7 times more likely to be detained if they are native american


