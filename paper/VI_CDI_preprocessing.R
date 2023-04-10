library(tidyverse)
library(wordbankr)
library(reshape)
library(readxl)
library(lazyeval)
library(profileR)
library(lme4)

# read in wordbank administrations
WB_WG_admins <- get_instrument_data(language = "English (American)", form = "WG", administration_info =TRUE) %>% 
  select(data_id, child_id, age, comprehension, production) %>%
  dplyr::rename(age_months = age, WordsProduced = production, WordsUnderstood = comprehension) %>%
  mutate(Version = "WG")
WB_WS_admins <- get_instrument_data(language = "English (American)", form = "WS", administration_info =TRUE) %>% 
  select(data_id, child_id, age, comprehension, production) %>%
  dplyr::rename(age_months = age, WordsProduced = production, WordsUnderstood = comprehension) %>%
  mutate(WordsUnderstood = 'NA', Version = "WS")
WB_admins <- rbind(WB_WG_admins, WB_WS_admins) %>%
  dplyr::rename(age=age_months)
WB_admin_ages <- WB_admins %>% 
  dplyr::select(data_id, age, WordsProduced, Version) %>%
  distinct


#word norms
wordbank_dict <- read_csv("./data/CDI/Wordbank/wordbank_dict.csv") %>%
  filter(type=="word") %>%
  dplyr::rename(Version=form)

Lancaster <- read_csv("./data/Norms/Lancaster_sensorimotor_norms_for_39707_words.csv") %>%
  mutate(Word = tolower(Word))

## pulling childes frequency takes forever. if you need to re-run, uncomment the following lines
## last pulled 10/21/2022
# childes_frequency <- childesr::get_tokens(collection="Eng-NA", token = '*') %>% 
#   filter(gloss %in% wordbank_dict$Word) %>%
#   mutate(gloss=tolower(gloss)) %>%
#   group_by(gloss) %>% 
#   summarise(ChildesFreq = n()) %>% 
#   dplyr::rename("Word" = "gloss")
# write_csv(childes_frequency, "data/Norms/Childes_frequency.csv") 
childes_frequency <- read_csv("./data/Norms/Childes_frequency.csv") 

Concreteness <- read_csv("./data/Norms/brysbaert_concreteness.csv") %>%
  select(-Bigram)

CBOI <- read_csv("./data/Norms/CBOI_mean_sd.csv") %>%
  select(c(Word, CBOI_Mean)) %>%
  mutate(Word=tolower(Word))

wordbank_dict <- wordbank_dict %>%
  left_join(Lancaster) %>%# join with lancaster norms
  left_join(Concreteness) %>%
  left_join(childes_frequency) %>%
  left_join(CBOI) %>%
  mutate(visualornot = as.factor(case_when(Dominant.perceptual=="Visual" ~ "visual",
                                                                 TRUE ~ "not_visual")))

write.csv(wordbank_dict, "./data/Norms/wordbank_dict_with_norms.csv")


# read in and wrangle demographic info
VI_demo <- read_csv("/Volumes/pn-opus/VIHI/SubjectInformation/Surveys/demographics/VI_qualtrics.csv") %>% 
  bind_rows((read_csv("/Volumes/pn-opus/VIHI/SubjectInformation/Surveys/demographics/VI_Herrera_demographics.csv"))) %>% 
  dplyr::select(ParticipantNumber, Sex, Parent1Ed, ChildRace, ChildEthnicity, VisionDiagnosisStandardized, SevereorProfound, ExcludefromVICDI) %>%
  mutate(ParticipantNumber = str_sub(ParticipantNumber, start=1, end=6)) %>%
  mutate(sex = case_when(Sex == "M" ~ "Male",
                         Sex == "F" ~ "Female",
                         TRUE ~ "NA"),
         mom_ed = case_when(Parent1Ed == "HSD" ~ "Secondary",
                            Parent1Ed == "Bachelor's degree" | Parent1Ed == "Bachelors" ~ "College",
                            Parent1Ed == "Some college" | Parent1Ed == "Associate's degree" ~ "Some College",
                            Parent1Ed == "Doctoral degree (e.g. MD, PhD, JD" | Parent1Ed == "Master's degree" ~ "Graduate",
                            TRUE ~ "NA"),
         Etiology = case_when(VisionDiagnosisStandardized=="Not specified" |
                                VisionDiagnosisStandardized=="Multiple" |
                                is.na(VisionDiagnosisStandardized)~ 'NA',
                              VisionDiagnosisStandardized == "Optic Nerve Hypoplasia" |
                                VisionDiagnosisStandardized ==  "CVI"|
                                VisionDiagnosisStandardized ==  "Optic Pathway Glioma" ~ "Neural",
                              TRUE ~ "Eye")) %>%
  select(-Parent1Ed, -Sex)



# Reading in CDI production #s and %s
VIHI_CDI <- read_csv("/Volumes/pn-opus/VIHI/SubjectInformation/Surveys/CDI/VIHI_CDI.csv") %>% 
  mutate(age = as.numeric(substring(VIHI_ID, 8)),
         group = str_sub(VIHI_ID, 1, 2),
         age_months = round(age/30.435),
         ParticipantNumber = str_sub(VIHI_ID, start=1, end=6)) %>% 
  mutate(ParticipantNumber = str_sub(VIHI_ID, start=1, end=6)) %>%
  left_join(VI_demo) %>%  # join with demographics info
  mutate(in_cdi_agerange = ifelse(Version=="WG" & age_months>=8 & age_months<=18, T,
                                  ifelse(Version=="WS" & age_months>=16 & age_months<=30, T, F))) %>%
  distinct(VIHI_ID, Version, .keep_all = TRUE) %>% 
  filter(is.na(ExcludefromVICDI) | ExcludefromVICDI != "yes",
         Source!="Herrera" | Version!="WG") #remove Robyn's WG admins

# # Reading in Word-level VI CDI data
VI_wordlevel_CDI <-
  read_csv( "/Volumes/pn-opus/VIHI/SubjectInformation/Surveys/CDI/VIHI/VI_wordlevelCDI_WG.csv"
  ) %>% #VIHI VI WG
  dplyr::select(VIHI_ID, age, baa_baa:some) %>%
  mutate(Version = "WG") %>% 
  mutate( across(baa_baa:some, as.character)) %>%
  mutate( across(baa_baa:some, replace_na, "no")) %>%
  bind_rows(read_csv("/Volumes/pn-opus/VIHI/SubjectInformation/Surveys/CDI/VIHI/VI_wordlevelCDI_WS.csv") %>% #VIHI VI WS
              dplyr::select(VIHI_ID, age, baa_baa:then) %>% mutate(Version = "WS") %>% 
              mutate( across(baa_baa:then, as.character)) %>%
              mutate( across(baa_baa:then, replace_na, "no"))) %>%
  bind_rows(read_xlsx("/Volumes/pn-opus/VIHI/SubjectInformation/Surveys/CDI/Herrara/VI_CDI_ws.xlsx") %>% #Herrera WS
              dplyr::select(VIHI_ID, age, baa_baa:then) %>% mutate(Version = "WS") %>% 
              mutate( across(baa_baa:then, as.character)) %>%
              mutate( across(baa_baa:then, replace_na, "no"))) %>%
  mutate(group = "VI") %>% 
  left_join(VIHI_CDI %>% dplyr::select(VIHI_ID, group, age)) %>%
  mutate(age=case_when(!is.na(age)~age,
                       TRUE~as.double(str_sub(VIHI_ID, start=8))/30.4167))

VI_wordlevel_CDI_long <- VI_wordlevel_CDI  %>%
  pivot_longer(cols=c(-VIHI_ID, -group, -Version, -age), names_to = "item", values_to = "Response") %>% #pivot the word columns to a single column with the word, and another column with whether or not it's produced
  filter(Response=="produces" | Response=="no") %>% #filtering only responses where the word is produced (since we're interested in CDI comprehension) 
  left_join(wordbank_dict)  #join with dictionary to add a Word column minus punctuation



#inverse logit function to calculate delay
inv_logit <- function(x){
  return(1 / (exp(-x) + 1))
} 
#get score for a given age using logit function
getScoreForAge = function(lm, age, lang, num_items){ #function that takes lm, child's age, and the number of possible words (from WG/WS)
  # just predict
  prop = inv_logit(predict.glm(lm, newdata = data.frame(age=age)))
  return(prop*num_items)
}
#get age for a given score using logit function
getAgeForScore = function(lm, score, num_items){
  proportion = (score + .000001) / num_items #added point .000001 to avoid getting inf delay when score is 0
  # http://www.talkstats.com/threads/inverse-prediction-from-binary-logistic-regression.52121/
  b0 = lm$coefficients[1]
  b1 = lm$coefficients[2]
  predicted_age = (log(proportion / (1-proportion)) - b0)/ b1
  return(predicted_age)
}
# English constants for growth curve
constants_eng = list() #create a list called constants
constants_eng[['WG']] = list() #add a WG section to the list
constants_eng[['WG']]$lowest_num_id = 33 #sets the lowest_num_id to 33 (the first question on WG asking 'does your child know X?')
constants_eng[['WG']]$highest_num_id = 430 #sets the highest_num_id to 430 (the last question on WG asking 'does your child know X?')
constants_eng[['WG']]$num_items = constants_eng[['WG']]$highest_num_id - constants_eng[['WG']]$lowest_num_id + 1 #substracts lowest_  and highest_num_ids and adds 1, to get highest possible score on WG
constants_eng[['WS']] = list() #add a WS section to the list
constants_eng[['WS']]$lowest_num_id = 1 #sets the lowest_num_id to 1 (the first question on WS asking 'does your child know X?')
constants_eng[['WS']]$highest_num_id = 680 #sets the highest_num_id to 680 (the last question on WS asking 'does your child know X?')
constants_eng[['WS']]$num_items = constants_eng[['WS']]$highest_num_id - constants_eng[['WS']]$lowest_num_id + 1 #substracts lowest_  and highest_num_ids and adds 1, to get highest possible score on WS
#English growth curves
calculate_delay = function(cdi_form, constants, verbose=F){
  print(paste('Processing ',cdi_form,'...', sep=''))	#Prints "Processing WG..." or WS message
  
  num_items = constants_eng[[cdi_form]][['num_items']]	
  print('Number of items:')
  print(num_items) #Prints number of items possible given CDI version
  
  eng_data <- get_instrument_data(language = "English (American)", 
                                  form = cdi_form, administration_info = TRUE) %>%
    mutate(num_item_id = as.numeric(str_sub(item_id, 6,)))
  eng_words = subset(eng_data, num_item_id < constants_eng[[cdi_form]][['highest_num_id']] & num_item_id > constants_eng[[cdi_form]][['lowest_num_id']]) #takes the subset of columns related to 'does your child know X word?'
  
  print('Computing counts...') #Prints "Computing counts" message
  counts <- eng_words %>% #creates counts df
    dplyr::filter(!is.na(.data$age)) %>% #filters entries without an age 
    dplyr::mutate(produces = !is.na(.data$value) & .data$value == "produces",
                  understands = !is.na(.data$value) &
                    (.data$value == "understands" | .data$value == "produces")) %>%
    dplyr::select(-.data$value) %>%
    tidyr::gather("measure_name", "value", .data$produces, .data$understands) %>%
    dplyr::filter(.data$measure_name == "produces") %>%
    dplyr::group_by(.data$age, .data$data_id) %>%
    dplyr::summarise(num_true = sum(.data$value),
                     num_false = n() - .data$num_true)
  print(counts)
  
  print('Fitting model...') #prints "Fitting model" message
  model <- stats::glm(cbind(num_true, num_false) ~ age, counts,
                      family = "binomial")
  
  if (verbose){ #if verbose argument is TRUE, prints a summary of the model
    print(summary(model))
  }
  
  
  new_data = data.frame(age = (seq(2*30.5,60*30.5,by=1) / 30.5), cdi_form)
  
  print('Getting scores...')
  
  new_scores = cbind(new_data, data.frame(predict(model, new_data, type='response', se.fit=T)))
  print(names(new_scores))
  new_scores$scores = new_scores$fit * num_items
  new_scores$se_high = new_scores$fit + new_scores$se.fit
  new_scores$se_low = new_scores$fit - new_scores$se.fit
  new_scores$se_high = new_scores$se_high * num_items
  new_scores$se_low = new_scores$se_low * num_items
  new_scores$predict_ages = new_scores$age
  
  print('Getting Wordbank norms...')
  print(num_items)
  wordbank_norms = read.csv(paste("./data/CDI/Wordbank/vocabulary_norms_table_",cdi_form,"_Prod",".csv", sep=""),
                            stringsAsFactors=F)	
  
  wordbank_norms_melted = melt(wordbank_norms, id.vars = c("language", "form", "measure", "age", "is_norming", "n", "downloaded"), measure.vars = "vocab")
  
  estimate_for_form <- subset(VIHI_CDI, Version == cdi_form)
  estimate_for_form$ProductionCDI_no = num_items - estimate_for_form$WordsProduced 
  estimate_for_form$expected_score_at_chron_age = sapply(estimate_for_form$age_months,
                                                         function(age){getScoreForAge(model, age, num_items=num_items)})
  estimate_for_form$expected_age_for_score = sapply(estimate_for_form$WordsProduced,
                                                    function(score){getAgeForScore(model, score, num_items)})
  
  print('Computing differences...')	
  estimate_for_form$diff_score_from_expected = -1 * (estimate_for_form$WordsProduced - estimate_for_form$expected_score_at_chron_age)
  # more negative, more baf
  estimate_for_form$diff_age_from_expected = estimate_for_form$age_months - estimate_for_form$expected_age_for_score
  estimate_for_form$Wordbank_n = wordbank_norms_melted %>% distinct(n) %>% as.numeric()
  estimate_for_form$Wordbank_norms_date = wordbank_norms_melted %>% distinct(downloaded) %>% as.character()
  
  if (verbose){
    print(estimate_for_form[,c('VIHI_ID','age_months', 
                               'WordsProduced', 'expected_score_at_chron_age', 
                               'diff_age_from_expected','diff_score_from_expected')])
  }
  
  print(head(estimate_for_form))
  rlist = list()		
  rlist[['normative_growth_curve_model']] = model
  rlist[['samples_from_growth_curve_model']] = new_scores
  rlist[['estimate_df']] = estimate_for_form
  rlist[['wordbank_norms_melted']] = wordbank_norms_melted
  rlist[['cdi_form']] = cdi_form
  assign(paste(cdi_form, "estimate_eng", sep = "_"), estimate_for_form, envir = .GlobalEnv)
  assign(paste(cdi_form, "estimate_eng_curves", sep = "_"), wordbank_norms_melted, envir = .GlobalEnv)
  assign(paste(cdi_form, "estimate_eng_gcurve", sep = "_"), new_scores, envir = .GlobalEnv)
}




# run delay function over VIHI CDI data
calculate_delay("WG", constants_eng, verbose=T)
calculate_delay("WS", constants_eng, verbose=T)
# join the dataframes created with delay function, and adjust at the lower tails based on Wordbank %. (otherwise overestimates delay because of long tail)
VIHI_CDI <- rbind(WG_estimate_eng, WS_estimate_eng) %>%
  mutate(diff_age_from_expected = case_when(WordsProduced==0 ~ (age_months - 8),
                                            WordsProduced==1 ~ (age_months - 9),
                                            WordsProduced==2 ~ (age_months - 10),
                                            WordsProduced==3 ~ (age_months - 11),
                                            WordsProduced==4 ~ (age_months - 11.5),
                                            WordsProduced==5 ~ (age_months - 12),
                                            WordsProduced==6 ~ (age_months - 12.5),
                                            WordsProduced==7 ~ (age_months - 12.5),
                                            WordsProduced==12 ~ (age_months - 13.5),
                                            WordsProduced==12 ~ (age_months - 14.5),
                                            TRUE ~ diff_age_from_expected)) %>%
  filter(group=="VI") %>%
  select(-c(Other_ID, Notes))
write.csv(VIHI_CDI, "./data/CDI/Derived/VIHI_CDI.csv")
write_rds(WG_estimate_eng_gcurve, "./data/CDI/Derived/WG_estimate_eng_gcurve.rds")
write_rds(WS_estimate_eng_gcurve, "./data/CDI/Derived/WS_estimate_eng_gcurve.rds")

# matching VI kids to wordbank kids based on vocab
n_vi_kids <- VIHI_CDI %>%
  filter(group=='VI') %>%
  nrow
VI_to_match <- VIHI_CDI %>%
  filter(group == 'VI' & WordsProduced > 0) %>%
  select(VIHI_ID, WordsProduced, Version)
VI_to_match_single <- VIHI_CDI %>%
  filter(group == 'VI' & WordsProduced > 0) %>%
  arrange(-WordsProduced) %>%
  dplyr::distinct(ParticipantNumber, .keep_all=TRUE) %>%
  select(VIHI_ID, WordsProduced, Version) 

VIvocabmatches <- VI_to_match %>%
  mutate(VI_age= as.numeric(substring(VIHI_ID, 8))/30.4167) %>%
  left_join(WB_admin_ages, by = c('WordsProduced', 'Version')) %>%
  # In case of multiple (possibly m:n)  matches, we want the first VIHI kid to
  # match the first databank kid, second - second, etc.
  mutate(age_diff = VI_age-age) %>%
  arrange(abs(age_diff)) %>%
  group_by(VIHI_ID) %>%
  mutate(WB_match_number = row_number()) %>%
  ungroup %>%
  group_by(data_id) %>%
  mutate(VIHI_match_number = row_number()) %>%
  ungroup() %>%
  filter(WB_match_number == VIHI_match_number) %>%
  filter(data_id != 'NA') %>%
  mutate(TD_ID = paste(data_id, age, sep="_")) # i created this variable (here and in HIvocabmatches, below) to make sure I'm getting the right administration from the right kid

VIvocabmatches_single <- VI_to_match_single %>%
  mutate(VI_age= as.numeric(substring(VIHI_ID, 8))/30.4167) %>%
  left_join(WB_admin_ages, by = c('WordsProduced', 'Version')) %>%
  # In case of multiple (possibly m:n)  matches, we want the first VIHI kid to
  # match the first databank kid, second - second, etc.
  mutate(age_diff = VI_age-age) %>%
  arrange(abs(age_diff)) %>%
  group_by(VIHI_ID) %>%
  mutate(WB_match_number = row_number()) %>%
  ungroup %>%
  group_by(data_id) %>%
  mutate(VIHI_match_number = row_number()) %>%
  ungroup() %>%
  filter(WB_match_number == VIHI_match_number) %>%
  filter(data_id != 'NA') %>%
  mutate(TD_ID = paste(data_id, age, sep="_")) # i created this variable (here and in HIvocabmatches, below) to make sure I'm getting the right administration from the right kid



# get word-level data for selected wordbank participants
WG_WB_wordlevel <- get_instrument_data("English (American)", "WG") %>%
  mutate(Version = "WG")
WS_WB_wordlevel <- get_instrument_data("English (American)", "WS") %>%
  mutate(Version = "WS")
WB_wordlevel <- bind_rows(WG_WB_wordlevel, WS_WB_wordlevel)
# wordbank data for the VI vocab matches
TD_VIvocab_matches_wordlevel <- WB_wordlevel %>%
  left_join(WB_admin_ages, by=c("data_id", "Version")) %>%
  mutate(TD_ID = paste(data_id, age, sep="_"), group = "TD") %>%
  filter(TD_ID %in% VIvocabmatches$TD_ID) %>% #match with previously selected matches based on age and wordbank id (data_id)
  left_join(wordbank_dict, by = c("item_id", "Version")) %>%
  dplyr::rename(VIHI_ID=TD_ID,
                Response = value) %>% #merge VIHI_ID and data_id into one column
  mutate(VIHI_ID = as.factor(VIHI_ID),
         Response = as.factor(case_when(Response=="produces" ~ "produces", #standardize options for Response to either "no" or "Produces"
                                        Response=="both"~"produces",
                                        Response=="yes"~"produces",
                                        TRUE ~ "no"))) %>%
  filter(!is.na(item))

TD_VIvocab_matches_wordlevel_single <- WB_wordlevel %>%
  left_join(WB_admin_ages, by=c("data_id", "Version")) %>%
  mutate(TD_ID = paste(data_id, age, sep="_"), group = "TD") %>%
  filter(TD_ID %in% VIvocabmatches_single$TD_ID) %>% #match with previously selected matches based on age and wordbank id (data_id)
  left_join(wordbank_dict, by = c("item_id", "Version")) %>%
  dplyr::rename(VIHI_ID=TD_ID,
                Response = value) %>% #merge VIHI_ID and data_id into one column
  mutate(VIHI_ID = as.factor(VIHI_ID),
         Response = as.factor(case_when(Response=="produces" ~ "produces", #standardize options for Response to either "no" or "Produces"
                                        Response=="both"~"produces",
                                        Response=="yes"~"produces",
                                        TRUE ~ "no"))) %>%
  filter(!is.na(item))

VITD_vocabmatches_wordlevel <- bind_rows(
  (VI_wordlevel_CDI_long %>%
     filter(VIHI_ID %in% VIvocabmatches$VIHI_ID)),
  TD_VIvocab_matches_wordlevel) %>%
  distinct(VIHI_ID, item, .keep_all = TRUE)  %>%
  mutate(group = as.factor(case_when(group=="VI" ~ "Blind",
                                     group=="TD" ~ "Sighted")))  %>%
  mutate(group = relevel(group, "Sighted")) %>%
  mutate(Response = as.factor(Response),
         lexical_category = as.factor(lexical_category),
         lexical_class = as.factor(lexical_class)) %>%
  select(-c(data_id,produces,understands,WordsProduced))
write.csv(VITD_vocabmatches_wordlevel,"./data/CDI/Derived/VITD_vocabmatches_wordlevel.csv")

VITD_vocabmatches_wordlevel_single <- bind_rows(
  (VI_wordlevel_CDI_long %>%
     filter(VIHI_ID %in% VIvocabmatches_single$VIHI_ID)),
  TD_VIvocab_matches_wordlevel_single) %>%
  distinct(VIHI_ID, item, .keep_all = TRUE)  %>%
  mutate(group = as.factor(case_when(group=="VI" ~ "Blind",
                                     group=="TD" ~ "Sighted")))  %>%
  mutate(group = relevel(group, "Sighted")) %>%
  mutate(Response = as.factor(Response),
         lexical_category = as.factor(lexical_category),
         lexical_class = as.factor(lexical_class),
         visualornot = as.factor(case_when(Dominant.perceptual=="Visual" ~ "visual",
                                           TRUE ~ "not_visual"))) %>%
  select(-c(data_id,produces,understands,WordsProduced))
write.csv(VITD_vocabmatches_wordlevel_single,"./data/CDI/Derived/VITD_vocabmatches_wordlevel_single.csv")

# america's next top models

exclusivity_model <- glmer(Response ~ visualornot*Exclusivity.perceptual*group + (1|VIHI_ID) + (1|item), data = VITD_vocabmatches_wordlevel, family = "binomial",
                           glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
exclusivity_model_summary <- modelsummary::get_estimates(exclusivity_model)
saveRDS(exclusivity_model, file = "./data/Models/exclusivity_model.rds")
saveRDS(exclusivity_model_summary, file = "./data/Models/exclusivity_model_summary.rds")


perceptualstrength_model <- glmer(Response ~ visualornot*Max_strength.perceptual*group + (1|VIHI_ID) + (1|item), data = VITD_vocabmatches_wordlevel, family = "binomial",
                                  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
saveRDS(perceptualstrength_model, file = "./data/Models/perceptualstrength_model.rds")
perceptualstrength_model_summary <- modelsummary::get_estimates(perceptualstrength_model)
saveRDS(perceptualstrength_model_summary, file = "./data/Models/perceptualstrength_model_summary.rds")

