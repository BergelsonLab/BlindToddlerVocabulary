This folder contains data from the American English Communicative Development Inventory (CDI) from blind children.

In this folder: 
- VIHI_CDI.csv: for each of the 40 blind children in the dataset, contains CDI production and comprehension scores as well as some demographic information
-- Columns:
--- VIHI_ID: a unique identifier for each CDI administration
--- Version: which version of the CDI was taken. WG stands for Words & Gestures; WS stands for Words & Sentences
--- WordsProduced: the number of words a child was reported to produce
--- WordsUnderstood: the number of words a child was reported to understand
--- Source: how the CDI was collected. "VIHI" indicates that data were collected by Erin Campbell in the Bergelson Lab at Duke University. "Herrera" indicates that data were collected by Robyn Casillas as part of her dissertation at University of California Los Angeles (Herrera, 2015).
--- age: child age (in days) at time of CDI
--- age_months: child's age (in months) at time of CDI
--- group: refers to sensory group. "VI" indicates that the child is blind.
--- ParticipantNumber: a unique ID for each participant (this is the same as VIHI_ID, without the final digits that indicate age in days)
--- VisionDiagnosisStandardized: child's primary vision diagnosis. CVI stands for cortical visual impairment. 
--- SevereorProfound: whether vision diagnosis resulted in severe or profound vision loss
--- ExcludefromVICDI: a yes/no column of whether administration should be excluded from the present analysis. checked yes for multilingual children and children with additional cognitive or developmental delay
--- sex: child's sex assigned at birth
--- mom_ed: maternal education: values include Secondary (high school diploma or GED), Some College (Associate's degree or some college), College (Bachelor's degree or equivalent), or Graduate (Master's degree, PhD, or other graduate degree)
--- Etiology: whether vision diagnosis affects eye structure (Eye) or optic nerve / brain structure (Neural)
--- in_cdi_agerange: whether child was in the normed age range for the version of the CDI they were administered. Words & Gestures normed age range is 8--18 months. Words & Sentences normed age range is 16--30 months. 
--- ProductionCDI_no: number of words unchecked on child's CDI
--- expected_score_for_age: child's expected number of words produced (given their age), produced by the "calculate_delay" function
--- expected_age_for_score: child's expected age (given their number of words produced), produced by the "calculate_delay" function
--- diff_score_from_expected: child's expected number of words produced (given their age) minus their actual number of words produced
--- diff_age_from_expected: child's expected age (given their number of words produced) minus their actual age, measured in months
--- Wordbank_n: the number of children from Wordbank who contributed to vocabulary norms for that child's instrument (WG or WS)
--- Wordbank_n: the date on which vocabulary norms were downloaded from Wordbank for that child's instrument (WG or WS)

- VITD_vocabmatches_wordlevel.csv: long-form word-level CDI data for each blind child and their sighted matches from Wordbank. 
-- Columns: 
--- VIHI_ID: a unique identifier for each CDI administration
--- age: child's age (in months) at time of CDI
--- Version: which version of the CDI was taken. WG stands for Words & Gestures; WS stands for Words & Sentences
--- Group: Blind or Sighted
--- item: the word on the CDI
--- Response: whether or not child produces the word (no or produces)
--- item2: a slightly different convention for writing the word on the CDI
--- item_id: the number of the word on the CDI, version-specific, preceded by "num"
--- num_item_id: the number of the word on the CDI, version-specific
--- type: what kind of question on the CDI. non-word items have been filtered out already
--- category: the semantic category of the word on the CDI (i.e., sounds, animals, vehicles, food_drink, clothing, body_parts, furniture_rooms, household, outside, people, games_routines, action_words, time_words, descriptive_words, pronouns, question_words, locations, quantifiers, toys)
--- lexical_category: part of speech, includes a smaller subset of options than lexical_class. (nouns, predicates, function_words, other)
--- lexical_class: part of speech (onomatopoeia, nouns,  interjection, adverbs, verbs, adjectives, function_words, other)
--- Word: lemma version of item
--- Syllables: number of syllables in the word. NA for things like "babysitter's name", etc.
--- VisibleFirstSound: whether the first phoneme of the word is labial
--- GET RID OF NUM PHONEMES
--- *.mean: 0-5 mean ratings from Lancaster Sensorimotor Norms (Lynott & Connell, 2020). How strongly is word associated with sense or body part?
--- *.SD: standard deviations for Lancaster Sensorimotor ratings
--- Dominant.perceptual: the modality which receives highest mean rating for the word out of Visual.mean, Olfactory.mean, Haptic.mean, Gustatory.mean, Auditory.mean, and Interoceptive.mean
--- Max_strength.perceptual: the highest mean rating for the word out of Visual.mean, Olfactory.mean, Haptic.mean, Gustatory.mean, Auditory.mean, and Interoceptive.mean
--- Minkowski3.perceptual: "Minkowski distance at m = 3 of the vector from the origin.  Theoretically, it represents [perceptual] strength in all dimensions but the influence of weaker dimensions is attenuated, and it has been proposed as the optimal value for modelling multisensory cue integration in perception" (Lynott & Connell, 2020)
--- Exclusivity.perceptual: the range of perceptual ratings from Lancaster Sensorimotor Norms divided by the sum of perceptual ratings
--- Dominant.action: the body part which receives highest mean rating for the word out of the motor ratings
--- Max_strength.action: the highest mean rating for the word out of the motor ratings
--- Minkowski3.action: "Minkowski distance at m = 3 of the vector from the origin.  Theoretically, it represents [action] strength in all dimensions but the influence of weaker dimensions is attenuated, and it has been proposed as the optimal value for modelling multisensory cue integration in perception" (Lynott & Connell, 2020)
--- Exclusivity.action: the range of motor ratings from Lancaster Sensorimotor Norms divided by the sum of motor ratings
--- Dominant.sensorimotor: the body part or perceptual modality which receives highest mean rating for the word out of the sensorimotor ratings
--- Max_strength.sensorimotor: the highest mean rating for the word out of the sensorimotor ratings
--- Minkowski3.sensorimotor: "Minkowski distance at m = 3 of the vector from the origin.  Theoretically, it represents sensorimotor strength in all dimensions but the influence of weaker dimensions is attenuated, and it has been proposed as the optimal value for modelling multisensory cue integration in perception" (Lynott & Connell, 2020)
--- Exclusivity.sensorimotor: the range of sensorimotor ratings from Lancaster Sensorimotor Norms divided by the sum of sensorimotor ratings
--- N_known.perceptual: the number of raters presented this word who knew the word (participants who did not know the word did not provide ratings)
--- List_N.perceptual: the number of raters presented this word
--- Percent_known.perceptual: the percent of raters presented this word who knew this word
--- N_known.action: the number of raters presented this word who knew the word (participants who did not know the word did not provide ratings)
--- List_N.action: the number of raters presented this word
--- Percent_known.action: the number of raters presented this word who knew the word (participants who did not know the word did not provide ratings). 
--- Mean_age.perceptual: mean age of raters for the perceptual ratings for this word
--- Mean_age.action: mean age of raters for the motor ratings for this word
--- List#.perceptual: which list was presented to the perceptual raters for this word
--- List#.action: which list was presented to the motor raters for this word
--- Conc.M: mean concreteness ratings for this word from Brysbaert et al., 2014 (http://crr.ugent.be/archives/1330). ranges 1-5
--- Conc.SD: standard deviation of concreteness ratings for this word from Brysbaert et al., 2014 (http://crr.ugent.be/archives/1330)
--- Unknown: the number of raters presented this word for concreteness ratings who did not know this word
--- Total: the number of raters presented this word for concreteness ratings who knew this word and provided a rating for this word
--- Percent_known: the percent of raters presented this word (for concreteness ratings) who knew this word
--- SUBTLEX: the frequency of this word in the SUBTLEX corpus of subtitles (https://osf.io/djpqz/)
--- ChildesFreq: the number of tokens of this word in the CHILDES collection of corpora (https://childes.talkbank.org/)
--- visualornot: whether the Dominant.perceptual modality is visual ("visual") or another modality (olfactory, haptic, gustatory, auditory, interoceptive, "not_visual")


- VITD_vocabmatches_wordlevel_single.csv: same as VITD_vocabmatches_wordlevel.csv, but with only one CDI administration per blind child (the CDI with the highest expressive vocab) and their sighted matches from wordbank

Last updated: 11/30/2022
Maintained by: Erin Campbell (erin.e.campbell@duke.edu)