This folder contains word norms.

In this folder: 
- brysbaert_concreteness.csv: Brysbaert concreteness norms https://link.springer.com/article/10.3758/s13428-013-0403-5
-- Columns: 
--- Conc.M: mean concreteness ratings for this word from Brysbaert et al., 2014 (http://crr.ugent.be/archives/1330). ranges 1-5
--- Conc.SD: standard deviation of concreteness ratings for this word from Brysbaert et al., 2014 (http://crr.ugent.be/archives/1330)
--- Unknown: the number of raters presented this word for concreteness ratings who did not know this word
--- Total: the number of raters presented this word for concreteness ratings who knew this word and provided a rating for this word
--- Percent_known: the percent of raters presented this word (for concreteness ratings) who knew this word
--- SUBTLEX: the frequency of this word in the SUBTLEX corpus of subtitles (https://osf.io/djpqz/)

- Lancaster_sensorimotor_norms_for_39707_words.csv: lancaster sensorimotor norms https://osf.io/7emr6/
-- Columns:
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

- Childes_frequency.csv: frequency ratings of all the words on the CDI in the CHILDES collection of corpora (https://childes.talkbank.org/). Generated in VI_CDI_preprocessing.R via the childesr package
-- Columns:
--- Word: each of the words on the CDI
--- ChildesFreq: the number of tokens of this word in CHILDES (https://childes.talkbank.org/)

- wordbank_dict_with_norms.csv: , plus all of the word norms from the other files listed above
-- Columns: 
--- Version: which version of the CDI was taken. WG stands for Words & Gestures; WS stands for Words & Sentences
--- item: the word on the CDI
--- item_id: the number of the word on the CDI, version-specific
--- type: what kind of question on the CDI. non-word items have been filtered out already
--- category: the semantic category of the word on the CDI (i.e., sounds, animals, vehicles, food_drink, clothing, body_parts, furniture_rooms, household, outside, people, games_routines, action_words, time_words, descriptive_words, pronouns, question_words, locations, quantifiers, toys)
--- lexical_category: part of speech, includes a smaller subset of options than lexical_class. (nouns, predicates, function_words, other)
--- lexical_class: part of speech (onomatopoeia, nouns,  interjection, adverbs, verbs, adjectives, function_words, other)
--- Word: lemma version of item
--- Syllables: number of syllables in the word. NA for things like "babysitter's name", etc.
--- VisibleFirstSound: whether the first phoneme of the word is labial


Last updated: 12/1/2022
Maintained by: Erin Campbell (erin.e.campbell@duke.edu)
