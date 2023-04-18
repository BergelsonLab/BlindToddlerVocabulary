

# helper functions

theme_vi_cdi <- function(textsize, legendposition, style="minimal"){
  if (style=="classic")
  {theme_classic() %+replace%    #replace elements we want to change
    theme(legend.position=legendposition, text=element_text(size=textsize))}  
  else{
    theme_minimal() %+replace%    #replace elements we want to change
    theme(legend.position=legendposition, text=element_text(size=textsize))}  
    }

fill_palette_vi_cdi <- function(labels=c(TD="Sighted", VI="Blind"), name=NULL) {
  ggplot2::scale_fill_manual(values=c("#75ddba", "#759fd3"), labels=labels, name=name)
  }
color_palette_vi_cdi <- function(labels=c(TD="Sighted", VI="Blind"), name=NULL) {
  ggplot2::scale_color_manual(values=c("#75ddba", "#759fd3"), labels=labels, name=name)
}

# comparison to wordbank norms
growth_curve_illustration <- ggplot(data= VIHI_CDI %>% filter(group=="VI")) +
  geom_point(alpha=.5, aes(x=age_months, y=WordsProduced, shape=Version), size=4, color="#759fd3") +
  theme_minimal() +
  scale_shape_discrete(labels = c("WG" = "Words & Gestures", "WS" = "Words & Sentences")) +
  theme(text=element_text(size=14), legend.position = "bottom") +
  xlab("Age (months)") +
  ylab("Words Produced") +
  annotate('rect', xmin=8, xmax=30, ymin=0, ymax=700, alpha=.2, fill='#75ddba') +
  geom_line(data = WS_estimate_eng_gcurve%>%filter(predict_ages>18), aes(x=predict_ages, y=scores),colour='#75ddba', size=1.5)+
  geom_line(data = WG_estimate_eng_gcurve%>%filter(predict_ages<18), aes(x=predict_ages, y=scores),colour='#75ddba',size=1.5) +
  geom_vline(xintercept =8, size =1.5) +
  geom_vline(xintercept =18, size =1.5) +
  geom_vline(xintercept =30, size =1.5) +
  annotate(geom="text", x=13, y=600, label="WG", size=8)+
  annotate(geom="text", x=24, y=600, label="WS", size=8) +
  annotate(geom="text", x=48, y=600, label="outside range", size=8)+
  annotate("point", x = 17.2, y = 421, size=6, shape=17, color="#759fd3", fill="#759fd3")+
  geom_segment(aes(x = 18.1, y = 421, xend = 27.9, yend = 421),
                 arrow = arrow(length = unit(0.5, "cm")))+
  geom_segment(aes(x = 41, y = 260, xend = 23.8, yend = 260),
               arrow = arrow(length = unit(0.5, "cm")))+
  annotate("point", x = 41, y = 260, size=6, shape=17, color="#759fd3", fill="#759fd3")+
  annotate(geom="text", x=22.5, y=365, label="9 months\nahead", size=4) +
  annotate(geom="text", x=35, y=310, label="17 months\nbehind", size=4)+
  coord_cartesian(ylim=c(0, 680))

# density plot
density_plot <- ggplot(VIHI_CDI, aes(x = diff_age_from_expected)) + 
  geom_histogram(aes(y = ..density..),
                 colour = "#134074", fill = "#134074") +
  geom_density(lwd = 1, colour = "#759fd3",
               fill = "#759fd3", alpha = 0.5) +
  theme_vi_cdi(16, "none") +
  xlab("Difference from Sighted Sample (months)")+
  ylab("N Participants") +
  scale_y_continuous(breaks = c((0/115),(2/115),(4/115),(6/115),(8/115),(10/115),(12/115)),
                     labels = (function(y) y * 115))

# splitting_plots
## Gender
gender_split_info <- VIHI_CDI_single %>% 
  group_by(sex) %>%
  summarise(N = n(),
            MeanDelay = mean(diff_age_from_expected, na.rm=TRUE),
            sdDelay = sd(diff_age_from_expected, na.rm=TRUE))
gender_diffs <- wilcox.test(VIHI_CDI_single$diff_age_from_expected ~ VIHI_CDI_single$sex, 
                            alternative = "two.sided",
                            exact = NULL)
gender_diff_plot <- ggplot(data=(VIHI_CDI %>% filter(group == "VI" & age_months>=9 & !is.na(sex))), aes(x=diff_age_from_expected, fill=sex)) +
  geom_density(alpha=.5) +
  geom_vline(xintercept = 0) +
  theme_vi_cdi(10, c(.8,.8)) +
  xlab(NULL) +
  scale_fill_manual(name="Gender", labels = c(glue('Female (N={gender_split_info$N[1]})'), glue('Male (N={gender_split_info$N[2]})')), values=c("#7F5A83", "#87B38D"))
severity_split_info <- VIHI_CDI_single %>% 
  group_by(SevereorProfound) %>%
  summarise(N = n(),
            MeanDelay = mean(diff_age_from_expected, na.rm=TRUE))
# Severity
severity_split_info <- VIHI_CDI_single %>% 
  group_by(SevereorProfound) %>%
  summarise(N = n(),
            MeanDelay = mean(diff_age_from_expected, na.rm=TRUE),
            sdDelay = sd(diff_age_from_expected, na.rm=TRUE))

severity_diffs <- wilcox.test(VIHI_CDI$diff_age_from_expected ~ VIHI_CDI$SevereorProfound, 
                              alternative = "two.sided",
                              exact = NULL)
severity_diff_plot <- ggplot(data=(VIHI_CDI %>% filter(group == "VI" & age_months>=9 & !is.na(SevereorProfound))), aes(x=diff_age_from_expected, fill=SevereorProfound)) +
  geom_density(alpha=.5) +
  geom_vline(xintercept = 0) +
  theme_vi_cdi(10, c(.8,.8)) +
  xlab(NULL) +
  scale_fill_manual(name="Severity", values=c("#B5FFE1", "#0D324D"), labels = c(glue('Profound (no light perception)\n(N={severity_split_info$N[1]})'), glue('Severe (some light perception)\n(N={severity_split_info$N[2]})')))

# Etiology
etiology_diffs <- wilcox.test((VIHI_CDI %>% filter(grepl("Hypoplasia|CVI",VisionDiagnosisStandardized)))$diff_age_from_expected, (VIHI_CDI %>% filter(!grepl("Hypoplasia|CVI",VisionDiagnosisStandardized)))$diff_age_from_expected, 
                              alternative = "two.sided",
                              exact = NULL)
etiology_split_info <- VIHI_CDI_single %>%
  group_by(Etiology) %>%
  summarise(N = n(),
            MeanDelay = mean(diff_age_from_expected, na.rm=TRUE),
            sdDelay = sd(diff_age_from_expected, na.rm=TRUE))
etiology_diff_plot <- ggplot(data=(VIHI_CDI %>% filter(group == "VI" & age_months>=9 & !is.na(VisionDiagnosisStandardized))), aes(x=diff_age_from_expected, fill=grepl("Hypoplasia|CVI",VisionDiagnosisStandardized))) +
  geom_density(alpha=.5) +
  geom_vline(xintercept = 0) +
  theme_vi_cdi(10, c(.8,.8)) +
  xlab("Difference from Sighted Sample (months)") +
  scale_fill_manual(name="Etiology", values=c("#1A5E63", "#F0F3BD"), labels = c(glue('Peripheral\n(N={etiology_split_info$N[1]})'), glue('Central Nervous System\n(N={etiology_split_info$N[2]})')))

cowplot::plot_grid(gender_diff_plot, severity_diff_plot, etiology_diff_plot, ncol=1) 


## longitudinal-plot
age_delay <- lmer(formula = diff_age_from_expected ~ age_months + (1|ParticipantNumber), data=VIHI_CDI)
summary_age_delay <- summary(age_delay)
age_delay_test <- Anova(age_delay)
effects_age <- effects::effect(term= "age_months", mod= age_delay)
x_age <- as.data.frame(effects_age)
longitudinal_plot <- ggplot() + 
  geom_hline(yintercept =0, color="black") +
  geom_line(data=x_age, aes(x=age_months, y=fit), color="grey8", linetype="dashed") +
  geom_ribbon(data= x_age, aes(x=age_months, ymin=lower, ymax=upper), alpha= 0.3) +
  geom_point(data=VIHI_CDI, aes(age_months, diff_age_from_expected, color=ParticipantNumber)) + 
  geom_line(data=VIHI_CDI, aes(age_months, diff_age_from_expected, group = ParticipantNumber, color=ParticipantNumber), alpha=.5) +
  labs(x="Age (months)", y="Delay (months)") +
  theme_vi_cdi(16, "none")

## vocab-match-demo
vocabcomp_plot <- ggplot(vocabcomp_details, aes(x=group, y=WordsProduced, fill=group, color=group)) +
  geom_violin(alpha=.4) +
  geom_point(size=2) +
  geom_line(aes(group=pair), color="grey10", alpha=.5) +
  theme_vi_cdi(16, "none", "classic") +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size=.8) +
  fill_palette_vi_cdi() +
  color_palette_vi_cdi()+
  ylab("Words Produced") +
  scale_x_discrete(labels=c("TD" = "Sighted", "VI" = "Blind"))
agecomp_plot <- ggplot(vocabcomp_details, aes(x=group, y=age, color=group,fill=group)) +
  geom_violin(alpha=.4) +
  geom_point(size=2) +
  geom_line(aes(group=pair), color="grey10", alpha=.5) +
  theme_vi_cdi(16, "none", "classic") +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size=.8) +
  fill_palette_vi_cdi() +
  color_palette_vi_cdi()+
  ylab("Age (months)") +
  scale_x_discrete(labels=c("TD" = "Sighted", "VI" = "Blind"))

## 
# word length
possible_wordlength_WG<- round(mean((wordbank_dict %>% filter(Version=="WG")%>%distinct(item, .keep_all = TRUE))$Syllables, na.rm=TRUE),2)
possible_wordlength_WS<- round(mean((wordbank_dict %>% filter(Version=="WS")%>%distinct(item, .keep_all = TRUE))$Syllables, na.rm=TRUE),2)
mean_length <- VITD_vocabmatches_wordlevel_single %>% group_by(group, VIHI_ID) %>%
  filter(Response=="produces" | Response == "both") %>% dplyr::summarise(Syllables = mean(Syllables, na.rm=TRUE))
length_summary <- mean_length %>% group_by(group) %>% summarise(Mean = round(mean(Syllables, na.rm=TRUE),2))
word_length_plot <- ggplot(mean_length, aes(color=group, x=group, y=Syllables)) +
  stat_summary(fun.data = "mean_cl_boot", size=.6, shape=0) +
  theme_vi_cdi(12, "none", "classic") +
  fill_palette_vi_cdi() +
  color_palette_vi_cdi()+
  ylab("Word Length\n(Syllables)") +
  ylim(c(1,2)) +
  scale_x_discrete(labels=c("TD" = "Sighted", "VI" = "Blind")) 

# pos
possible_pos_Ns <- wordbank_dict %>% group_by(lexical_class) %>% 
  summarise(N_WG = sum(Version=="WG"),
            N_WS = sum(Version=="WS"))
VITD_pos_Ns <- VITD_vocabmatches_wordlevel_single %>% 
  filter(Response=="produces" | Response == "both") %>%
  group_by(group, VIHI_ID, lexical_class) %>%
  summarise(N=n()) %>%
  ungroup() %>%
  pivot_wider(names_from = lexical_class, values_from = N) %>%
  replace(is.na(.), 0) 
VITD_pos_Ns_long <- VITD_pos_Ns %>% 
  pivot_longer(cols=c(3:9), values_to = "N", names_to = "lexical_class") %>% 
  left_join(possible_pos_Ns)
pos_plot <- ggplot(VITD_pos_Ns_long, aes(x=reorder(lexical_class, N), y=N, color=group)) +
  theme_vi_cdi(12, "none", "classic") +
  stat_summary(fun.data = "mean_cl_boot", size=.6, position = position_dodge(width=0.5), shape=0) +
  fill_palette_vi_cdi() +
  color_palette_vi_cdi() +
  scale_x_discrete(labels=c("interjections", "onomatopoeia", "adverbs", "adjectives","function words", "verbs", "nouns")) +
  ylab("Words Produced") +
  xlab("Part of Speech") +
  coord_cartesian(ylim = c(0,160)) 
# semantic
possible_category_Ns <- wordbank_dict %>% 
  group_by(category) %>% 
  summarise(N_WG = sum(Version=="WG"),
            N_WS = sum(Version=="WS"))
VITD_category_Ns <- VITD_vocabmatches_wordlevel_single %>% 
  filter(Response=="produces" | Response == "both") %>%
  group_by(group, VIHI_ID, category) %>%
  summarise(N=n()) %>%
  ungroup() %>%
  pivot_wider(names_from = category, values_from = N) %>%
  replace(is.na(.), 0) #if participant produces no words, that cell ends up blank. replace blank cells with 0s.
VITD_category_Ns_long <- VITD_category_Ns %>% 
  pivot_longer(cols=3:24, values_to = "N", names_to = "category") %>% 
  left_join(possible_category_Ns)
category_plot <- ggplot(VITD_category_Ns_long, aes(x=(reorder(category, N)), y=N, color=group)) +
  theme_vi_cdi(12, "none", "classic") +
  stat_summary(fun.data = "mean_cl_boot", size=.6, position = position_dodge(width=0.6), shape=0) +
  theme(axis.text.x=element_text(angle=20, vjust=.8, hjust=1), axis.title.x = element_text(vjust=-0.6)) +
  fill_palette_vi_cdi() +
  color_palette_vi_cdi()+  
  scale_x_discrete(labels=c("connecting_words"="connecting words","question_words" = "question words", "time_words"="time words", "helping_verbs"="helping verbs", "games_routines"="games & routines", "furniture_rooms"="furniture & rooms", "body_parts"="body parts", "descriptive_words"="descriptive words", "food_drink"="food & drink", "action_words"="action words")) +
  ylab("Words Produced")+
  xlab("Semantic Category (on CDI)") +
  coord_cartesian(ylim = c(0,50)) 

# concreteness
possible_concreteness_WG<- round(mean((wordbank_dict %>% filter(Version=="WG") %>%distinct(item, .keep_all = TRUE))$Conc.M, na.rm=TRUE),2)
possible_concreteness_WS<- round(mean((wordbank_dict %>% filter(Version=="WS") %>%distinct(item, .keep_all = TRUE))$Conc.M, na.rm=TRUE),2)


mean_concreteness <- VITD_vocabmatches_wordlevel_single %>% group_by(group, VIHI_ID) %>%
  filter(Response=="produces" | Response == "both") %>% dplyr::summarise(Conc.M = mean(Conc.M, na.rm=TRUE))
test_mean_concreteness <- wilcox.test(mean_concreteness$Conc.M ~ mean_concreteness$group)
Z_meanconcrete<-round(qnorm(test_mean_concreteness$p.value/2),2)
concrete_summary <- mean_concreteness %>% group_by(group) %>% summarise(Mean = round(mean(Conc.M, na.rm=TRUE),2))

concreteness_plot<- ggplot(mean_concreteness, aes(color=group, x=group, y=Conc.M)) +
  stat_summary(fun.data = "mean_cl_boot", size=.6, shape=0) +
  theme_vi_cdi(12, "none", "classic") +
  fill_palette_vi_cdi() +
  color_palette_vi_cdi()+
  ylab("Brysbaert\nConcreteness rating (0-5)") +
  scale_x_discrete(labels=c("TD" = "Sighted", "VI" = "Blind")) +
  ylim(c(3,5)) 

# Child-Body-Object Interaction
mean_CBOI <- VITD_vocabmatches_wordlevel_single %>% group_by(group, VIHI_ID) %>%
  filter(Response=="produces" | Response == "both") %>% dplyr::summarise(CBOI = mean(CBOI_Mean, na.rm=TRUE))
test_mean_CBOI <- wilcox.test(mean_CBOI$CBOI ~ mean_CBOI$group)
Z_mean_CBOI<-round(qnorm(test_mean_CBOI$p.value/2),2)
CBOI_summary <- mean_CBOI %>% group_by(group) %>% summarise(Mean = round(mean(CBOI, na.rm=TRUE),1))

CBOI_plot<- ggplot(mean_CBOI, aes(color=group, x=group, y=CBOI)) +
  stat_summary(fun.data = "mean_cl_boot", size=.6, shape=0) +
  theme_vi_cdi(12, "none", "classic") +
  fill_palette_vi_cdi() +
  color_palette_vi_cdi()+
  ylab("Interactiveness\nrating (0-7)") +
  scale_x_discrete(labels=c("TD" = "Sighted", "VI" = "Blind")) +
  ylim(c(4,7))

# modality
VITD_modality_Ns <- VITD_vocabmatches_wordlevel_single %>% 
  filter(Response=="produces" | Response == "both") %>%
  filter(!is.na(Dominant.perceptual)) %>%
  group_by(group, VIHI_ID, Dominant.perceptual) %>%
  summarise(N=n()) %>%
  ungroup() %>%
  pivot_wider(names_from = Dominant.perceptual, values_from = N) %>%
  replace(is.na(.), 0) #if participant produces no words, that cell ends up blank. replace blank cells with 0s.
possible_modality_Ns <- wordbank_dict %>% group_by(Dominant.perceptual) %>% summarise(N_WG = sum(Version=="WG"),
                                                                                      N_WS = sum(Version=="WS"))


VITD_modality_Ns_long <- VITD_modality_Ns %>% 
  pivot_longer(cols=c(3,4,5,6,7,8), values_to = "N", names_to = "Dominant.perceptual") %>%
  left_join(possible_modality_Ns)

modality_plot<- ggplot(VITD_modality_Ns_long, aes(x=reorder(Dominant.perceptual,N), y=N, color=group)) +
  theme_vi_cdi(12, "right","classic") +
  stat_summary(fun.data = "mean_cl_boot", size=.6, position = position_dodge(width=0.5), shape=0) +
  theme(legend.box.background = element_rect(colour = "black", size = 2)) +
  fill_palette_vi_cdi() +
  color_palette_vi_cdi()+  
  ylab("Words Produced") +
  xlab("Perceptual Modality\n(Lancaster Norms)") +
  coord_cartesian(ylim = c(0,180))

# model plots
excl_list <- wordbank_dict %>% distinct(Exclusivity.perceptual) %>% unlist(use.names = FALSE) %>% append(c(0,1))
pe_fit <- ggeffects::ggpredict(exclusivity_model, terms = c("Exclusivity.perceptual [excl_list]", "visualornot", "group"),ci.lvl = .683) %>%
  left_join((wordbank_dict %>% distinct(item,.keep_all=TRUE) %>% select(-Version)), by = c("x" = "Exclusivity.perceptual","group" = "visualornot"))


exclusivity_plot_sighted<- ggplot(data=(pe_fit%>%filter(facet=="Sighted"&!is.na(item))))  + 
  geom_smooth(size = 1, aes(color=group, fill=group, x=x, y=predicted)) +
  geom_point(aes(x=x, y=predicted, color=group,fill=group), size=.2) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=group, x=x, y=predicted)) +
  theme_vi_cdi(10, "none") +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = NULL, x = "Perceptual Exclusivity") +
  fill_palette_vi_cdi(labels=c('Non-Visual', 'Visual'), name = "Word Modality") +
  color_palette_vi_cdi(labels=c('Non-Visual', 'Visual'), name = "Word Modality")+  scale_x_continuous(breaks = c(0,.25,.5,.75,1), labels = c("0", ".25", ".5", ".75", "1")) +
  ylim(0,.4)+
  ggtitle("Sighted")
exclusivity_plot_sighted_with_density <- ggMarginal(exclusivity_plot_sighted, groupColour = TRUE, groupFill = TRUE)
exclusivity_plot_blind<- ggplot(data=(pe_fit%>%filter(facet=="Blind"&!is.na(item))))  + 
  geom_smooth(size = 1, aes(color=group, fill=group, x=x, y=predicted)) +
  geom_point(aes(x=x, y=predicted, color=group), size=.2) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=group, x=x, y=predicted)) +
  theme_vi_cdi(10, "none") +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = NULL, x = "Perceptual Exclusivity") +
  fill_palette_vi_cdi(labels=c('Non-Visual', 'Visual'), name = "Word Modality") +
  color_palette_vi_cdi(labels=c('Non-Visual', 'Visual'), name = "Word Modality")+  scale_x_continuous(breaks = c(0,.25,.5,.75,1), labels = c("0", ".25", ".5", ".75", "1")) +
  scale_x_continuous(breaks = c(0,.25,.5,.75,1), labels = c("0", ".25", ".5", ".75", "1")) +
  ylim(0,.4)+
  ggtitle("Blind")
exclusivity_plot_blind_with_density <-ggMarginal(exclusivity_plot_blind, groupColour = TRUE, groupFill = TRUE)


strength_list <- wordbank_dict %>% distinct(Max_strength.perceptual) %>% unlist(use.names = FALSE) %>% append(0,5)
ps_fit <- ggeffects::ggpredict(perceptualstrength_model, terms = c("Max_strength.perceptual [strength_list]", "visualornot", "group"),ci.lvl = .683) %>%
  left_join((wordbank_dict %>% distinct(item,.keep_all=TRUE) %>% select(-Version)), by = c("x" = "Max_strength.perceptual","group" = "visualornot"))
perceptualstrength_plot_sighted<- ggplot(data=(ps_fit%>%filter(facet=="Sighted"&!is.na(item))))  + 
  geom_smooth(size = 1, aes(color=group, fill=group, x=x, y=predicted)) +
  geom_point(aes(x=x, y=predicted, color=group, fill=group), size=.2) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=group, x=x, y=predicted)) +
  theme_vi_cdi(10, "none") +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = "Predicted Probability of\nWord Production", x = "Perceptual Strength") +
  fill_palette_vi_cdi(labels=c('Non-Visual', 'Visual'), name = "Word Modality") +
  color_palette_vi_cdi(labels=c('Non-Visual', 'Visual'), name = "Word Modality")+ 
  ylim(0,.4) +
  ggtitle("Sighted")
perceptualstrength_plot_sighted_with_density <- ggMarginal(perceptualstrength_plot_sighted, groupColour = TRUE, groupFill = TRUE)
perceptualstrength_plot_blind<- ggplot(data=(ps_fit%>%filter(facet=="Blind"&!is.na(item))))  + 
  geom_smooth(size = 1, aes(color=group, fill=group, x=x, y=predicted)) +
  geom_point(aes(x=x, y=predicted, color=group), size=.2) +
  geom_ribbon(alpha = .3, aes(ymin= conf.low, ymax=conf.high,  fill=group, x=x, y=predicted)) +
  theme_vi_cdi(10, "none") +
  scale_y_continuous(limits=c(0,1)) +
  labs(y = NULL, x = "Perceptual Strength") +
  fill_palette_vi_cdi(labels=c('Non-Visual', 'Visual'), name = "Word Modality") +
  color_palette_vi_cdi(labels=c('Non-Visual', 'Visual'), name = "Word Modality")+  
  ylim(0,.4)+
  ggtitle("Blind")
perceptualstrength_plot_blind_with_density <-ggMarginal(perceptualstrength_plot_blind, groupColour = TRUE, groupFill = TRUE)


model_plots <- cowplot::plot_grid(perceptualstrength_plot_sighted_with_density, perceptualstrength_plot_blind_with_density, exclusivity_plot_sighted_with_density, exclusivity_plot_blind_with_density, rel_widths = c(1.2,1,1,1), nrow=1, labels = c("A","","B",""))

word_perceptual_properties <- ggplot((wordbank_dict%>% distinct(item, .keep_all=TRUE)), aes(x=Max_strength.perceptual, y = Exclusivity.perceptual)) +
  geom_label_repel(aes(label = item, color=visualornot), size = 3, max.overlaps = 15, box.padding = .1, label.padding = .1, force_pull = 3, 
                   show.legend = F) +
  geom_point(aes(color=visualornot), alpha=.3)+
  color_palette_vi_cdi(labels=c('Non-Visual', 'Visual'), name = "Word Modality")+  
  theme_vi_cdi(10, "bottom") +
  xlab("Perceptual Strength") +
  ylab("Perceptual Exclusivity") +
  guides(colour = guide_legend(override.aes = list(size = 6, alpha=1)))

