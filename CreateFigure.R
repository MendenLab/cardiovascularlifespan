library(readxl)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(reshape2)
library(broom)
library(car)
library(ppcor)
library(rockchalk)
library(scatterplot3d)
library(cowplot)
library(writexl)

raw_data <- read_excel("/Users/martin.meinel/Documents/Matters Arising/dataset_for_analysis.xlsx")
### Analysis of metabolic rate with lifespan ###############################
corr_life_meta <- cor.test(log10(raw_data$lifespan),log10(raw_data$mass_specific_BMR),method="pearson")
corr_meta_spearman <- cor.test(log10(raw_data$lifespan),log10(raw_data$mass_specific_BMR),method="spearman")


lm_meta <- lm(log10(lifespan)~log10(mass_specific_BMR), data = raw_data)
summary(lm_meta)

predicted_lm_meta <- data.frame(meta_pred=predict(lm_meta,raw_data,interval="confidence"),meta_x=log10(raw_data$mass_specific_BMR))


Metabolic_Rate_Plot <- raw_data %>% ggplot2::ggplot(aes(x = log10(raw_data$mass_specific_BMR), y = log10(raw_data$lifespan))) +
  geom_point()+ 
  geom_label_repel(aes(label = common_name), size = 3.0, label.size = NA, max.overlaps = 15)+
  geom_line(color='red', data = predicted_lm_meta, aes(x=meta_x, y=meta_pred.fit))+
  geom_ribbon(data = predicted_lm_meta, aes(ymin = meta_pred.lwr, ymax = meta_pred.upr, color = NULL), alpha = .15) +
  theme_classic()+
  xlab("Log10 Mass-specific BMR [W/g]")+
  ylab("Log10 Lifespan [years]")+
  annotate("label", x=-2.7,y=0.38 ,label= paste("y =", round(lm_meta$coefficients[2], 2), "x +", round(lm_meta$coefficients[1], 2),
                                               ", p-value =", round(summary(lm_meta)$coefficients[2,4],4),
                                               "\n R^2 =", round(summary(lm_meta)$r.squared, 2), ", adj R^2 =", round(summary(lm_meta)$adj.r.squared, 2),
                                               "\n Pearson: R =",  round(corr_life_meta$estimate, 2), ", p-value =",  round(corr_life_meta$p.value, 4)),size=3.0)

print(Metabolic_Rate_Plot)
###### Analysis of litter size with lifespan ##############################
corr_life_litter <- cor.test(log10(raw_data$lifespan),raw_data$litter_size,method="pearson")
corr_litter_spearman <- cor.test(log10(raw_data$lifespan),raw_data$litter_size,method="spearman", exact = F)

# Linear fit
lm_litter_size <- lm(log10(lifespan)~litter_size, data=raw_data)
summary(lm_litter_size)

predicted_lm_Litter_size <- data.frame(litter_size_pred=predict(lm_litter_size, raw_data, interval="confidence"), litter_size_x=raw_data$litter_size)


Litter_Size_Plot <- raw_data %>% ggplot2::ggplot(aes(x = raw_data$litter_size, y = log10(raw_data$lifespan))) +
  geom_point()+ 
  geom_label_repel(aes(label = common_name), size = 3.0, label.size = NA, max.overlaps = 15)+
  geom_line(color='red', data = predicted_lm_Litter_size, aes(x=litter_size_x, y=litter_size_pred.fit))+
  geom_ribbon(data = predicted_lm_Litter_size, aes(ymin = litter_size_pred.lwr, ymax = litter_size_pred.upr, color = NULL), alpha = .15) +
  theme_classic()+
  xlab("Litter size")+
  ylab("Log10 Lifespan [years]")+
  annotate("label", x=2.72,y=0.5 ,label= paste("y =", round(lm_litter_size$coefficients[2], 2), "x +", round(lm_litter_size$coefficients[1], 2),
                                               ", p-value =", round(summary(lm_litter_size)$coefficients[2,4],4),
                                               "\n R^2 =", round(summary(lm_litter_size)$r.squared, 2), ", adj R^2 =", round(summary(lm_litter_size)$adj.r.squared, 2),
                                               "\n Pearson: R =",  round(corr_life_litter$estimate, 2), ", p-value =",  round(corr_life_litter$p.value, 4)),size=3.0)


print(Litter_Size_Plot)
######### Analysis of Heart rate with lifespan ################################
corr_life_heart <- cor.test(log10(raw_data$lifespan), log10(raw_data$heart_rate), method = "pearson")
corr_heart_spearman <- cor.test(log10(raw_data$lifespan),log10(raw_data$heart_rate),method="spearman")

lm_heartBeats <- lm(log10(lifespan)~log10(heart_rate), data = raw_data)
lm_heartBeats
summary(lm_heartBeats)

#save model predictions
predicted_Lm_HeartRate <- data.frame(lifespan_pred = predict(lm_heartBeats, raw_data, interval = "confidence"), HeartRate_pred_x=log10(raw_data$heart_rate))

#plot with fitted allometric model line
Heart_Plot <- raw_data %>% ggplot2::ggplot(aes(x = log10(raw_data$heart_rate), y = log10(raw_data$lifespan))) +
  geom_point()+ 
  geom_label_repel(aes(label = common_name), size = 3., label.size = NA, max.overlaps = 15)+
  geom_line(color='red', data = predicted_Lm_HeartRate, aes(x=HeartRate_pred_x, y=lifespan_pred.fit))+
  geom_ribbon(data = predicted_Lm_HeartRate, aes(ymin = lifespan_pred.lwr, ymax = lifespan_pred.upr, color = NULL), alpha = .15) +
  theme_classic()+
  xlab("Log10 Resting heart rate [beats per minute]")+
  ylab("Log10 Lifespan [years]")+
  annotate("label", x=1.82,y=0.5 ,label= paste("y =", round(lm_heartBeats$coefficients[2], 2), "x +", round(lm_heartBeats$coefficients[1], 2),
                                                ", p-value =", round(summary(lm_heartBeats)$coefficients[2,4],4),
                                                "\n R^2 =", round(summary(lm_heartBeats)$r.squared, 2), ", adj R^2 =", round(summary(lm_heartBeats)$adj.r.squared, 2),
                                                "\n Pearson: R =",  round(corr_life_heart$estimate, 2), ", p-value =",  round(corr_life_heart$p.value, 4)),size=3.)


print(Heart_Plot)
######################## Analysis of Respiratory Rate with lifespan ######################################
corr_life_lung <- cor.test(log10(raw_data$lifespan), log10(raw_data$respiratory_rate) , method = "pearson")
corr_lung_spearman <- cor.test(log10(raw_data$lifespan), log10(raw_data$respiratory_rate) , method = "spearman")

lm_lungBreaths <- lm(log10(lifespan)~log10(respiratory_rate) , data = raw_data)
lm_lungBreaths
summary(lm_lungBreaths)

#save model predictions
predicted_Lm_lungBreaths <- data.frame(lungBreaths_pred_y = predict(lm_lungBreaths, raw_data, interval = "confidence"), lungBreaths_pred_x=log10(raw_data$respiratory_rate))


lung_plot<- raw_data%>% ggplot2::ggplot(aes(x = log10(raw_data$respiratory_rate), y = log10(raw_data$lifespan))) + 
  geom_point()+ 
  geom_label_repel(aes(label = common_name), size = 3., label.size = NA, max.overlaps = 15)+
  geom_line(color='red', data = predicted_Lm_lungBreaths, aes(x=lungBreaths_pred_x, y=lungBreaths_pred_y.fit))+
  geom_ribbon(data = predicted_Lm_lungBreaths, aes(ymin = lungBreaths_pred_y.lwr, ymax = lungBreaths_pred_y.upr, color = NULL), alpha = .15) +
  theme_classic()+
  xlab("Log10 Respiratory rate [breaths per minute]")+
  ylab("Log10 Lifespan [years]")+
  annotate("label", x=1.22, y=0.55, label= paste("y =", round(lm_lungBreaths$coefficients[2], 2), "x +", round(lm_lungBreaths$coefficients[1], 2),
                                               ", p-value =", round(summary(lm_lungBreaths)$coefficients[2,4],4),
                                               "\n R^2 =", round(summary(lm_lungBreaths)$r.squared, 2), ", adj R^2 =", round(summary(lm_lungBreaths)$adj.r.squared, 2),
                                               "\n Pearson: R =",  round(corr_life_lung$estimate, 2), ", p-value =",  round(corr_life_lung$p.value, 4)),size=3.0)

print(lung_plot)
######### Analysis of  Weight with Lifespan #################################################
corr_life_weight <- cor.test(log10(raw_data$lifespan), log10(raw_data$adult_mass), method = "pearson")
corr_weight_spearman <- cor.test(log10(raw_data$lifespan), log10(raw_data$adult_mass), method = "spearman")

Lm_BodyMass <- lm(log10(lifespan)~log10(adult_mass) , data = raw_data)
Lm_BodyMass
summary(Lm_BodyMass)

#save model predictions
predicted_Lm_BodyMass <- data.frame(BodyMass_pred_y = predict(Lm_BodyMass, raw_data, interval = "confidence"), BodyMass_pred_x=log10(raw_data$adult_mass))

#plot with fitted allometric regression model line
Mass_Plot <- raw_data %>% ggplot2::ggplot(aes(x = log10(raw_data$adult_mass), y = log10(raw_data$lifespan))) + 
  geom_point()+ 
  geom_label_repel(aes(label = common_name), size = 3., label.size = NA, max.overlaps = 15)+
  geom_line(color='red', data = predicted_Lm_BodyMass, aes(x=BodyMass_pred_x, y=BodyMass_pred_y.fit))+
  geom_ribbon(data = predicted_Lm_BodyMass, aes(ymin = BodyMass_pred_y.lwr , ymax = BodyMass_pred_y.upr, color = NULL), alpha = .15) +
  theme_classic()+
  xlab("Log10 Adult mass [g]")+
  ylab("Log10 Lifespan [years]")+
  annotate("label", x=2.18, y=1.8, label= paste("y =", round(Lm_BodyMass$coefficients[2], 2), "x +", round(Lm_BodyMass$coefficients[1], 2),
                                               ", p-value =", round(summary(Lm_BodyMass)$coefficients[2,4],4),
                                               "\n R^2 =", round(summary(Lm_BodyMass)$r.squared, 2), ", adj R^2 =", round(summary(Lm_BodyMass)$adj.r.squared, 2),
                                               "\n Pearson: R =",  round(corr_life_weight$estimate, 2), ", p-value =",  round(corr_life_weight$p.value, 4)),size=3.)

print(Mass_Plot)
##################### Analysis of somatic mutation rate and lifespan ################3
corr_life_mut <- cor.test(log10(raw_data$lifespan), log10(raw_data$mutation_rate), method = "pearson")
corr_mut_spearman <- cor.test(log10(raw_data$lifespan), log10(raw_data$mutation_rate), method = "spearman")


# allometric regression between lifespan and mutation rate
Lm_mutationRate <- lm(log10(lifespan)~log10(mutation_rate), data = raw_data)
Lm_mutationRate
summary(Lm_mutationRate)

#save model predictions
predicted_Lm_mutationRate <- data.frame(mutationRate_pred_y = predict(Lm_mutationRate, raw_data, interval = "confidence"), mutationRate_pred_x=log10(raw_data$mutation_rate))

#plot with fitted allometric regression model line
mut_rate_Plot<- raw_data %>% ggplot2::ggplot(aes(x = log10(raw_data$mutation_rate), y = log10(raw_data$lifespan))) + 
  geom_point()+ 
  geom_label_repel(aes(label = common_name), size = 3., label.size = NA, max.overlaps = 15)+
  geom_line(color='red', data = predicted_Lm_mutationRate, aes(x=mutationRate_pred_x, y=mutationRate_pred_y.fit))+
  geom_ribbon(data = predicted_Lm_mutationRate, aes(ymin = mutationRate_pred_y.lwr, ymax = mutationRate_pred_y.upr, color = NULL), alpha = .15) +
  theme_classic()+
  xlab("Log10 Somatic mutation rate [SBS/genome/year]")+
  ylab("Log10 Lifespan [years]")+
  annotate("label", x=1.9, y=0.6, label= paste("y =", round(Lm_mutationRate$coefficients[2], 2), "x +", round(Lm_mutationRate$coefficients[1], 2),
                                                ", p-value =", round(summary(Lm_mutationRate)$coefficients[2,4],8),
                                                "\n R^2 =", round(summary(Lm_mutationRate)$r.squared, 2), ", adj R^2 =", round(summary(Lm_mutationRate)$adj.r.squared, 2),
                                                "\n Pearson: R =",  round(corr_life_mut$estimate, 2), ", p-value =",  round(corr_life_mut$p.value, 8)),size=3.)

print(mut_rate_Plot)


############ Analysis of male maturity with lifespan ###########################################
#calculate correlation
corr_life_male <- cor.test(log10(raw_data$lifespan),log10(raw_data$time_to_sexual_maturity_male),  method = "pearson")
corr_male_spearman <- cor.test(log10(raw_data$lifespan),log10(raw_data$time_to_sexual_maturity_male),  method = "spearman")

Lm_MaleSexMat <- lm(log10(lifespan)~log10(time_to_sexual_maturity_male), data = raw_data)
Lm_MaleSexMat
summary(Lm_MaleSexMat)

#save model predictions
predicted_Lm_MaleMat <- data.frame(MaleMat_pred_y = predict(Lm_MaleSexMat, raw_data, interval = "confidence"), MaleMat_pred_x=log10(raw_data$time_to_sexual_maturity_male))

#plot with fitted allometric regression model line
Male_Sex_Plot <- raw_data %>% ggplot2::ggplot(aes(x = log10(raw_data$time_to_sexual_maturity_male), y = log10(raw_data$lifespan))) + 
  geom_point()+ 
  geom_label_repel(aes(label = common_name), size = 3., label.size = NA, max.overlaps = 15)+
  geom_line(color='red', data = predicted_Lm_MaleMat, aes(x=MaleMat_pred_x, y=MaleMat_pred_y.fit))+
  geom_ribbon(data = predicted_Lm_MaleMat, aes(ymin = MaleMat_pred_y.lwr , ymax = MaleMat_pred_y.upr, color = NULL), alpha = .15) +
  theme_classic()+
  xlab("Log10 Male sexual maturity [days]")+
  ylab("Log10 Lifespan [years]")+
  annotate("label", x=2.05, y=1.85, label= paste("y =", round(Lm_MaleSexMat$coefficients[2], 2), "x +", round(Lm_MaleSexMat$coefficients[1], 2),
                                                ", p-value =", round(summary(Lm_MaleSexMat)$coefficients[2,4],6),
                                                "\n R^2 =", round(summary(Lm_MaleSexMat)$r.squared, 2), ", adj R^2 =", round(summary(Lm_MaleSexMat)$adj.r.squared, 2),
                                                "\n Pearson: R =",  round(corr_life_male$estimate, 2), ", p-value =",  round(corr_life_male$p.value, 6)),size=3.)

print(Male_Sex_Plot)
########### Analysis of Female Maturity with lifespan ################################################
corr_life_female <- cor.test(log10(raw_data$lifespan), log10(raw_data$time_to_sexual_maturity_female), method = "pearson")
corr_female_spearman <- cor.test(log10(raw_data$lifespan), log10(raw_data$time_to_sexual_maturity_female), method = "spearman")


#allometric regression = linear regression on log10
Lm_FemSexMat <- lm(log10(lifespan)~log10(time_to_sexual_maturity_female), data = raw_data)
Lm_FemSexMat
summary(Lm_FemSexMat)

#save model predictions
predicted_Lm_FemMat <- data.frame(FemMat_pred_y = predict(Lm_FemSexMat, raw_data, interval = "confidence"), FemMat_pred_x=log10(raw_data$time_to_sexual_maturity_female))

#plot with fitted allometric regression model line
Female_Sex_plot<- raw_data %>% ggplot2::ggplot(aes(x = log10(raw_data$time_to_sexual_maturity_female), y = log10(raw_data$lifespan))) + 
  geom_point()+ 
  geom_label_repel(aes(label = common_name), size = 3.0, label.size = NA, max.overlaps = 15)+
  geom_line(color='red', data = predicted_Lm_FemMat, aes(x=FemMat_pred_x, y=FemMat_pred_y.fit))+
  geom_ribbon(data = predicted_Lm_FemMat, aes(ymin = FemMat_pred_y.lwr , ymax = FemMat_pred_y.upr, color = NULL), alpha = .15) +
  theme_classic()+
  xlab("Log10 Female sexual maturity [days]")+
  ylab("Log10 Lifespan [years]")+
  annotate("label", x=2.015, y=1.87, label= paste("y =", round(Lm_FemSexMat$coefficients[2], 2), "x +", round(Lm_FemSexMat$coefficients[1], 2),
                                                ", p-value =", round(summary(Lm_FemSexMat)$coefficients[2,4],4),
                                                "\n R^2 =", round(summary(Lm_FemSexMat)$r.squared, 2), ", adj R^2 =", round(summary(Lm_FemSexMat)$adj.r.squared, 2),
                                                "\n Pearson: R =",  round(corr_life_female$estimate, 2), ", p-value =",  round(corr_life_female$p.value, 4)),size=3.)

print(Female_Sex_plot)
############## Create Grid plot for all traits ############
# Add Nulls in between plots and line of null plots
plot_grid(mut_rate_Plot,  NULL, Metabolic_Rate_Plot, 
          Litter_Size_Plot, NULL, Mass_Plot, 
          Heart_Plot, NULL, Female_Sex_plot, 
          lung_plot, NULL, Male_Sex_Plot, ncol=3, rel_widths = c(0.95,0.05,0.95), scale = 0.95)

### Correlation Analysis ###########################

####### Pearson Correlation bar plot for every phenotypic trait with Lifespan ########################
pearson_lifespan <- data.frame(corr_comp = c("Resting heart rate", "Respiratory rate", "Adult mass",
                                             "Som. mutation rate",
                                             "Male maturity", "Female maturity", "Litter size", "Mass-specific BMR"),
                               correlation_coeff = c(corr_life_heart$estimate, corr_life_lung$estimate, 
                                                     corr_life_weight$estimate, corr_life_mut$estimate,
                                                     corr_life_male$estimate, corr_life_female$estimate, corr_life_litter$estimate, corr_life_meta$estimate),
                               p_value = c(corr_life_heart$p.value, corr_life_lung$p.value,
                                            corr_life_weight$p.value, corr_life_mut$p.value,
                                            corr_life_male$p.value, corr_life_female$p.value, corr_life_litter$p.value, corr_life_meta$p.value)) %>% 
  dplyr::mutate(signi_star = case_when(p_value <= 0.001 ~ "***",
                                       p_value <= 0.01 ~ "**",
                                       p_value <= 0.05 ~ "*",
                                       p_value <= 0.1 ~ ".",
                                       p_value <= 1 ~ " "))


pearson_lifespan %>% ggplot(aes(x = reorder(corr_comp, correlation_coeff), y = correlation_coeff, fill = -log10(p_value)))+
  geom_bar(stat = "identity", width = 0.5)+
  scale_fill_gradient(low = "darkblue", high = "skyblue")+
  theme_classic()+
  ylab("Pearson correlation coefficient")+
  ylim(-1.2,1)+
  theme(
        text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1,
        size = 12, hjust = 1))+
  ggtitle("Pearson correlation of traits with lifespan")+
  annotate("text", x = pearson_lifespan$corr_comp, y = pearson_lifespan$correlation_coeff+(0.05*sign(pearson_lifespan$correlation_coeff)), 
           label = pearson_lifespan$signi_star, 
           size = 5, color = "black")


### Partial Pearson Correlations between all traits and lifespan corrected for mutation rate #############################

partial_heart <- pcor.test(log10(raw_data$heart_rate), log10(raw_data$lifespan), log10(raw_data$mutation_rate), method = "pearson")

partial_lung <- pcor.test(log10(raw_data$respiratory_rate) , log10(raw_data$lifespan) , log10(raw_data$mutation_rate), method = "pearson")

partial_mass <- pcor.test(log10(raw_data$adult_mass) , log10(raw_data$lifespan) , log10(raw_data$mutation_rate), method = "pearson")

partial_fem <- pcor.test(log10(raw_data$time_to_sexual_maturity_female) , log10(raw_data$lifespan) , log10(raw_data$mutation_rate), method = "pearson")

partial_male <- pcor.test(log10(raw_data$time_to_sexual_maturity_male) , log10(raw_data$lifespan) , log10(raw_data$mutation_rate), method = "pearson")

partial_litter <- pcor.test(raw_data$litter_size, log10(raw_data$lifespan), log10(raw_data$mutation_rate), method = "pearson")

partial_meta <- pcor.test(log10(raw_data$mass_specific_BMR), log10(raw_data$lifespan), log10(raw_data$mutation_rate),method = "pearson")



ctrl_Mut_lifespan <- data.frame(Var1 = c("Resting heart rate", "Respiratory rate", "Adult mass", "Female maturity",
                                           "Male maturity", "Litter size", "Mass-specific BMR"),
                                  partial_corr = c(partial_heart$estimate, partial_lung$estimate, 
                                                   partial_mass$estimate, partial_fem$estimate,
                                                   partial_male$estimate, partial_litter$estimate, partial_meta$estimate), 
                                  p_value = c(partial_heart$p.value, partial_lung$p.value, 
                                              partial_mass$p.value, partial_fem$p.value,
                                              partial_male$p.value, partial_litter$p.value, partial_meta$p.value)) %>%
  dplyr::mutate(Var2 = "lifespan")



## Create barplot for all traits corrected for somatic mutation rate
ctrl_Mut_lifespan <- ctrl_Mut_lifespan %>% 
  dplyr::mutate(signi_star = case_when(p_value <= 0.001 ~ "***",
                                       p_value <= 0.01 ~ "**",
                                       p_value <= 0.05 ~ "*",
                                       p_value <= 0.1 ~ ".",
                                       p_value <= 1 ~ " "))



ctrl_Mut_lifespan %>% ggplot(aes(x = reorder(Var1, partial_corr), y = partial_corr, fill = -log10(p_value)))+
  geom_bar(stat = "identity", width = 0.5)+
  scale_fill_gradient(low = "darkblue", high = "skyblue")+
  ylim(-1,1)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
 size = 12, hjust = 1))+
 ggtitle("Partial Pearson correlation of traits with lifespan")+
  ylab("Partial Pearson correlation")+
  annotate("text", x = ctrl_Mut_lifespan$Var1, y = ctrl_Mut_lifespan$partial_corr+(0.05*sign(ctrl_Mut_lifespan$partial_corr)), 
           label = ctrl_Mut_lifespan$signi_star, 
           size = 5, color = "black")

##################### Partial Spearman correlation of all the traits woth lifespan corrected for mutation rate #########################################
partial_heart_spearman <- pcor.test(log10(raw_data$heart_rate), log10(raw_data$lifespan), log10(raw_data$mutation_rate), method = "spearman")

partial_meta_spearman <- pcor.test(log10(raw_data$mass_specific_BMR), log10(raw_data$lifespan), log10(raw_data$mutation_rate), method = "spearman")

partial_litter_spearman <- pcor.test(raw_data$litter_size, log10(raw_data$lifespan), log10(raw_data$mutation_rate), method = "spearman")

partial_lung_spearman <- pcor.test(log10(raw_data$respiratory_rate), log10(raw_data$lifespan), log10(raw_data$mutation_rate), method = "spearman")

partial_female_spearman <- pcor.test(log10(raw_data$time_to_sexual_maturity_female), log10(raw_data$lifespan), log10(raw_data$mutation_rate), method = "spearman")

partial_mass_spearman <- pcor.test(log10(raw_data$adult_mass), log10(raw_data$lifespan), log10(raw_data$mutation_rate), method = "spearman")

partial_male_spearman <- pcor.test(log10(raw_data$time_to_sexual_maturity_male), log10(raw_data$lifespan), log10(raw_data$mutation_rate), method = "spearman")


########### Fit a model for lifespan with mutation rate and lifespan with mutation rate and heart beat
lm_heart_mut <- lm(log10(lifespan) ~ log10(heart_rate) + log10(mutation_rate), data = raw_data)
lm_heart_mut
summary(lm_heart_mut)
stats_mut_and_heart <- glance(lm_heart_mut)

Lm_life_mutationRate <- lm(log10(lifespan) ~ log10(mutation_rate), data = raw_data)
summary(Lm_life_mutationRate)
stats_mut_only <- glance(Lm_life_mutationRate)


stats_mut_and_heart
stats_mut_only

##### Create 3d plot for linear fit of lifespan with heart rate and mutation rate###############################################################
s3d <- scatterplot3d(x=log10(raw_data$mutation_rate),y=log10(raw_data$heart_rate),z=log10(raw_data$lifespan),
                     color="skyblue",pch = 20,angle=330,type = "h",box=FALSE,grid=F,xlab = "Log10 Somatic mutation rate [SBS/genome/year]",ylab="Log10 Resting heart reate [beats/minute]",zlab = "Log10 Lifespan [years]",main="Allometric fit of somatic mutation and heart rate with lifespan")
s3d$plane3d(lm_heart_mut, draw_lines = T)
s3d.coords <- s3d$xyz.convert(x=log10(raw_data$mutation_rate),y=log10(raw_data$heart_rate),z=log10(raw_data$lifespan))
text(s3d.coords$x,s3d.coords$y,labels = raw_data$common_name,cex = .5,pos = 4)


### Create supplementary table with pearson and spearman correlation coefficient and p_value for each phenotypic trait with lifespan
traits <- c("Somatic mutation rate", "Litter size", "Resting heart rate", "Respiratory rate","Mass-specific BMR", "Adult mass", "Female sexual maturity", "Male sexual maturity")
pearson_coefficients <- c(corr_life_mut$estimate, corr_life_litter$estimate, corr_life_heart$estimate,  corr_life_lung$estimate, corr_life_meta$estimate, corr_life_weight$estimate, corr_life_female$estimate, corr_life_male$estimate)
pearson_p_values <- c(corr_life_mut$p.value, corr_life_litter$p.value, corr_life_heart$p.value, corr_life_lung$p.value, corr_life_meta$p.value, corr_life_weight$p.value, corr_life_female$p.value, corr_life_male$p.value)
spearman_coefficients <- c(corr_mut_spearman$estimate, corr_litter_spearman$estimate, corr_heart_spearman$estimate, corr_lung_spearman$estimate, corr_meta_spearman$estimate,
                           corr_weight_spearman$estimate, corr_female_spearman$estimate, corr_male_spearman$estimate)
spearman_p_values <- c(corr_mut_spearman$p.value, corr_litter_spearman$p.value, corr_heart_spearman$p.value, corr_lung_spearman$p.value, corr_meta_spearman$p.value,
                       corr_weight_spearman$p.value, corr_female_spearman$p.value, corr_male_spearman$p.value)

pearson_p_adj <- p.adjust(pearson_p_values, method = "BH")
spearman_p_adj <- p.adjust(spearman_p_values, method = "BH")
correlations <- data.frame(traits, pearson_coefficients, pearson_p_values, pearson_p_adj, spearman_coefficients, spearman_p_values, spearman_p_adj)
correlations <- correlations %>% rename("Phenotypic trait" = "traits",
                                        "Pearson coefficient" = "pearson_coefficients",
                                        "Pearson p-value" = "pearson_p_values",
                                          "Pearson p_adj" = "pearson_p_adj",
                                        "Spearman coefficient" = "spearman_coefficients",
                                        "Spearman p-value" = "spearman_p_values",
                                        "Spearman p_adj" = "spearman_p_adj")

#### Create supplementary table for Partial Correlations
partial_traits <- c("Resting heart rate", "Litter size", "Respiratory rate", "Mass-specific BMR","Female sexual Maturity", "Adult Mass", "Male sexual maturity")
partial_pearson_coefficients <- c(partial_heart$estimate, partial_litter$estimate, partial_lung$estimate, partial_meta$estimate, partial_fem$estimate, partial_mass$estimate, partial_male$estimate)
partial_pearson_p_values <- c(partial_heart$p.value, partial_litter$p.value, partial_lung$p.value, partial_meta$p.value, partial_fem$p.value, partial_mass$p.value, partial_male$p.value)
partial_spearman_coefficients <- c(partial_heart_spearman$estimate, partial_litter_spearman$estimate, partial_lung_spearman$estimate,partial_meta_spearman$estimate, partial_female_spearman$estimate, partial_mass_spearman$estimate, partial_male_spearman$estimate)
partial_spearman_p_values <- c(partial_heart_spearman$p.value, partial_litter_spearman$p.value, partial_lung_spearman$p.value, partial_meta_spearman$p.value, partial_female_spearman$p.value, partial_mass_spearman$p.value, partial_male_spearman$p.value)
partial_pearson_p_adj <- p.adjust(partial_pearson_p_values, method = "BH")
partial_spearman_p_adj <- p.adjust(partial_spearman_p_values, method = "BH")
partial_correlations <- data.frame(partial_traits, partial_pearson_coefficients, partial_pearson_p_values, partial_pearson_p_adj, partial_spearman_coefficients, partial_spearman_p_values, partial_spearman_p_adj)
partial_correlations <- partial_correlations %>% rename("Phenotypic trait" = "partial_traits",
                                                        "Partial Pearson coefficient" = "partial_pearson_coefficients",
                                                        "Partial Pearson p-value" = "partial_pearson_p_values",
                                                        "Partial Pearson p_adj" = "partial_pearson_p_adj",
                                                        "Partial Spearman coefficient" = "partial_spearman_coefficients",
                                                        "Partial Spearman p-value" = "partial_spearman_p_values",
                                                        "Partial Spearman p_adj" = "partial_spearman_p_adj")

### Create supplements for linear fits
linear_models <- c("Somatic mutation rate", "Resting heart beat + Somatic mutation rate", "Litter size + Somatic mutation rate", "Respiratory rate + Somatic mutation rate", "Mass-specific BMR + Somatic mutation rate", "Female sexual maturity + Somatic mutation rate", "Adult mass  + Somatic mutation rate", "Male sexual maturity + Somatic mutation rate")
lm_meta_mut <- lm(log10(lifespan) ~ log10(mass_specific_BMR) + log10(mutation_rate),data = raw_data)
lm_litter_mut <- lm(log10(lifespan) ~ litter_size + log10(mutation_rate), data = raw_data)
lm_lung_mut <- lm(log10(lifespan) ~ log10(respiratory_rate) + log10(mutation_rate), data = raw_data)
lm_female_mut <- lm(log10(lifespan) ~ log10(time_to_sexual_maturity_female) + log10(mutation_rate), data = raw_data)
lm_mass_mut <- lm(log10(lifespan) ~ log10(adult_mass) +  log10(mutation_rate), data = raw_data)
lm_male_mut <- lm(log10(lifespan) ~ log10(time_to_sexual_maturity_male) + log10(mutation_rate), data = raw_data)

stats_mut_and_meta <- glance(lm_meta_mut)
stats_mut_and_litter <- glance(lm_litter_mut)
stats_mut_and_lung <- glance(lm_lung_mut)
stats_mut_and_female <- glance(lm_female_mut)
stats_mut_and_mass <- glance(lm_mass_mut)
stats_mut_and_male <- glance(lm_male_mut)

Adj_R_squared <- c(stats_mut_only$adj.r.squared, stats_mut_and_heart$adj.r.squared, stats_mut_and_litter$adj.r.squared,
                   stats_mut_and_lung$adj.r.squared, stats_mut_and_meta$adj.r.squared, stats_mut_and_female$adj.r.squared, stats_mut_and_mass$adj.r.squared, stats_mut_and_male$adj.r.squared)
AIC <- c(stats_mut_only$AIC, stats_mut_and_heart$AIC, stats_mut_and_litter$AIC, stats_mut_and_lung$AIC,  stats_mut_and_meta$AIC, stats_mut_and_female$AIC, stats_mut_and_mass$AIC, stats_mut_and_male$AIC)
BIC <- c(stats_mut_only$BIC, stats_mut_and_heart$BIC, stats_mut_and_litter$BIC, stats_mut_and_lung$BIC, stats_mut_and_meta$BIC, stats_mut_and_female$BIC, stats_mut_and_mass$BIC, stats_mut_and_male$BIC)
model_results <- data.frame(linear_models, Adj_R_squared, AIC, BIC)
model_results <- model_results %>% rename("Adjusted R-squared" = "Adj_R_squared", "Phenotypic traits included in model" = "linear_models")

## Create supplements table with all the previously computed information
write_xlsx(x=list("3a - Correlation analysis" = correlations, "3b - Partial correlation analysis" =  partial_correlations,"3c - Linear model fits" = model_results), "/Users/martin.meinel/Documents/Matters Arising/supplements_May.xlsx")