#################### SET-UP ##########################################################
## Import all packages ############################################

# Setting the automatic import function
import.package <- function(pkg){
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# List of packages to import
packages <- c(
  "gridExtra", "vegan", "ape", "ggplot2", "lavaan", "haven", "Hmisc", 
  "semPlot", "tidyverse", "broom", "dplyr", "tidyr", "viridis", "forcats", 
  "ggsignif", "emmeans", "ggpubr", "ggrepel", "factoextra", "lme4", "car", 
  "MuMIn", "RVAideMemoire", "lsmeans", "corrplot", "openxlsx", "multcomp", 
  "stats", "DHARMa", "glmmTMB", "FactoMineR", "ade4", "readr", "missMDA", 
  "Factoshiny", "nlme", "piecewiseSEM", "fitdistrplus", "betareg", 
  "cowplot", "multcompView", "semPlot"
)

# Run function for all packages
sapply(packages, import.package)

## Import all libraries ###########
# Graphics
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(ggsignif)
library(viridis)
library(cowplot)
library(corrplot)

# Data 
library(dplyr)
library(tidyr)
library(forcats)
library(tidyverse)
library(readr)
library(broom)

# Statistics
library(stats)
library(Hmisc)
library(car)
library(MuMIn)
library(RVAideMemoire)
library(multcomp)
library(multcompView)
library(fitdistrplus)
library(betareg)
library(emmeans)
library(lsmeans)

# Modeling
library(lme4)
library(nlme)
library(glmmTMB)
library(DHARMa)
library(piecewiseSEM)
library(lavaan)
library(semPlot)

# Multivariate analysis
library(vegan)
library(ade4)
library(FactoMineR)
library(factoextra)
library(Factoshiny)
library(missMDA)

# Misc
library(ape)
library(haven)
library(openxlsx)
library(gridExtra)




# Import dataset ####
# Define the file access path 
file_access_path2 <- # to complete with the file named "Summer_Diapause_Biotic_Abiotic.csv"

Biot_Abiot <- read.csv(
  file = file_access_path2,
  header = TRUE,
  sep = ";",
  dec = "."
)




# description of the dataset
# metadata
# field_year : code combining the field code name and the sampling year 
# year : year of sampling (Spring2022 or Spring2023)
# country : code for the sampling location along the gradient 
# BR: France-Brittany - near Rennes
# PI: France-Picardy - near Mesbrecourt 
# BE: Belgium - near Louvain-La-Neuve
# GE: Germany - near Wurtzburg
# CZ: Czech Republic - near Praha
# field : Identification of the field composed of the country code (BR, PI, BE, GE or CZ) and the field number or ID
# data related to the identification of the collected mummies    
# Parasitoid : Total number of parasitoids that emerged from the collected mummies
# Hyperparasitoid : Total number of hyperparasitoids that emerged from the collected mummies
# Diapausing : Total number of diapausing larvae from the collected mummies
# Emerged : Total number of emerged organisms (Parasitoid + Hyperparasitoids)
# Dead : Total number of non-emerged dead organisms
# Total_mummies : parasitoid + hyperparasitoid + diapausing + dead
# Emergence_rate : emerged/total_mummies
# Diapause_rate : diapausing/total_mummies
# Hyper_rate : hyperparasitoid/total_mummies
# Mortality_rate : dead/total_mummies
# data related to the aphid counting to assess their abundances and infestation rates
# Ab_RP : total abundance of R. padi 	
# Ab_SA : total abundance of S. avenae
# Ab_MD : total abundance of M. dirhodum
# Ab_aphid : Ab_RP + Ab_SA + Ab_MD
# Sum_plants : total number of plants on which aphids were counted	
# Infest_RP : Infestation rate of R. padi = number of plants infested by at least one R. padi / total number of sampled plants
# Infest_SA : Infestation rate of S. avenae = number of plants infested by at least one S. avenae / total number of sampled plants 
# Infest_MD : Infestation rate of M. dirhodum = number of plants infested by at least one M. dirhodum / total number of sampled plants
# Infest_aphid : Infestation rate = number of plants infested by at least one aphid / total number of sampled plants
# Prop_SA : Relative abundance of S. avenae = Ab_SA/Ab_aphid
# Prop_MD : Relative abundance of M. dirhodum = Ab_MD/Ab_aphid	
# Prop_RP : Relative abundance of R. padi = Ab_RP/Ab_aphid	
# data related to the assessment of parasitism on 50 infested plants 
# Aphids_parasitism : total number of aphids counted on 50 infested plants
# Mummies_parasitism : total number of mummies counted on 50 infested plants 
# parasitism_rate : parasitism rate computed on 50 infested plants = Aphids_parasitism/Mummies_parasitism
# climatic indices : raw data were obtained from ERA5 and indices were calculated over a time window beginning three weeks before the start of sampling and lasting until the end of sampling
# CDm24 : average number of consecutive days with maximum temperatures above 24°C 
# CDm26 : average number of consecutive days with maximum temperatures above 26°C 
# CDm32 : average number of consecutive days with maximum temperatures above 32°C
# Frq24 : frequency of days with maximum temperature above 24°C 
# Frq26 : frequency of days with maximum temperature above 26°C 
# Frq32 : frequency of days with maximum temperature above 32°C 
# Td24 : total number of days with maximum temperature above 24°C 
# Td26 : total number of days with maximum temperature above 26°C
# Td32 : total number of days with maximum temperature above 32°C
# Pmean : average precipitation +/- Pmean_sd
# Ptot : total precipitation 
# Tmean : average temperature +/- Tmean_sd	
# Tmax : maximum temperature
# Tmin : minimum temperature



# Abiotic variable selection ####

# Data set exploration

climate_exploration <- Biot_Abiot[,c("field_year","year","country","CDm24","CDm26","CDm32","Frq24","Frq26","Frq32","Tmean","Tmax","Pmean","Tmin")] 
head(climate_exploration)
climate_exploration$country <- fct_relevel(climate_exploration$country, c("BR", "PI", "BE", "GE", "CZ"))

# List of variables to represent
variables <- c("Tmin", "Tmax", "Tmean", "CDm24", "CDm26", "CDm32", "Frq24", "Frq26", "Frq32")

# Function to generate a boxplot for a given variable
plot_boxplot <- function(var) {
  ggplot(climate_exploration, aes(x = country, y = .data[[var]], fill = country)) +
    geom_boxplot() +
    facet_wrap(~ year) +  # Split by year
    labs(#title = paste("Distribution of", var, "by years and localities"),
      x = "Localities along the geographic gradient", y = var) +
    theme_minimal() +
    theme(legend.position = "none")  # Hide legend to avoid clutter
}

# Generate all graphs and store them in a list
plots <- lapply(variables, plot_boxplot)

# Display charts in batches (e.g., 2 x 4)
gridExtra::grid.arrange(grobs = plots, ncol = 3)

# The goal is to reduce the number of variables studied by selecting those that explain the greatest variability between fields and years.

Tab_ACP_abiotic = Biot_Abiot[,c("field_year","year","country","CDm24","CDm26","CDm32","Frq24","Frq26","Frq32","Tmean","Tmax","Pmean")] 
Tab_ACP_abiotic$country <- fct_relevel(Tab_ACP_abiotic$country, c("BR", "PI", "BE", "GE", "CZ"))
head(Tab_ACP_abiotic)

rownames(Tab_ACP_abiotic) <- Tab_ACP_abiotic$field_year
Tab_ACP_abiotic = Tab_ACP_abiotic[,-1] ## pass cle_combiened as the line name to be able to locate our individuals defined by a field_year
summary(Tab_ACP_abiotic)
Tab_ACP_abiotic$country = fct_relevel(Tab_ACP_abiotic$country, c("BR", "PI", "BE","GE","CZ"))
print(Tab_ACP_abiotic)
head(Tab_ACP_abiotic)



## correlation matrix
Tab_ACP_abiotic_cor= Tab_ACP_abiotic[,-c(1,2)]
summary(Tab_ACP_abiotic_cor)
Tab_ACP_abiotic_matcor = Tab_ACP_abiotic_cor # remove qualitative factors
matcor <- cor(Tab_ACP_abiotic_matcor)
corrplot(matcor, method = "number",  order = "hclust", type = "lower")


# PCA
library(FactoMineR)
library(factoextra)


head(Tab_ACP_abiotic)
res.pca =  PCA(Tab_ACP_abiotic, quali.sup = 1:2, scale.unit = TRUE, ncp = 5, graph = TRUE)
print(res.pca)

summary(res.pca, nbelements=Inf)
# eigenvalues output
eig.val = get_eigenvalue(res.pca) 
eig.val

# eigenvalue plot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

# correlation circle
fviz_pca_var(res.pca, col.var = "black")

# corrplot to see which dimension explains which variable / cos2()
library("corrplot")
var = get_pca_var(res.pca)
var
head(var$cos2, 4)


corrplot(var$cos2, is.corr=T)

summary(res.pca)
# histograms of cos2 values
fviz_cos2(res.pca, choice = "var", axes = 1:2) # Total cos2 of variables on Dim.1 and Dim.2

# correlation circle with variables colored by cos2 to assess how well each is represented on the dimensions
fviz_pca_var(res.pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) # repel = TRUE avoids text overlap


fviz_pca_ind(res.pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) # repel = TRUE avoids text overlap

library(stats)
unique(Tab_ACP_abiotic$key_loc)


fviz_pca_biplot(res.pca, axes = c(1,2),
                quali.sup = NULL,
                addEllipses = F, label = "var", repel = TRUE, 
                alpha.var ="contrib")  +
  geom_point(aes( color = Tab_ACP_abiotic$country, fill = Tab_ACP_abiotic$country, shape = Tab_ACP_abiotic$year), size =2.5)+ 
  scale_fill_manual(values = c("#CC0033","#cccc00","#339900","#3399FF","#cc33cc"))+
  scale_color_manual(values = c("#CC0033","#cccc00","#339900","#3399FF","#cc33cc"))+
  scale_shape_manual(values = c(21,25))



# Biotic variable selection ####

# the goal is to reduce the number of studied variables
Tab_ACP_biotic = Biot_Abiot[,c("field_year","year","country","Parasitism_rate", "Hyper_rate", "Infest_aphid", "Ab_SA", "Ab_RP", "Ab_MD")] 

head(Tab_ACP_biotic)


## pass cle_combiened as the line name to be able to locate our individuals defined by a field_year
TAB2 = Tab_ACP_biotic  ## select the subset with studied climatic data 
rownames(TAB2) <- TAB2$field_year
TAB2 = TAB2[,-1]
summary(TAB2)
TAB2$country = fct_relevel(TAB2$country, c("BR", "PI", "BE","GE","CZ"))
print(TAB2)
head(TAB2)



## correlation matrix
TAB_cor2= TAB2[,-c(1,2)]
summary(TAB_cor2)
TAB_matcor2 = TAB_cor2 # remove qualitative factors
matcor2 <- cor(TAB_matcor2)
corrplot(matcor2, method = "number",  order = "hclust", type = "lower")


# PCA

head(TAB2)
res.pca2 =  PCA(TAB2, quali.sup = 1:2, scale.unit = TRUE, ncp = 5, graph = TRUE)
print(res.pca2)

summary(res.pca2, nbelements=Inf)
# eigenvalues output
eig.val2 = get_eigenvalue(res.pca2) 
eig.val2

# eigenvalue plot
fviz_eig(res.pca2, addlabels = TRUE, ylim = c(0, 50))

# correlation circle
fviz_pca_var(res.pca2, col.var = "black")

# corrplot to see which dimension explains which variable / cos2()
library("corrplot")
var = get_pca_var(res.pca2)
var
head(var$cos2, 4)

corrplot(var$cos2, is.corr=T)

summary(res.pca2)
# histograms of cos2 values
fviz_cos2(res.pca2, choice = "var", axes = 1:2) # Total cos2 of variables on Dim.1 and Dim.2

# correlation circle with variables colored by cos2 to assess how well each is represented on the dimensions
fviz_pca_var(res.pca2, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) # repel = TRUE avoids text overlap


fviz_pca_ind(res.pca2, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) # repel = TRUE avoids text overlap


fviz_pca_biplot(res.pca2, axes = c(1,3),
                quali.sup = NULL,
                addEllipses = F, label = "var", repel = TRUE, 
                alpha.var ="contrib")  +
  geom_point(aes( color = TAB2$country, fill = TAB2$country, shape = TAB2$year), size =2)+ 
  scale_fill_manual(values = c("#CC0033","#cccc00","#339900","#3399FF","#cc33cc"))+
  scale_color_manual(values = c("#CC0033","#cccc00","#339900","#3399FF","#cc33cc"))+
  scale_shape_manual(values = c(21,25))


## STATISTICAL ANALYSES #############
data = Biot_Abiot
summary(data)

# DIAPAUSING RATE ################
# tested glm ######
glm_mod1 <- glm(cbind(Diapausing, Diapausing+Emerged) ~  Tmax + Parasitism_rate + Hyper_rate + Infest_aphid, data = data, family = binomial(link = "logit"), na.action = "na.fail")

glm_mod2 <- glm(cbind(Diapausing, Diapausing+Emerged) ~  Tmean + Parasitism_rate + Hyper_rate + Infest_aphid, data = data, family = binomial(link = "logit"), na.action = "na.fail")

glm_mod3 <- glm(cbind(Diapausing, Diapausing+Emerged) ~  Pmean + Parasitism_rate + Hyper_rate + Infest_aphid, data = data, family = binomial(link = "logit"), na.action = "na.fail")

glm_mod4 <- glm(cbind(Diapausing, Diapausing+Emerged) ~  Frq24 + Parasitism_rate + Hyper_rate + Infest_aphid, data = data, family = binomial(link = "logit"), na.action = "na.fail")

glm_mod5 <- glm(cbind(Diapausing, Diapausing+Emerged) ~  Frq26 + Parasitism_rate + Hyper_rate + Infest_aphid, data = data, family = binomial(link = "logit"), na.action = "na.fail")

glm_mod6 <- glm(cbind(Diapausing, Diapausing+Emerged) ~  Frq32 + Parasitism_rate + Hyper_rate + Infest_aphid, data = data, family = binomial(link = "logit"), na.action = "na.fail")

glm_mod7 <- glm(cbind(Diapausing, Diapausing+Emerged) ~  CDm24 + Parasitism_rate + Hyper_rate + Infest_aphid, data = data, family = binomial(link = "logit"), na.action = "na.fail")

glm_mod8 <- glm(cbind(Diapausing, Diapausing+Emerged) ~  CDm26 + Parasitism_rate + Hyper_rate + Infest_aphid, data = data, family = binomial(link = "logit"), na.action = "na.fail")

glm_mod9 <- glm(cbind(Diapausing, Diapausing+Emerged) ~  CDm32 + Parasitism_rate + Hyper_rate + Infest_aphid, data = data, family = binomial(link = "logit"), na.action = "na.fail")


# glmer tested models ##########

glmer_mod1 <- glmer(cbind(Diapausing, Diapausing+Emerged+Dead) ~  Tmax + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

glmer_mod2 <- glmer(cbind(Diapausing, Diapausing+Emerged+Dead) ~  Tmean + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

glmer_mod3 <- glmer(cbind(Diapausing, Diapausing+Emerged+Dead) ~  Pmean  + Parasitism_rate + Hyper_rate + Infest_aphid  + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

glmer_mod4 <- glmer(cbind(Diapausing, Diapausing+Emerged+Dead) ~  Frq24 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

glmer_mod5 <- glmer(cbind(Diapausing, Diapausing+Emerged+Dead) ~  Frq26 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

glmer_mod6 <- glmer(cbind(Diapausing, Diapausing+Emerged+Dead) ~  Frq32 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

glmer_mod7 <- glmer(cbind(Diapausing, Diapausing+Emerged+Dead) ~  CDm24 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

glmer_mod8 <- glmer(cbind(Diapausing, Diapausing+Emerged+Dead) ~  CDm26 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

glmer_mod9 <- glmer(cbind(Diapausing, Diapausing+Emerged+Dead) ~  CDm32 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")


mod_diap = glm_mod1 # SELECT MODEL

plotresid(mod_diap)

ggqqplot(residuals(mod_diap))

simulateResiduals(fittedmod_diapel= mod_diap, plot=T) 

shapiro.test(residuals(mod_diap)) 
r.squaredGLMM(mod_diap)

vif(mod_diap) # correlation between variables

summary(mod_diap) # summary for the full model 

diap_dev_resid <- residuals(mod_diap, type = "deviance")

diap_df_resid <- df.residual(mod_diap)
diap_overdispersion <- sum(diap_dev_resid^2) / diap_df_resid
diap_overdispersion 

Anova(mod_diap) # effect of country & year 


diap_glm_model_list <- list(glm_mod1, glm_mod2, glm_mod3, glm_mod4, glm_mod5, glm_mod6, glm_mod7, glm_mod8, glm_mod9)
diap_glmer_model_list <- list(glmer_mod1, glmer_mod2, glmer_mod3, glmer_mod4, glmer_mod5, glmer_mod6, glmer_mod7, glmer_mod8, glmer_mod9)


diap_model_avg <- model.avg(diap_glm_model_list)

diap_summarymodavg <- summary(diap_model_avg)

diap_summarymodavg

# Extract fixed effects with their standard errors
diap_coef_table <- diap_summarymodavg$coefmat.full
diap_coef_table <- as.data.frame(diap_coef_table)
diap_coef_table <- diap_coef_table[rownames(diap_coef_table) != "(Intercept)", ]  # Exclude intercept if needed
diap_coef_table$term <- rownames(diap_coef_table)
rownames(diap_coef_table) <- NULL

# Visualize fixed effects with ggplot2 without intercept
ggplot(diap_coef_table, aes(x = Estimate, y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = Estimate - `Std. Error`, xmax = Estimate + `Std. Error`), width = 0.2) +
  labs(title = "Fixed-effect estimates (no intercept)", x = "Term", y = "Estimate") +
  theme_minimal()

# Compute p-values
diap_coef_table$p_value <- 2 * pnorm(abs(diap_coef_table$Estimate / diap_coef_table$`Std. Error`), lower.tail = FALSE)

# Add a column for significance stars
diap_coef_table$significance <- ifelse(diap_coef_table$p_value < 0.001, "***",
                                       ifelse(diap_coef_table$p_value < 0.01, "**",
                                              ifelse(diap_coef_table$p_value < 0.05, "*", "")))
# Add a column for color based on significance
diap_coef_table$color <- ifelse(diap_coef_table$p_value < 0.05, "significant", "non_significant")


# Visualize fixed effects with ggplot2 without intercept
ggplot(diap_coef_table, aes(x = Estimate, y =term, color = color )) +
  geom_point() +
  geom_errorbar(aes(xmin = Estimate - `Std. Error`, xmax = Estimate + `Std. Error`), width = 0.2) +
  geom_text(aes(label = significance), vjust = -0.3, color = "red3") + # Add significance stars
  scale_color_manual(values = c("significant" = "red3", "non_significant" = "black")) +
  labs(title = "Fixed-effect estimates (no intercept)", x = "Estimates", y = "") +
  theme_minimal()




## MORTALITY RATE ##############
# glmer tested models ##########




mod10 <- glmer(cbind(Dead, Diapausing+Emerged+Dead) ~  Tmax + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod11 <- glmer(cbind(Dead, Diapausing+Emerged+Dead) ~  Tmean + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod12 <- glmer(cbind(Dead, Diapausing+Emerged+Dead) ~  Pmean  + Parasitism_rate + Hyper_rate + Infest_aphid  + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod13 <- glmer(cbind(Dead, Diapausing+Emerged+Dead) ~  Frq24 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod14 <- glmer(cbind(Dead, Diapausing+Emerged+Dead) ~  Frq26 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod15 <- glmer(cbind(Dead, Diapausing+Emerged+Dead)~  Frq32 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod16 <- glmer(cbind(Dead, Diapausing+Emerged+Dead) ~  CDm24 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod17 <- glmer(cbind(Dead, Diapausing+Emerged+Dead) ~  CDm26 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod18 <- glmer(cbind(Dead, Diapausing+Emerged+Dead) ~  CDm32 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")



mod_dead = mod10 # SELECT MODEL

plotresid(mod_dead)

ggqqplot(residuals(mod_dead))

simulateResiduals(fittedModel= mod_dead, plot=T) 

shapiro.test(residuals(mod_dead)) # 0.93
r.squaredGLMM(mod_dead)

vif(mod_dead) # variables are correlated 

summary(mod_dead) # summary for the full model 

dead_dev_resid <- residuals(mod_dead, type = "deviance")

dead_df_resid <- df.residual(mod_dead)
dead_overdispersion <- sum(dead_dev_resid^2) / dead_df_resid
dead_overdispersion # 3.12

Anova(mod_dead) # effect of country & year 


dead_model_list <- list(mod10, mod11, mod12, mod13, mod14, mod15, mod16, mod17, mod18)


dead_model_avg <- model.avg(dead_model_list)

dead_summarymodavg <- summary(dead_model_avg)

dead_summarymodavg


# Extract fixed effects with their standard errors
dead_coef_table <- dead_summarymodavg$coefmat.full
dead_coef_table <- as.data.frame(dead_coef_table)
dead_coef_table <- dead_coef_table[rownames(dead_coef_table) != "(Intercept)", ]  # Exclude intercept if needed
dead_coef_table$term <- rownames(dead_coef_table)
rownames(dead_coef_table) <- NULL

# Visualize fixed effects with ggplot2 without intercept
ggplot(dead_coef_table, aes(x = Estimate, y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = Estimate - `Std. Error`, xmax = Estimate + `Std. Error`), width = 0.2) +
  labs(title = "Fixed-effect estimates (no intercept)", x = "Term", y = "Estimate") +
  theme_minimal()

# Compute p-values
dead_coef_table$p_value <- 2 * pnorm(abs(dead_coef_table$Estimate / dead_coef_table$`Std. Error`), lower.tail = FALSE)

# Add a column for significance stars
dead_coef_table$significance <- ifelse(dead_coef_table$p_value < 0.001, "***",
                                       ifelse(dead_coef_table$p_value < 0.01, "**",
                                              ifelse(dead_coef_table$p_value < 0.05, "*", "")))
# Add a column for color based on significance
dead_coef_table$color <- ifelse(dead_coef_table$p_value < 0.05, "significant", "non_significant")


# Visualize fixed effects with ggplot2 without intercept
ggplot(dead_coef_table, aes(x = Estimate, y =term, color = color )) +
  geom_point() +
  geom_errorbar(aes(xmin = Estimate - `Std. Error`, xmax = Estimate + `Std. Error`), width = 0.2) +
  geom_text(aes(label = significance), vjust = -0.3, color = "red3") + # Add significance stars
  scale_color_manual(values = c("significant" = "red3", "non_significant" = "black")) +
  labs(title = "Fixed-effect estimates (no intercept)", x = "Estimates", y = "") +
  theme_minimal()



######## EMERGENCE RATE ############



mod19 <- glmer(cbind(Emerged, Diapausing+Emerged+Dead) ~  Tmax + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod20 <- glmer(cbind(Emerged, Diapausing+Emerged+Dead) ~  Tmean + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod21 <- glmer(cbind(Emerged, Diapausing+Emerged+Dead) ~  Pmean  + Parasitism_rate + Hyper_rate + Infest_aphid  + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod22 <- glmer(cbind(Emerged, Diapausing+Emerged+Dead) ~  Frq24 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod23 <- glmer(cbind(Emerged, Diapausing+Emerged+Dead) ~  Frq26 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod24 <- glmer(cbind(Emerged, Diapausing+Emerged+Dead)~  Frq32 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod25 <- glmer(cbind(Emerged, Diapausing+Emerged+Dead) ~  CDm24 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod26 <- glmer(cbind(Emerged, Diapausing+Emerged+Dead) ~  CDm26 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")

mod27 <- glmer(cbind(Emerged, Diapausing+Emerged+Dead) ~  CDm32 + Parasitism_rate + Hyper_rate + Infest_aphid + (1|year), data = data, family = binomial(link = "logit"), na.action = "na.fail")



emerg_mod = mod27 # SELECT MODEL

plotresid(emerg_mod)

ggqqplot(residuals(emerg_mod))

simulateResiduals(fittedModel= emerg_mod, plot=T) 

shapiro.test(residuals(emerg_mod)) # 0.93
r.squaredGLMM(emerg_mod)

vif(emerg_mod) # variables are correlated 

summary(emerg_mod) # summary for the full model 

emerg_dev_resid <- residuals(emerg_mod, type = "deviance")

emerg_df_resid <- df.residual(emerg_mod)
emerg_overdispersion <- sum(emerg_dev_resid^2) / emerg_df_resid
emerg_overdispersion 

Anova(emerg_mod) # effect of country & year 


emerg_model_list <- list(mod19, mod20, mod21, mod22, mod23, mod24, mod25, mod26, mod27)


emerg_model_avg <- model.avg(emerg_model_list)

emerg_summarymodavg <- summary(emerg_model_avg)

emerg_summarymodavg


# Extract fixed effects with their standard errors
emerg_coef_table <- emerg_summarymodavg$coefmat.full
emerg_coef_table <- as.data.frame(emerg_coef_table)
emerg_coef_table <- emerg_coef_table[rownames(emerg_coef_table) != "(Intercept)", ]  # Exclude intercept if needed
emerg_coef_table$term <- rownames(emerg_coef_table)
rownames(emerg_coef_table) <- NULL

# Visualize fixed effects with ggplot2 without intercept
ggplot(emerg_coef_table, aes(x = Estimate, y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = Estimate - `Std. Error`, xmax = Estimate + `Std. Error`), width = 0.2) +
  labs(title = "Fixed-effect estimates (no intercept)", x = "Term", y = "Estimate") +
  theme_minimal()

# Compute p-values
emerg_coef_table$p_value <- 2 * pnorm(abs(emerg_coef_table$Estimate / emerg_coef_table$`Std. Error`), lower.tail = FALSE)

# Add a column for significance stars
emerg_coef_table$significance <- ifelse(emerg_coef_table$p_value < 0.001, "***",
                                        ifelse(emerg_coef_table$p_value < 0.01, "**",
                                               ifelse(emerg_coef_table$p_value < 0.05, "*", "")))
# Add a column for color based on significance
emerg_coef_table$color <- ifelse(emerg_coef_table$p_value < 0.05, "significant", "non_significant")


# Visualize fixed effects with ggplot2 without intercept
ggplot(emerg_coef_table, aes(x = Estimate, y =term, color = color )) +
  geom_point() +
  geom_errorbar(aes(xmin = Estimate - `Std. Error`, xmax = Estimate + `Std. Error`), width = 0.2) +
  geom_text(aes(label = significance), vjust = -0.3, color = "red3") + # Add significance stars
  scale_color_manual(values = c("significant" = "red3", "non_significant" = "black")) +
  labs(title = "Fixed-effect estimates (no intercept)", x = "Estimates", y = "") +
  theme_minimal()



####### INDIRECT EFFECTS #############

## SEM ###########

view(data)

# Fit Diapause ##########
Fit1 <- 'Diapause_rate ~ Frq24 + CDm24  + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq24 + CDm24  + Pmean +  Parasitism_rate 
Parasitism_rate~ Frq24 +  CDm24  + Pmean + Hyper_rate
Hyper_rate~ Frq24 + CDm24 + Pmean 
'

Fit2 <- '
Diapause_rate ~ Frq26 + CDm26  + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq26 + CDm26  + Pmean +  Parasitism_rate 
Parasitism_rate~ Frq26 +  CDm26  + Pmean + Hyper_rate
Hyper_rate~ Frq26 + CDm26 + Pmean 
'

Fit3 <- '
Diapause_rate ~ Frq32 + CDm32  + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq32 + CDm32  + Pmean +  Parasitism_rate 
Parasitism_rate~ Frq32 +  CDm32  + Pmean + Hyper_rate
Hyper_rate~ Frq32 + CDm32 + Pmean 
'

Fit4 <- '
Diapause_rate ~ Frq24 + CDm24   +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq24 + CDm24   +  Parasitism_rate 
Parasitism_rate~ Frq24 +  CDm24   + Hyper_rate
Hyper_rate~ Frq24 + CDm24  
'

Fit5 <- '
Diapause_rate ~ Frq26 + CDm26  +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq26 + CDm26   +  Parasitism_rate 
Parasitism_rate~ Frq26 +  CDm26   + Hyper_rate
Hyper_rate~ Frq26 + CDm26 
'

Fit6 <- '
Diapause_rate ~ Frq32 + CDm32  +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq32 + CDm32  +  Parasitism_rate 
Parasitism_rate~ Frq32 +  CDm32   + Hyper_rate
Hyper_rate~ Frq32 + CDm32 
'

Fit7 <- '
Diapause_rate ~ Tmax + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~  Tmax + Pmean  +  Parasitism_rate 
Parasitism_rate~  Tmax + Pmean    + Hyper_rate
Hyper_rate~  Tmax + Pmean 
'

Fit8 <- '
Diapause_rate ~ Tmean + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~  Tmean + Pmean  +  Parasitism_rate 
Parasitism_rate~  Tmean + Pmean    + Hyper_rate
Hyper_rate~  Tmean + Pmean 
'

Fit9 <- '
Diapause_rate ~ Tmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~  Tmean  +  Parasitism_rate 
Parasitism_rate~  Tmean     + Hyper_rate
Hyper_rate~  Tmean 
'
Fit10 <- '
Diapause_rate ~ Tmax +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~  Tmax +   Parasitism_rate 
Parasitism_rate~  Tmax +  Hyper_rate
Hyper_rate~  Tmax '

# Fit Emergence ########## 
Fit11 <- 'Emergence_rate ~ Frq24 + CDm24  + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq24 + CDm24  + Pmean +  Parasitism_rate 
Parasitism_rate~ Frq24 +  CDm24  + Pmean + Hyper_rate
Hyper_rate~ Frq24 + CDm24 + Pmean 
'

Fit12 <- '
Emergence_rate ~ Frq26 + CDm26  + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq26 + CDm26  + Pmean +  Parasitism_rate 
Parasitism_rate~ Frq26 +  CDm26  + Pmean + Hyper_rate
Hyper_rate~ Frq26 + CDm26 + Pmean 
'

Fit13 <- '
Emergence_rate ~ Frq32 + CDm32  + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq32 + CDm32  + Pmean +  Parasitism_rate 
Parasitism_rate~ Frq32 +  CDm32  + Pmean + Hyper_rate
Hyper_rate~ Frq32 + CDm32 + Pmean 
'

Fit14 <- '
Emergence_rate ~ Frq24 + CDm24   +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq24 + CDm24   +  Parasitism_rate 
Parasitism_rate~ Frq24 +  CDm24   + Hyper_rate
Hyper_rate~ Frq24 + CDm24  
'

Fit15 <- '
Emergence_rate ~ Frq26 + CDm26  +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq26 + CDm26   +  Parasitism_rate 
Parasitism_rate~ Frq26 +  CDm26   + Hyper_rate
Hyper_rate~ Frq26 + CDm26 
'

Fit16 <- '
Emergence_rate ~ Frq32 + CDm32  +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq32 + CDm32  +  Parasitism_rate 
Parasitism_rate~ Frq32 +  CDm32   + Hyper_rate
Hyper_rate~ Frq32 + CDm32 
'

Fit17 <- '
Emergence_rate ~ Tmax + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~  Tmax + Pmean  +  Parasitism_rate 
Parasitism_rate~  Tmax + Pmean    + Hyper_rate
Hyper_rate~  Tmax + Pmean 
'

Fit18 <- '
Emergence_rate ~ Tmean + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~  Tmean + Pmean  +  Parasitism_rate 
Parasitism_rate~  Tmean + Pmean    + Hyper_rate
Hyper_rate~  Tmean + Pmean 
'

Fit19 <- '
Emergence_rate ~ Tmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~  Tmean  +  Parasitism_rate 
Parasitism_rate~  Tmean     + Hyper_rate
Hyper_rate~  Tmean 
'
Fit20 <- '
Emergence_rate ~ Tmax +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~  Tmax +   Parasitism_rate 
Parasitism_rate~  Tmax +  Hyper_rate
Hyper_rate~  Tmax '

## Fit Mortality ######

Fit21 <- 'Mortality_rate ~ Frq24 + CDm24  + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq24 + CDm24  + Pmean +  Parasitism_rate 
Parasitism_rate~ Frq24 +  CDm24  + Pmean + Hyper_rate
Hyper_rate~ Frq24 + CDm24 + Pmean 
'

Fit22 <- '
Mortality_rate ~ Frq26 + CDm26  + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq26 + CDm26  + Pmean +  Parasitism_rate 
Parasitism_rate~ Frq26 +  CDm26  + Pmean + Hyper_rate
Hyper_rate~ Frq26 + CDm26 + Pmean 
'

Fit23 <- '
Mortality_rate ~ Frq32 + CDm32  + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq32 + CDm32  + Pmean +  Parasitism_rate 
Parasitism_rate~ Frq32 +  CDm32  + Pmean + Hyper_rate
Hyper_rate~ Frq32 + CDm32 + Pmean 
'

Fit24 <- '
Mortality_rate ~ Frq24 + CDm24   +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq24 + CDm24   +  Parasitism_rate 
Parasitism_rate~ Frq24 +  CDm24   + Hyper_rate
Hyper_rate~ Frq24 + CDm24  
'

Fit25 <- '
Mortality_rate ~ Frq26 + CDm26  +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq26 + CDm26   +  Parasitism_rate 
Parasitism_rate~ Frq26 +  CDm26   + Hyper_rate
Hyper_rate~ Frq26 + CDm26 
'

Fit26 <- '
Mortality_rate ~ Frq32 + CDm32  +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~ Frq32 + CDm32  +  Parasitism_rate 
Parasitism_rate~ Frq32 +  CDm32   + Hyper_rate
Hyper_rate~ Frq32 + CDm32 
'

Fit27 <- '
Mortality_rate ~ Tmax + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~  Tmax + Pmean  +  Parasitism_rate 
Parasitism_rate~  Tmax + Pmean    + Hyper_rate
Hyper_rate~  Tmax + Pmean 
'

Fit28 <- '
Mortality_rate ~ Tmean + Pmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~  Tmean + Pmean  +  Parasitism_rate 
Parasitism_rate~  Tmean + Pmean    + Hyper_rate
Hyper_rate~  Tmean + Pmean 
'

Fit29 <- '
Mortality_rate ~ Tmean +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~  Tmean  +  Parasitism_rate 
Parasitism_rate~  Tmean     + Hyper_rate
Hyper_rate~  Tmean 
'
Fit30 <- '
Mortality_rate ~ Tmax +  Hyper_rate + Parasitism_rate + Infest_aphid
Infest_aphid ~  Tmax +   Parasitism_rate 
Parasitism_rate~  Tmax +  Hyper_rate
Hyper_rate~  Tmax '

# Test all models ###

# Load required packages
library(lavaan)
library(broom)
library(semPlot)
library(openxlsx)

# SEM best adjustment ###########

unique(data$year)
scale_data <- as.data.frame(scale(data[,c("Diapause_rate", "Emergence_rate", "Mortality_rate", # response variables
                                          "Hyper_rate", "Parasitism_rate", "Infest_aphid", # biotic factors
                                          "Frq24", "Frq26", "Frq32", "CDm24", "CDm26", "CDm32","Pmean", "Tmax", "Tmean", "Tmin")]))  # abiotic variables
summary(scale_data)

# select model of interest ##############
install.packages("semPlot")
library(semPlot)
name_model= Fit30

simplified_model <-  name_model
fit_simplified_model <- sem(simplified_model, data = scale_data)

fitMeasures(fit_simplified_model,c("chisq","df","pvalue","cfi","gfi","ifi","rmsea","EVCI","srmr"))
inspect(fit_simplified_model,what="r2")
AIC(fit_simplified_model)
summary(fit_simplified_model,standardized=T)
p <- semPaths(fit_simplified_model, "std", edge.label.cex = 1,
              rotation = 1, fade = TRUE, layout = "spring",
              optimizeLatRes = TRUE, residuals = FALSE)

# test all models 

# Path to save results
save_path=""


objets <- ls()

# Create a list to store the models
modele_list <- list()

# Loop through objects to retrieve those that start with "Fit"
for (objet in objets) {
  if (startsWith(objet, "Fit")) {
    modele_list[[objet]] <- get(objet)
  }
}


# Loop over model names in the list
for (name_model in names(modele_list)) {
  
  
  print(paste0("Je travaille avec le modele ", name_model))
  
  # Select the model of interest
  simplified_model <- modele_list[[name_model]]
  
  # Fit the model
  fit_simplified_model <- sem(simplified_model, data = df)
  
  # Inspect and save R²
  inspect_df <- inspect(fit_simplified_model, what = "r2")
  write.xlsx(inspect_df, file = paste0(save_path, "/", "Inspect_model_", name_model, ".xlsx"), sep = ",", rawnames = T)
  
  # Get and save the model summary
  summary_fit <- summary(fit_simplified_model, standardized = TRUE)
  
  # Extract regression parameters
  regressions <- parameterEstimates(fit_simplified_model, standardized = TRUE)
  regressions <- regressions[regressions$op == "~", ]
  
  # Convert to data frame and save
  regressions_df <- as.data.frame(regressions)
  write.xlsx(regressions_df, file = paste0(save_path, "/", "Summary_model_", name_model, ".xlsx"), rawnames = T)
  
  # Get fit statistics
  stats <- fitMeasures(fit_simplified_model, c("chisq", "df", "pvalue", "cfi", "gfi", "ifi", "rmsea", "EVCI", "srmr"))
  stats$aic <- AIC(fit_simplified_model)
  
  # Convert statistics to a data frame
  stats_df <- as.data.frame(t(as.data.frame(stats)))
  
  # Save statistics
  write.xlsx(stats_df, file = paste0(save_path, "/", "Statistiques_model_", name_model, ".xlsx"), rawnames = T)
  
  # Generate and save SEM path diagrams
  NOMpng <- paste0(save_path, "/", "Fig_semPaths_model_", name_model, ".png")
  png(file = NOMpng, width = 700, height = 700)
  p <- semPaths(fit_simplified_model, "std", edge.label.cex = 1,
                rotation = 1, fade = FALSE, layout = "spring",
                optimizeLatRes = FALSE, residuals = FALSE)
  print(p)
  dev.off()
  
} # end of the model loop



# selection of the best model among the best candidate models 
anova(fit_simplified_model1, fit_simplified_model8, fit_simplified_model4)  # Chi-square test
AIC(fit1, fit2)    # AIC comparison
BIC(fit1, fit2)    # BIC comparison


## correlation plots ############

create_regression_plot <- function(A, R, x_label, y_label, title) {
  data <- data.frame(A = A, R = R)
  
  # Fit the linear regression model
  model <- lm(R ~ A, data = data)
  
  # Extract the estimate, p-value and R²
  coef_estimate <- summary(model)$coefficients["A", "Estimate"]
  p_value <- summary(model)$coefficients["A", "Pr(>|t|)"]
  r_squared <- summary(model)$r.squared
  
  # Format values for annotation
  annotation_text <- paste0("Estimate: ", round(coef_estimate, 5), 
                            "\nP-value: ", round(p_value, 5), 
                            "\nR²: ", round(r_squared, 5))
  
  # Plot the relationship between A and R with regression line
  plot <- ggplot(data, aes(x = A, y = R)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    ggtitle(title) +
    xlab(x_label) +
    ylab(y_label) +
    annotate("text", x = Inf, y = Inf, label = annotation_text, 
             hjust = 1.1, vjust = 1.1, size = 4, color = "black")
  
  # Display the plot
  print(plot)
}

hist(log(data$Infest_aphid)+1)
shapiro.test(log(data$Infest_aphid)+1)

hist(data$Hyper_rate)
shapiro.test(data$Hyper_rate)

hist(log(data$Parasitism_rate)+1)
shapiro.test(log(data$Parasitism_rate)+1)

# R = Tmax, Tmean, Pmean, Frq24, Frq26, Frq32, CDm24, CDm26, CDm32

plot1 <- create_regression_plot(log(data$Infest_aphid)+1, data$Tmax, "Infest_aphid", "Tmax", "Infest_aphid ~ Tmax")
plot2 <- create_regression_plot(log(data$Infest_aphid)+1, data$Tmean, "Infest_aphid", "Tmean", "Infest_aphid ~ Tmean")
plot3 <- create_regression_plot(log(data$Infest_aphid)+1, data$Pmean, "Infest_aphid", "Pmean", "Infest_aphid ~ Pmean")
plot4 <- create_regression_plot(log(data$Infest_aphid)+1, data$Frq24, "Infest_aphid", "Frq24", "Infest_aphid ~ Frq24")
plot5 <- create_regression_plot(log(data$Infest_aphid)+1, data$Frq26, "Infest_aphid", "Frq26", "Infest_aphid ~ Frq26")
plot6 <- create_regression_plot(log(data$Infest_aphid)+1, data$Frq32, "Infest_aphid", "Frq32", "Infest_aphid ~ Frq32")
plot7 <- create_regression_plot(log(data$Infest_aphid)+1, data$CDm24, "Infest_aphid", "CDm24", "Infest_aphid ~ CDm24")
plot8 <- create_regression_plot(log(data$Infest_aphid)+1, data$CDm26, "Infest_aphid", "CDm26", "Infest_aphid ~ CDm26")
plot9 <- create_regression_plot(log(data$Infest_aphid)+1, data$CDm32, "Infest_aphid", "CDm32", "Infest_aphid ~ CDm32")

all_plots1 <- list(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9)
multiplot1 <- do.call(gridExtra::grid.arrange,  c(all_plots1, ncol = 3))
multiplot1

plot10 <- create_regression_plot(data$Hyper_rate, data$Tmax, "Hyper_rate", "Tmax", "Hyper_rate ~ Tmax")
plot11 <- create_regression_plot(data$Hyper_rate, data$Tmean, "Hyper_rate", "Tmean", "Hyper_rate ~ Tmean")
plot12 <- create_regression_plot(data$Hyper_rate, data$Pmean, "Hyper_rate", "Pmean", "Hyper_rate ~ Pmean")
plot13 <- create_regression_plot(data$Hyper_rate, data$Frq24, "Hyper_rate", "Frq24", "Hyper_rate ~ Frq24")
plot14  <- create_regression_plot(data$Hyper_rate, data$Frq26, "Hyper_rate", "Frq26", "Hyper_rate ~ Frq26")
plot15  <- create_regression_plot(data$Hyper_rate, data$Frq32, "Hyper_rate", "Frq32", "Hyper_rate ~ Frq32")
plot16  <- create_regression_plot(data$Hyper_rate, data$CDm24, "Hyper_rate", "CDm24", "Hyper_rate ~ CDm24")
plot17  <- create_regression_plot(data$Hyper_rate, data$CDm26, "Hyper_rate", "CDm26", "Hyper_rate ~ CDm26")
plot18  <- create_regression_plot(data$Hyper_rate, data$CDm32, "Hyper_rate", "CDm32", "Hyper_rate ~ CDm32")

all_plots2 <- list(plot10, plot11, plot12, plot13, plot14, plot15, plot16, plot17, plot18)
multiplot2 <- do.call(gridExtra::grid.arrange,  c(all_plots2, ncol = 3))
multiplot2

plot19 <- create_regression_plot(log(data$Parasitism_rate)+1, data$Tmax, "Parasitism_rate", "Tmax", "Parasitism_rate ~ Tmax")
plot20 <- create_regression_plot(log(data$Parasitism_rate)+1, data$Tmean, "Parasitism_rate", "Tmean", "Parasitism_rate ~ Tmean")
plot21 <- create_regression_plot(log(data$Parasitism_rate)+1, data$Pmean, "Parasitism_rate", "Pmean", "Parasitism_rate ~ Pmean")
plot22 <- create_regression_plot(log(data$Parasitism_rate)+1, data$Frq24, "Parasitism_rate", "Frq24", "Parasitism_rate ~ Frq24")
plot23 <- create_regression_plot(log(data$Parasitism_rate)+1, data$Frq26, "Parasitism_rate", "Frq26", "Parasitism_rate ~ Frq26")
plot24 <- create_regression_plot(log(data$Parasitism_rate)+1, data$Frq32, "Parasitism_rate", "Frq32", "Parasitism_rate ~ Frq32")
plot25 <- create_regression_plot(log(data$Parasitism_rate)+1, data$CDm24, "Parasitism_rate", "CDm24", "Parasitism_rate ~ CDm24")
plot26 <- create_regression_plot(log(data$Parasitism_rate)+1, data$CDm26, "Parasitism_rate", "CDm26", "Parasitism_rate ~ CDm26")
plot27 <- create_regression_plot(log(data$Parasitism_rate)+1, data$CDm32, "Parasitism_rate", "CDm32", "Parasitism_rate ~ CDm32")

all_plots3 <- list(plot19, plot20, plot21, plot22, plot23, plot24, plot25, plot26, plot27)
multiplot3 <- do.call(gridExtra::grid.arrange,  c(all_plots3, ncol = 3))
multiplot3

hist(sqrt(data$diapause_rate)+1)
shapiro.test(sqrt(data$diapause_rate)+1)

plot28 <- create_regression_plot(sqrt(data$diapause_rate)+1, data$Tmax, "diapause_rate", "Tmax", "diapause_rate ~ Tmax")
plot29 <- create_regression_plot(sqrt(data$diapause_rate)+1, data$Tmean, "diapause_rate", "Tmean", "diapause_rate ~ Tmean")
plot30 <- create_regression_plot(sqrt(data$diapause_rate)+1, data$Pmean, "diapause_rate", "Pmean", "diapause_rate ~ Pmean")
plot31 <- create_regression_plot(sqrt(data$diapause_rate)+1, data$Frq24, "diapause_rate", "Frq24", "diapause_rate ~ Frq24")
plot32 <- create_regression_plot(sqrt(data$diapause_rate)+1, data$Frq26, "diapause_rate", "Frq26", "diapause_rate ~ Frq26")
plot33 <- create_regression_plot(sqrt(data$diapause_rate)+1, data$Frq32, "diapause_rate", "Frq32", "diapause_rate ~ Frq32")
plot34 <- create_regression_plot(sqrt(data$diapause_rate)+1, data$CDm24, "diapause_rate", "CDm24", "diapause_rate ~ CDm24")
plot35 <- create_regression_plot(sqrt(data$diapause_rate)+1, data$CDm26, "diapause_rate", "CDm26", "diapause_rate ~ CDm26")
plot36 <- create_regression_plot(sqrt(data$diapause_rate)+1, data$CDm32, "diapause_rate", "CDm32", "diapause_rate ~ CDm32")

all_plots4 <- list(plot28, plot29, plot30, plot31, plot32, plot33, plot34, plot35, plot36)
multiplot4 <- do.call(gridExtra::grid.arrange,  c(all_plots4, ncol = 3))
multiplot4

plot31 <- create_regression_plot(sqrt(data$diapause_rate)+1, data$Parasitism_rate, "diapause_rate", "parasitsim_rate", "diapause_rate ~ parasitsim_rate")
plot32 <- create_regression_plot(sqrt(data$diapause_rate)+1, data$Hyper_rate, "diapause_rate", "Hyper_rate", "diapause_rate ~ Hyper_rate")
plot33 <- create_regression_plot(sqrt(data$diapause_rate)+1, data$Infest_aphid, "diapause_rate", "Infest_aphid", "diapause_rate ~ Infest_aphid")


all_plots5 <- list(plot31, plot32, plot33)
multiplot5 <- do.call(gridExtra::grid.arrange,  c(all_plots5, ncol = 3))
multiplot5



## INDIRECT EFFECTS : SPEARMAN ###########



### first we check correlation between variables 
x_list <- c("Tmax", "Tmean", "CDm32", "CDm26", "CDm24", "Frq32", "Frq26", "Frq24")
y_list <- c("diapause_rate", "Parasitism_rate", "Hyper_rate", "Infest_aphid")


# Initialize a list to store all plots
all_plots <- list()

# Loop to generate the plots
for (x_factor in x_list) {
  for (y_factor in y_list) {
    # Convert variables to numeric
    data[[x_factor]] <- as.numeric(as.character(data[[x_factor]]))
    data[[y_factor]] <- as.numeric(as.character(data[[y_factor]]))
    
    # Create the plot
    plot <- ggplot(data, aes(x = !!sym(x_factor), y = !!sym(y_factor))) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, fullrange = TRUE, color = "red") +
      stat_cor(method = "spearman", size = 3) +
      scale_y_continuous(labels = scales::percent_format()) +
      xlab(x_factor) +
      ylab(y_factor) +
      theme_bw() +
      theme(axis.text = element_text(size = 5),
            axis.title = element_text(size = 7),
            axis.text.x = element_text(size = 5),
            axis.text.y = element_text(size = 5))
    
    # Add the plot to the list
    all_plots[[paste(x_factor, y_factor, sep = "_")]] <- plot
  }
}

# Merge all plots into a single figure
multiplot <- do.call(gridExtra::grid.arrange,  c(all_plots, ncol = 8))

# Display the combined plot
print(multiplot)

### second check (biotic drivers vs Diapause_rate)
x_list2 <- c("Parasitism_rate", "Hyper_rate", "Aphid_Infest_rate")
y_list2 <- c("Diapause_rate")

# Initialize a list to store all plots
all_plots2 <- list()

# Loop to generate the plots
for (x_factor in x_list2) {
  for (y_factor in y_list2) {
    # Convert variables to numeric
    data[[x_factor]] <- as.numeric(as.character(data[[x_factor]]))
    data[[y_factor]] <- as.numeric(as.character(data[[y_factor]]))
    
    # Create the plot
    plot <- ggplot(data, aes(x = !!sym(x_factor), y = !!sym(y_factor))) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, fullrange = TRUE, color = "red") +
      stat_cor(method = "spearman", size = 3) +
      scale_y_continuous(labels = scales::percent_format()) +
      xlab(x_factor) +
      ylab(y_factor) +
      theme_bw() +
      theme(axis.text = element_text(size = 5),
            axis.title = element_text(size = 7),
            axis.text.x = element_text(size = 5),
            axis.text.y = element_text(size = 5))
    
    # Add the plot to the list
    all_plots2[[paste(x_factor, y_factor, sep = "_")]] <- plot
  }
}

# Merge all plots into a single figure
multiplot2 <- do.call(gridExtra::grid.arrange,  c(all_plots, ncol = 3))

# Display the combined plot
print(multiplot2)

# Create an empty data frame to store the results
results <- data.frame(variable1 = character(), variable2 = character(), rho = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# Loop to compute Spearman correlations and p-values
for (x_factor in x_list) {
  for (y_factor in y_list) {
    # Convert variables to numeric
    data[[x_factor]] <- as.numeric(as.character(data[[x_factor]]))
    data[[y_factor]] <- as.numeric(as.character(data[[y_factor]]))
    
    # Compute the Spearman correlation and p-value
    corr_result <- cor.test(data[[x_factor]], data[[y_factor]], method = "spearman")
    
    # Append results to the data frame
    results <- rbind(results, data.frame(variable1 = x_factor, variable2 = y_factor, 
                                         rho = corr_result$estimate, p_value = corr_result$p.value))
  }
}



