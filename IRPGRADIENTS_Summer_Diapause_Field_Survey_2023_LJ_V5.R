## SUMMER DIAPAUSE V5 ##########################

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
  "cowplot", "multcompView"
)

# Run function for all packages
sapply(packages, import.package)

## Import all library ###########
# Graphics
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(ggsignif)
library(viridis)
library(cowplot)
library(corrplot)
library(gridExtra)


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

# Modelisation
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

# Divers
library(ape)
library(haven)
library(openxlsx)
library(gridExtra)




## Import dataset #################################################
# The dataset relating to the field survey of aphid and parasitoids along a 1200km longitudinal climatic gradient in Europe will be available on Zenodo from May 4, 2027:

# Define the file access path
file_access_path1 <- "E:/PhD GRADIENT/Summer_Diapause/GCB/Zenodo_DataSet/Summer_Diapause_Mummies_Field_Survey_2023_LJ.csv"

# Import dataset
Identification <- read.csv(
  file = file_access_path1,
  header = TRUE,
  sep = ";",
  dec = ","
)

# Check that the import was completed successfully
#view(Identification)
summary(Identification)


# Identification: 
# Around a 100 mummies were collected in the field (45 min non-random search)
# Identification/dissection of mummy contents allowed to calculate diapause rates, mortality rate and hyperparasitsim rate 
# Warning to calculate diapause and mortality rate  we sum all data for one field/year (without taking in account the sessions) and then we calculate the rate

# Explanation of column names:
# country: Location of the sampling along the gradient 
# BR: France-Brittany - near Rennes
# PI: France-Picardy - near Mesbrecourt 
# BE: Belgium - near Louvain-La-Neuve
# GE: Germany - near Wurtzburg
# CZ: Czech-Republic - near Praha
# year: Year of sampling, which took place in spring 2022 or in spring 2023 between may and june at the same wheat growth across the studied location. There is a phenological delay from west to east. 
# field: Identification of the field which is composed by the counrty code (BR, PI, BE, GE or CZ) and the field number or identification
# mummy_number: correspond to the identification number of the mummy 
# sampling_type: indicate if the mummies where collected in the field (value = "field") or inside the rearing of aphid collected in the field (value = "lab")
# session: correspond to the identification code of the sampling session (time laps during which all field from one location were sampled)
# sampling_date: correspond to the date when the sampling was performed in the field 
# emergence_date: correspond to the date of parasitoid or hyperparasitoid emergence, if no parasitoid had emerged the date is remplace by "dissection" meaning the the mummy was dissected in order to identify a diapausing or dead individual 
# dissection_date: indicate de the dissection date
# aphid_species: correspond to the aphid species name identified according morphological critaria from the mummies (antena, cornicula and legs)
# M_dirhodum: for Metopolophium dirhodum 
# R_padi: for Rhopalosiphum padi 
# S_avenae: for Sitobion avenae
# NA: if the mummy was to destroyed and the aphid species not identifiable
# mummy_color: correspond to the mummy color which could help to identify some parasitoid species (value: glody, brown, black, light and praon which is not a color but a morphological criteria helping to identify parasitoid species)
# emerging_organism: indicate if the emerging organism is a primary parasitoid (PP) or an hyperparasitoid (HY). If dead  or diapausing the value is NA
# emerging_parasitoid_species: details the name of the primary paraistoid emerged.
# emerging_parasitoid: is an abreviation of the emerging_parasitoid_species to help for graphical representations (for exemple: trophic networks)
# emerging_hyperparasitoid_species: details the name of the hyperparaistoid emerged. 
# emerging_hyper: is an abreviation of the emerging_hyperparasitoid_species to help for graphical representations (for exemple: trophic networks)
# organism_statut: summary of the organism status (value: "dead", "diapausing", "PP", "HY")


summary(Identification)
unique(Identification$field)

# add one column full of one to count the number of mummies
Identification$count = 1

## Explore dataset #################################################

### Calculate the number of mummies per field and session ##########
mummies_count <- Identification %>%
  group_by(year, country, field, session) %>%
  summarise(total_mummies = sum(count))

#view(mummies_count)

### Calculate the number of mummies per field, all session together #######

### Sort in the order of the geographic distribution of studied locatlity along the gradient
Identification$country = fct_relevel(Identification$country, c("BR", "PI", "BE", "GE", "CZ"))

### Sort also the fields in the order of the geographic distribution of studied locatlity along the gradient
Identification <- Identification %>%
  mutate(
    field = factor(
      field,
      # on trie selon l'ordre défini pour country
      levels = Identification %>%
        arrange(country) %>%
        pull(field) %>%
        unique()
    )
  )

# Group by "year", "country" and "fields"
# We group all session together
mummies_count_field <- Identification %>%
  group_by(year, country, field) %>%
  summarise(total_mummies = sum(count))

#view(mummies_count_field)

mummies_count_field$country_year <- interaction(mummies_count_field$country, mummies_count_field$year)
mummies_count_field$country_year = fct_relevel(mummies_count_field$country_year, c("BR.2022", "BR.2023", 
                                                                                   "PI.2022", "PI.2023", 
                                                                                   "BE.2022", "BE.2023",
                                                                                   "GE.2022", "GE.2023",
                                                                                   "CZ.2022", "CZ.2023"))

# Palette avec nuances (foncé pour 2022, clair pour 2023)
# Chaque paire correspond à une région
cols <- c(
  "#990022", "#CC0033",   # Brittany : dark 2022, light 2023
  "#999900", "#cccc00",   # Picardy
  "#267300", "#339900",   # Belgium
  "#0066CC", "#3399FF",   # Germany
  "#800080", "#cc33cc"    # Czech Republic
)

# Graph 
ggplot(mummies_count_field, aes(x = field, y = total_mummies, fill = country_year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "",
       x = "Fields",
       y = "Number of collected mummies",
       fill = "Localities along \nthe geographic gradient \nin 2022 and 2023") +
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=6))

# PART 2: DIAPAUSE RATES IN 2022 AND 2023 PER LOCALITY ##################
# Use the dataset with only field with more than 10 mummies for all session together
## calculate the sampling size per location and year ###########
Tableau <- Identification %>%
  # Group the data by country, session, and field
  group_by(year, country) %>%
  # Calculate various summary statistics within each group
  summarise(
    Field = n_distinct(field), # calculate the number of fields
    Diapausing = sum(na.omit(organism_statut) == "diapausing"), # Sum of diapausing larvea
    Dead = sum(na.omit(organism_statut) == "dead"), # Sum of dead organisms
    Parasitoid = sum(na.omit(organism_statut)== "PP"), # Sum of emerged primary parasitoids
    Hyperparasitoid = sum(na.omit(organism_statut)== "Hy"), # Sum of emerged hyperparasitoids
    Emerged = Hyperparasitoid + Parasitoid, # Sum of emerged organism
    Mummies = Dead + Diapausing + Hyperparasitoid + Parasitoid, # Total number of mummies
  )


print(Tableau)


## Calculate rates per field, location and year#######

Diapause_rate <- Identification %>%
  # Group the data by country, session, and field
  group_by(year, country, field) %>%
  # Calculate various summary statistics within each group
  summarise(
    # Sum
    Diapausing = sum(na.omit(organism_statut) == "diapausing"), # Sum of diapausing larvea
    Dead = sum(na.omit(organism_statut) == "dead"), # Sum of dead organisms
    Parasitoid = sum(na.omit(organism_statut)== "PP"), # Sum of emerged primary parasitoids
    Hyperparasitoid = sum(na.omit(organism_statut)== "Hy"), # Sum of emerged hyperparasitoids
    Emerged = Hyperparasitoid + Parasitoid, # Sum of emerged organism
    Mummies = Dead + Diapausing + Hyperparasitoid + Parasitoid, # Total number of mummies
    # Rates
    Diapause_rate = Diapausing/Mummies, # Calculate the diapausing rate
    Mortality_rate = Dead/Mummies, # Calculate mortality rate
    Emergence_rate = Emerged/Mummies, # Calculate mortality rate
    Hyper_rate = Hyperparasitoid/Emerged, # Calculate hyperparasitism rate 
    # Calculate the abundance of each parasitized aphid species 
    SA = sum(na.omit(aphid_species)=="S_avenae"),
    MD = sum(na.omit(aphid_species)=="M_dirhodum"),
    RP = sum(na.omit(aphid_species)=="R_padi"), 
    # Calculate relative abundance of each parasitized aphid species 
    Ab_SA = SA/(MD+RP+SA), 
    Ab_MD = MD/(MD+RP+SA),
    Ab_RP = RP/(MD+RP+SA)
  )

#view(Diapause_rate)

### with standard deviation ###########
Diapause_rate_sd <- Diapause_rate %>%
  # Group the data by country, session, and field
  group_by(year, country) %>%
  # Calculate various summary statistics within each group
  summarise(
    mean_diapause_rate = mean(Diapause_rate),
    mean_emergence_rate = mean(Emergence_rate),
    mean_mortality_rate = mean(Mortality_rate),
    sd_diapause = sd(Diapause_rate),
    sd_emerged = sd(Emergence_rate),
    sd_dead = sd(Mortality_rate)
  ) 

view(Diapause_rate_sd)

### graph preparation #######

Diapause_graph <- gather(Diapause_rate[,c(1,2,3,10,11,12)],variable, proportion, -c(year, country, field) ) # select variables for the graph
summary(Diapause_graph)
Diapause_graph$year = as.factor(Diapause_graph$year)

# Reorder the levels of the 'variable' factor in 'Diapause_graph'.
Diapause_graph$variable <- factor(Diapause_graph$variable, levels = c("Diapause_rate", "Emergence_rate", "Mortality_rate"))
levels(Diapause_graph$variable)


### box plot with localities sorted in the order of the geographic gradient ##################

Global_diapause<-ggplot2::ggplot(Diapause_graph, aes(x = country, y =proportion, fill=year)) +
  geom_boxplot() +
  facet_wrap(~variable,nrow = 1, labeller = labeller(variable = c("Diapause_rate" = "Diapause", "Emergence_rate" = "Emergence rate", "Mortality_rate" = "Mortality rate")))+
  labs(x = "Localities along the European gradient", y = "Percentage") +
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format())+
  theme(axis.text = element_text(size =10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size=10),
        legend.title = element_text(size = 10),
        strip.text.x = element_text(size=10))+
  scale_fill_discrete(name = "Year: ", labels = c( "2022", "2023"))
plot(Global_diapause)

### box plot sorted in the order of the geographic gradient 
# note : The climatic gradient was defined in the script : IRPGRADIENTS_Summer_Diapause_Biot_Abiot_Factors_2023_LJ_V4


# For 2022
Diapause_graph2022 <- subset(Diapause_graph, year == "2022")
summary(Diapause_graph2022)

Diapause_graph2022$country <- fct_relevel(Diapause_graph2022$country, c("PI", "BR", "BE", "GE", "CZ"))

Global_diapause22 <- ggplot(Diapause_graph2022, aes(x = country, y = proportion, fill = country)) +
  geom_boxplot() +
  facet_wrap(~variable, nrow = 1, labeller = labeller(variable = c("global_diapause_rate" = "Diapause", "Emergence_rate" = "Emergence", "Mortality_rate" = "Mortality"))) +
  labs(x = "Localities of the longitudinal gradient in 2022", y = "Percentage ") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format()) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text.x = element_text(size = 10)) +
  scale_fill_manual(values = viridis(5), name = "Localities sorted by a climatic gradient \nof decreasing incidence of heat events: ", labels = c("Picardy", "Brittany", "Belgium", "Germany", "Czech Republic"))

plot(Global_diapause22)


# For 2023
Diapause_graph2023 <- subset(Diapause_graph, year == "2023")
summary(Diapause_graph2023)

Diapause_graph2023$country <- fct_relevel(Diapause_graph2023$country, c("PI", "BE", "BR", "GE", "CZ"))

Global_diapause23 <- ggplot(Diapause_graph2023, aes(x = country, y = proportion, fill = country)) +
  geom_boxplot() +
  facet_wrap(~variable, nrow = 1, labeller = labeller(variable = c("global_diapause_rate" = "Diapause", "Emergence_rate" = "Emergence", "Mortality_rate" = "Mortality"))) +
  labs(x = "Localities of the longitudinal gradient in 2023", y = "Percentage ") +
  theme_bw() +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text.x = element_text(size = 10)) +
  scale_fill_manual(values = viridis(5), name = "Localities sorted by a climatic gradient \nof decreasing incidence of heat events: ", labels = c("Picardy", "Belgium", "Brittany", "Germany", "Czech Republic"))

plot(Global_diapause23)

grid.arrange(Global_diapause22, Global_diapause23, ncol = 1)




## diferences between years and fields ? ###########################
### we use glm with a binomial distribution and a logit function 
### note : cbind(success, failure) 

summary(Diapause_rate)


## assses differences in diapause rate  between years and country
glm_diap <- glm(cbind(Diapausing, Diapausing+ Emerged + Dead) ~  country*year,
                family = binomial(link = "logit"), data = Diapause_rate) 


plotresid(glm_diap) # not ok 
simulateResiduals(fittedModel=glm_diap, plot=T) # no deviation detected 
vif(glm_diap) # there is interaction between year and country 
shapiro.test(residuals(glm_diap)) #W = 0.99 p-value=0.99 --> normality of residuals ok 
summary(glm_diap) # S23**
r.squaredGLMM(glm_diap) # correct

dev_resid <- residuals(glm_diap, type = "deviance")
df_resid <- df.residual(glm_diap)
overdispersion <- sum(dev_resid^2) / df_resid
overdispersion # 3.38

Anova(glm_diap) #effet country & year 
Diapause_comp <- lsmeans(glm_diap,~ country|year)
contrast(Diapause_comp, "pairwise") ### problem : Df = INF


## assses differences in emmergence rate  between years and country

glm_em <- glm(cbind(Emerged, Emerged + Diapausing + Dead) ~  country*year,
              family = binomial(link = "logit"), data = Diapause_rate) 


plotresid(glm_em) # not ok 
simulateResiduals(fittedModel=glm_em, plot=T) # no deviation detected 
vif(glm_em) # there is interaction between year and country 
shapiro.test(residuals(glm_em)) #W = 0.99 p-value=0.99 --> normality of residuals ok 
summary(glm_em) # S23**
r.squaredGLMM(glm_em) # correct

dev_resid <- residuals(glm_em, type = "deviance")
df_resid <- df.residual(glm_em)
overdispersion <- sum(dev_resid^2) / df_resid
overdispersion # 1.98

Anova(glm_em) #effet country & year 
Emergence_comp <- lsmeans(glm_em,~ country|year)
contrast(Emergence_comp, "pairwise") ### problem : Df = INF


## assses differences in mortality rate  between years and country

glm_dead <- glm(cbind(Dead, Emerged + Diapausing + Dead) ~  country*year,
                family = binomial(link = "logit"), data = Diapause_rate) 


plotresid(glm_dead) # not ok 
simulateResiduals(fittedModel=glm_dead, plot=T) # no deviation detected 
vif(glm_dead) # there is interaction between year and country 
shapiro.test(residuals(glm_dead)) #W = 0.99 p-value=0.99 --> normality of residuals ok 
summary(glm_dead) # S23**
r.squaredGLMM(glm_dead) # correct

dev_resid <- residuals(glm_dead, type = "deviance")
df_resid <- df.residual(glm_dead)
overdispersion <- sum(dev_resid^2) / df_resid
overdispersion # 5.64

Anova(glm_dead) #effet country & year 
Dead_comp <- lsmeans(glm_dead,~ country|year)
contrast(Dead_comp, "pairwise") ### problem : Df = INF

