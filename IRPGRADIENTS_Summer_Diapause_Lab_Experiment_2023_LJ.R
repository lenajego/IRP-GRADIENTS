# Set-up #############################
## Import all packages ################

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

##################### LAB EXPERIMENT ###############################
# Import Full dataset ##########
# The dataset relating to the laboratory experiment will be available on Zenodo from May 4, 2027:

# Define the file access path
file_access_path <- "E:/PhD GRADIENT/Summer_Diapause/GCB/Zenodo_DataSet/Summer_Diapause_Lab_Experiment_2023_LJ_IRPGRADIENTS.csv"

# Import dataset
Protocol <- read.csv(
  file = file_access_path,
  header = TRUE,
  sep = ";",
  dec = ","
)

# Check that the import was completed successfully
view(Protocol)

# Explanation of column names
    # Code: Identifies the trial.
    # Essay: Refers to the repetition of the trial with new individuals for the maternal generation.
    # Code_generation: Combines the tested condition (A, B, or C) with the observed generation.
    # Condition: Indicates the tested thermal conditions:
        # A: Moderate warming (24±2°C)
        # B: Intensive warming (26±2°C)
        # C: Intensive warming with a heat stress (26±2°C(+6°C))
    # Female_identification_number: Identification number assigned to females of the maternal generation.
    # Generation: Observed offspring generation.
    # Offspring_status: Status of the offspring, which can take one of the following values:
        # Diapausing: Dissection of the mummy reveals a yellow diapausing larva.
        # Emergent: The individual emerged within the 20 days of development at 20°C, 16hL:8hD.
        # Dead: The individual died during larval development.
    # Diapausing_status: Diapause status of the offspring:
        # Diapausing: Dissection of the mummy reveals a yellow diapausing larva.
        # Non_diapausing: The offspring individual either died during larval development or emerged.
    # Oviposition_date: Date marking the end of oviposition (which lasts three days).
    # Mummification_date: Date when the mummy was found in the trial and isolated until emergence.
    # Emergence_date: Date of emergence.
    # Sex: Sex of the offspring individual.
    # Offspring_identification_number: Identification number of the offspring.
    # Development_time: Development time of offspring individuals that emerged during the trials.


# Check the names of the generations
unique(Protocol$Generation)

# Explore dataset ###########
# Calculate the number of female per generation and thermal tested conditions 
female_count <- Protocol %>%
  group_by(Condition, Generation, Female_identification_number) %>%
  summarise(count = 1)

tot_females <- female_count %>%
  group_by(Condition, Generation) %>%
  summarise(Total_females = sum(count))

print(tot_females)

## Graph
ggplot(tot_females, aes(x = Generation, y = Total_females, fill = Condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0("N=", Total_females)),  # Ajout de "N=" devant la valeur
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 5) +  
  labs(title = "N = number of females per thermal treatment and generation",
       x = "Generation",
       y = "Number of females",
       fill = "Thermal Treatment") +
  theme_minimal() +
  scale_fill_viridis_d(
    option = "D",  
    labels = c("A: 24 ± 2°C", "B: 26 ± 2°C", "C: 26±2 (+6°C)") ) +
  guides(fill = guide_legend(title = "Thermal Treatment"))

# Notes : For generation 2, there's not enough data
# Not enough females for treatment C (only 2) and none for treatment B
# Only generation 1 will be studied

## DIAPAUSE RATE FOR THE FIRST GENERATION ##################

# Select only the first generation beacause there is not enought data for the second
Protocol_F1 <- filter(Protocol, Generation == "F1")
summary(Protocol_F1)
Protocol_F1$Female_identification_number = as.factor(Protocol_F1$Female_identification_number)
view(Protocol_F1)

# Calculate diapause, mortality and emergence rates

Diapause_lab_F1 <- Protocol_F1%>%
  dplyr::group_by(Condition, Female_identification_number)%>%
  summarise(
    total_of_emergent=sum(na.omit(Offspring_status)=="emergent"), 
    total_of_dead=sum(na.omit(Offspring_status)=="dead"),
    total_of_diapausing=(sum(na.omit(Offspring_status)=="diapausing")),
    total_of_Nondiapausing=total_of_emergent+total_of_dead,
    Diapause = total_of_diapausing/(total_of_diapausing+total_of_emergent + total_of_dead),
    Emergence = total_of_emergent/(total_of_diapausing+total_of_emergent + total_of_dead),
    Mortality = total_of_dead/(total_of_diapausing+total_of_emergent + total_of_dead),
    Offspring =total_of_diapausing+total_of_Nondiapausing)

# Keep only females with more than three descendants
Diapause_lab_filtred_F1 <- filter(Diapause_lab_F1,Offspring > 3 )
print(Diapause_lab_filtred_F1)

female_count_off3 <- Diapause_lab_filtred_F1  %>%
  group_by(Condition, Female_identification_number) %>%
  summarise(count = 1)

tot_females_off3 <- female_count_off3  %>%
  group_by(Condition) %>%
  summarise(Total_females = sum(count))

print(tot_females_off3)



# Transform dataset to obtain four column "Conditions", "Female_identification_number", "Offspring_status" and "proportion"corresponding to the proportion of individuals in each "Offspring_status" category
Diapause_lab_gather_F1 <- gather(Diapause_lab_filtred_F1 [,c(1,2,7,8,9,10)], variable, proportion, -c(Condition,Female_identification_number, Offspring))
print(Diapause_lab_gather_F1)


N_counts_descendants <- Diapause_lab_filtred_F1 %>%
  group_by(Condition) %>%
  summarise(
    N_Diapausing = sum(total_of_diapausing, na.rm = TRUE),
    N_Emergence = sum(total_of_emergent, na.rm = TRUE),
    N_Mortality = sum(total_of_dead, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("N_"), names_to = "variable", values_to = "N") %>%
  mutate(variable = str_replace(variable, "N_Diapausing", "Diapause"),
         variable = str_replace(variable, "N_Emergence", "Emergence"),
         variable = str_replace(variable, "N_Mortality", "Mortality"))

print(N_counts_descendants)


# Calculate means and standard deviations
summary_stats_F1 <- Diapause_lab_gather_F1 %>%
  group_by(Condition, variable) %>%
  summarise(
    mean_proportion = mean(proportion),
    sd_proportion = sd(proportion),
    .groups = 'drop'
  ) %>%
  mutate(
    se_proportion = sd_proportion / sqrt(n()),
    ymin = mean_proportion - se_proportion,
    ymax = mean_proportion + se_proportion
  )

# labels allows to add manually the letters of statistical differences in the graph 
# you need first to do the stats (later in this script)
summary_stats_F1$variable <- fct_relevel(summary_stats_F1$variable, c("Diapause", "Emergence", "Mortality"))



# Fit the linear model
mod1 <- lm(proportion ~ Condition * variable, data = Diapause_lab_gather_F1)
summary(mod1)

# Least squares means
Diapause_country_season <- lsmeans(mod1, ~ Condition | variable)
summary(Diapause_country_season)

# Get significance letters
cld_results <- cld(Diapause_country_season, Letters = letters)
summary(cld_results)

# Convert to dataframe
significance_labels <- data.frame(
  Condition = cld_results$Condition,
  variable = cld_results$variable,
  label = as.character(cld_results$.group) # Ensure it's a character column
)

# Merge with summary stats
summary_stats_F1 <- left_join(summary_stats_F1, significance_labels, by = c("Condition", "variable"))

# Check if label column exists
print(summary_stats_F1)

# Barplot with statistical letters
ggplot(summary_stats_F1, aes(x = Condition, y = mean_proportion, fill = Condition)) +
  facet_wrap(~variable, ncol = 3) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, position = position_dodge(width = 0.9)) +
  # Ajouter les lettres avec meilleur alignement
  geom_text(aes(y = ymax + 0.05, label = label),  position = position_dodge(width = 0.9),  vjust = 0.4, hjust = 0.7, size = 5) +
  labs(
    title = "",
    x = "Thermal treatment",
    y = "Percentage"
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1.05)) +
  scale_fill_viridis_d(
    option = "D",  
    direction = -1,
    labels = c("A: 24 ± 2°C", "B: 26 ± 2°C", "C: 26±2 (+6°C)") ) +
  geom_text(
    data = N_counts_descendants,
    aes(x = Condition, y = 1.02, label = paste0("n=", N)),
    position = position_dodge(width = 0.9) )+
  scale_x_discrete(labels = c("24±2°C", "26±2°C", "26±2°C \n(+6°C)")) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )


## DETAILS OF THE STATICAL ANALYSE FOR THE FIRST GENERATION ###########
view(Diapause_lab_gather_F1)
# variable = diapausing, emerged or dead

anova_diap <- aov(Diapause ~ Condition, data = Diapause_lab_filtred_F1)
posthoc_diap <- TukeyHSD(anova_diap)

anova_diap
posthoc_diap

mod1 <- lm(proportion ~ Condition*variable, data = Diapause_lab_gather_F1)
plotresid(mod1) 
simulateResiduals(fittedModel=mod1, plot=T) 
shapiro.test(residuals(mod1)) 
summary(mod1)
r.squaredGLMM(mod1) 

dev_resid <- residuals(mod1, type = "deviance")
df_resid <- df.residual(mod1)
overdispersion <- sum(dev_resid^2) / df_resid
overdispersion 

Anova(mod1) 
Diapause_country_season <- lsmeans(mod1,~ Condition|variable)
contrast(Diapause_country_season, "pairwise") 


