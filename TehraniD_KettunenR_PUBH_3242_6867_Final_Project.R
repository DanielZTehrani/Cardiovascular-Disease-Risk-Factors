

### PUBH_3242/6867_Final_Project

# Load CVD data

setwd("C:/Users/Janne/Documents/PUBH6867/Final Project")
cvd_data <- read.csv("CVD Dataset.csv")
str(cvd_data)

library(ggplot2)
library(scales)
library(ggpubr)
library(ggVennDiagram)
library(dplyr)
library(tidyverse)
library(tidyr)


# Filter only the variables that will be used
cvd_filtered <- cvd_data %>% 
  select (Sex,Age,BMI,Total.Cholesterol..mg.dL.,Blood.Pressure.Category,
          Smoking.Status,Diabetes.Status,Systolic.BP,Diastolic.BP,
          CVD.Risk.Level,CVD.Risk.Score,Weight..kg.,Height..m.)

# Handle missing values by removing rows 
cvd_clean<- na.omit(cvd_filtered)

## 1026 observations of 13 variables


### Data quality assessment and verification of categorical variables.

### Figure 1.

## Association between VD Risk Score adn CVD Risk Level

# Box plot for the distribution of CVD risk scored by CVD risk category
# kruskal.test
kw_test <- kruskal.test(CVD.Risk.Score ~ CVD.Risk.Level, data = cvd_clean)

#cvd_clean <- cvd_clean %>%
#  mutate(CVD.Risk.Level = factor(CVD.Risk.Level,
#                                 levels = c("LOW", "INTERMEDIARY", "HIGH")))

ggplot(cvd_clean, aes(x = CVD.Risk.Level, y = CVD.Risk.Score, fill = CVD.Risk.Level)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 16, outlier.color = "black", linewidth = 0.4) +
  stat_compare_means(method = "kruskal.test", label.y = max(cvd_clean$CVD.Risk.Score, na.rm = TRUE) + 1) +
  theme_bw(base_size = 14) +
  labs(
    #title = "Association between CVD Risk Score and CVD Risk Level",
    #subtitle = "Distribution of CVD Risk Scores across CVD Risk Level Categories",
    x = "CVD Risk Level",
    y = "CVD Risk Score",
    fill = "CVD Risk Level"
  ) +
  scale_fill_manual(values = c(
    "LOW"          = "forestgreen",
    "INTERMEDIARY" = "gold",
    "HIGH"         = "coral"
  )) +
  theme(
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(face = "bold", hjust = 0.5, vjust = 1, size = 10),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "top",
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

## Eliminate title and subtitle for APA style

## There is a significant overlap between CVD risk level distribution of CVD risk score
## In this CAIR CVD 2025 dataset, CVD.Risk.level is not derived based on CVD risk score
## KW test not neccessay capture the overlap means, that categories are not clear cut
## Plot violin plot to visualize the distribution of CVD risk score across CVD risk level


 ### Figure 2.

## Distribution of CVD score across CVD risk level



ggplot(cvd_clean, aes(x = CVD.Risk.Level, y = CVD.Risk.Score, fill = CVD.Risk.Level)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  theme_bw(base_size = 14) +
  labs(
    #title = "Overlap of CVD Risk Scores Across Levels",
    x = "CVD Risk Level",
    y = "CVD Risk Score"
  ) +
  scale_fill_manual(values = c(
    "LOW"          = "forestgreen",
    "INTERMEDIARY" = "gold",
    "HIGH"         = "coral"
  )) +   
  theme(
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(face = "bold", hjust = 0.5, vjust = 1, size = 10),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "top",
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

# Validate consistency with Regression-back calculation

### BMI value and its category validation

cvd_clean <- cvd_clean %>%
  mutate(
    BMI_new = Weight..kg. / (Height..m.^2)
  )

# Compare with CAIR CVD BMI 
cvd_clean<- cvd_clean %>%
  mutate(
    BMI_diff = BMI_new - BMI 
  )

summary(cvd_clean$BMI_diff)

# Scatter plot for an association between recorded BMI and calculated BMI
ggplot(cvd_clean, aes(x = BMI, y = BMI_new)) +
  geom_point(alpha = 0.4, size = 3, color="steelblue") +
  labs(
    #title = "Validation of BMI values",
    #subtitle = "Associaiton between Recorded BMI and Calculated BMI with Height and Weight",
    x = "Recorded BMI",
    y = "Calculated BMI"
  ) +
  theme_minimal(base_size = 14) +  
  
  theme(    
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),      
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),   
    axis.title = element_text(face = "bold", size = 14),      
    axis.text = element_text(size = 12),    
    legend.title = element_text(face = "bold"),      
    legend.background = element_rect(fill = "white", color = NA),      
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),     
    panel.grid.minor = element_blank()
    ) 

## Miscalculation of BMI??? I'm getting a headache...

## BMI_new categorization based on HPA
cvd_clean <- cvd_clean %>%
  mutate(
    BMI_class = case_when(
      BMI_new < 18.5 ~ "Underweight",
      BMI_new >= 18.5 & BMI_new < 25.0 ~ "Healthy Weight",
      BMI_new >= 25.0 & BMI_new < 30.0 ~ "Overweight",
      BMI_new >= 30.0 ~ "Obesity",
      TRUE ~ "Unknown"
    ))

unique(cvd_clean$BMI_class)


### BP categories validation

cvd_clean <- cvd_clean %>%
  mutate(
    BP_class = case_when(
      Systolic.BP < 120 & Diastolic.BP < 80 ~ "Normal",
      Systolic.BP >= 120 & Systolic.BP <= 129 & Diastolic.BP < 80 ~ "Elevated",
      Systolic.BP >= 130 & Systolic.BP <= 139 | (Diastolic.BP >= 80 & Diastolic.BP <= 89) ~ "Hypertension Stage 1",
      Systolic.BP >= 140 | Diastolic.BP >= 90 ~ "Hypertension Stage 2",
      Systolic.BP >= 180 | Diastolic.BP >= 120 ~ "Hypertensive Crisis",
      TRUE ~ "Uncategorized"
    )
  )

# Check!
summary(cvd_clean$BP_cal)
unique(cvd_clean$BP_cal)

# Compare with CAIR CVD BP categories
cvd_clean <- cvd_clean %>%
  mutate(
    BP_match = Blood.Pressure.Category == BP_class
  )

summary(cvd_clean$BP_match)

## All 1026 obs. were TRUE
## We assumed that BP categories were stratified based on thresholds aligned with
## AHA/WHO guidelines, with verification conforming accuracy.



### Figure 3.

## Association between CVD risk level with Systolic BP, total Cholesterol, BMI, Age, Smoking, and diabetes

## Calculate proportions of CVD risk level within each diabetes status
# Grouped bar chart

Dia_prop <- cvd_clean %>%
  group_by(Diabetes.Status, CVD.Risk.Level) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Diabetes.Status) %>%
  mutate(prop = n / sum(n))

# Plot stacked bar plot (%)
RL1 <-ggplot(Dia_prop, aes(x = Diabetes.Status, y = prop, fill = CVD.Risk.Level)) +
  geom_bar(stat = "identity", width = 0.4, color = "black")+
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Proportion of Diatetes Status by CVD Risk Level",
    x = "Diabetes Status",
    y = "Proportion",
    fill = "CVD Risk Level"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("LOW"="forestgreen", "INTERMEDIARY" = "gold", "HIGH" = "coral")) +
  theme(
    plot.title = element_text(hjust = 0.3, face = "bold", size = 10),
    plot.subtitle = element_text(hjust = 0.3, size = 8, color = "gray40"),
    axis.text = element_text(size = 8, color = "black"),
    legend.position = "top",
    legend.title = element_blank()) 


## Calculate proportions of CVD risk level within smoking status
# Grouped bar chart

Smok_prop <- cvd_clean %>%
  group_by(Smoking.Status, CVD.Risk.Level) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Smoking.Status) %>%
  mutate(prop = n / sum(n))

# Plot stacked bar plot (%)
RL2 <-ggplot(Smok_prop, aes(x = Smoking.Status, y = prop, fill = CVD.Risk.Level)) +
  geom_bar(stat = "identity", width = 0.4, color = "black")+
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Proportion of Smoking Status by CVD Risk Level",
    x = "Smoking Status",
    y = "Proportion",
    fill = "CVD Risk Level"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("LOW"="forestgreen", "INTERMEDIARY" = "gold", "HIGH" = "coral")) +
  theme(
    plot.title = element_text(hjust = 0.3, face = "bold", size = 10),
    plot.subtitle = element_text(hjust = 0.3, size = 8, color = "gray40"),
    axis.text = element_text(size = 8, color = "black"),
    legend.position = "top",
    legend.title = element_blank()) 


## Calculate proportions of CVD risk level within BMI class
# Grouped bar chart

BMI_prop <- cvd_clean %>%
  group_by(BMI_class, CVD.Risk.Level) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(BMI_class) %>%
  mutate(prop = n / sum(n)) %>%   # <-- close here
  mutate(BMI_class = factor(BMI_class,
                            levels = c("Underweight", "Healthy Weight", "Overweight", "Obesity")))

# Plot stacked bar plot (%)
RL3 <-ggplot(BMI_prop, aes(x = BMI_class, y = prop, fill = CVD.Risk.Level)) +
  geom_bar(stat = "identity", width = 0.4, color = "black")+
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Proportion of BMI class by CVD Risk Level",
    x = "BMI Class",
    y = "Proportion",
    fill = "CVD Risk Level"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("LOW"="forestgreen", "INTERMEDIARY" = "gold", "HIGH" = "coral")) +
  theme(
    plot.title = element_text(hjust = 0.3, face = "bold", size = 10),
    plot.subtitle = element_text(hjust = 0.3, size = 8, color = "gray40"),
    axis.text = element_text(size = 8, color = "black"),
    legend.position = "top",
    legend.title = element_blank())



## Calculate proportions of CVD risk level within BMI class
# Grouped bar chart

BP_prop <- cvd_clean %>%
  group_by(BP_class, CVD.Risk.Level) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(BP_class) %>%
  mutate(prop = n / sum(n)) %>% 
  mutate(BP_class = factor(BP_class,
                           levels = c("Normal", "Elevated", "Hypertension Stage 1", 
                                      "Hypertension Stage 2", "Hypertensive Crisis")))

# Plot stacked bar plot (%)
RL4 <-ggplot(BP_prop, aes(x = BP_class, y = prop, fill = CVD.Risk.Level)) +
  geom_bar(stat = "identity", width = 0.4, color = "black")+
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Proportion of Blood Pressure class by CVD Risk Level",
    x = "Blood Pressure Class",
    y = "Proportion",
    fill = "CVD Risk Level"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("LOW"="forestgreen", "INTERMEDIARY" = "gold", "HIGH" = "coral")) +
  theme(
    plot.title = element_text(hjust = 0.3, face = "bold", size = 10),
    plot.subtitle = element_text(hjust = 0.3, size = 8, color = "gray40"),
    axis.text = element_text(size = 7, color = "black"),
    legend.position = "top",
    legend.title = element_blank())


### Check mean age for each Sex

cvd_clean %>%
  group_by(Sex) %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    sd_age   = sd(Age, na.rm = TRUE),
    n        = n()
  )


## CVD risk level with Age group

cvd_clean <- cvd_clean %>%
  mutate(
    Age_group = case_when(
      Age >= 20 & Age <= 39 ~ "20-39",
      Age >= 40 & Age <= 59 ~ "40-59",
      Age >= 60             ~ "60+",
      TRUE                  ~ "Unknown"
    )
  )

# Check!
summary(cvd_clean$Age_group)
unique(cvd_clean$Age_group)



Age_prop <- cvd_clean %>%
  group_by(Age_group, CVD.Risk.Level) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Age_group) %>%
  mutate(prop = n / sum(n)) %>% 
  mutate(Age_group = factor(Age_group,
                            levels = c("20-39", "40-59", "60+","Unknown")))

# Plot stacked bar plot (%)
RL5 <-ggplot(Age_prop, aes(x = Age_group, y = prop, fill = CVD.Risk.Level)) +
  geom_bar(stat = "identity", width = 0.4, color = "black")+
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Proportion of Age group by CVD Risk Level",
    x = "Age group",
    y = "Proportion",
    fill = "CVD Risk Level"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("LOW"="forestgreen", "INTERMEDIARY" = "gold", "HIGH" = "coral")) +
  theme(
    plot.title = element_text(hjust = 0.3, face = "bold", size = 10),
    plot.subtitle = element_text(hjust = 0.3, size = 8, color = "gray40"),
    axis.text = element_text(size = 8, color = "black"),
    legend.position = "top",
    legend.title = element_blank())



## CVD risk level and total cholesterol

# Categorize total cholesterol

cvd_clean <- cvd_clean %>%
  mutate(
    Chol_group = case_when(
      Total.Cholesterol..mg.dL. < 200 ~ "Lower Risk",
      Total.Cholesterol..mg.dL. >= 200 & Total.Cholesterol..mg.dL. <= 240 ~ "At-Risk",
      Total.Cholesterol..mg.dL. > 240 ~ "Higher Risk",
      TRUE ~ "Unknown"
    ),
    # Order the factor levels for plotting or tables
    Chol_group = factor(Chol_group,
                        levels = c("Lower Risk",
                                   "At-Risk",
                                   "Higher Risk"))
  )

# Check!
summary(cvd_clean$Chol_group)
unique(cvd_clean$Chol_group)


TChol_prop <- cvd_clean %>%
  group_by(Chol_group, CVD.Risk.Level) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Chol_group) %>%
  mutate(prop = n / sum(n)) %>% 
  mutate(Chol_group = factor(Chol_group,
                             levels = c("Lower Risk", "At-Risk", "Higher Risk","Unknown")))

# Plot stacked bar plot (%)
RL6 <- ggplot(TChol_prop, aes(x = Chol_group, y = prop, fill = CVD.Risk.Level)) +
  geom_bar(stat = "identity", width = 0.4, color = "black")+
  geom_text(aes(label = scales::percent(prop, accuracy = 0.1)),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Proportion of Total Cholesterol group by CVD Risk Level",
    x = "Total Cholesterol group",
    y = "Proportion",
    fill = "CVD Risk Level"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("LOW"="forestgreen", "INTERMEDIARY" = "gold", "HIGH" = "coral")) +
  theme(
    plot.title = element_text(hjust = 0.3, face = "bold", size = 10),
    plot.subtitle = element_text(hjust = 0.3, size = 8, color = "gray40"),
    axis.text = element_text(size = 8, color = "black"),
    legend.position = "top",
    legend.title = element_blank())


library(ggplot2)
library(patchwork)

# Add labels to each plot
RL1 <- RL1 + annotate("text", x = -Inf, y = Inf, label = "(a)", 
                      hjust = -0.1, vjust = 1.1, size = 5)
RL2 <- RL2 + annotate("text", x = -Inf, y = Inf, label = "(b)", 
                      hjust = -0.1, vjust = 1.1, size = 5)
RL3 <- RL3 + annotate("text", x = -Inf, y = Inf, label = "(c)", 
                      hjust = -0.1, vjust = 1.1, size = 5)
RL4 <- RL4 + annotate("text", x = -Inf, y = Inf, label = "(d)", 
                      hjust = -0.1, vjust = 1.1, size = 5)
RL5 <- RL5 + annotate("text", x = -Inf, y = Inf, label = "(e)", 
                      hjust = -0.1, vjust = 1.1, size = 5)
RL6 <- RL6 + annotate("text", x = -Inf, y = Inf, label = "(f)", 
                      hjust = -0.1, vjust = 1.1, size = 5)

# Combine plots RL1-RL6
combined <- (RL1 | RL2 | RL3) / (RL4 | RL5 | RL6)

# Collect legend to be one and remove titles for APA style
combined +
  plot_layout(guides = "collect") +
  plot_annotation(title = NULL) &
  theme(
    legend.position = "bottom",
    plot.title = element_blank()
  )


### Figure 4.

## CVD risk scores plotted  by total cholesterol, systolic blood pressure, BMI and age. 
## CVD risk score vs calculated BMI
# Scatter plot for an association between calculated BMI and CVD risk score

p1 <-ggplot(cvd_clean, aes(x = BMI_new, y =CVD.Risk.Score )) +
  geom_point(alpha = 0.4, size = 3, color="coral") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "solid", size = 0.7) +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 5) +
  labs(
    title = "Association between CVD Risk Score and BMI",
    x = "BMI",
    y = "CVD Risk Score"
  ) +
  theme_minimal(base_size = 14) +  
  theme(    
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),      
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),   
    axis.title = element_text(face = "bold", size = 14),      
    axis.text = element_text(size = 12),    
    legend.title = element_text(face = "bold"),      
    legend.background = element_rect(fill = "white", color = NA),      
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),     
    panel.grid.minor = element_blank()) 


## CVD risk score vs total cholesterol

p2 <-ggplot(cvd_clean, aes(x =Total.Cholesterol..mg.dL. , y = CVD.Risk.Score)) +
  geom_point(alpha = 0.4, size = 3, color = "forestgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "solid", size = 0.7) +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 5) +
  labs(
    title = "Association between CVD Risk Score and BMI",
    x = "Total Cholesterol (mg/dL)",
    y = "CVD Risk Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(    
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),      
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),   
    axis.title = element_text(face = "bold", size = 14),      
    axis.text = element_text(size = 12),    
    legend.title = element_text(face = "bold"),      
    legend.background = element_rect(fill = "white", color = NA),      
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),     
    panel.grid.minor = element_blank()) 


## CVD risk score vs Systolic BP

p3 <-ggplot(cvd_clean, aes(x =Systolic.BP, y = CVD.Risk.Score)) +
  geom_point(alpha = 0.4, size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "solid", size = 0.7) +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 5) +
  labs(
    title = "Association between CVD Risk Score and Systolic Blood Pressure",
    x = "Systolic Blood Pressure (mmHg)",
    y = "CVD Risk Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(    
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),      
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),   
    axis.title = element_text(face = "bold", size = 14),      
    axis.text = element_text(size = 12),    
    legend.title = element_text(face = "bold"),      
    legend.background = element_rect(fill = "white", color = NA),      
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),     
    panel.grid.minor = element_blank()) 


## CVD risk score vs Age

p4 <-ggplot(cvd_clean, aes(x =Age, y = CVD.Risk.Score)) +
  geom_point(alpha = 0.4, size = 3, color = "orchid4") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "solid", size = 0.7) +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 5) +
  labs(
    title = "Association between CVD Risk Score and Age",
    x = "Age",
    y = "CVD Risk Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(    
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),      
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),   
    axis.title = element_text(face = "bold", size = 14),      
    axis.text = element_text(size = 12),    
    legend.title = element_text(face = "bold"),      
    legend.background = element_rect(fill = "white", color = NA),      
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),     
    panel.grid.minor = element_blank()) 


# Add labels to each plot (a)-(d)
p2 <- p2 + annotate("text", x = -Inf, y = Inf, label = "(a)",
                    hjust = -0.1, vjust = 1.1, size = 5)
p3 <- p3 + annotate("text", x = -Inf, y = Inf, label = "(b)",
                    hjust = -0.1, vjust = 1.1, size = 5)
p1 <- p1 + annotate("text", x = -Inf, y = Inf, label = "(c)",
                    hjust = -0.1, vjust = 1.1, size = 5)
p4 <- p4 + annotate("text", x = -Inf, y = Inf, label = "(d)",
                    hjust = -0.1, vjust = 1.1, size = 5)

combined <- (p2 | p3) / (p1 | p4)

combined +
  plot_annotation(title = NULL) &     
  theme(
    plot.title = element_blank()
  )



### Figure 5.

### Histograph: cholesterol stratify by CVD risk level



ggplot(cvd_clean, aes(x = Total.Cholesterol..mg.dL., fill = CVD.Risk.Level)) +
  geom_histogram(position = "identity", alpha = 0.9, bins = 30, color = "black") +
  labs(
    title = "Distribution of Total Cholesterol Stratified by CVD Risk Level",
    x = "Total Cholesterol (mg/dL)",
    y = "Frequency",
    fill = "CVD Risk Level"
  ) +
  scale_fill_manual(values = c(
    "LOW"        = "forestgreen",
    "INTERMEDIARY" = "gold",
    "HIGH"       = "coral"
  ), drop = FALSE) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "top"
  )


### Figure 6.


# Line chart: association between CVD risk level and Sex

# Summarize counts and proportions of sex across CVD risk levels
sex_summary <- cvd_clean %>%
  group_by(CVD.Risk.Level, Sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(CVD.Risk.Level) %>%
  mutate(prop = n / sum(n),
         CVD.Risk.Level = factor(CVD.Risk.Level,
                                 levels = c("LOW", "INTERMEDIARY", "HIGH")))

# Line chart: proportion of sex groups across CVD risk levels
ggplot(sex_summary, aes(x = CVD.Risk.Level, y = prop, group = Sex, color = Sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Association Between CVD Risk Level and Sex Group",
    subtitle = "Proportion of Female and Male participants across CVD risk level",
    x = "CVD Risk Level",
    y = "Proportion within Risk Level",
    color = "Sex"
  ) +
  scale_color_manual(values = c("F" = "#CD5C5C", "M" = "#4682B4")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    legend.position = "top",
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )


### FIgure 7. 

#Separating Systolic BP into Clinical Categories
cvd_data <- cvd_data %>%  
  mutate(category_systolic = case_when(     
    
    Systolic.BP < 120  ~ "Normal",
    Systolic.BP >= 120 & Systolic.BP <= 129  ~ "Elevated",
    Systolic.BP >= 130 & Systolic.BP <= 139  ~ "Stage 1 HTN",
    Systolic.BP >= 140 & Systolic.BP <= 180 ~ "Stage 2 HTN",
    Systolic.BP > 180  ~ "Hypertensive Crisis",
    
    # Catch-all for unexpected cases
  ))


# Pie Chart of Systolic Blood Pressure Categories 
cvd_data$category_systolic <- factor(cvd_data$category_systolic, levels = c("Normal", "Elevated", "Stage 1 HTN", "Stage 2 HTN", "Hypertensive Crisis"))
category_systolic <- cvd_data %>%
  count(category_systolic)%>%  
  mutate(Percentage = n / sum(n) * 100,  # Calculate percentage         
         Label = paste0(category_systolic, " ", round(Percentage, 1), "%"))  # Format label
ggplot(category_systolic, aes(x = "", y = n, fill = category_systolic)) +  
  geom_bar(stat = "identity", width = 1) +  # Create bars (converted to pie slices)
  coord_polar("y", start = 0) +  # Convert to pie chart  
  labs(title = "Pie Chart of Systolic Blood Pressure Category Distribution", x = NULL, y = NULL) +  
  theme_void() +  # Remove axis and gridlines  
  scale_fill_manual(values = c("Normal" = "green", "Elevated" = "yellow" , "Stage 1 HTN" = "orange", "Stage 2 HTN" = "red", "Hypertensive Crisis" = "darkred" )) +  
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 4, color = "black")  # Add percentage labels


### Figure 8.

# Calculate mean risk score by Diabetes status
mean_vals <- cvd_clean %>%
  group_by(Diabetes.Status) %>%
  summarise(mean_score = mean(CVD.Risk.Score, na.rm = TRUE))

# Independent t-test
t_test <- t.test(CVD.Risk.Score ~ Diabetes.Status, data = cvd_clean, var.equal = FALSE)

# p-value
p_val <- signif(t_test$p.value, 3)

# Diatetes status group means
mean_N <- mean(cvd_clean$CVD.Risk.Score[cvd_clean$Diabetes.Status == "N"], na.rm = TRUE)
mean_Y <- mean(cvd_clean$CVD.Risk.Score[cvd_clean$Diabetes.Status == "Y"], na.rm = TRUE)

ggplot(cvd_clean, aes(x = CVD.Risk.Score,
                      fill = Diabetes.Status,
                      color = Diabetes.Status)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("N" = "steelblue", "Y" = "#CD5C5C"), 
                    labels = c("N" = "No-Diabetes", "Y" = "Diabetes"),
                    name = "Diabetes Status") +
  scale_color_manual(values = c("N" = "steelblue","Y" = "#CD5C5C"), 
                     labels = c("N" = "No-Diabetes", "Y" = "Diabetes"),
                     name = "Diabetes Status") +
  geom_vline(data = mean_vals,
             aes(xintercept = mean_score, color = Diabetes.Status),
             linetype = "dashed", size = 0.8) +
  labs(title = "The Distribution of Cardiovascular Risk Score Amongst Diabetes and Non-Diabetes",
       x = "Cardiovascular Risk Score",
       y = "Density") +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(size = 14, face = "bold"), 
    axis.text.x = element_text(face="bold", hjust = 1, vjust = 1),
    legend.position = "top",
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"), 
    panel.grid.minor = element_blank()
  ) +
  annotate("text", 
           x = mean(cvd_clean$CVD.Risk.Score) * 0.78,   
           y = max(density(cvd_clean$CVD.Risk.Score)$y) * 0.95,  
           label = paste0("t-test p = ", p_val,
                          "\nMean (No-Diabetes) = ", round(mean_N,2),
                          "\nMean (Diabetes) = ", round(mean_Y,2)),
           size = 5, fontface = "plain", hjust = 1)

# Independent t-test for comparing two independent groups, No-smoking and Smoking.
(t_test_result <- t.test(CVD.Risk.Score ~ Diabetes.Status, 
                         data = cvd_clean, 
                         var.equal = FALSE))  

### Figure 9.


# Calculate mean risk score by Diabetes status
mean_vals <- cvd_clean %>%
  group_by(Smoking.Status) %>%
  summarise(mean_score = mean(CVD.Risk.Score, na.rm = TRUE))

# Independent t-test
t_test <- t.test(CVD.Risk.Score ~ Smoking.Status, data = cvd_clean, var.equal = FALSE)

# p-value
p_val <- signif(t_test$p.value, 3)

# Smoking status group means
mean_N <- mean(cvd_clean$CVD.Risk.Score[cvd_clean$Smoking.Status == "N"], na.rm = TRUE)
mean_Y <- mean(cvd_clean$CVD.Risk.Score[cvd_clean$Smoking.Status == "Y"], na.rm = TRUE)

# Example annotation at the top of the plot
ggplot(cvd_clean, aes(x = CVD.Risk.Score,
                      fill =Smoking.Status,
                      color =Smoking.Status)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c("N" = "steelblue", "Y" = "orchid4"), 
                    labels = c("N" = "No-Smoking", "Y" = "Smoking"),
                    name = "Smoking Status") +
  scale_color_manual(values = c("N" = "steelblue","Y" = "orchid4"), 
                     labels = c("N" = "No-Smoking", "Y" = "Smoking"),
                     name = "Smoking Status") +
  geom_vline(data = mean_vals,
             aes(xintercept = mean_score, color =Smoking.Status),
             linetype = "dashed", size = 0.8) +
  labs(
#    title ="The Distribution of Cardiovascular Risk Score Amongst Smoking and Non-Smoking",
       x = "Cardiovascular Risk Score",
       y = "Density") +
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "gray90", color = "black"),
    strip.text = element_text(size = 14, face = "bold"), 
    axis.text.x = element_text(face="bold", hjust = 1, vjust = 1),
    legend.position = "top",
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"), 
    panel.grid.minor = element_blank()
  ) +
  annotate("text", 
           x = mean(cvd_clean$CVD.Risk.Score) * 0.78,   
           y = max(density(cvd_clean$CVD.Risk.Score)$y) * 0.95,  
           label = paste0("t-test p = ", p_val,
                          "\nMean (No-Smoking) = ", round(mean_N,2),
                          "\nMean (Smoking) = ", round(mean_Y,2)),
           size = 5, fontface = "plain", hjust = 1)

# Independent t-test for comparing two independent groups, No-smoking and Smoking.
(t_test_result <- t.test(CVD.Risk.Score ~ Smoking.Status, 
                         data = cvd_clean, 
                         var.equal = FALSE))  



### Figure 10.

# ANOVA: compare mean CVD risk score across age?
# One-way ANOVA: mean CVD risk score across age groups

library(ggplot2)
library(dplyr)

# Run ANOVA
anova_model <- aov(CVD.Risk.Score ~ Age_group, data = cvd_clean)
p_val <- summary(anova_model)[[1]][["Pr(>F)"]][1]
p_val_text <- paste0("ANOVA p = ", signif(p_val, 3))

# Boxplot + jitter
ggplot(
  dplyr::filter(cvd_clean, !is.na(Age_group)),
  aes(x = Age_group, y = CVD.Risk.Score, fill = Age_group)
) +
  geom_boxplot(alpha = 0.7, lwd = 0.4, outlier.color = "red") +   
  geom_jitter(width = 0.2, alpha = 0.2, size = 2, color = "black") +
  labs(
    #    title = "Distribution of Cardiovascular Risk Scores Across Age Groups",
    #    subtitle = paste("ANOVA test for mean differences\n", p_val_text),
    x = "Age Group",
    y = "Cardiovascular Risk Score",
    fill = "Age Group"
  ) +
  scale_fill_manual(values = c(
    "20-39" = "steelblue",
    "40-59" = "forestgreen",
    "60+"   = "tomato"
  )) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "top"
  )


### Figure 11.


### CVD risk score vs BMI_new by Sex: Interaction b/w BMI and Sex

cvd_clean$Sex <- factor(cvd_clean$Sex, levels = c("M", "F"))

# Fit linear model with interaction
summary(lm_int <- lm(CVD.Risk.Score ~ BMI_new * Sex, data = cvd_clean))

# p-value for the interaction term (BMI:Sex)
p_val <- summary(lm_int)$coefficients["BMI_new:SexF", "Pr(>|t|)"]


ggplot(cvd_clean, aes(x = BMI_new, y = CVD.Risk.Score, color = Sex)) +
  geom_point(alpha = 0.4, size = 3) +
  geom_smooth(method = "lm", se = TRUE, linetype = "solid", size = 1.2, alpha = 0.3) +
  scale_color_manual(values = c("M" = "steelblue", "F" = "orchid4")) +
  labs(
#    title = "Association Between CVD Risk Score and Body Mass Index by Sex",
#    subtitle = "Linear regression trend lines with 95% confidence intervals",
    x = "Body Mass Index (BMI)",
    y = "Cardiovascular Risk Score",
    color = "Sex"
  ) +
  annotate(
    "text",
    x = max(cvd_clean$BMI),
    y = max(cvd_clean$CVD.Risk.Score),
    label = paste0("Interaction p = ", signif(p_val, 3)),
    hjust = 1.1, vjust = 1,
    size = 4, color = "black"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )


### Table 1.

# Create summary table using gtsummary, that Professor Epsi suggested us to try them out. 
# We received email from her on Nov. 19th. --> check this out!

library(gtsummary)
library(tidyverse)
library(gt)

unique(cvd_clean$Sex)

cvd_clean <- cvd_clean %>%
  mutate(
    
    Age_group = case_when(
      Age >= 20 & Age <= 39 ~ "20-39",
      Age >= 40 & Age <= 59 ~ "40-59",
      Age >= 60 ~ "60+"
    ),
    
    BMI_class = case_when(
      BMI_new < 18.5 ~ "Underweight",
      BMI_new >= 18.5 & BMI_new < 25.0 ~ "Healthy Weight",
      BMI_new >= 25.0 & BMI_new < 30.0 ~ "Overweight",
      BMI_new >= 30.0 ~ "Obesity",
      TRUE ~ "Unknown"
    ),
    Chol_group = case_when(
      Total.Cholesterol..mg.dL. < 200 ~ "Lower Risk",
      Total.Cholesterol..mg.dL. >= 200 & Total.Cholesterol..mg.dL. <= 240 ~ "At-Risk",
      Total.Cholesterol..mg.dL. > 240 ~ "Higher Risk",
      TRUE ~ "Unknown"
    ),
    BP_class = case_when(
      Systolic.BP < 120 & Diastolic.BP < 80 ~ "Normal",
      Systolic.BP >= 120 & Systolic.BP <= 129 & Diastolic.BP < 80 ~ "Elevated",
      Systolic.BP >= 130 & Systolic.BP <= 139 | (Diastolic.BP >= 80 & Diastolic.BP <= 89) ~ "Hypertension Stage 1",
      Systolic.BP >= 140 | Diastolic.BP >= 90 ~ "Hypertension Stage 2",
      Systolic.BP >= 180 | Diastolic.BP >= 120 ~ "Hypertensive Crisis",
      TRUE ~ "Uncategorized"
    ),
    
    # Order factor level
    BP_class = factor(
      BP_class,
      levels = c("Normal", "Elevated", "Hypertension Stage 1", "Hypertension Stage 2")
    ),
    BMI_class = factor(
      BMI_class,
      levels = c("Underweight", "Healthy Weight", "Obesity", "Overweight")
    ), 
    CVD.Risk.Level = factor(CVD.Risk.Level,
                            levels = c("LOW", "INTERMEDIARY", "HIGH")
    ), 
    Chol_group = factor(
      Chol_group,
      level = c("Lower Risk", "At-Risk", "Higher")
    ),
    Diabetes.Status = factor(
      Diabetes.Status, 
      levels = c("Y" , "N")
    ), 
    Smoking.Status = factor(
      Smoking.Status, 
      levels = c("Y" , "N")
    ))


tbl <- cvd_clean %>%
  select(
    Sex, 
    Age_group, 
    CVD.Risk.Level, 
    BP_class,
    BMI_class, 
    Chol_group,
    Diabetes.Status,
    Smoking.Status,
    
  ) %>%
  tbl_summary(
    by = CVD.Risk.Level,
    missing = "no",
    label = list(
      Sex ~ "Sex",
      BP_class ~ "Blood Pressure Category",
      BMI_class ~ "Body Mass Index",
      Diabetes.Status ~ "Diabetes Status",
      Smoking.Status ~ "Smoking Status",
      Age_group ~ "Age",
      Chol_group ~ "Total Cholesterol"
    )
  ) %>%
  add_p() %>%
  add_overall() %>%
  add_n() %>%
  bold_labels()

# Save as image
tbl %>%
  as_gt() %>%
  gt::gtsave(filename = "table1.png")









