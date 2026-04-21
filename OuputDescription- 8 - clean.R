# Process Item #8
# Description: Script to create descriptions of output metrics of Bayesian mixture models
# Author: Kate Nelson
# Date updated: 03/27/2024


# Install and load packages and data ----

library(pacman)
p_load(tidyverse, ggsci, ggpubr)

summ_table <- read.csv("SummaryTableCategories082025.csv")

length(distinct(summ_table, nmodels, metrics, crops, classifications, boundaries, scopes))


#Key for color uses from ColorBrewer ----

# General: #9e9ac8, #756bb1
# Neutral: darkgray
# General Contrasts: #404040, #bababa, #f4a582, #ca0020
                    #5e3c99, #b2abd2, #fdb863, #e66101
                    #ca0020, #f4a582, #92c5de, #0571b0
                    #7b3294, #c2a5cf, #a6dba0, #008837
# Wheat: #a6611a, #dfc27d
# Corn: #d95f0e, #fe9929, #fed98e
# Soy: #016c59, #1c9099, #67a9cf
# Alfalfa: #f768a1, #fbb4b9
# Hay: #74c476, #bae4b3

# AUC Summary plots ----

## boxplots to see range of AUC for a subset category relative to the full dataset AUC?

summ_table <- summ_table %>%
             filter(X  == 1) %>%
  mutate(ID = seq(1:nrow(.)),
         norm_area = area / median_effect_change)

## Simple AUC boxplots by subset -----

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area), 
               fill = "#bcbddc", varwidth = TRUE, lwd = 0.1, outlier.size = 0.5) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Mixture Model Subsets") +
  ylab("Area of 95% Credibility Interval")

ggsave("auc_boxplots.jpeg", width = 4, height = 4, units = "in")

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area), 
               fill = "#bcbddc", varwidth = TRUE, lwd = 0.1, outlier.size = 0) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  geom_point(data = summ_table[2:nrow(summ_table),], aes(x = reorder(SubsetBy, ID), y = area), size = 0.25) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))+
  xlab("Mixture Model Subsets") +
  ylab("Area of 95% Credibility Interval")

ggsave("auc_boxplots_wpoints.jpeg", width = 4, height = 4, units = "in")


## Simple Normalized AUC boxplots by subset -----

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = norm_area)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = norm_area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = norm_area), 
               fill = "#bcbddc", varwidth = TRUE, lwd = 0.1, outlier.size = 0.5) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = norm_area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(-300,1500)+
  xlab("Mixture Model Subsets") +
  ylab("Area of 95% Credibility Interval Normalized by Change in Median Effect")

ggsave("normalized_auc_boxplots.jpeg", width = 4, height = 4, units = "in")

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area), 
               fill = "#bcbddc", varwidth = TRUE, lwd = 0.1, outlier.size = 0) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  geom_point(data = summ_table[2:nrow(summ_table),], aes(x = reorder(SubsetBy, ID), y = area), size = 0.25) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))+
  xlab("Mixture Model Subsets") +
  ylab("Area of 95% Credibility Interval")

ggsave("auc_boxplots_wpoints.jpeg", width = 4, height = 4, units = "in")

## Boxplots split by specific changes in subset types ----


ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area, fill = classifications)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area, fill = boundaries)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area, fill = scopes)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area, fill = metrics)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area, fill = crops)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

#geom_point might be more valid

ggplot() +
  geom_point(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area, color = classifications)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_point(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area, color = boundaries)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_point(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area, color = scopes)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_point(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area, color = metrics)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_point(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area, color = crops)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))


## Aggregate the Subsets ----

# aggsumm_table <- summ_table %>%
#   mutate(nDim = ifelse(SubsetBy == "None", 0, str_count(SubsetBy, "-") + 1)) %>%
#   mutate(FirstDim = ifelse(SubsetBy == "Crop-Metric" | SubsetBy == "Crop-Metric-Classification" | 
#                              SubsetBy == "Crop-Metric-Classification-Scope" | 
#                              SubsetBy == "Crop-Metric-Classification-Boundaries", 
#                            SubsetBy, 
#                            str_extract(SubsetBy, "\\b\\w+"))) %>%
#   group_by(nDim, FirstDim) %>%
#   summarise(aveArea = mean(area)) %>%
#   mutate(Dimensions = paste0(FirstDim, "-", nDim))
# 
# agg_table <- summ_table %>%
#   mutate(nDim = ifelse(SubsetBy == "None", 0, str_count(SubsetBy, "-") + 1)) %>%
#   mutate(FirstDim = ifelse(SubsetBy == "Crop-Metric" | SubsetBy == "Crop-Metric-Classification" | 
#                              SubsetBy == "Crop-Metric-Classification-Scope" | 
#                              SubsetBy == "Crop-Metric-Classification-Boundaries", 
#                            SubsetBy, 
#                            str_extract(SubsetBy, "\\b\\w+"))) %>%
#   mutate(Dimensions = paste0(FirstDim, "-", nDim))

aggsumm_table <- summ_table %>%
  mutate(nDim = ifelse(SubsetBy == "None", 0, str_count(SubsetBy, "-") + 1)) %>%
  group_by(SubsetBy, nDim) %>%
  summarise(aveArea = mean(area)) %>%
  mutate(Dimensions = paste0(SubsetBy, "-", nDim))

agg_table <- summ_table %>%
  mutate(nDim = ifelse(SubsetBy == "None", 0, str_count(SubsetBy, "-") + 1)) %>%
  mutate(Dimensions = paste0(SubsetBy, "-", nDim))


ggplot() +
  geom_point(data = aggsumm_table, aes(x = reorder(Dimensions, aveArea), y = aveArea)) +
  geom_line(data = aggsumm_table, aes(x = reorder(Dimensions, aveArea), y = aveArea, group = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(data = aggsumm_table, aes(x = reorder(Dimensions, aveArea), y = aveArea, label = round(aveArea,2)),
            nudge_x = 0.4, nudge_y = 0.05) +
  xlab("Dimension on which Mixtures of Models Vary - # of Dims varied") +
  ylab("Average area of 95% Credibility Interval of different mixtures")
    


ggplot() +
  geom_col(data = aggsumm_table, aes(x = reorder(Dimensions, aveArea), y = aveArea), fill = "#bcbddc") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))+
  geom_text(data = aggsumm_table, aes(x = reorder(Dimensions, aveArea), y = aveArea, label = round(aveArea,2)),
            nudge_x = 0, nudge_y = 0.08, size = 2) +
  xlab("Dimension on which Mixtures of Models Vary - # of Dims varied") +
  ylab("Average area of 95% Credibility Interval ")

ggsave("auc_aggregated_avepoints.jpeg", width = 4, height = 4, units = "in")


ggplot() +
  geom_col(data = aggsumm_table %>% filter(str_detect(SubsetBy, "Crop|None")), aes(x = reorder(Dimensions, aveArea), y = aveArea), fill = "#bcbddc") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(data = aggsumm_table %>% filter(str_detect(SubsetBy, "Crop|None")), aes(x = reorder(Dimensions, aveArea), y = aveArea, label = round(aveArea,2)),
            nudge_x = 0, nudge_y = 0.08) +
  xlab("Dimension on which Mixtures of Models Vary - # of Dims varied") +
  ylab("Average area of 95% Credibility Interval of different mixtures")

ggsave("auc_aggregated_avepoints_cropspecific.jpeg", width = 4, height = 4, units = "in")


ggplot() +
  geom_col(data = aggsumm_table %>% filter(str_detect(SubsetBy, "Metric|None")), aes(x = reorder(Dimensions, aveArea), y = aveArea), fill = "#bcbddc") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_text(data = aggsumm_table %>% filter(str_detect(SubsetBy, "Metric|None")), aes(x = reorder(Dimensions, aveArea), y = aveArea, label = round(aveArea,2)),
            nudge_x = 0, nudge_y = 0.08) +
  xlab("Dimension on which Mixtures of Models Vary - # of Dims varied") +
  ylab("Average area of 95% Credibility Interval of different mixtures")

ggsave("auc_aggregated_avepoint_metricspecific.jpeg", width = 4, height = 4, units = "in")



ggplot() +
  geom_point(data = agg_table, aes(x = reorder(Dimensions, area), y = area)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Dimension on which Mixtures of Models Vary - # of Dims varied") +
  ylab("Area of 95% Credibility Interval of different mixtures")

ggplot() +
  geom_boxplot(data = agg_table, aes(x = reorder(Dimensions, area), y = area, fill = as.factor(nDim))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_brewer(palette = "Purples")+
  xlab("Dimension on which Mixtures of Models Vary - # of Dims varied") +
  ylab("Area of 95% Credibility Interval of different mixtures")

ggsave("auc_aggregated_boxplots.jpeg", width = 4, height = 4, units = "in")



ggplot() +
  geom_boxplot(data = agg_table %>% filter(str_detect(SubsetBy, "Crop|None")), aes(x = reorder(Dimensions, area), y = area, fill = as.factor(nDim))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_brewer(palette = "Purples")+
  xlab("Dimension on which Mixtures of Models Vary - # of Dims varied") +
  ylab("Area of 95% Credibility Interval of different mixtures")

ggsave("auc_aggregated_boxplots_cropspecificmixes.jpeg", width = 4, height = 4, units = "in")



ggplot() +
  geom_boxplot(data = agg_table %>% filter(str_detect(SubsetBy, "Metric|None")), aes(x = reorder(Dimensions, area), y = area, fill = as.factor(nDim))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_brewer(palette = "Purples")+
  xlab("Dimension on which Mixtures of Models Vary - # of Dims varied") +
  ylab("Area of 95% Credibility Interval of different mixtures")

ggsave("auc_aggregated_boxplots_metricspecificmixes.jpeg", width = 4, height = 4, units = "in")


## AUC across all models ----


summ_table <- summ_table %>%
  mutate(fullcond = paste0(crops, metrics, classifications, boundaries, scopes))


ggplot() +
  geom_point(data = summ_table[1, ], aes(x = reorder(ID, -area), y = area), size = 2) +
  geom_point(data = summ_table[2:nrow(summ_table),], aes(x = reorder(ID, -area), y = area), size = 0.25) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), panel.grid.major = element_blank())+
  xlab("Mixture Model Subsets") +
  ylab("Area of 95% Credibility Interval")


## AUC v. median effect ----


ggplot() +
  geom_point(data = summ_table[1, ], aes(x = area, y = median_effect_change), size = 2) +
  geom_point(data = summ_table[2:nrow(summ_table),], aes(x = area, y = median_effect_change), size = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), panel.grid.major = element_blank())+
  xlab("Area of 95% Credibility Interval") +
  ylab("Median Effect Change")


ggplot() +
  geom_point(data = summ_table[1, ], aes(x = area, y = median_effect_change), size = 2) +
  geom_point(data = summ_table[2:nrow(summ_table),], 
             aes(x = area, y = median_effect_change, color = SubsetBy), size = 1) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), panel.grid.major = element_blank())+
  xlab("Area of 95% Credibility Interval") +
  ylab("Median Effect Change")


## Simple Med Effect boxplots by subset -----

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = median_effect_change)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = median_effect_change), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = median_effect_change), 
               fill = "#bcbddc", varwidth = TRUE, lwd = 0.1, outlier.size = 0.5) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = median_effect_change), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Mixture Model Subsets") +
  ylab("Median Effect Change from 10th to 100th Percentile")

ggsave("medeffect_boxplots.jpeg", width = 4, height = 4, units = "in")

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = median_effect_change), 
               fill =  "#f1a340", varwidth = TRUE, lwd = 0.1, outlier.size = 0) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = median_effect_change), size = 2) +
  geom_point(data = summ_table[2:nrow(summ_table),], aes(x = reorder(SubsetBy, ID), y = median_effect_change), size = 0.25) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))+
  xlab("Mixture Model Subsets") +
  ylab("Median Effect Change from 10th to 100th Percentile")

ggsave("medeffect_boxplots_wpoints.jpeg", width = 4, height = 4, units = "in")

#significnat effects only
ggplot() +
  geom_boxplot(data = summ_table %>% filter(sig_difference == "TRUE"), aes(x = reorder(SubsetBy, ID), y = median_effect_change), 
               fill =  "#f1a340", varwidth = TRUE, lwd = 0.1, outlier.size = 0) +
  geom_point(data = summ_table[1, ] %>% filter(sig_difference == "TRUE"), aes(x = reorder(SubsetBy, ID), y = median_effect_change), size = 2) +
  geom_point(data = summ_table[2:nrow(summ_table), ]%>% filter(sig_difference == "TRUE"), aes(x = reorder(SubsetBy, ID), y = median_effect_change), size = 0.25) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))+
  xlab("Mixture Model Subsets") +
  ylab("Median Effect Change from 10th to 100th Percentile")

ggsave("medeffect_boxplots_wpoints_sigonly.jpeg", width = 4, height = 4, units = "in")


#significnat effects only no PRD
summ_table %>% filter(sig_difference == "TRUE" & metrics != "PRD") %>% glimpse()

ggplot() +
  geom_boxplot(data = summ_table %>% filter(sig_difference == "TRUE" & metrics != "PRD"), aes(x = reorder(SubsetBy, ID), y = median_effect_change), 
               fill =  "#f1a340", varwidth = TRUE, lwd = 0.1, outlier.size = 0) +
   geom_point(data = summ_table[2:nrow(summ_table), ]%>% filter(sig_difference == "TRUE"& metrics != "PRD"), 
              aes(x = reorder(SubsetBy, ID), y = median_effect_change, pch = crops), size = 0.5) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 5), legend.title = element_blank(),
        legend.margin = margin(-1,-1,-1,-1),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 5, vjust = 1.1),
        axis.title = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank())+
  xlab("Mixture Model Subsets") +
  ylab("Median Effect Change \nfrom 10th to 100th Percentile")

ggsave("medeffect_boxplots_wpoints_sigonly_noPRD.jpeg", width = 4, height = 4, units = "in")


ggplot() +
  geom_boxplot(data = agg_table, aes(x = reorder(Dimensions, median_effect_change), y = median_effect_change, 
                                     fill = as.factor(nDim))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_brewer(palette = "Purples")+
  xlab("Dimension on which Mixtures of Models Vary - # of Dims varied") +
  ylab("Median Effect Change from 10th to 100th Percentile")

ggsave("medeffect_aggregated_boxplots.jpeg", width = 4, height = 4, units = "in")


ggplot() +
  geom_point(data = summ_table[1, ], aes(x = reorder(ID, -median_effect_change), y = median_effect_change), size = 2) +
  geom_point(data = summ_table[2:nrow(summ_table),], aes(x = reorder(ID, -median_effect_change), y = median_effect_change), size = 0.25) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), panel.grid.major = element_blank())+
  xlab("Mixture Model Subsets") +
  ylab("Median Effect Change from 10th to 100th Percentile")


## Simple AUC/Med Effect boxplots by subset -----

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area/median_effect_change)) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area/median_effect_change), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area/median_effect_change), 
               fill =  "#f1a340", varwidth = TRUE, lwd = 0.1, outlier.size = 0.5) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area/median_effect_change), size = 2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Mixture Model Subsets") +
  ylab("Area of the 95% credibility interval normalized by Median Effect Change")

ggsave("auc_medeffect_boxplots.jpeg", width = 4, height = 4, units = "in")

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area/median_effect_change), 
               fill = "#bcbddc", varwidth = TRUE, lwd = 0.1, outlier.size = 0) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area/median_effect_change), size = 2) +
  geom_point(data = summ_table[2:nrow(summ_table),], aes(x = reorder(SubsetBy, ID), y = area/median_effect_change), size = 0.25) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Mixture Model Subsets") +
  ylab("Area of the 95% credibility interval normalized by Median Effect Change")

ggsave("auc_medeffect_boxplots_wpoints.jpeg", width = 4, height = 4, units = "in")


ggplot() +
  geom_boxplot(data = agg_table, aes(x = reorder(Dimensions, area/median_effect_change), y = area/median_effect_change, 
                                     fill = as.factor(nDim))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_brewer(palette = "Purples")+
  xlab("Dimension on which Mixtures of Models Vary - # of Dims varied") +
  ylab("Area of the 95% credibility interval normalized by Median Effect Change")

ggsave("auc_medeffect_aggregated_boxplots.jpeg", width = 4, height = 4, units = "in")




## AUC - Med effect stacked plots ----

multiplier <- 50

ggplot() +
  geom_boxplot(data = summ_table, aes(x = reorder(SubsetBy, ID), y = area), 
               fill = "#bcbddc", varwidth = TRUE, lwd = 0.1, outlier.size = 0) +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 2) +
  geom_point(data = summ_table[2:nrow(summ_table),], 
             aes(x = reorder(SubsetBy, ID), y = median_effect_change*multiplier), size = 0.25, color = "#f1a340") +
  scale_y_continuous(name = "Area of 95% Credibility Interval",
                     sec.axis = sec_axis(transform= ~./multiplier, name = "Median Effect Change")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5)) +
   xlab("Mixture Model Subsets") 

ggsave("auc_boxplots_w_medeffect_points.jpeg", width = 4, height = 4, units = "in")

### Figure S9 ----

multiplier <- 50

ggplot() +
    geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 1, color = "#998ec3") +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = median_effect_change*multiplier), 
             size = 1, color = "#f1a340") +
  geom_point(data = summ_table[2:nrow(summ_table),], 
             aes(x = reorder(SubsetBy, ID), y = area), size = 0.4, color = "#998ec3") +
  geom_point(data = summ_table[2:nrow(summ_table),], 
             aes(x = reorder(SubsetBy, ID), y = median_effect_change*multiplier), size = 0.1, color = "#f1a340") +
  scale_y_continuous(name = "Area of 95% Credibility Interval",
                     sec.axis = sec_axis(transform= ~./multiplier, name = "Median Effect Change")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab("Mixture Model Subsets") 
  

ggsave("Figs10_auc_points_w_medeffect_points.jpeg", width = 6, height = 4, units = "in")


pa <- ggplot() +
  geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = area), size = 1, color = "#998ec3") +
  geom_point(data = summ_table[2:nrow(summ_table),], 
             aes(x = reorder(SubsetBy, ID), y = area), size = 0.4, color = "#998ec3") +
   scale_y_continuous(name = "Area of 95% Credibility Interval") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab("Mixture Model Subsets") 




pb <- ggplot() +
    geom_point(data = summ_table[1, ], aes(x = reorder(SubsetBy, ID), y = median_effect_change), 
             size = 1, color = "#f1a340") +
   geom_point(data = summ_table[2:nrow(summ_table),], 
             aes(x = reorder(SubsetBy, ID), y = median_effect_change), size = 0.1, color = "#f1a340") +
  scale_y_continuous(name = "Median Effect Change") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  xlab("Mixture Model Subsets") 

p <- ggarrange(pa + theme(axis.title.x = element_blank(), axis.text.x = element_blank()), pb,  
               ncol = 1, nrow = 2, 
                common.legend = TRUE, legend = "bottom",
                labels = c("A", "B"),
               heights = c(0.75,1))

ggsave("Figs9_auc_points_w_medeffect_points_take2.jpeg", width = 6, height = 7, units = "in")


ggplot() +
  geom_point(data = summ_table[1, ], aes(x = reorder(ID, -area), y = area), 
             size = 2, color = "#998ec3") +
  geom_point(data = summ_table[1, ], aes(x = reorder(ID, -area), y = median_effect_change*multiplier), 
             size = 2, color = "#f1a340") +
  geom_point(data = summ_table[2:nrow(summ_table),], aes(x = reorder(ID, -area), y = area), 
             size = 0.25, color = "#998ec3") +
  geom_point(data = summ_table[2:nrow(summ_table),], aes(x = reorder(ID, -area), y = median_effect_change*multiplier), 
            size = 0.25, color = "#f1a340") +
  scale_y_continuous(name = "Area of 95% Credibility Interval",
                     sec.axis = sec_axis(transform= ~./multiplier, name = "Median Effect Change")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), panel.grid.major = element_blank())+
  xlab("Mixture Model Subsets")


ggplot() +
  geom_point(data = summ_table[1, ], aes(x = reorder(ID, -median_effect_change), y = area), 
             size = 2, color = "#998ec3") +
  geom_point(data = summ_table[1, ], aes(x = reorder(ID, -median_effect_change), y = median_effect_change*multiplier), 
             size = 2, color = "#f1a340") +
  geom_point(data = summ_table[2:nrow(summ_table),], aes(x = reorder(ID, -median_effect_change), y = area), 
             size = 0.25, color = "#998ec3") +
  geom_point(data = summ_table[2:nrow(summ_table),], aes(x = reorder(ID, -median_effect_change), y = median_effect_change*multiplier), 
             size = 0.25, color = "#f1a340") +
  scale_y_continuous(name = "Area of 95% Credibility Interval",
                     sec.axis = sec_axis(transform= ~./multiplier, name = "Median Effect Change")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(), panel.grid.major = element_blank())+
  xlab("Mixture Model Subsets")

ggsave("medeffect_v_auc_points.jpeg", width = 4, height = 4, units = "in")




# Figure S10 - Barchart of Sig Effects by subset----
bar <- summ_table %>% 
  group_by(SubsetBy) %>%
  mutate(count_sub = n(),
         count_sig = sum(sig_difference == "TRUE"),
         perc_sig = count_sig/count_sub*100) %>%
  ungroup() %>%
  arrange(perc_sig) %>%
  mutate(ID2 = seq(1:nrow(.)))
  

ggplot() +
  geom_col(data = bar, aes(x = reorder(SubsetBy, ID2), y = count_sig), position = "dodge",fill = "#f1a340") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = .5)) +
  ylab("Percent Mixtures with\nSignificant Trends")+
  xlab("Mixture Model Subsets")

ggsave("FigS11_sig_diff_barchart.jpeg", width = 6, height = 4, units = "in")

# Figure 5 Uncertainty Decomposition----

## Cumulative Uncertainty Explained ----


referenceAUC <- summ_table %>%
  filter(SubsetBy == "None") %>%
  select(area) %>%
  pull()

#simplest version with key parameters only (stacked)

  pe_table <- summ_table %>%
    group_by(SubsetBy) %>%
    mutate(averageAUC = mean(area)) %>%
    mutate(percentAUC = round(averageAUC/referenceAUC,2)) %>%
    mutate(cumulativeAUCexplained = (1- percentAUC)*100) %>%
    distinct(percentAUC, SubsetBy, .keep_all = T) %>%
    filter(SubsetBy %in% c("Crop", "Crop-Metric", "Crop-Metric-Classification", 
                           "Crop-Metric-Classification-Scope")) %>%
    select(area:scopes, SubsetBy, averageAUC, percentAUC, cumulativeAUCexplained) %>%
    ungroup() %>%
    add_row(., SubsetBy = "Other", 
            averageAUC = NA, percentAUC = NA, cumulativeAUCexplained = 100) %>%
    arrange(cumulativeAUCexplained) %>%
    mutate(percentAUCexplained = cumulativeAUCexplained - lag(cumulativeAUCexplained, default = 0)) %>%
    mutate(constant = "Constant") %>%
    mutate(ID = seq(1:nrow(.)))



ggplot(data = pe_table) +
  geom_col(aes(y = reorder(SubsetBy, percentAUCexplained), 
               x = constant, fill = SubsetBy)) +
  geom_text(aes(y = reorder(SubsetBy, percentAUCexplained), 
                x = constant,label = paste0(percentAUCexplained, "%")),
            position = position_stack(vjust = 0.5)) +
  ggsci::scale_fill_npg()+
  theme_minimal() +
  theme(axis.text = element_blank(), 
        panel.grid = element_blank(), 
        legend.position = "bottom",
        element_text(size = 5)) +
  ylab("") +
  xlab("") +
  coord_flip() +
  guides(fill = guide_legend(nrow = 2)) +
  labs(fill = "") 
  
ggsave(paste0("Images/","Figure5_percentexplained_stackedbar_FINAL_", Sys.Date(), ".jpg"), width = 6, height = 2, units = "in", dpi = 300)

### Figure S12 ----

# full version with all parameters (not stacked)

cumulative_table <- summ_table %>%
  group_by(SubsetBy) %>%
  mutate(averageAUC = mean(area)) %>%
  mutate(percentAUC = round(averageAUC/referenceAUC,2)) %>%
  mutate(cumulativeAUCexplained = (1- percentAUC)*100) %>%
  distinct(percentAUC, SubsetBy, .keep_all = T) %>%
  select(area:scopes, SubsetBy, averageAUC, percentAUC, cumulativeAUCexplained) %>%
  ungroup() %>%
  add_row(., SubsetBy = "Other", 
          averageAUC = NA, percentAUC = NA, cumulativeAUCexplained = 100) %>%
  arrange(cumulativeAUCexplained) %>%
  mutate(percentAUCexplained = cumulativeAUCexplained - lag(cumulativeAUCexplained, default = 0)) %>%
  mutate(constant = "Constant") %>%
  mutate(cumulativeAUCexplained = round(cumulativeAUCexplained)) %>%
  mutate(ID = seq(1:nrow(.))) 

ggplot() +
  geom_bar(data = cumulative_table, aes(x = reorder(SubsetBy, ID),
                                        y = cumulativeAUCexplained), 
           color = "black", fill = "#00A087B2", stat = "identity") +
  geom_text(data = cumulative_table, aes(x = reorder(SubsetBy, ID), 
                y = round(cumulativeAUCexplained),label = paste0(cumulativeAUCexplained, "%"), 
                 vjust = -0.2), size = 3) +
  scale_fill_viridis_d()+
  theme_minimal() +
  theme(axis.text = element_text(angle = 90, size = 8), 
        panel.grid = element_blank(), 
        legend.position = "bottom") +
  ylab("") + 
  xlab("") +
  guides(fill = guide_legend(nrow = 5)) +
  labs(fill = "")

ggsave("FigS6_Cumulative_percentexplained_fullbar.jpeg", width = 6, height = 6, units = "in")

# Response curve summary and comparison plots ----



#Key for color uses from ColorBrewer
  # General: #9e9ac8, #756bb1
  # Neutral: darkgray
  # General Contrasts: #404040, #bababa, #f4a582, #ca0020
                      #5e3c99, #b2abd2, #fdb863, #e66101
                      #ca0020, #f4a582, #92c5de, #0571b0
                      #7b3294, #c2a5cf, #a6dba0, #008837
  # Wheat: #a6611a, #dfc27d
  # Corn: #d95f0e, #fe9929, #fed98e
  # Soy: #016c59, #1c9099, #67a9cf
  # Alfalfa: #f768a1, #fbb4b9
  # Hay: #74c476, #bae4b3

## Figure 2 - Full mixture  ----


rw.quant.plot_full <-  readRDS("Images/D, RICH, RPR, SDI, SIDIalfalfa, corn, hay, soy, wwheatCDL, RCAG, ALLCNTY, BBOX2024-12-13.rds") 

rw.quant.plot_full <- rw.quant.plot_full %>%
  mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]

ribbon1_full <- rw.quant.plot_full %>%
  dplyr::select(quant0.025:quant0.475, Percentile) %>%
  pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
  arrange(name)%>%
  group_by(name) %>%
  mutate(group =  cur_group_id()) 

ribbon2_full <- rw.quant.plot_full %>%
  dplyr::select(quant0.525:quant0.975, Percentile) %>%
  pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
  arrange(desc(name))%>%
  group_by(name) %>%
  mutate(group =  cur_group_id()) %>%
  mutate(group = 20 - group)

ribbon_full <- left_join(ribbon1_full, ribbon2_full, by = c( "group","Percentile"))


ggplot() + 
  xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
  geom_ribbon(data = ribbon_full, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group), fill = "#9e9ac8") +
  geom_point(data=rw.quant.plot_full, aes(x=Percentile, y=`quant0.5`), color = "#756bb1", size = 0.5) +
  geom_line(data=rw.quant.plot_full, aes(x=Percentile, y=`quant0.25`), color = "gray", linetype = 2, linewidth = 0.5) + #01226 changed color from dark gray to gray
  geom_line(data=rw.quant.plot_full, aes(x=Percentile, y=`quant0.75`), color = "gray", linetype = 2, linewidth = 0.5) +
  geom_line(data=rw.quant.plot_full, aes(x=Percentile, y=`quant0.025`), color = "dimgray",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
  geom_line(data=rw.quant.plot_full, aes(x=Percentile, y=`quant0.975`), color = "dimgray",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
  theme_light() + theme(panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank(),
                        panel.ontop = FALSE) +
  theme(legend.position = "none", plot.caption = element_text(hjust = 0, size = 8)) +
  scale_alpha(range = c(0.1, 0.8)) #012226 increased alpha range for revision 1

ggsave(paste0("Images/Figure2_Full_mixture_Final_", Sys.Date(), ".jpg"), width = 6, height = 4, units = "in", dpi = 300)


# Figure S8 - diversity trends by marginal effect percentiles ----

###change from 10th to 100th percentile summary ----
rw.quant.change <- rw.quant.plot_full %>%
  dplyr::select(Percentile, quant0.025, quant0.5, quant0.975) %>%
  mutate(change_10_100_median = quant0.5[Percentile == 100] - quant0.5[Percentile == 10],
         change_10_100_lower = quant0.025[Percentile == 100] - quant0.025[Percentile == 10],
         change_10_100_upper = quant0.975[Percentile == 100] - quant0.975[Percentile == 10]) %>%
  filter(Percentile == 10) %>%
  dplyr::select(change_10_100_median, change_10_100_lower, change_10_100_upper)

rw.quant.change

rw.quant.change_full <- rw.quant.plot_full %>%
  mutate(across(starts_with("quant"), ~ .x[Percentile == 100] - .x[Percentile == 10])) %>%
  filter(Percentile == 10) 

rw.quant.change_full

ggplot() +
  geom_point(data = rw.quant.change_full %>% pivot_longer(., cols = starts_with("quant")),
             aes(x = name, y = value), size = 2)  +
  theme_minimal() +
  scale_x_discrete(
    labels = function(x) {
      # Keep label for odd positions, blank for even
      ifelse(seq_along(x) %% 2 == 1, x, "")} ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Marginal Effect Percentile") +
  ylab("Change in % Yield from 5th to 95th Percentile")

ggsave(paste0("Images/SIFigure_R1_Full_mixture_Changeinmarginaleffects.jpg"), width = 6, height = 4, units = "in")


## Combine the CDL and RC mixtures (100 models each) ----
    rw.quant.plot_cdl <-  readRDS("Images/D, RICH, RPR, SDI, SIDIalfalfa, corn, hay, soy, wwheatCDLAG, ALLCNTY, BBOX2024-12-10.rds") 
    
rw.quant.plot_cdl <- rw.quant.plot_cdl %>%
  mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]

  
  ribbon1_cdl <- rw.quant.plot_cdl %>%
      dplyr::select(quant0.025:quant0.475, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_cdl <- rw.quant.plot_cdl %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_cdl <- left_join(ribbon1_cdl, ribbon2_cdl, by = c( "group","Percentile"))
    
    rw.quant.plot_rc <-  readRDS("Images/D, RICH, RPR, SDI, SIDIalfalfa, corn, hay, soy, wwheatRCAG, ALLCNTY, BBOX2024-12-10.rds") 
    
    rw.quant.plot_rc <- rw.quant.plot_rc %>%
      mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
    
    
    ribbon1_rc <- rw.quant.plot_rc %>%
      dplyr::select(quant0.025:quant0.475, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_rc <- rw.quant.plot_rc %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_rc <- left_join(ribbon1_rc, ribbon2_rc, by = c( "group","Percentile"))
    
    #added 6/4/2025 to check shape
    
    # ribbon_cdl <- ribbon_cdl %>%
    #   mutate(Min = exp(Min),
    #          Max - exp(Max),
    #          `quant0.5` = exp(`quant0.5`),
    #          `quant0.25` = exp(`quant0.25`),
    #          `quant0.75` = exp(`quant0.75`))
    # 
    # ribbon_rc <- ribbon_rc %>%
    #   mutate(Min = exp(Min),
    #          Max = exp(Max),
    #          `quant0.5` = exp(`quant0.5`),
    #          `quant0.25` = exp(`quant0.25`),
    #          `quant0.75` = exp(`quant0.75`))
    # 
    # 
    #end of addition
    
   
   pcdlrc <-   ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of 100 CDL and RC models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all landscape metrics, all crops,  
        #                     all scopes, and all boundaries, but with different classifications. \n",
        #                       "The distributions are based on 100 models each for CDL and RC. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The points indicate median posterior values while the dashed lines indicate one standard deviation.")) +
        geom_ribbon(data = ribbon_cdl, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "CDL")) +
        guides(alpha = FALSE, group = FALSE) +
        geom_ribbon(data = ribbon_rc, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "RC")) +
        guides(alpha = FALSE, group = FALSE) +
        geom_point(data=rw.quant.plot_cdl, aes(x=Percentile, y=`quant0.5`, color = "CDL"), size = 1) +
        geom_line(data=rw.quant.plot_cdl, aes(x=Percentile, y=`quant0.25`, color = "CDL"), linetype = 2, linewidth = 0.5) +
        geom_line(data=rw.quant.plot_cdl, aes(x=Percentile, y=`quant0.75`, color = "CDL"), linetype = 2, linewidth = 0.5) +
     geom_line(data=rw.quant.plot_cdl, aes(x=Percentile, y=`quant0.025`), color = "#ca0020",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
     geom_line(data=rw.quant.plot_cdl, aes(x=Percentile, y=`quant0.975`), color = "#ca0020",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
        geom_point(data=rw.quant.plot_rc, aes(x=Percentile, y=`quant0.5`, color = "RC"), size = 1) +
        geom_line(data=rw.quant.plot_rc, aes(x=Percentile, y=`quant0.25`, color = "RC"), linetype = 2, linewidth = 0.5) +
        geom_line(data=rw.quant.plot_rc, aes(x=Percentile, y=`quant0.75`, color = "RC"), linetype = 2, linewidth = 0.5) +
     geom_line(data=rw.quant.plot_rc, aes(x=Percentile, y=`quant0.025`), color = "#404040",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
     geom_line(data=rw.quant.plot_rc, aes(x=Percentile, y=`quant0.975`), color = "#404040",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
         theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("CDL", "RC"), values = c("#f4a582", "#bababa"))+
        scale_color_manual(name = "", labels = c("CDL", "RC"), values = c("#ca0020", "#404040"))+
      scale_alpha(range = c(0.1, 0.8))
   
    
    
    ggsave(paste0("Images/","R1_CDL_RC_comparison_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
               
    
    
    ## Combine the AG and ALL mixtures (100 models each) ----
    rw.quant.plot_ag <-  readRDS("Images/D, RICH, RPR, SDI, SIDIalfalfa, corn, hay, soy, wwheatCDL, RCAGCNTY, BBOX2024-12-10.rds") 
    
    rw.quant.plot_ag<- rw.quant.plot_ag %>%
      mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
    
    
    ribbon1_ag <- rw.quant.plot_ag %>%
      dplyr::select(quant0.025:quant0.475, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_ag <- rw.quant.plot_ag %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_ag <- left_join(ribbon1_ag, ribbon2_ag, by = c( "group","Percentile"))
    
    rw.quant.plot_all <-  readRDS("Images/D, RICH, RPR, SDI, SIDIalfalfa, corn, hay, soy, wwheatCDL, RCALLCNTY, BBOX2024-12-10.rds") 
    
    rw.quant.plot_all <- rw.quant.plot_all %>%
      mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
    
    
    ribbon1_all <- rw.quant.plot_all %>%
      dplyr::select(quant0.025:quant0.475, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_all <- rw.quant.plot_all %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_all <- left_join(ribbon1_all, ribbon2_all, by = c( "group","Percentile"))
    
    
  pagall <-  ggplot() + 
      xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
      # ggtitle(paste0("Equal-weighted ensembles of 100 AG and ALL models")) +
      # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all landscape metrics, all crops,  
      #                       all classification, and all boundaries, but with different scopes. \n",
      #                       "The distributions are based on 100 models each for AG and ALL. \n",
      #                       "The 95% credibility intervals are shown in the shaded areas. \n",
      #                       "The points indicate median posterior values while the dashed lines indicate one standard deviation.")) +
      # geom_ribbon(data = ribbon_ag, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "AG")) +
    geom_ribbon(data = ribbon_ag, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "AG")) +
    guides(alpha = FALSE, group = FALSE) +
    geom_ribbon(data = ribbon_all, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "ALL")) +
    guides(alpha = FALSE, group = FALSE) +
    geom_point(data=rw.quant.plot_ag, aes(x=Percentile, y=`quant0.5`, color = "AG"), size = 1) +
    geom_line(data=rw.quant.plot_ag, aes(x=Percentile, y=`quant0.25`, color = "AG"), linetype = 2, linewidth = 0.5) +
    geom_line(data=rw.quant.plot_ag, aes(x=Percentile, y=`quant0.75`, color = "AG"), linetype = 2, linewidth = 0.5) +
    geom_line(data=rw.quant.plot_ag, aes(x=Percentile, y=`quant0.025`), color = "#ca0020",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
    geom_line(data=rw.quant.plot_ag, aes(x=Percentile, y=`quant0.975`), color = "#ca0020",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
    geom_point(data=rw.quant.plot_all, aes(x=Percentile, y=`quant0.5`, color = "ALL"), size = 1) +
    geom_line(data=rw.quant.plot_all, aes(x=Percentile, y=`quant0.25`, color = "ALL"), linetype = 2, linewidth = 0.5) +
    geom_line(data=rw.quant.plot_all, aes(x=Percentile, y=`quant0.75`, color = "ALL"), linetype = 2, linewidth = 0.5) +
    geom_line(data=rw.quant.plot_all, aes(x=Percentile, y=`quant0.025`), color = "#404040",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
    geom_line(data=rw.quant.plot_all, aes(x=Percentile, y=`quant0.975`), color = "#404040",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
    theme_light() + theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          panel.ontop = FALSE) +
    theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
    scale_fill_manual(name = "", labels = c("AG", "ALL"), values = c("#f4a582", "#bababa"))+
    scale_color_manual(name = "", labels = c("AG", "ALL"), values = c("#ca0020", "#404040"))+
    scale_alpha(range = c(0.1, 0.8))
  
    
    
    ggsave(paste0("Images/","R1_AG_ALL_comparison_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
    
    
    
    ## Combine the CNTY and BBOX mixtures (100 models each) ----
    
    rw.quant.plot_bbox <-  readRDS("Images/D, RICH, RPR, SDI, SIDIalfalfa, corn, hay, soy, wwheatCDL, RCAG, ALLBBOX2024-12-10.rds") 
    
    rw.quant.plot_bbox <- rw.quant.plot_bbox %>%
      mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
    
     ribbon1_bbox <- rw.quant.plot_bbox %>%
      dplyr::select(quant0.025:quant0.475, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_bbox <- rw.quant.plot_bbox %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_bbox <- left_join(ribbon1_bbox, ribbon2_bbox, by = c( "group","Percentile"))
    
    rw.quant.plot_cnty <-  readRDS("Images/D, RICH, RPR, SDI, SIDIalfalfa, corn, hay, soy, wwheatCDL, RCAG, ALLCNTY2024-12-10.rds") 
    
    rw.quant.plot_cnty <- rw.quant.plot_cnty %>%
      mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
    
    
    ribbon1_cnty <- rw.quant.plot_cnty %>%
      dplyr::select(quant0.025:quant0.475, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_cnty <- rw.quant.plot_cnty %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_cnty <- left_join(ribbon1_cnty, ribbon2_cnty, by = c( "group","Percentile"))
    
    
    ggplot() + 
      xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
      # ggtitle(paste0("Equal-weighted ensembles of 100 BBOX and CNTY models")) +
      # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all landscape metrics, all crops,
      #                       all classifications, and all scopes, but with different boundaries. \n",
      #                       "The distributions are based on 100 models each for BBOX and ALL. \n",
      #                       "The 95% credibility intervals are shown in the shaded areas. \n",
      #                       "The points indicate median posterior values while the dashed lines indicate one standard deviation.")) +
      geom_ribbon(data = ribbon_bbox, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "BBOX")) +
      guides(alpha = FALSE, group = FALSE) +
      geom_ribbon(data = ribbon_cnty, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "CNTY")) +
      guides(alpha = FALSE, group = FALSE) +
      geom_point(data=rw.quant.plot_bbox, aes(x=Percentile, y=`quant0.5`, color = "BBOX"), size = 1) +
      geom_line(data=rw.quant.plot_bbox, aes(x=Percentile, y=`quant0.25`, color = "BBOX"), linetype = 2, linewidth = 0.5) +
      geom_line(data=rw.quant.plot_bbox, aes(x=Percentile, y=`quant0.75`, color = "BBOX"), linetype = 2, linewidth = 0.5) +
      geom_line(data=rw.quant.plot_bbox, aes(x=Percentile, y=`quant0.025`), color = "#ca0020",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
      geom_line(data=rw.quant.plot_bbox, aes(x=Percentile, y=`quant0.975`), color = "#ca0020",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
      geom_point(data=rw.quant.plot_cnty, aes(x=Percentile, y=`quant0.5`, color = "CNTY"), size = 1) +
      geom_line(data=rw.quant.plot_cnty, aes(x=Percentile, y=`quant0.25`, color = "CNTY"), linetype = 2, linewidth = 0.5) +
      geom_line(data=rw.quant.plot_cnty, aes(x=Percentile, y=`quant0.75`, color = "CNTY"), linetype = 2, linewidth = 0.5) +
      geom_line(data=rw.quant.plot_cnty, aes(x=Percentile, y=`quant0.025`), color = "#404040",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
      geom_line(data=rw.quant.plot_cnty, aes(x=Percentile, y=`quant0.975`), color = "#404040",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
      theme_light() + theme(panel.grid.major.x = element_blank(),
                            panel.grid.minor.x = element_blank(),
                            panel.ontop = FALSE) +
      theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
      scale_fill_manual(name = "", labels = c("BBOX", "CNTY"), values = c("#f4a582", "#bababa"))+
      scale_color_manual(name = "", labels = c("BBOX", "CNTY"), values = c("#ca0020", "#404040"))+
    scale_alpha(range = c(0.1, 0.8))
    
    
    ggsave(paste0("Images/","R1_BBOX_CNTY_comparison_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
    
    
    ## Combine the 5 Crop mixtures (40 models each) ----
    
rw.quant.plot_corn <-  readRDS("Images/D, RICH, RPR, SDI, SIDIcornCDL, RCAG, ALLCNTY, BBOX2024-12-10.rds") %>%
      mutate(CROP = 'CORN')
    
    rw.quant.plot_corn <- rw.quant.plot_corn %>%
      mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
    
    
    ribbon1_corn <- rw.quant.plot_corn %>%
      dplyr::select(quant0.025:quant0.475, Percentile, CROP) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_corn <- rw.quant.plot_corn %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_corn <- left_join(ribbon1_corn, ribbon2_corn, by = c( "group","Percentile"))
    
rw.quant.plot_soy <-  readRDS("Images/D, RICH, RPR, SDI, SIDIsoyCDL, RCAG, ALLCNTY, BBOX2024-12-10.rds") %>%
  mutate(CROP = 'SOY')

rw.quant.plot_soy <- rw.quant.plot_soy %>%
  mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]

    
    ribbon1_soy <- rw.quant.plot_soy %>%
      dplyr::select(quant0.025:quant0.475, Percentile, CROP) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_soy <- rw.quant.plot_soy %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_soy <- left_join(ribbon1_soy, ribbon2_soy, by = c( "group","Percentile"))
    
   
rw.quant.plot_wwheat <-  readRDS("Images/D, RICH, RPR, SDI, SIDIwwheatCDL, RCAG, ALLCNTY, BBOX2024-12-10.rds") %>%
  mutate(CROP = 'WHEAT')
    
rw.quant.plot_wwheat <- rw.quant.plot_wwheat %>%
  mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]

    ribbon1_wwheat <- rw.quant.plot_wwheat %>%
      dplyr::select(quant0.025:quant0.475, Percentile, CROP) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_wwheat <- rw.quant.plot_wwheat %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_wwheat <- left_join(ribbon1_wwheat, ribbon2_wwheat, by = c( "group","Percentile"))
    

rw.quant.plot_alfalfa <-  readRDS("Images/D, RICH, RPR, SDI, SIDIalfalfaCDL, RCAG, ALLCNTY, BBOX2024-12-10.rds") %>%
  mutate(CROP = 'ALFALFA')
    
rw.quant.plot_alfalfa <- rw.quant.plot_alfalfa %>%
  mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]

    ribbon1_alfalfa <- rw.quant.plot_alfalfa %>%
      dplyr::select(quant0.025:quant0.475, Percentile, CROP) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_alfalfa <- rw.quant.plot_alfalfa %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_alfalfa <- left_join(ribbon1_alfalfa, ribbon2_alfalfa, by = c( "group","Percentile"))
  

rw.quant.plot_hay <-  readRDS("Images/D, RICH, RPR, SDI, SIDIhayCDL, RCAG, ALLCNTY, BBOX2024-12-10.rds") %>%
  mutate(CROP = 'HAY')

rw.quant.plot_hay <- rw.quant.plot_hay %>%
  mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]


    ribbon1_hay <- rw.quant.plot_hay %>%
      dplyr::select(quant0.025:quant0.475, Percentile, CROP) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_hay <- rw.quant.plot_hay %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_hay <- left_join(ribbon1_hay, ribbon2_hay, by = c( "group","Percentile"))
    
 #### Plot it   
    
    p <- ggplot() + 
      xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
      # ggtitle(paste0("Equal-weighted ensembles of 40 CORN and SOY models")) +
      # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all landscape metrics, all boundaries,  
      #                       all classifications, and all scopes, but with different crops. \n",
      #                       "The distributions are based on 40 models each for CORN, SOY, WHEAT, ALFALFA, and HAY. \n",
      #                       "The 95% credibility intervals are shown in the shaded areas. \n",
      #                       "The points indicate median posterior values while the dashed lines indicate one standard deviation.")) +
      geom_ribbon(data = ribbon_corn, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "CORN")) +
      guides(alpha = FALSE, group = FALSE) +
      geom_ribbon(data = ribbon_soy, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "SOY")) +
      guides(alpha = FALSE, group = FALSE) +
      geom_ribbon(data = ribbon_wwheat, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "WHEAT")) +
      guides(alpha = FALSE, group = FALSE) +
      geom_ribbon(data = ribbon_alfalfa, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "ALFALFA")) +
      guides(alpha = FALSE, group = FALSE) +
      geom_ribbon(data = ribbon_hay, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "HAY")) +
      guides(alpha = FALSE, group = FALSE) +
      geom_point(data=rw.quant.plot_corn, aes(x=Percentile, y=`quant0.5`, color = "CORN"), size = 1) +
      geom_line(data=rw.quant.plot_corn, aes(x=Percentile, y=`quant0.25`, color = "CORN"), linetype = 2, linewidth = 0.5) +
      geom_line(data=rw.quant.plot_corn, aes(x=Percentile, y=`quant0.75`, color = "CORN"), linetype = 2, linewidth = 0.5) +
      geom_point(data=rw.quant.plot_soy, aes(x=Percentile, y=`quant0.5`, color = "SOY"), size = 1) +
      geom_line(data=rw.quant.plot_soy, aes(x=Percentile, y=`quant0.25`, color = "SOY"), linetype = 2, linewidth = 0.5) +
      geom_line(data=rw.quant.plot_soy, aes(x=Percentile, y=`quant0.75`, color = "SOY"), linetype = 2, linewidth = 0.5) +
      geom_point(data=rw.quant.plot_wwheat, aes(x=Percentile, y=`quant0.5`, color = "WHEAT"), size = 1) +
      geom_line(data=rw.quant.plot_wwheat, aes(x=Percentile, y=`quant0.25`, color = "WHEAT"), linetype = 2, linewidth = 0.5) +
      geom_line(data=rw.quant.plot_wwheat, aes(x=Percentile, y=`quant0.75`, color = "WHEAT"), linetype = 2, linewidth = 0.5) +
      geom_point(data=rw.quant.plot_alfalfa, aes(x=Percentile, y=`quant0.5`, color = "ALFALFA"), size = 1) +
      geom_line(data=rw.quant.plot_alfalfa, aes(x=Percentile, y=`quant0.25`, color = "ALFALFA"), linetype = 2, linewidth = 0.5) +
      geom_line(data=rw.quant.plot_alfalfa, aes(x=Percentile, y=`quant0.75`, color = "ALFALFA"), linetype = 2, linewidth = 0.5) +
      geom_point(data=rw.quant.plot_hay, aes(x=Percentile, y=`quant0.5`, color = "HAY"), size = 1) +
      geom_line(data=rw.quant.plot_hay, aes(x=Percentile, y=`quant0.25`, color = "HAY"), linetype = 2, linewidth = 0.5) +
      geom_line(data=rw.quant.plot_hay, aes(x=Percentile, y=`quant0.75`, color = "HAY"), linetype = 2, linewidth = 0.5) +
      theme_light() + theme(panel.grid.major.x = element_blank(),
                            panel.grid.minor.x = element_blank(),
                            panel.ontop = FALSE) +
      theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
      scale_fill_manual(name = "", labels = c("CORN", "SOY", "WHEAT", "ALFALFA", "HAY"), values = c("#fed98e", "#1c9099", "#dfc27d", "#fbb4b9", "#bae4b3"))+
      scale_color_manual(name = "", labels = c("CORN", "SOY", "WHEAT", "ALFALFA", "HAY"), values = c("#d95f0e", "#016c59", "#a6611a", "#f768a1", "#74c476"))+
      scale_alpha(range = c(0.25, 0.75))
    
    p + xlab("Landscape Diversity Percentile") + theme(plot.caption = element_blank(), plot.title = element_blank())
    
    ggsave(paste0("Images/","R1_CORN_SOY_WHEAT_ALFALFA_HAY_comparison_", Sys.Date(), ".jpg"), width = 6, height = 4, units = "in")
    #items seem to be reordered...
       
    # Wheat: #a6611a, #8c510a, #dfc27d, #d8b365, #bf812d
    # Corn: #d95f0e, #d95f0e, #fed98e
    # Soy: #016c59, #1c9099, #67a9cf
    # Alfalfa: #f768a1, #fbb4b9
    # Hay: #74c476, #bae4b3
    
    ### Figure S13 ----
    
    p2 <-   ggplot() + 
      xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
      # ggtitle(paste0("Equal-weighted ensembles of 40 CORN and SOY models")) +
      # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all landscape metrics, all boundaries,  
      #                       all classifications, and all scopes, but with different crops. \n",
      #                       "The distributions are based on 40 models each for CORN, SOY, WHEAT, ALFALFA, and HAY. \n",
      #                       "The 95% credibility intervals are shown in the shaded areas. \n",
      #                       "The points indicate median posterior values while the dashed lines indicate one standard deviation.")) +
      geom_ribbon(data = ribbon_wwheat, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "WHEAT")) +
      guides(alpha = FALSE, group = FALSE) +
      geom_ribbon(data = ribbon_corn, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group*1.1, fill = "CORN")) +
      guides(alpha = FALSE, group = FALSE) +
      geom_ribbon(data = ribbon_hay, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group, fill = "HAY")) +
      guides(alpha = FALSE, group = FALSE) +
      geom_point(data=rw.quant.plot_wwheat, aes(x=Percentile, y=`quant0.5`, color = "WHEAT"), size = 0.5) +
      geom_line(data=rw.quant.plot_wwheat, aes(x=Percentile, y=`quant0.25`, color = "WHEAT"), linetype = 2, linewidth = 0.4) +
      geom_line(data=rw.quant.plot_wwheat, aes(x=Percentile, y=`quant0.75`, color = "WHEAT"), linetype = 2, linewidth = 0.4) +
      geom_line(data=rw.quant.plot_wwheat, aes(x=Percentile, y=`quant0.025`), color = "#fdae6b",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
      geom_line(data=rw.quant.plot_wwheat, aes(x=Percentile, y=`quant0.975`), color = "#fdae6b",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
      
      geom_point(data=rw.quant.plot_corn, aes(x=Percentile, y=`quant0.5`, color = "CORN"), size = 0.5) +
      geom_line(data=rw.quant.plot_corn, aes(x=Percentile, y=`quant0.25`, color = "CORN"), linetype = 2, linewidth = 0.4) +
      geom_line(data=rw.quant.plot_corn, aes(x=Percentile, y=`quant0.75`, color = "CORN"), linetype = 2, linewidth = 0.4) +
      geom_line(data=rw.quant.plot_corn, aes(x=Percentile, y=`quant0.025`), color = "#74c476",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
      geom_line(data=rw.quant.plot_corn, aes(x=Percentile, y=`quant0.975`), color = "#74c476",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
      
      geom_point(data=rw.quant.plot_hay, aes(x=Percentile, y=`quant0.5`, color = "HAY"), size = 0.5) +
      geom_line(data=rw.quant.plot_hay, aes(x=Percentile, y=`quant0.25`, color = "HAY"), linetype = 2, linewidth = 0.4) +
      geom_line(data=rw.quant.plot_hay, aes(x=Percentile, y=`quant0.75`, color = "HAY"), linetype = 2, linewidth = 0.4) +
      geom_line(data=rw.quant.plot_hay, aes(x=Percentile, y=`quant0.025`), color = "#543005",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
      geom_line(data=rw.quant.plot_hay, aes(x=Percentile, y=`quant0.975`), color = "#543005",  linewidth = 0.1, alpha = 0.5) + #added 012226 for revision 1
      
      theme_light() + theme(panel.grid.major.x = element_blank(),
                            panel.grid.minor.x = element_blank(),
                            panel.ontop = FALSE) +
      theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
      scale_fill_manual(name = "", labels = c("CORN", "HAY", "WHEAT"), values = c("#fed98e",  "#bae4b3", "#8c510a")) +
      scale_color_manual(name = "", labels = c("CORN","HAY", "WHEAT"), values = c("#fdae6b",  "#74c476", "#543005")) +
      scale_alpha(range = c(0.1, 0.8))
    
    #tried reordering alphabetically, which seems to work, but cannot place corn on top as I would like
    
    p2 + xlab("Landscape Diversity Percentile") + theme(plot.caption = element_blank(), plot.title = element_blank())
    
    ggsave(paste0("Images/","R1_FigS7_CORN_WHEAT_HAY_comparison_", Sys.Date(), ".jpg"), width = 6, height = 4, units = "in")
    
    ### Figure S14 ----
    
    #### build combined plotting dataframes ----
    
    crop_ribbons <-  bind_rows(ribbon_corn, ribbon_soy, ribbon_wwheat, ribbon_alfalfa, ribbon_hay) %>% 
      mutate(CROP = factor(CROP, levels = c("CORN", "SOY", "WHEAT", "ALFALFA", "HAY"))) %>% 
      filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
    
    crop_quants <- bind_rows(rw.quant.plot_corn, rw.quant.plot_soy, rw.quant.plot_wwheat, rw.quant.plot_alfalfa, rw.quant.plot_hay) %>% 
      mutate(CROP = factor(CROP, levels = c( "CORN","SOY", "WHEAT", "ALFALFA",  "HAY")))
    
    p3 <-  ggplot() + 
      xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
      # ggtitle(paste0("Equal-weighted ensembles of 40 CORN and SOY models")) +
      # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all landscape metrics, all boundaries,  
      #                       all classifications, and all scopes, but with different crops. \n",
      #                       "The distributions are based on 40 models each for CORN, SOY, WHEAT, ALFALFA, and HAY. \n",
      #                       "The 95% credibility intervals are shown in the shaded areas. \n",
      #                       "The lines indicate median posterior values.")) +
      geom_ribbon(data = crop_ribbons %>% filter(name.y == 'quant0.975'), aes(x=Percentile, ymin = Min, ymax = Max, group = fct_relevel(CROP, "WHEAT", "SOY",  "CORN", "HAY", "ALFALFA"), fill = CROP), alpha = 0.4) +
      guides(alpha = FALSE, group = FALSE) +
      geom_line(data=crop_quants, aes(x=Percentile, y=`quant0.5`, color = CROP), linewidth = 0.5) +
      geom_point(data=crop_quants, aes(x=Percentile, y=`quant0.5`, color = CROP), size = 1) +
      # geom_line(data=crop_quants, aes(x=Percentile, y=`quant0.25`, color = CROP), linetype = 2, linewidth = 0.5) +
      # geom_line(data=crop_quants, aes(x=Percentile, y=`quant0.75`, color = CROP), linetype = 2, linewidth = 0.5) +
      theme_light() + theme(panel.grid.major.x = element_blank(),
                            panel.grid.minor.x = element_blank(),
                            panel.ontop = FALSE) +
      theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
      scale_fill_manual(name = "", labels = c("CORN", "SOY", "WHEAT", "ALFALFA", "HAY"), values = c("#fed98e", "#1c9099", "#8c510a", "#fbb4b9", "#bae4b3"))+
      scale_color_manual(name = "", labels = c("CORN", "SOY", "WHEAT", "ALFALFA", "HAY"), values = c("#fdae6b", "#016c59", "#543005", "#f768a1", "#74c476"))+
      scale_alpha(range = c(0.05, 0.25))
    
    p3 + xlab("Landscape Diversity Percentile") + theme(plot.caption = element_blank(), plot.title = element_blank())
    
    ggsave(paste0("Images/","R1_Figs8_CORN_SOY_WHEAT_ALFALFA_HAY_comparison_simplified", Sys.Date(), ".jpg"), width = 6, height = 4, units = "in")
    
    
      
  ## Combine the 5 Metric mixtures (40 models each) ----
      
      rw.quant.plot_rich <-  readRDS("Images/RICHalfalfa, corn, hay, soy, wwheatCDL, RCAG, ALLCNTY, BBOX2024-12-10.rds") %>%
        mutate(METRIC = 'RICH')
    
    rw.quant.plot_rich <- rw.quant.plot_rich %>%
      mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
    
      
      ribbon1_rich <- rw.quant.plot_rich %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rich <- rw.quant.plot_rich %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rich <- left_join(ribbon1_rich, ribbon2_rich, by = c( "group","Percentile"))
      
      
      rw.quant.plot_sdi <-  readRDS("Images/SDIalfalfa, corn, hay, soy, wwheatCDL, RCAG, ALLCNTY, BBOX2024-12-10.rds") %>%
        mutate(METRIC = 'SDI')
      
      rw.quant.plot_sdi <- rw.quant.plot_sdi %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_sdi <- rw.quant.plot_sdi %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_sdi <- rw.quant.plot_sdi %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_sdi <- left_join(ribbon1_sdi, ribbon2_sdi, by = c( "group","Percentile"))
      
      
      rw.quant.plot_sidi <-  readRDS("Images/SIDIalfalfa, corn, hay, soy, wwheatCDL, RCAG, ALLCNTY, BBOX2024-12-10.rds") %>%
        mutate(METRIC = 'SIDI')
      
      rw.quant.plot_sidi <- rw.quant.plot_sidi %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_sidi <- rw.quant.plot_sidi %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_sidi <- rw.quant.plot_sidi %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_sidi <- left_join(ribbon1_sidi, ribbon2_sidi, by = c( "group","Percentile"))
      
      
      rw.quant.plot_d <-  readRDS("Images/Dalfalfa, corn, hay, soy, wwheatCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'D')
      
      rw.quant.plot_d <- rw.quant.plot_d %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_d <- rw.quant.plot_d %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_d <- rw.quant.plot_d %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_d <- left_join(ribbon1_d, ribbon2_d, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rpr <-  readRDS("Images/RPRalfalfa, corn, hay, soy, wwheatCDL, RCAG, ALLCNTY, BBOX2024-12-10.rds") %>%
        mutate(METRIC = 'RPR')
      
      rw.quant.plot_rpr <- rw.quant.plot_rpr %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rpr <- rw.quant.plot_rpr %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rpr <- rw.quant.plot_rpr %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rpr <- left_join(ribbon1_rpr, ribbon2_rpr, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_rich, ribbon_sdi, ribbon_sidi, ribbon_d, ribbon_rpr) %>% 
        mutate(METRIC = factor(METRIC, levels = c("RICH", "SDI", "SIDI", "D", "RPR"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_rich, rw.quant.plot_sdi, rw.quant.plot_sidi, rw.quant.plot_d, rw.quant.plot_rpr) %>% 
        mutate(METRIC = factor(METRIC, levels = c("RICH", "SDI", "SIDI", "D", "RPR")))
      
      ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of 40 METRIC models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all crops, all boundaries,  
        #                     all classifications, and all scopes, but with different metrics. \n",
        #                       "The distributions are based on 40 models each for RICH, SDI, SIDI, D, and RPR. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("RICH", "SDI", "SIDI", "D", "RPR"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa", "#fddbc7"))+
        scale_color_manual(name = "", labels = c("RICH", "SDI", "SIDI", "D", "RPR"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa", "#fddbc7"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
      ggsave(paste0("Images/","R1_RICH_SDI_SIDI_D_RPR_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      

## Corn - Combine the 5 Metric mixtures for CORN only (40 models each) ----
      
      rw.quant.plot_rich <-  readRDS("Images/RICHcornCDL, RCAG, ALLCNTY, BBOX2024-12-11.rds") %>%
        mutate(METRIC = 'RICH')
      
      rw.quant.plot_rich <- rw.quant.plot_rich %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rich <- rw.quant.plot_rich %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rich <- rw.quant.plot_rich %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rich <- left_join(ribbon1_rich, ribbon2_rich, by = c( "group","Percentile"))
      
      
      rw.quant.plot_sdi <-  readRDS("Images/SDIcornCDL, RCAG, ALLCNTY, BBOX2024-12-11.rds") %>%
        mutate(METRIC = 'SDI')
      
      rw.quant.plot_sdi <- rw.quant.plot_sdi %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      ribbon1_sdi <- rw.quant.plot_sdi %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_sdi <- rw.quant.plot_sdi %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_sdi <- left_join(ribbon1_sdi, ribbon2_sdi, by = c( "group","Percentile"))
      
      
      rw.quant.plot_sidi <-  readRDS("Images/SIDIcornCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'SIDI')
      
      rw.quant.plot_sidi <- rw.quant.plot_sidi %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_sidi <- rw.quant.plot_sidi %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_sidi <- rw.quant.plot_sidi %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_sidi <- left_join(ribbon1_sidi, ribbon2_sidi, by = c( "group","Percentile"))
      
      
      rw.quant.plot_d <-  readRDS("Images/DcornCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'D')
      
      rw.quant.plot_d <- rw.quant.plot_d %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_d <- rw.quant.plot_d %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_d <- rw.quant.plot_d %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_d <- left_join(ribbon1_d, ribbon2_d, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rpr <-  readRDS("Images/RPRcornCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RPR')
      
      rw.quant.plot_rpr <- rw.quant.plot_rpr %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rpr <- rw.quant.plot_rpr %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rpr <- rw.quant.plot_rpr %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rpr <- left_join(ribbon1_rpr, ribbon2_rpr, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_rich, ribbon_sdi, ribbon_sidi, ribbon_d, ribbon_rpr) %>% 
        mutate(METRIC = factor(METRIC, levels = c("RICH", "SDI", "SIDI", "D", "RPR"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_rich, rw.quant.plot_sdi, rw.quant.plot_sidi, rw.quant.plot_d, rw.quant.plot_rpr) %>% 
        mutate(METRIC = factor(METRIC, levels = c("RICH", "SDI", "SIDI", "D", "RPR")))
      
   pcm <-   ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of CORN-METRIC models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     all classifications, and all scopes for corn but with different metrics. \n",
        #                       "The distributions are based on 8 models each for RICH, SDI, SIDI, D, and RPR. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("RICH", "SDI", "SIDI", "D", "RPR"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa", "#fddbc7"))+
        scale_color_manual(name = "", labels = c("RICH", "SDI", "SIDI", "D", "RPR"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa", "#fddbc7"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
      ggsave(paste0("Images/","R1_CORN_RICH_SDI_SIDI_D_RPR_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
  
      
## Wheat - Combine the 5 Metric mixtures for WHEAT only (40 models each) ----
      
      rw.quant.plot_rich <-  readRDS("Images/RICHwwheatCDL, RCAG, ALLCNTY, BBOX2024-12-11.rds") %>%
        mutate(METRIC = 'RICH')
      
      rw.quant.plot_rich <- rw.quant.plot_rich %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rich <- rw.quant.plot_rich %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rich <- rw.quant.plot_rich %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rich <- left_join(ribbon1_rich, ribbon2_rich, by = c( "group","Percentile"))
      
      
      rw.quant.plot_sdi <-  readRDS("Images/SDIwwheatCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'SDI')
      
      rw.quant.plot_sdi <- rw.quant.plot_sdi %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_sdi <- rw.quant.plot_sdi %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_sdi <- rw.quant.plot_sdi %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_sdi <- left_join(ribbon1_sdi, ribbon2_sdi, by = c( "group","Percentile"))
      
      
      rw.quant.plot_sidi <-  readRDS("Images/SIDIwwheatCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'SIDI')
      
      rw.quant.plot_sidi <- rw.quant.plot_sidi %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_sidi <- rw.quant.plot_sidi %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_sidi <- rw.quant.plot_sidi %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_sidi <- left_join(ribbon1_sidi, ribbon2_sidi, by = c( "group","Percentile"))
      
      
      rw.quant.plot_d <-  readRDS("Images/DwwheatCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'D')
      
      rw.quant.plot_d <- rw.quant.plot_d %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_d <- rw.quant.plot_d %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_d <- rw.quant.plot_d %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_d <- left_join(ribbon1_d, ribbon2_d, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rpr <-  readRDS("Images/RPRwwheatCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RPR')
      
      rw.quant.plot_rpr <- rw.quant.plot_rpr %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rpr <- rw.quant.plot_rpr %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rpr <- rw.quant.plot_rpr %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rpr <- left_join(ribbon1_rpr, ribbon2_rpr, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_rich, ribbon_sdi, ribbon_sidi, ribbon_d, ribbon_rpr) %>% 
        mutate(METRIC = factor(METRIC, levels = c("RICH", "SDI", "SIDI", "D", "RPR"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_rich, rw.quant.plot_sdi, rw.quant.plot_sidi, rw.quant.plot_d, rw.quant.plot_rpr) %>% 
        mutate(METRIC = factor(METRIC, levels = c("RICH", "SDI", "SIDI", "D", "RPR")))
      
   pwm <-   ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of WHEAT-METRIC models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     all classifications, and all scopes for wheat but with different metrics. \n",
        #                       "The distributions are based on 8 models each for RICH, SDI, SIDI, D, and RPR. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("RICH", "SDI", "SIDI", "D", "RPR"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa", "#fddbc7"))+
        scale_color_manual(name = "", labels = c("RICH", "SDI", "SIDI", "D", "RPR"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa", "#fddbc7"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
      ggsave(paste0("Images/","R1_WHEAT_RICH_SDI_SIDI_D_RPR_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
      
      
 ## Hay - Combine the 5 Metric mixtures for HAY only (40 models each) ----
      
      rw.quant.plot_rich <-  readRDS("Images/RICHhayCDL, RCAG, ALLCNTY, BBOX2024-12-11.rds") %>%
        mutate(METRIC = 'RICH')
      
      rw.quant.plot_rich <- rw.quant.plot_rich %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rich <- rw.quant.plot_rich %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rich <- rw.quant.plot_rich %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rich <- left_join(ribbon1_rich, ribbon2_rich, by = c( "group","Percentile"))
      
      
      rw.quant.plot_sdi <-  readRDS("Images/SDIhayCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'SDI')
      
      rw.quant.plot_sdi <- rw.quant.plot_sdi %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_sdi <- rw.quant.plot_sdi %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_sdi <- rw.quant.plot_sdi %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_sdi <- left_join(ribbon1_sdi, ribbon2_sdi, by = c( "group","Percentile"))
      
      
      rw.quant.plot_sidi <-  readRDS("Images/SIDIhayCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'SIDI')
      
      rw.quant.plot_sidi <- rw.quant.plot_sidi %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_sidi <- rw.quant.plot_sidi %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_sidi <- rw.quant.plot_sidi %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_sidi <- left_join(ribbon1_sidi, ribbon2_sidi, by = c( "group","Percentile"))
      
      
      rw.quant.plot_d <-  readRDS("Images/DhayCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'D')
      
      rw.quant.plot_d <- rw.quant.plot_d %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_d <- rw.quant.plot_d %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_d <- rw.quant.plot_d %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_d <- left_join(ribbon1_d, ribbon2_d, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rpr <-  readRDS("Images/RPRhayCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RPR')
      
      rw.quant.plot_rpr <- rw.quant.plot_rpr %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rpr <- rw.quant.plot_rpr %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rpr <- rw.quant.plot_rpr %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rpr <- left_join(ribbon1_rpr, ribbon2_rpr, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_rich, ribbon_sdi, ribbon_sidi, ribbon_d, ribbon_rpr) %>% 
        mutate(METRIC = factor(METRIC, levels = c("RICH", "SDI", "SIDI", "D", "RPR"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_rich, rw.quant.plot_sdi, rw.quant.plot_sidi, rw.quant.plot_d, rw.quant.plot_rpr) %>% 
        mutate(METRIC = factor(METRIC, levels = c("RICH", "SDI", "SIDI", "D", "RPR")))
      
    phm <-  ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of HAY-METRIC models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     all classifications, and all scopes for hay but with different metrics. \n",
        #                       "The distributions are based on 8 models each for RICH, SDI, SIDI, D, and RPR. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("RICH", "SDI", "SIDI", "D", "RPR"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa", "#fddbc7"))+
        scale_color_manual(name = "", labels = c("RICH", "SDI", "SIDI", "D", "RPR"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa", "#fddbc7"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
       ggsave(paste0("Images/","R1_HAY_RICH_SDI_SIDI_D_RPR_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
      
  
  ## Soy - Combine the 5 Metric mixtures for SOY only (40 models each) ----
      
      rw.quant.plot_rich <-  readRDS("Images/RICHsoyCDL, RCAG, ALLCNTY, BBOX2024-12-11.rds") %>%
        mutate(METRIC = 'RICH')
       
       rw.quant.plot_rich <- rw.quant.plot_rich %>%
         mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
       
      
      ribbon1_rich <- rw.quant.plot_rich %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rich <- rw.quant.plot_rich %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rich <- left_join(ribbon1_rich, ribbon2_rich, by = c( "group","Percentile"))
      
      
      rw.quant.plot_sdi <-  readRDS("Images/SDIsoyCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'SDI')
      
      rw.quant.plot_sdi <- rw.quant.plot_sdi %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_sdi <- rw.quant.plot_sdi %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_sdi <- rw.quant.plot_sdi %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_sdi <- left_join(ribbon1_sdi, ribbon2_sdi, by = c( "group","Percentile"))
      
      
      rw.quant.plot_sidi <-  readRDS("Images/SIDIsoyCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'SIDI')
      
      rw.quant.plot_sidi <- rw.quant.plot_sidi %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_sidi <- rw.quant.plot_sidi %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_sidi <- rw.quant.plot_sidi %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_sidi <- left_join(ribbon1_sidi, ribbon2_sidi, by = c( "group","Percentile"))
      
      
      rw.quant.plot_d <-  readRDS("Images/DsoyCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'D')
      
      rw.quant.plot_d <- rw.quant.plot_d %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_d <- rw.quant.plot_d %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_d <- rw.quant.plot_d %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_d <- left_join(ribbon1_d, ribbon2_d, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rpr <-  readRDS("Images/RPRsoyCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RPR')
      
      rw.quant.plot_rpr <- rw.quant.plot_rpr %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rpr <- rw.quant.plot_rpr %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rpr <- rw.quant.plot_rpr %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rpr <- left_join(ribbon1_rpr, ribbon2_rpr, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_rich, ribbon_sdi, ribbon_sidi, ribbon_d, ribbon_rpr) %>% 
        mutate(METRIC = factor(METRIC, levels = c("RICH", "SDI", "SIDI", "D", "RPR"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_rich, rw.quant.plot_sdi, rw.quant.plot_sidi, rw.quant.plot_d, rw.quant.plot_rpr) %>% 
        mutate(METRIC = factor(METRIC, levels = c("RICH", "SDI", "SIDI", "D", "RPR")))
      
   psm <-   ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of SOY-METRIC models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     all classifications, and all scopes for soy but with different metrics. \n",
        #                       "The distributions are based on 8 models each for RICH, SDI, SIDI, D, and RPR. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("RICH", "SDI", "SIDI", "D", "RPR"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa", "#fddbc7"))+
        scale_color_manual(name = "", labels = c("RICH", "SDI", "SIDI", "D", "RPR"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa", "#fddbc7"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
       ggsave(paste0("Images/","R1_SOY_RICH_SDI_SIDI_D_RPR_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
     
       
## Alfalfa - Combine the 5 Metric mixtures for ALFALFA only (40 models each) ----
      
      rw.quant.plot_rich <-  readRDS("Images/RICHalfalfaCDL, RCAG, ALLCNTY, BBOX2024-12-11.rds") %>%
        mutate(METRIC = 'RICH')
       
       rw.quant.plot_rich <- rw.quant.plot_rich %>%
         mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
       
      
      ribbon1_rich <- rw.quant.plot_rich %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rich <- rw.quant.plot_rich %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rich <- left_join(ribbon1_rich, ribbon2_rich, by = c( "group","Percentile"))
      
      
      rw.quant.plot_sdi <-  readRDS("Images/SDIalfalfaCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'SDI')
      
      rw.quant.plot_sdi <- rw.quant.plot_sdi %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_sdi <- rw.quant.plot_sdi %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_sdi <- rw.quant.plot_sdi %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_sdi <- left_join(ribbon1_sdi, ribbon2_sdi, by = c( "group","Percentile"))
      
      
      rw.quant.plot_sidi <-  readRDS("Images/SIDIalfalfaCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'SIDI')
      
      rw.quant.plot_sidi <- rw.quant.plot_sidi %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_sidi <- rw.quant.plot_sidi %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_sidi <- rw.quant.plot_sidi %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_sidi <- left_join(ribbon1_sidi, ribbon2_sidi, by = c( "group","Percentile"))
      
      
      rw.quant.plot_d <-  readRDS("Images/DalfalfaCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'D')
      
      rw.quant.plot_d <- rw.quant.plot_d %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_d <- rw.quant.plot_d %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_d <- rw.quant.plot_d %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_d <- left_join(ribbon1_d, ribbon2_d, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rpr <-  readRDS("Images/RPRalfalfaCDL, RCAG, ALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RPR')
      
      rw.quant.plot_rpr <- rw.quant.plot_rpr %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rpr <- rw.quant.plot_rpr %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rpr <- rw.quant.plot_rpr %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rpr <- left_join(ribbon1_rpr, ribbon2_rpr, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_rich, ribbon_sdi, ribbon_sidi, ribbon_d, ribbon_rpr) %>% 
        mutate(METRIC = factor(METRIC, levels = c("RICH", "SDI", "SIDI", "D", "RPR"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_rich, rw.quant.plot_sdi, rw.quant.plot_sidi, rw.quant.plot_d, rw.quant.plot_rpr) %>% 
        mutate(METRIC = factor(METRIC, levels = c("RICH", "SDI", "SIDI", "D", "RPR")))
      
    pam <-  ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of ALFALFA-METRIC models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     all classifications, and all scopes for alfalfa but with different metrics. \n",
        #                       "The distributions are based on 8 models each for RICH, SDI, SIDI, D, and RPR. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("RICH", "SDI", "SIDI", "D", "RPR"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa", "#fddbc7"))+
        scale_color_manual(name = "", labels = c("RICH", "SDI", "SIDI", "D", "RPR"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa", "#fddbc7"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
       ggsave(paste0("Images/","R1_ALFALFA_RICH_SDI_SIDI_D_RPR_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
   
 
# Create spaghetti combinations that highlight variability ----
      
      
  ## WHEAT-RICH by AG,ALL, CDL, RC ----
      
      rw.quant.plot_cdlag <-  readRDS("Images/RICHwwheatCDLAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-AG')
       
       rw.quant.plot_cdlag <- rw.quant.plot_cdlag %>%
         mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
       
      
      ribbon1_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlag <- left_join(ribbon1_cdlag, ribbon2_cdlag, by = c( "group","Percentile"))
      
     
       rw.quant.plot_rcag <-  readRDS("Images/RICHwwheatRCAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-AG')
       
       rw.quant.plot_rcag <- rw.quant.plot_rcag %>%
         mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
       
      
      ribbon1_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcag <- left_join(ribbon1_rcag, ribbon2_rcag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_cdlall <-  readRDS("Images/RICHwwheatCDLALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-ALL')
      
      rw.quant.plot_cdlall <- rw.quant.plot_cdlall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlall <- left_join(ribbon1_cdlall, ribbon2_cdlall, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcall <-  readRDS("Images/RICHwwheatRCALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-ALL')
      
      rw.quant.plot_rcall <- rw.quant.plot_rcall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcall <- left_join(ribbon1_rcall, ribbon2_rcall, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_cdlag, ribbon_rcag, ribbon_cdlall, ribbon_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_cdlag, rw.quant.plot_rcag, rw.quant.plot_cdlall, rw.quant.plot_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL")))
      
 wr <- ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of WHEAT-RICH-Classification-Scope models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     for wheat, RICH, and varying classification and scope. \n",
        #                       "The distributions are based on 2 models each for combinations of CDL, RC, AG, and ALL. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), 
                    aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_color_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
       ggsave(paste0("Images/","R1_WHEAT_RICH_ClassScope_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
      
## HAY-RICH by AG,ALL, CDL, RC ----
      
      rw.quant.plot_cdlag <-  readRDS("Images/RICHhayCDLAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-AG')
       
       rw.quant.plot_cdlag <- rw.quant.plot_cdlag %>%
         mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
       
      
      ribbon1_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlag <- left_join(ribbon1_cdlag, ribbon2_cdlag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcag <-  readRDS("Images/RICHhayRCAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-AG')
      
      rw.quant.plot_rcag <- rw.quant.plot_rcag %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcag <- left_join(ribbon1_rcag, ribbon2_rcag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_cdlall <-  readRDS("Images/RICHhayCDLALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-ALL')
      
      rw.quant.plot_cdlall <- rw.quant.plot_cdlall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlall <- left_join(ribbon1_cdlall, ribbon2_cdlall, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcall <-  readRDS("Images/RICHhayRCALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-ALL')
      
      rw.quant.plot_rcall <- rw.quant.plot_rcall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcall <- left_join(ribbon1_rcall, ribbon2_rcall, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_cdlag, ribbon_rcag, ribbon_cdlall, ribbon_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_cdlag, rw.quant.plot_rcag, rw.quant.plot_cdlall, rw.quant.plot_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL")))
      
     hr <- ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of HAY-RICH-Classification-Scope models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     for hay, RICH, and varying classification and scope. \n",
        #                       "The distributions are based on 2 models each for combinations of CDL, RC, AG, and ALL. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), 
                    aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_color_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
       ggsave(paste0("Images/","R1_HAY_RICH_ClassScope_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
      
## CORN-RICH by AG,ALL, CDL, RC ----
      
      rw.quant.plot_cdlag <-  readRDS("Images/RICHcornCDLAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-AG')
       
       rw.quant.plot_cdlag <- rw.quant.plot_cdlag %>%
         mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
       
      
      ribbon1_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlag <- left_join(ribbon1_cdlag, ribbon2_cdlag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcag <-  readRDS("Images/RICHcornRCAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-AG')
      
      rw.quant.plot_rcag <- rw.quant.plot_rcag %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcag <- left_join(ribbon1_rcag, ribbon2_rcag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_cdlall <-  readRDS("Images/RICHcornCDLALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-ALL')
      
      rw.quant.plot_cdlall <- rw.quant.plot_cdlall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlall <- left_join(ribbon1_cdlall, ribbon2_cdlall, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcall <-  readRDS("Images/RICHcornRCALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-ALL')
      
      rw.quant.plot_rcall <- rw.quant.plot_rcall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcall <- left_join(ribbon1_rcall, ribbon2_rcall, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_cdlag, ribbon_rcag, ribbon_cdlall, ribbon_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_cdlag, rw.quant.plot_rcag, rw.quant.plot_cdlall, rw.quant.plot_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL")))
      
  cr <- ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of CORN-RICH-Classification-Scope models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     for corn, RICH, and varying classification and scope. \n",
        #                       "The distributions are based on 2 models each for combinations of CDL, RC, AG, and ALL. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), 
                    aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_color_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
      ggsave(paste0("Images/","R1_CORN_RICH_ClassScope_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
      
      
      ## ALFALFA-RICH by AG,ALL, CDL, RC ----
      
      rw.quant.plot_cdlag <-  readRDS("Images/RICHalfalfaCDLAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-AG')
      
      rw.quant.plot_cdlag <- rw.quant.plot_cdlag %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlag <- left_join(ribbon1_cdlag, ribbon2_cdlag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcag <-  readRDS("Images/RICHalfalfaRCAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-AG')
      
      rw.quant.plot_rcag <- rw.quant.plot_rcag %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcag <- left_join(ribbon1_rcag, ribbon2_rcag, by = c( "group","Percentile"))
      
     
       rw.quant.plot_cdlall <-  readRDS("Images/RICHalfalfaCDLALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-ALL')
       
       rw.quant.plot_cdlall <- rw.quant.plot_cdlall %>%
         mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
       
      
      ribbon1_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlall <- left_join(ribbon1_cdlall, ribbon2_cdlall, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcall <-  readRDS("Images/RICHalfalfaRCALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-ALL')
      
      rw.quant.plot_rcall <- rw.quant.plot_rcall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcall <- left_join(ribbon1_rcall, ribbon2_rcall, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_cdlag, ribbon_rcag, ribbon_cdlall, ribbon_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_cdlag, rw.quant.plot_rcag, rw.quant.plot_cdlall, rw.quant.plot_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL")))
      
   ar<- ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of ALFALFA-RICH-Classification-Scope models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     for alfalfa, RICH, and varying classification and scope. \n",
        #                       "The distributions are based on 2 models each for combinations of CDL, RC, AG, and ALL. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'),
                    aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_color_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
       ggsave(paste0("Images/","R1_ALFALFA_RICH_ClassScope_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
      
      ## SOY-RICH by AG,ALL, CDL, RC ----
      
      rw.quant.plot_cdlag <-  readRDS("Images/RICHsoyCDLAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-AG')
       
       rw.quant.plot_cdlag <- rw.quant.plot_cdlag %>%
         mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
       
      
      ribbon1_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlag <- left_join(ribbon1_cdlag, ribbon2_cdlag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcag <-  readRDS("Images/RICHsoyRCAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-AG')
      
      rw.quant.plot_rcag <- rw.quant.plot_rcag %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcag <- left_join(ribbon1_rcag, ribbon2_rcag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_cdlall <-  readRDS("Images/RICHsoyCDLALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-ALL')
      
      rw.quant.plot_cdlall <- rw.quant.plot_cdlall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlall <- left_join(ribbon1_cdlall, ribbon2_cdlall, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcall <-  readRDS("Images/RICHsoyRCALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-ALL')
      
      rw.quant.plot_rcall <- rw.quant.plot_rcall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcall <- left_join(ribbon1_rcall, ribbon2_rcall, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_cdlag, ribbon_rcag, ribbon_cdlall, ribbon_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_cdlag, rw.quant.plot_rcag, rw.quant.plot_cdlall, rw.quant.plot_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL")))
      
  sr <-    ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of SOY-RICH-Classification-Scope models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     for soy, RICH, and varying classification and scope. \n",
        #                       "The distributions are based on 2 models each for combinations of CDL, RC, AG, and ALL. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), 
                    aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_color_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
       ggsave(paste0("Images/","R1_SOY_RICH_ClassScope_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
      
      
      
    ## WHEAT-D by AG,ALL, CDL, RC ----
      
      rw.quant.plot_cdlag <-  readRDS("Images/DwwheatCDLAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-AG')
       
       rw.quant.plot_cdlag <- rw.quant.plot_cdlag %>%
         mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
       
      
      ribbon1_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlag <- left_join(ribbon1_cdlag, ribbon2_cdlag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcag <-  readRDS("Images/DwwheatRCAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-AG')
      
      rw.quant.plot_rcag <- rw.quant.plot_rcag %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcag <- left_join(ribbon1_rcag, ribbon2_rcag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_cdlall <-  readRDS("Images/DwwheatCDLALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-ALL')
      
      rw.quant.plot_cdlall <- rw.quant.plot_cdlall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlall <- left_join(ribbon1_cdlall, ribbon2_cdlall, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcall <-  readRDS("Images/DwwheatRCALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-ALL')
      
      rw.quant.plot_rcall <- rw.quant.plot_rcall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcall <- left_join(ribbon1_rcall, ribbon2_rcall, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_cdlag, ribbon_rcag, ribbon_cdlall, ribbon_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_cdlag, rw.quant.plot_rcag, rw.quant.plot_cdlall, rw.quant.plot_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL")))
      
    pwd <-  ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of WHEAT-D-Classification-Scope models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     for wheat, D, and varying classification and scope. \n",
        #                       "The distributions are based on 2 models each for combinations of CDL, RC, AG, and ALL. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), 
                    aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_color_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
       ggsave(paste0("Images/","R1_WHEAT_D_ClassScope_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
      
      ## HAY-D by AG,ALL, CDL, RC ----
      
      rw.quant.plot_cdlag <-  readRDS("Images/DhayCDLAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-AG')
       
       rw.quant.plot_cdlag <- rw.quant.plot_cdlag %>%
         mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
       
      
      ribbon1_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlag <- left_join(ribbon1_cdlag, ribbon2_cdlag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcag <-  readRDS("Images/DhayRCAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-AG')
      
      rw.quant.plot_rcag <- rw.quant.plot_rcag %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcag <- left_join(ribbon1_rcag, ribbon2_rcag, by = c( "group","Percentile"))
      
      rw.quant.plot_cdlall <-  readRDS("Images/DhayCDLALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-ALL')
      
      rw.quant.plot_cdlall <- rw.quant.plot_cdlall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlall <- left_join(ribbon1_cdlall, ribbon2_cdlall, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcall <-  readRDS("Images/DhayRCALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-ALL')
      
      rw.quant.plot_rcall <- rw.quant.plot_rcall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcall <- left_join(ribbon1_rcall, ribbon2_rcall, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_cdlag, ribbon_rcag, ribbon_cdlall, ribbon_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_cdlag, rw.quant.plot_rcag, rw.quant.plot_cdlall, rw.quant.plot_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL")))
      
    phd <-  ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of HAY-D-Classification-Scope models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     for hay, D, and varying classification and scope. \n",
        #                       "The distributions are based on 2 models each for combinations of CDL, RC, AG, and ALL. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), 
                    aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_color_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
       ggsave(paste0("Images/","R1_HAY_D_ClassScope_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
      
      ## CORN-D by AG,ALL, CDL, RC ----
      
      rw.quant.plot_cdlag <-  readRDS("Images/DcornCDLAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-AG')
       
       rw.quant.plot_cdlag <- rw.quant.plot_cdlag %>%
         mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
       
      
      ribbon1_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlag <- left_join(ribbon1_cdlag, ribbon2_cdlag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcag <-  readRDS("Images/DcornRCAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-AG')
      
      rw.quant.plot_rcag <- rw.quant.plot_rcag %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcag <- left_join(ribbon1_rcag, ribbon2_rcag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_cdlall <-  readRDS("Images/DcornCDLALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-ALL')
      
      rw.quant.plot_cdlall <- rw.quant.plot_cdlall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlall <- left_join(ribbon1_cdlall, ribbon2_cdlall, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcall <-  readRDS("Images/DcornRCALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-ALL')
      
      rw.quant.plot_rcall <- rw.quant.plot_rcall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcall <- left_join(ribbon1_rcall, ribbon2_rcall, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_cdlag, ribbon_rcag, ribbon_cdlall, ribbon_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_cdlag, rw.quant.plot_rcag, rw.quant.plot_cdlall, rw.quant.plot_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL")))
      
   pcd <-   ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of CORN-D-Classification-Scope models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     for corn, D, and varying classification and scope. \n",
        #                       "The distributions are based on 2 models each for combinations of CDL, RC, AG, and ALL. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), 
                    aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_color_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))
        # scale_alpha_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c(0.3, 0.6, 0.3, 0.6))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
       ggsave(paste0("Images/","R1_CORN_D_ClassScope_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
      
      ## ALFALFA-D by AG,ALL, CDL, RC ----
      
      rw.quant.plot_cdlag <-  readRDS("Images/DalfalfaCDLAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-AG')
       
       rw.quant.plot_cdlag <- rw.quant.plot_cdlag %>%
         mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
       
      
      ribbon1_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlag <- rw.quant.plot_cdlag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlag <- left_join(ribbon1_cdlag, ribbon2_cdlag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcag <-  readRDS("Images/DalfalfaRCAGCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-AG')
      
      rw.quant.plot_rcag <- rw.quant.plot_rcag %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcag <- rw.quant.plot_rcag %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcag <- left_join(ribbon1_rcag, ribbon2_rcag, by = c( "group","Percentile"))
      
      
      rw.quant.plot_cdlall <-  readRDS("Images/DalfalfaCDLALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'CDL-ALL')
      
      rw.quant.plot_cdlall <- rw.quant.plot_cdlall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_cdlall <- rw.quant.plot_cdlall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_cdlall <- left_join(ribbon1_cdlall, ribbon2_cdlall, by = c( "group","Percentile"))
      
      
      rw.quant.plot_rcall <-  readRDS("Images/DalfalfaRCALLCNTY, BBOX2024-12-12.rds") %>%
        mutate(METRIC = 'RC-ALL')
      
      rw.quant.plot_rcall <- rw.quant.plot_rcall %>%
        mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
      
      
      ribbon1_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
        arrange(name)%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) 
      
      ribbon2_rcall <- rw.quant.plot_rcall %>%
        dplyr::select(quant0.525:quant0.975, Percentile) %>%
        pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
        arrange(desc(name))%>%
        group_by(name) %>%
        mutate(group =  cur_group_id()) %>%
        mutate(group = 20 - group)
      
      ribbon_rcall <- left_join(ribbon1_rcall, ribbon2_rcall, by = c( "group","Percentile"))
      
      
      
      #### build combined plotting dataframes ----
      
      metric_ribbons <-  bind_rows(ribbon_cdlag, ribbon_rcag, ribbon_cdlall, ribbon_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"))) %>% 
        filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
      
      metric_quants <- bind_rows(rw.quant.plot_cdlag, rw.quant.plot_rcag, rw.quant.plot_cdlall, rw.quant.plot_rcall) %>% 
        mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL")))
      
   pad <-   ggplot() + 
        xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
        # ggtitle(paste0("Equal-weighted ensembles of ALFALFA-D-Classification-Scope models")) +
        # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
        #                     for alfalfa, D, and varying classification and scope. \n",
        #                       "The distributions are based on 2 models each for combinations of CDL, RC, AG, and ALL. \n",
        #                       "The 95% credibility intervals are shown in the shaded areas. \n",
        #                       "The lines indicate median posterior values.")) +
        geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), 
                    aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
        guides(alpha = FALSE, group = FALSE) +
        geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
        theme_light() + theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              panel.ontop = FALSE) +
        theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
        scale_fill_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_color_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
        scale_alpha(range = c(0.05, 0.25))
      
      #ca0020, #f4a582, #92c5de, #0571b0
      #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
      
       ggsave(paste0("Images/","R1_ALFALFA_D_ClassScope_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
      
   
   ## SOY-D by AG,ALL, CDL, RC ----
   
   rw.quant.plot_cdlag <-  readRDS("Images/DsoyCDLAGCNTY, BBOX2024-12-12.rds") %>%
     mutate(METRIC = 'CDL-AG')
       
       rw.quant.plot_cdlag <- rw.quant.plot_cdlag %>%
         mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
       
   
   ribbon1_cdlag <- rw.quant.plot_cdlag %>%
     dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
     pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
     arrange(name)%>%
     group_by(name) %>%
     mutate(group =  cur_group_id()) 
   
   ribbon2_cdlag <- rw.quant.plot_cdlag %>%
     dplyr::select(quant0.525:quant0.975, Percentile) %>%
     pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
     arrange(desc(name))%>%
     group_by(name) %>%
     mutate(group =  cur_group_id()) %>%
     mutate(group = 20 - group)
   
   ribbon_cdlag <- left_join(ribbon1_cdlag, ribbon2_cdlag, by = c( "group","Percentile"))
   
   
   rw.quant.plot_rcag <-  readRDS("Images/DsoyRCAGCNTY, BBOX2024-12-12.rds") %>%
     mutate(METRIC = 'RC-AG')
   
   rw.quant.plot_rcag <- rw.quant.plot_rcag %>%
     mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
   
   
   ribbon1_rcag <- rw.quant.plot_rcag %>%
     dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
     pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
     arrange(name)%>%
     group_by(name) %>%
     mutate(group =  cur_group_id()) 
   
   ribbon2_rcag <- rw.quant.plot_rcag %>%
     dplyr::select(quant0.525:quant0.975, Percentile) %>%
     pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
     arrange(desc(name))%>%
     group_by(name) %>%
     mutate(group =  cur_group_id()) %>%
     mutate(group = 20 - group)
   
   ribbon_rcag <- left_join(ribbon1_rcag, ribbon2_rcag, by = c( "group","Percentile"))
   
   
   rw.quant.plot_cdlall <-  readRDS("Images/DsoyCDLALLCNTY, BBOX2024-12-12.rds") %>%
     mutate(METRIC = 'CDL-ALL')
   
   rw.quant.plot_cdlall <- rw.quant.plot_cdlall %>%
     mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
   
   
   ribbon1_cdlall <- rw.quant.plot_cdlall %>%
     dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
     pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
     arrange(name)%>%
     group_by(name) %>%
     mutate(group =  cur_group_id()) 
   
   ribbon2_cdlall <- rw.quant.plot_cdlall %>%
     dplyr::select(quant0.525:quant0.975, Percentile) %>%
     pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
     arrange(desc(name))%>%
     group_by(name) %>%
     mutate(group =  cur_group_id()) %>%
     mutate(group = 20 - group)
   
   ribbon_cdlall <- left_join(ribbon1_cdlall, ribbon2_cdlall, by = c( "group","Percentile"))
   
   
   rw.quant.plot_rcall <-  readRDS("Images/DsoyRCALLCNTY, BBOX2024-12-12.rds") %>%
     mutate(METRIC = 'RC-ALL')
   
   rw.quant.plot_rcall <- rw.quant.plot_rcall %>%
     mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
   
   
   ribbon1_rcall <- rw.quant.plot_rcall %>%
     dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
     pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
     arrange(name)%>%
     group_by(name) %>%
     mutate(group =  cur_group_id()) 
   
   ribbon2_rcall <- rw.quant.plot_rcall %>%
     dplyr::select(quant0.525:quant0.975, Percentile) %>%
     pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
     arrange(desc(name))%>%
     group_by(name) %>%
     mutate(group =  cur_group_id()) %>%
     mutate(group = 20 - group)
   
   ribbon_rcall <- left_join(ribbon1_rcall, ribbon2_rcall, by = c( "group","Percentile"))
   
   
   
   #### build combined plotting dataframes ----
   
   metric_ribbons <-  bind_rows(ribbon_cdlag, ribbon_rcag, ribbon_cdlall, ribbon_rcall) %>% 
     mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"))) %>% 
     filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
   
   metric_quants <- bind_rows(rw.quant.plot_cdlag, rw.quant.plot_rcag, rw.quant.plot_cdlall, rw.quant.plot_rcall) %>% 
     mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL")))
   
   psd <-  ggplot() + 
     xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
     # ggtitle(paste0("Equal-weighted ensembles of SOY-D-Classification-Scope models")) +
     # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
     #                        for soy, D, and varying classification and scope. \n",
     #                       "The distributions are based on 2 models each for combinations of CDL, RC, AG, and ALL. \n",
     #                       "The 95% credibility intervals are shown in the shaded areas. \n",
     #                       "The lines indicate median posterior values.")) +
     geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), 
                 aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
     guides(alpha = FALSE, group = FALSE) +
     geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
     theme_light() + theme(panel.grid.major.x = element_blank(),
                           panel.grid.minor.x = element_blank(),
                           panel.ontop = FALSE) +
     theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
     scale_fill_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
     scale_color_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
     scale_alpha(range = c(0.05, 0.25))
   
   #ca0020, #f4a582, #92c5de, #0571b0
   #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
   
    ggsave(paste0("Images/","R1_SOY_D_ClassScope_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
   
   
     
    
    ## WHEAT-SIDI by AG,ALL, CDL, RC ----
    
    rw.quant.plot_cdlag <-  readRDS("Images/SIDIwwheatCDLAGCNTY, BBOX2024-12-12.rds") %>%
      mutate(METRIC = 'CDL-AG')
    
    rw.quant.plot_cdlag <- rw.quant.plot_cdlag %>%
      mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
    
    
    ribbon1_cdlag <- rw.quant.plot_cdlag %>%
      dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_cdlag <- rw.quant.plot_cdlag %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_cdlag <- left_join(ribbon1_cdlag, ribbon2_cdlag, by = c( "group","Percentile"))
    
    
    rw.quant.plot_rcag <-  readRDS("Images/SIDIwwheatRCAGCNTY, BBOX2024-12-12.rds") %>%
      mutate(METRIC = 'RC-AG')
    
    rw.quant.plot_rcag <- rw.quant.plot_rcag %>%
      mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
    
    
    ribbon1_rcag <- rw.quant.plot_rcag %>%
      dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_rcag <- rw.quant.plot_rcag %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_rcag <- left_join(ribbon1_rcag, ribbon2_rcag, by = c( "group","Percentile"))
    
    
    rw.quant.plot_cdlall <-  readRDS("Images/SIDIwwheatCDLALLCNTY, BBOX2024-12-12.rds") %>%
      mutate(METRIC = 'CDL-ALL')
    
    rw.quant.plot_cdlall <- rw.quant.plot_cdlall %>%
      mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
    
    
    ribbon1_cdlall <- rw.quant.plot_cdlall %>%
      dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_cdlall <- rw.quant.plot_cdlall %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_cdlall <- left_join(ribbon1_cdlall, ribbon2_cdlall, by = c( "group","Percentile"))
    
    
    rw.quant.plot_rcall <-  readRDS("Images/SIDIwwheatRCALLCNTY, BBOX2024-12-12.rds") %>%
      mutate(METRIC = 'RC-ALL')
    
    rw.quant.plot_rcall <- rw.quant.plot_rcall %>%
      mutate(across(starts_with("quant"), ~ 100*((exp(.x) - 1)))) ## 012226 R1 addition - explicit transformation of coefficients to a percent change interpretation [% Change in Outcome Variable = 100 *(eβ-1)]
    
    
    ribbon1_rcall <- rw.quant.plot_rcall %>%
      dplyr::select(quant0.025:quant0.475, Percentile, METRIC) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2_rcall <- rw.quant.plot_rcall %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    ribbon_rcall <- left_join(ribbon1_rcall, ribbon2_rcall, by = c( "group","Percentile"))
    
    
    
    #### build combined plotting dataframes ----
    
    metric_ribbons <-  bind_rows(ribbon_cdlag, ribbon_rcag, ribbon_cdlall, ribbon_rcall) %>% 
      mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"))) %>% 
      filter(name.y == 'quant0.975') # 12132024 just take the 95% credibility interval for simplicity (plots get too crowded with more than 2 distributions)
    
    metric_quants <- bind_rows(rw.quant.plot_cdlag, rw.quant.plot_rcag, rw.quant.plot_cdlall, rw.quant.plot_rcall) %>% 
      mutate(METRIC = factor(METRIC, levels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL")))
    
    wsidi <- ggplot() + 
      xlab("Landscape Diversity Percentile") + ylab("Percent Change in Yield") + 
      # ggtitle(paste0("Equal-weighted ensembles of WHEAT-SIDI-Classification-Scope models")) +
      # labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with all boundaries,  
      #                     for wheat, SIDI, and varying classification and scope. \n",
      #                       "The distributions are based on 2 models each for combinations of CDL, RC, AG, and ALL. \n",
      #                       "The 95% credibility intervals are shown in the shaded areas. \n",
      #                       "The lines indicate median posterior values.")) +
      geom_ribbon(data = metric_ribbons %>% filter(name.y == 'quant0.975'), 
                  aes(x=Percentile, ymin = Min, ymax = Max, group = METRIC, fill = METRIC), alpha = 0.4) +
      guides(alpha = FALSE, group = FALSE) +
      geom_line(data=metric_quants, aes(x=Percentile, y=`quant0.5`, color = METRIC), size = 1) +
      theme_light() + theme(panel.grid.major.x = element_blank(),
                            panel.grid.minor.x = element_blank(),
                            panel.ontop = FALSE) +
      theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 8)) +
      scale_fill_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
      scale_color_manual(name = "", labels = c("CDL-AG", "RC-AG", "CDL-ALL", "RC-ALL"), values = c("#b2182b", "#f4a582", "#4d4d4d", "#bababa"))+
      scale_alpha(range = c(0.05, 0.25))
    
    #ca0020, #f4a582, #92c5de, #0571b0
    #b2182b, #f4a582, #4d4d4d, #bababa, #fddbc7
    
    ggsave(paste0("Images/","R1_WHEAT_SIDI_ClassScope_comparison_simplified_", Sys.Date(), ".jpg"), width = 6, height = 6, units = "in")
    
    

  # Create multi-panel plots ----
      
    ## Figure 3  ----
   
      pd <- ggarrange(pcd + labs(subtitle = "Corn") + #ylim(-13,14) +
                        theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05), axis.title.x = element_blank()), 
                      pwd + labs(subtitle = "Wheat") + #ylim(-13,14) +
                        theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),axis.title.x = element_blank(), axis.title.y = element_blank()), 
                      phd + labs(subtitle = "Hay") + #ylim(-13,14) +
                        theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),axis.title.x = element_blank()), 
                      pad + labs(subtitle = "Alfalfa") + xlab("Landscape Diversity Percentile") + #ylim(-13,14) +
                        theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),axis.title.y = element_blank()), 
                      psd + labs(subtitle = "Soybean") + xlab("Landscape Diversity Percentile") + #ylim(-13,14) +
                        theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),), 
                      ncol = 2, nrow = 3, 
                      common.legend = TRUE, legend = "bottom",
                      labels = c("(a)", "(b)", "(c)", "(d)", "(e)"),
                      label.y = 1.02,
                      label.x = 0.08,
                      align = "hv",
                      heights = c(1, 1, 1), widths = c(1, 1))
    
    pd
   
   ggsave(paste0("Images/","Figure3_D_crop_scope_class_freey_Final_", Sys.Date(), ".jpg") , width = 6, height = 7, dpi = 300)
   
   
   ## Figure 4 ----
   pr <- ggarrange(cr + labs(subtitle = "Corn") + #ylim(-5,10) +
                     theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05), axis.title.x = element_blank()), 
                   wr + labs(subtitle = "Wheat") + #ylim(-5,10) +
                     theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),axis.title.x = element_blank(), axis.title.y = element_blank()), 
                   hr + labs(subtitle = "Hay") + #ylim(-5,10) +
                     theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),axis.title.x = element_blank()), 
                   ar + labs(subtitle = "Alfalfa") + xlab("Landscape Diversity Percentile") + #ylim(-5,10) +
                     theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),axis.title.y = element_blank()), 
                   sr + labs(subtitle = "Soybean") + xlab("Landscape Diversity Percentile") + #ylim(-5,10) +
                     theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),), 
                   ncol = 2, nrow = 3, 
                   common.legend = TRUE, legend = "bottom",
                   labels = c("(a)", "(b)", "(c)", "(d)", "(e)"),
                   label.y = 1.02,
                   label.x = 0.08,
                   align = "hv",
                   heights = c(1, 1, 1), widths = c(1, 1)
                   )
   
   pr
   
   ggsave(paste0("Images/","Figure4_RICH_crop_scope_class_freey_FINAL_", Sys.Date(), ".jpg") , width = 6, height = 7, dpi = 300)
   
   
   ## Fig S11 Crop Comparison  ----
   
   pcropmetric <- ggarrange(pcm + labs(subtitle = "Corn") + #ylim(-12,13) +
                     theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05), axis.title.x = element_blank()), 
                   pwm + labs(subtitle = "Wheat") + #ylim(-12,13) +
                     theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),axis.title.x = element_blank(), axis.title.y = element_blank()), 
                   phm + labs(subtitle = "Hay") + #ylim(-12,13) +
                     theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),axis.title.x = element_blank()), 
                   pam + labs(subtitle = "Alfalfa") + xlab("Landscape Diversity Percentile") + #ylim(-12,13) +
                     theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),axis.title.y = element_blank()), 
                   psm + labs(subtitle = "Soybean") + xlab("Landscape Diversity Percentile") + #ylim(-12,13) +
                     theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),), 
                   ncol = 2, nrow = 3, 
                   common.legend = TRUE, legend = "bottom",
                   labels = c("A", "B", "C", "D", "E"),
                   label.y = 1.02,
                   label.x = 0.08,
                   align = "hv",
                   heights = c(1, 1, 1), widths = c(1, 1))
   
   pcropmetric
   
   ggsave(paste0("Images/","R1_Figure5_crop_metric_freey", Sys.Date(), ".jpg") , width = 6, height = 7)
  
   
   ## Figure S15  ----
   
   ggarrange(pcdlrc + labs(subtitle = "Classification") + 
               xlab("Landscape Diversity Percentile") + 
               theme(plot.caption = element_blank(), plot.title = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),),
             pagall + labs(subtitle = "Scope") + 
               xlab("Landscape Diversity Percentile") + 
               theme(plot.caption = element_blank(), plot.title = element_blank(), axis.text.y = element_blank(), plot.subtitle = element_text(vjust = -7, hjust = .05),),
                          labels = c("A", "B"))
   
   
   ggsave("R1_FigureS9_scope_class.jpg", , width = 6, height = 4)
   