# Process Item #9
# Description: Script for exploratory descriptives of metrics (including maps)
# Author: Kate Nelson
# Date updated: 03/27/2024


library(pacman)
p_load(tidyverse, corrplot, ggridges, tigris, ggsci, sf, ggpubr)


master_lm <- readRDS("StartingData/master_lm_04032024.rds")



# Correlations ----
cor_master_tbl <- cor(master_lm %>% 
                        dplyr::select(SDI_CDL_AG:RPR_CDL_AG, 
                                      SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                                      SDI_CDL_ALL:RPR_CDL_ALL,
                                      SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                                      SDI_RC_AG:RPR_RC_AG,
                                      SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                                      SDI_RC_ALL:RPR_RC_ALL,
                                      SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                                      D_CDL_BBOX_ALL:D_RC_AG) %>% 
                        na.omit())

testRes = cor.mtest(master_lm %>% 
                      dplyr::select(SDI_CDL_AG:RPR_CDL_AG, 
                                    SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                                    SDI_CDL_ALL:RPR_CDL_ALL,
                                    SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                                    SDI_RC_AG:RPR_RC_AG,
                                    SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                                    SDI_RC_ALL:RPR_RC_ALL,
                                    SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                                    D_CDL_BBOX_ALL:D_RC_AG) %>% 
                      na.omit(), conf.level = 0.95)

jpeg("corrplot_071425.jpeg", width = 6, height = 6, units = "in", res = 300)

corrplot(cor_master_tbl, method = 'color', order = 'hclust', type = 'upper',
         tl.col = 'black', tl.cex = .5, col = COL2('PuOr', 10))

dev.off()

ggsave("corrplot_071425.jpeg", width = 6, height = 6, units = "in")


# Distributions ----

div_df <- master_lm %>% 
  dplyr::select(SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_AG:D_RC_AG) %>%
  na.omit()

div_df <- div_df %>%
  mutate_all(scale) %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  arrange(metric)


ggplot(div_df, aes(x = values, y = reorder(metric, metric))) + 
  geom_density_ridges(scale = 0.9, bandwidth = 0.25) +
  scale_x_continuous(limits = c(-3, 3)) +
  coord_cartesian(clip = "off") +
  theme_minimal()



ggplot(div_df, aes(x = values, y = reorder(metric, metric),  fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE, bandwidth = 0.25, scale = 0.9) +
  scale_fill_viridis_d(name = "Quartiles") +
  scale_x_continuous(limits = c(-3, 3)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))


## distributions take 2 ----

div_df <- master_lm %>% 
  dplyr::select(SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_ALL:D_RC_AG) %>%
  na.omit()
  

div_df <- div_df %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  mutate(metric_type = str_extract(metric, ".*(?=_CDL|_RC)"),
         metric_variety = str_extract(metric, "(_CDL|_RC).*$")) %>%
  filter(metric_type != "PRD")
 


ggplot(div_df, aes(x = values, y = metric, fill = metric_variety)) + 
  geom_density_ridges(scale = 0.9) +
  facet_wrap(~metric_type, nrow = 3, scales = "free") +
  theme_minimal() +
  theme(axis.text = element_text(size = 5),
        legend.position = "none") +
  scale_fill_viridis_d(name = "")



ggsave("ridgeplots_071425.jpeg", width = 4, height = 6, units = "in")

# Descriptive Statistics ----

div_df <- master_lm %>% 
  dplyr::select(SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_ALL:D_RC_AG) %>%
  na.omit()


des_tbl <- div_df %>%
  summarise(across(everything(),
                   .fns = list(mean = ~mean(.x, na.rm = TRUE),
                               median = ~median(.x, na.rm = TRUE),
                               sd = ~sd(.x, na.rm = TRUE),
                               low = ~quantile(.x, 0.05, na.rm = TRUE),
                               high = ~max(.x, 0.95, na.rm = TRUE)),
                   .names = "{.col}.{.fn}")) %>% # pivot and break names by last underscore
  pivot_longer(everything(), 
               names_to = c("metric", ".value"), 
               names_sep = "\\.") 

# Maps!!! ----

projection <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs" #102003 epsg code for USA albers equal area conic


counties <- tigris::counties() %>%
  filter(!(STATEFP %in% c("02", "15"))) %>%
  filter(STATEFP < 60) %>%
  st_transform(projection) 



cnty_lm <- left_join(counties, master_lm, by = c("GEOID" = "CNTY"))

cnty_lm_long <- master_lm %>% 
  dplyr::select(CNTY, YEAR, SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_ALL:D_RC_AG) %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  mutate(metric_type = str_extract(metric, ".*(?=_CDL|_RC)"),
         metric_variety = str_extract(metric, "(CDL|RC).*$")) 

cnty_lm_long_sf <- left_join(counties, cnty_lm_long, by = c("GEOID" = "CNTY"))

ggplot() +
  geom_sf(data = cnty_lm_long_sf %>% filter(YEAR == 2008 & metric_type == "SDI"), 
          aes(fill = values), color = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_viridis_c() +
  ggtitle("SDI") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")


ggsave("SDI_2008_maps_071425.jpeg", height = 8, width = 6, units = "in")

ggplot() +
  geom_sf(data = cnty_lm_long_sf %>% filter(YEAR == 2008 & metric_type == "RICH"), 
          aes(fill = values), color = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_viridis_c() +
  ggtitle("RICH") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")


ggsave("RICH_2008_maps_071425.jpeg", height = 8, width = 6, units = "in")


ggplot() +
  geom_sf(data = cnty_lm_long_sf %>% filter(YEAR == 2008 & metric_type == "D"), 
          aes(fill = values), color = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_viridis_c() +
  ggtitle("D") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")


ggsave("D_2008_maps_071425.jpeg", height = 8, width = 6, units = "in")




ggplot() +
  geom_sf(data = cnty_lm_long_sf %>% filter(YEAR == 2008 & metric_type == "SIDI"), 
          aes(fill = values), color = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_viridis_c() +
  ggtitle("SIDI") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")


ggsave("SIDI_2008_maps_071425.jpeg", height = 8, width = 6, units = "in")

ggplot() +
  geom_sf(data = cnty_lm_long_sf %>% filter(YEAR == 2008 & metric_type == "RPR"), 
          aes(fill = values), color = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_viridis_c() +
  ggtitle("RPR") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")


ggsave("RPR_2008_maps_071425.jpeg", height = 8, width = 6, units = "in")


# Summary maps of diversity metrics ----

# 95th and 5th percentile maps for SDI, RICH, D, PRD, SIDI, RPR using master landscape metric dataset

master_lm_95th_sf <- cnty_lm_long_sf %>%
  filter(metric_type != "PRD") %>%
  group_by(metric_type, metric_variety) %>%
  filter(values >= quantile(values, probs = 0.95, na.rm = TRUE)) %>%
  mutate(Prob = 0.95)

master_lm_5th_sf <- cnty_lm_long_sf %>%
  filter(metric_type != "PRD") %>%
  group_by(metric_type, metric_variety) %>%
  filter(values <= quantile(values, probs = 0.05, na.rm = TRUE)) %>%
  mutate(Prob = 0.05)

## Master 95th and 5th percentile maps ----

#label counties that exceed the 95th percentile from master_lm_95th


master_tails_sf <- master_lm_95th_sf %>%
  bind_rows(master_lm_5th_sf) %>%
  mutate(metric_type = factor(metric_type, levels = c("SDI", "RICH", "D", "SIDI", "RPR")),
         Prob = factor(Prob))
  
state_bndry  <- tigris::states() %>%
  filter(!(STATEFP %in% c("02", "15"))) %>%
  filter(STATEFP < 60) %>%
  st_transform(projection)

### SDI ----
ggplot() +
  geom_sf(data = master_tails_sf %>% filter(YEAR == 2008 & metric_type == "SDI"), 
          aes(fill = Prob), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_npg() +
  ggtitle("SDI") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")


ggsave("SDI_2008_tails_maps.jpeg", height = 8, width = 6, units = "in")

### SIDI ----
ggplot() +
  geom_sf(data = master_tails_sf %>% filter(YEAR == 2008 & metric_type == "SIDI"), 
          aes(fill = Prob), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_npg() +
  ggtitle("SIDI") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")


ggsave("SIDI_2008_tails_maps.jpeg", height = 8, width = 6, units = "in")

### D ----
ggplot() +
  geom_sf(data = master_tails_sf %>% filter(YEAR == 2008 & metric_type == "D"), 
          aes(fill = Prob), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_npg() +
  ggtitle("D") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")


ggsave("D_2008_tails_maps.jpeg", height = 8, width = 6, units = "in")

### RICH ----
ggplot() +
  geom_sf(data = master_tails_sf %>% filter(YEAR == 2008 & metric_type == "RICH"), 
          aes(fill = Prob), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_npg() +
  ggtitle("RICH") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")


ggsave("RICH_2008_tails_maps.jpeg", height = 8, width = 6, units = "in")

### RPR ----
ggplot() +
  geom_sf(data = master_tails_sf %>% filter(YEAR == 2008 & metric_type == "RPR"), 
          aes(fill = Prob), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_npg() +
  ggtitle("RPR") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")

ggsave("RPR_2008_tails_maps.jpeg", height = 8, width = 6, units = "in")

### All Metrics Figure  ----

master_tails_summ <- master_tails_sf %>%
  st_set_geometry(NULL) %>%
  filter(YEAR == 2008) %>%
  distinct(GEOID, metric, Prob, .keep_all = TRUE) %>% # some counties exist as multipolygons and therefore join multiple times...
  group_by(GEOID, Prob) %>%
  mutate(count = n(),
         freq = round(count/40*100)) %>% 
  distinct(GEOID, Prob, .keep_all = TRUE) %>%
  ungroup() 

master_tails_summ_sf <- left_join(counties, master_tails_summ, by = c("GEOID" = "GEOID")) %>%
  mutate(freq = if_else(is.na(freq) == T, 0, freq))

  


ggplot() +
  geom_sf(data = master_tails_summ_sf %>% filter(YEAR == 2008), 
          aes(fill = freq), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  #ggtitle("Frequency in 95th and 5th Percentile") +
  scale_fill_viridis_c() +
  facet_wrap(~Prob, nrow = 1) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

ggsave("ALLMETRICS_2018_tails_maps.jpeg", height = 4, width = 6, units = "in")



## Master percentile maps ----

# percentile maps for SDI, RICH, D, PRD, SIDI, RPR using master landscape metric dataset

master_lm_perc_sf <- cnty_lm_long_sf %>%
  filter(metric_type != "PRD") %>%
  group_by(metric) %>%
  mutate(quants = ntile(values, 10),
         Prob = quants*10)



#label counties based on percentile rather than value


master_tiles_sf <- master_lm_perc_sf %>%
   mutate(metric_type = factor(metric_type, levels = c("SDI", "RICH", "D", "SIDI", "RPR")))

state_bndry  <- tigris::states() %>%
  filter(!(STATEFP %in% c("02", "15"))) %>%
  filter(STATEFP < 60) %>%
  st_transform(projection)

### SDI ----
ggplot() +
  geom_sf(data = master_tiles_sf %>% filter(YEAR == 2018 & metric_type == "SDI"), 
          aes(fill = Prob), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_viridis_c() +
  ggtitle("SDI") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")


ggsave("SDI_2018_percentiles_maps.jpeg", height = 8, width = 6, units = "in")

### SIDI ----
ggplot() +
  geom_sf(data = master_tiles_sf %>% filter(YEAR == 2018 & metric_type == "SIDI"), 
          aes(fill = Prob), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_viridis_c() +
  ggtitle("SIDI") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")


ggsave("SIDI_2018_percentiles_maps.jpeg", height = 8, width = 6, units = "in")

### D ----
ggplot() +
  geom_sf(data = master_tiles_sf %>% filter(YEAR == 2018 & metric_type == "D"), 
          aes(fill = Prob), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_viridis_c() +
  ggtitle("D") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")


ggsave("D_2018_percentiles_maps.jpeg", height = 8, width = 6, units = "in")

### RICH ----
ggplot() +
  geom_sf(data = master_tiles_sf %>% filter(YEAR == 2018 & metric_type == "RICH"), 
          aes(fill = Prob), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_viridis_c() +
  ggtitle("RICH") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")


ggsave("RICH_2018_percentiles_maps.jpeg", height = 8, width = 6, units = "in")

### RPR ----
ggplot() +
  geom_sf(data = master_tiles_sf %>% filter(YEAR == 2018 & metric_type == "RPR"), 
          aes(fill = Prob), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_viridis_c() +
  ggtitle("RPR") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(fill = "")

ggsave("RPR_2018_percentiles_maps.jpeg", height = 8, width = 6, units = "in")


## Crop based 95th and 5th percentile maps ----


### Corn ----

corn <- readRDS("PreppedData/corn_ntiles_04032024.rds")

corn_long <- corn %>% 
  dplyr::select(CNTY, YEAR, GEOID, SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_ALL:D_RC_AG) %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  mutate(metric_type = str_extract(metric, ".*(?=_CDL|_RC)"),
         metric_variety = str_extract(metric, "(CDL|RC).*$")) 

corn_lm_95th_sf <- corn_long %>%
  filter(metric_type != "PRD") %>%
  group_by(metric_type, metric_variety) %>%
  filter(values >= quantile(values, probs = 0.95, na.rm = TRUE)) %>%
  mutate(Prob = 0.95)

corn_lm_5th_sf <- corn_long %>%
  filter(metric_type != "PRD") %>%
  group_by(metric_type, metric_variety) %>%
  filter(values <= quantile(values, probs = 0.05, na.rm = TRUE)) %>%
  mutate(Prob = 0.05)

corn_tails_sf <- corn_lm_95th_sf %>%
  bind_rows(corn_lm_5th_sf) %>%
  mutate(metric_type = factor(metric_type, levels = c("SDI", "RICH", "D", "SIDI", "RPR")),
         Prob = factor(Prob))

corn_tails_summ <- corn_tails_sf %>%
  st_set_geometry(NULL) %>%
  filter(YEAR == 2018) %>%
  distinct(GEOID, metric, Prob, .keep_all = TRUE) %>% # some counties exist as multipolygons and therefore join multiple times...
  group_by(GEOID, Prob) %>%
  mutate(count = n(),
         freq = round(count/40*100)) %>% 
  distinct(GEOID, Prob, .keep_all = TRUE) %>%
  ungroup() 

corn_tails_summ_sf <- left_join(counties, corn_tails_summ, by = c("GEOID" = "GEOID")) %>%
  mutate(freq = if_else(is.na(freq) == T, 0, freq))


mc <- ggplot() +
  geom_sf(data = corn_tails_summ_sf %>% filter(YEAR == 2008), 
          aes(fill = freq), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  ggtitle("Frequency in 95th and 5th Percentile for Corn Counties") +
  scale_fill_viridis_c() +
  facet_wrap(~Prob, nrow = 1) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

# ggsave("ALLMETRICS_2008_tails_maps_corn.jpeg", height = 8, width = 6, units = "in")

### Soy ----
soy <- readRDS("PreppedData/soy_ntiles_04032024.rds")

soy_long <- soy %>% 
  dplyr::select(CNTY, YEAR, GEOID, SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_ALL:D_RC_AG) %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  mutate(metric_type = str_extract(metric, ".*(?=_CDL|_RC)"),
         metric_variety = str_extract(metric, "(CDL|RC).*$")) 

soy_lm_95th_sf <- soy_long %>%
  filter(metric_type != "PRD") %>%
  group_by(metric_type, metric_variety) %>%
  filter(values >= quantile(values, probs = 0.95, na.rm = TRUE)) %>%
  mutate(Prob = 0.95)

soy_lm_5th_sf <- soy_long %>%
  filter(metric_type != "PRD") %>%
  group_by(metric_type, metric_variety) %>%
  filter(values <= quantile(values, probs = 0.05, na.rm = TRUE)) %>%
  mutate(Prob = 0.05)

soy_tails_sf <- soy_lm_95th_sf %>%
  bind_rows(soy_lm_5th_sf) %>%
  mutate(metric_type = factor(metric_type, levels = c("SDI", "RICH", "D", "SIDI", "RPR")),
         Prob = factor(Prob))

soy_tails_summ <- soy_tails_sf %>%
  st_set_geometry(NULL) %>%
  filter(YEAR == 2018) %>%
  distinct(GEOID, metric, Prob, .keep_all = TRUE) %>% # some counties exist as multipolygons and therefore join multiple times...
  group_by(GEOID, Prob) %>%
  mutate(count = n(),
         freq = round(count/40*100)) %>% 
  distinct(GEOID, Prob, .keep_all = TRUE) %>%
  ungroup() 

soy_tails_summ_sf <- left_join(counties, soy_tails_summ, by = c("GEOID" = "GEOID")) %>%
  mutate(freq = if_else(is.na(freq) == T, 0, freq))


ms <- ggplot() +
  geom_sf(data = soy_tails_summ_sf %>% filter(YEAR == 2008), 
          aes(fill = freq), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  ggtitle("Frequency in 95th and 5th Percentile for Soy Counties") +
  scale_fill_viridis_c() +
  facet_wrap(~Prob, nrow = 1) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

# ggsave("ALLMETRICS_2008_tails_maps_soy.jpeg", height = 8, width = 6, units = "in")

### Wheat ----
wwheat <- readRDS("PreppedData/wwheat_ntiles_04032024.rds")

wwheat_long <- wwheat %>% 
  dplyr::select(CNTY, YEAR, GEOID, SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_ALL:D_RC_AG) %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  mutate(metric_type = str_extract(metric, ".*(?=_CDL|_RC)"),
         metric_variety = str_extract(metric, "(CDL|RC).*$")) 

wwheat_lm_95th_sf <- wwheat_long %>%
  filter(metric_type != "PRD") %>%
  group_by(metric_type, metric_variety) %>%
  filter(values >= quantile(values, probs = 0.95, na.rm = TRUE)) %>%
  mutate(Prob = 0.95)

wwheat_lm_5th_sf <- wwheat_long %>%
  filter(metric_type != "PRD") %>%
  group_by(metric_type, metric_variety) %>%
  filter(values <= quantile(values, probs = 0.05, na.rm = TRUE)) %>%
  mutate(Prob = 0.05)

wwheat_tails_sf <- wwheat_lm_95th_sf %>%
  bind_rows(wwheat_lm_5th_sf) %>%
  mutate(metric_type = factor(metric_type, levels = c("SDI", "RICH", "D", "SIDI", "RPR")),
         Prob = factor(Prob))

wwheat_tails_summ <- wwheat_tails_sf %>%
  st_set_geometry(NULL) %>%
  filter(YEAR == 2018) %>%
  distinct(GEOID, metric, Prob, .keep_all = TRUE) %>% # some counties exist as multipolygons and therefore join multiple times...
  group_by(GEOID, Prob) %>%
  mutate(count = n(),
         freq = round(count/40*100)) %>% 
  distinct(GEOID, Prob, .keep_all = TRUE) %>%
  ungroup() 

wwheat_tails_summ_sf <- left_join(counties, wwheat_tails_summ, by = c("GEOID" = "GEOID")) %>%
  mutate(freq = if_else(is.na(freq) == T, 0, freq))


mw <- ggplot() +
  geom_sf(data = wwheat_tails_summ_sf %>% filter(YEAR == 2008), 
          aes(fill = freq), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  ggtitle("Frequency in 95th and 5th Percentile for Winter Wheat Counties") +
  scale_fill_viridis_c() +
  facet_wrap(~Prob, nrow = 1) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

# ggsave("ALLMETRICS_2008_tails_maps_wwheat.jpeg", height = 8, width = 6, units = "in")

### Hay ----
hay <- readRDS("PreppedData/hay_ntiles_04032024.rds")

hay_long <- hay %>% 
  dplyr::select(CNTY, YEAR, GEOID, SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_ALL:D_RC_AG) %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  mutate(metric_type = str_extract(metric, ".*(?=_CDL|_RC)"),
         metric_variety = str_extract(metric, "(CDL|RC).*$")) 

hay_lm_95th_sf <- hay_long %>%
  filter(metric_type != "PRD") %>%
  group_by(metric_type, metric_variety) %>%
  filter(values >= quantile(values, probs = 0.95, na.rm = TRUE)) %>%
  mutate(Prob = 0.95)

hay_lm_5th_sf <- hay_long %>%
  filter(metric_type != "PRD") %>%
  group_by(metric_type, metric_variety) %>%
  filter(values <= quantile(values, probs = 0.05, na.rm = TRUE)) %>%
  mutate(Prob = 0.05)

hay_tails_sf <- hay_lm_95th_sf %>%
  bind_rows(hay_lm_5th_sf) %>%
  mutate(metric_type = factor(metric_type, levels = c("SDI", "RICH", "D", "SIDI", "RPR")),
         Prob = factor(Prob))

hay_tails_summ <- hay_tails_sf %>%
  st_set_geometry(NULL) %>%
  filter(YEAR == 2018) %>%
  distinct(GEOID, metric, Prob, .keep_all = TRUE) %>% # some counties exist as multipolygons and therefore join multiple times...
  group_by(GEOID, Prob) %>%
  mutate(count = n(),
         freq = round(count/40*100)) %>% 
  distinct(GEOID, Prob, .keep_all = TRUE) %>%
  ungroup() 

hay_tails_summ_sf <- left_join(counties, hay_tails_summ, by = c("GEOID" = "GEOID")) %>%
  mutate(freq = if_else(is.na(freq) == T, 0, freq))


mh <- ggplot() +
  geom_sf(data = hay_tails_summ_sf %>% filter(YEAR == 2008), 
          aes(fill = freq), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  ggtitle("Frequency in 95th and 5th Percentile for Hay Counties") +
  scale_fill_viridis_c() +
  facet_wrap(~Prob, nrow = 1) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

# ggsave("ALLMETRICS_2008_tails_maps_hay.jpeg", height = 8, width = 6, units = "in")


### Alfalfa ----
alfalfa <- readRDS("PreppedData/alfalfa_ntiles_04032024.rds")

alfalfa_long <- alfalfa %>% 
  dplyr::select(CNTY, YEAR, GEOID, SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_ALL:D_RC_AG) %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  mutate(metric_type = str_extract(metric, ".*(?=_CDL|_RC)"),
         metric_variety = str_extract(metric, "(CDL|RC).*$")) 

alfalfa_lm_95th_sf <- alfalfa_long %>%
  filter(metric_type != "PRD") %>%
  group_by(metric_type, metric_variety) %>%
  filter(values >= quantile(values, probs = 0.95, na.rm = TRUE)) %>%
  mutate(Prob = 0.95)

alfalfa_lm_5th_sf <- alfalfa_long %>%
  filter(metric_type != "PRD") %>%
  group_by(metric_type, metric_variety) %>%
  filter(values <= quantile(values, probs = 0.05, na.rm = TRUE)) %>%
  mutate(Prob = 0.05)

alfalfa_tails_sf <- alfalfa_lm_95th_sf %>%
  bind_rows(alfalfa_lm_5th_sf) %>%
  mutate(metric_type = factor(metric_type, levels = c("SDI", "RICH", "D", "SIDI", "RPR")),
         Prob = factor(Prob))

alfalfa_tails_summ <- alfalfa_tails_sf %>%
  st_set_geometry(NULL) %>%
  filter(YEAR == 2018) %>%
  distinct(GEOID, metric, Prob, .keep_all = TRUE) %>% # some counties exist as multipolygons and therefore join multiple times...
  group_by(GEOID, Prob) %>%
  mutate(count = n(),
         freq = round(count/40*100)) %>% 
  distinct(GEOID, Prob, .keep_all = TRUE) %>%
  ungroup() 

alfalfa_tails_summ_sf <- left_join(counties, alfalfa_tails_summ, by = c("GEOID" = "GEOID")) %>%
  mutate(freq = if_else(is.na(freq) == T, 0, freq))


ma <- ggplot() +
  geom_sf(data = alfalfa_tails_summ_sf %>% filter(YEAR == 2008), 
          aes(fill = freq), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  ggtitle("Frequency in 95th and 5th Percentile for Alfalfa Counties") +
  scale_fill_viridis_c() +
  facet_wrap(~Prob, nrow = 1) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

# ggsave("ALLMETRICS_2008_tails_maps_alfalfa.jpeg", height = 8, width = 6, units = "in")

### Figure S2 - Multi-panel map ----

multimap <- ggarrange(mc + labs(subtitle = "Corn") + 
                        theme(plot.title = element_blank(), plot.subtitle = element_text( hjust = .05)), 
                      mw + labs(subtitle = "Wheat") + 
                        theme(plot.title = element_blank(), plot.subtitle = element_text( hjust = .05)), 
                      mh + labs(subtitle = "Hay") + 
                        theme(plot.title = element_blank(), plot.subtitle = element_text( hjust = .05)), 
                      ma + labs(subtitle = "Alfalfa") + 
                        theme(plot.title = element_blank(), plot.subtitle = element_text( hjust = .05)), 
                      ms + labs(subtitle = "Soybean") + 
                        theme(plot.title = element_blank(), plot.subtitle = element_text( hjust = .05)), 
                      common.legend = TRUE, legend = "bottom", ncol = 2, nrow =3)

                      
ggsave("FigureS1_Croptails_maps.jpg", width = 6, height = 6, units = "in")

### Fig 1 - Across all crops master tails maps ----

  ## Figure S4 uses same script for different year

master_crops_tails_summ <- corn_tails_summ %>%
  bind_rows(soy_tails_summ) %>%
  bind_rows(wwheat_tails_summ) %>%
  bind_rows(hay_tails_summ) %>%
  bind_rows(alfalfa_tails_summ) %>%
  group_by(GEOID) %>%
  mutate(tot_count = n(),
         tot_freq = round(tot_count/200*100)) %>%
  distinct(GEOID, Prob, .keep_all = TRUE) %>%
  ungroup()


master_crops_tails_summ_sf <- left_join(counties, master_crops_tails_summ, by = c("GEOID" = "GEOID")) %>%
  mutate(tot_freq = if_else(is.na(tot_freq) == T, 0, freq))

ggplot() +
  geom_sf(data = master_crops_tails_summ_sf %>% filter(YEAR == 2018), 
          aes(fill = tot_freq), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  scale_fill_viridis_c() +
  facet_wrap(~Prob, nrow = 1) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

# ggsave("R1_Fig1_ALLMETRICS_ALLCROPS_2018_tails_maps.jpeg", height = 3, width = 6, units = "in")

p1 <- ggplot() +
  geom_sf(data = master_crops_tails_summ_sf %>% filter(YEAR == 2018 & Prob == 0.05), 
          aes(fill = tot_freq), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  scale_fill_viridis_c() +
  ggtitle("5th Percentile") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0, size = 8))+
  guides(fill = guide_legend(title.position = "bottom"))+
  labs(fill = "Percent of Landscape Diversity Metrics")

p2 <- ggplot() +
  geom_sf(data = master_crops_tails_summ_sf %>% filter(YEAR == 2018 & Prob == 0.95), 
          aes(fill = tot_freq), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  scale_fill_viridis_c() +
  ggtitle("95th Percentile") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        plot.caption = element_text(hjust = 0, size = 8))+
  guides(fill = guide_legend(title.position = "bottom"))+
  labs(fill = "Percent of Landscape Diversity Metrics")

ggarrange(p1 + 
            theme(plot.caption = element_blank()),
          p2 + 
            theme(plot.caption = element_blank()),
          labels = c("(a)", "(b)"), 
          common.legend = TRUE, 
          legend = "bottom")

ggsave(paste0("Images/","Figure1_freq_map_final", Sys.Date(), ".jpg") , width = 6, height = 4, dpi = 300)


## Crop growth spatial coverage maps ----


### Corn ----

corn <- readRDS("PreppedData/corn_ntiles_04032024.rds")

corn_long <- corn %>% 
  dplyr::select(CNTY, YEAR, GEOID, YIELD, SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_ALL:D_RC_AG) %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  mutate(metric_type = str_extract(metric, ".*(?=_CDL|_RC)"),
         metric_variety = str_extract(metric, "(CDL|RC).*$")) 

corn_locs_sf <- corn_long %>%
  filter(metric_type != "PRD") %>%
  filter(!is.na(YIELD)) %>%
  group_by(GEOID) %>%
  summarize(Grown = "TRUE", YearsGrown = n_distinct(YEAR)) 



mc2 <- ggplot() +
  geom_sf(data = corn_locs_sf, 
          aes(fill = YearsGrown), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  ggtitle("Number of Years Corn Grown by County") +
  scale_fill_distiller(palette = "YlGnBu") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

# ggsave("corn_locations_map.jpeg", height = 6, width = 6, units = "in")

### Soy ----
soy <- readRDS("PreppedData/soy_ntiles_04032024.rds")

soy_long <- soy %>% 
  dplyr::select(CNTY, YEAR, GEOID, YIELD, SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_ALL:D_RC_AG) %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  mutate(metric_type = str_extract(metric, ".*(?=_CDL|_RC)"),
         metric_variety = str_extract(metric, "(CDL|RC).*$")) 

soy_locs_sf <- soy_long %>%
  filter(metric_type != "PRD") %>%
  filter(!is.na(YIELD)) %>%
  group_by(GEOID) %>%
  summarize(Grown = "TRUE", YearsGrown = n_distinct(YEAR)) 



ms2 <- ggplot() +
  geom_sf(data = soy_locs_sf, 
          aes(fill = YearsGrown), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  ggtitle("Number of Years Soy Grown by County") +
  scale_fill_distiller(palette = "YlGnBu") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

# ggsave("soy_locations_map.jpeg", height = 6, width = 6, units = "in")

### Wheat ----
wwheat <- readRDS("PreppedData/wwheat_ntiles_04032024.rds")

wwheat_long <- wwheat %>% 
  dplyr::select(CNTY, YEAR, GEOID, YIELD, SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_ALL:D_RC_AG) %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  mutate(metric_type = str_extract(metric, ".*(?=_CDL|_RC)"),
         metric_variety = str_extract(metric, "(CDL|RC).*$")) 

wwheat_locs_sf <- wwheat_long %>%
  filter(metric_type != "PRD") %>%
  filter(!is.na(YIELD)) %>%
  group_by(GEOID) %>%
  summarize(Grown = "TRUE", YearsGrown = n_distinct(YEAR)) 



mw2 <- ggplot() +
  geom_sf(data = wwheat_locs_sf, 
          aes(fill = YearsGrown), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  ggtitle("Number of Years Wheat Grown by County") +
  scale_fill_distiller(palette = "YlGnBu") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

# ggsave("wwheat_locations_map.jpeg", height = 6, width = 6, units = "in")

### Hay ----
hay <- readRDS("PreppedData/hay_ntiles_04032024.rds")

hay_long <- hay %>% 
  dplyr::select(CNTY, YEAR, GEOID, YIELD, SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_ALL:D_RC_AG) %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  mutate(metric_type = str_extract(metric, ".*(?=_CDL|_RC)"),
         metric_variety = str_extract(metric, "(CDL|RC).*$")) 

hay_locs_sf <- hay_long %>%
  filter(metric_type != "PRD") %>%
  filter(!is.na(YIELD)) %>%
  group_by(GEOID) %>%
  summarize(Grown = "TRUE", YearsGrown = n_distinct(YEAR)) 



mh2 <- ggplot() +
  geom_sf(data = hay_locs_sf, 
          aes(fill = YearsGrown), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  ggtitle("Number of Years Hay Grown by County") +
  scale_fill_distiller(palette = "YlGnBu") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

# ggsave("hay_locations_map.jpeg", height = 6, width = 6, units = "in")

### Alfalfa ----
alfalfa <- readRDS("PreppedData/alfalfa_ntiles_04032024.rds")

alfalfa_long <- alfalfa %>% 
  dplyr::select(CNTY, YEAR, GEOID, YIELD, SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_AG:RPR_CDL_AG, 
                SDI_CDL_BBOX_AG:RPR_CDL_BBOX_AG,
                SDI_CDL_ALL:RPR_CDL_ALL,
                SDI_CDL_BBOX_ALL:RPR_CDL_BBOX_ALL,
                SDI_RC_AG:RPR_RC_AG,
                SDI_RC_BBOX_AG:RPR_RC_BBOX_AG,
                SDI_RC_ALL:RPR_RC_ALL,
                SDI_RC_BBOX_ALL:RPR_RC_BBOX_ALL,
                D_CDL_BBOX_ALL:D_RC_AG) %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  mutate(metric_type = str_extract(metric, ".*(?=_CDL|_RC)"),
         metric_variety = str_extract(metric, "(CDL|RC).*$")) 

alfalfa_locs_sf <- alfalfa_long %>%
  filter(metric_type != "PRD") %>%
  filter(!is.na(YIELD)) %>%
  group_by(GEOID) %>%
  summarize(Grown = "TRUE", YearsGrown = n_distinct(YEAR)) 



ma2 <- ggplot() +
  geom_sf(data = alfalfa_locs_sf, 
          aes(fill = YearsGrown), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  ggtitle("Number of Years Alfalfa Grown by County") +
  scale_fill_distiller(palette = "YlGnBu") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

# ggsave("alfalfa_locations_map.jpeg", height = 6, width = 6, units = "in")


### Figure S3 - Multi-panel map ----

multimap2 <- ggarrange(mc2 + labs(subtitle = "Corn") + 
                        theme(plot.title = element_blank(), plot.subtitle = element_text( hjust = .05)), 
                      mw2 + labs(subtitle = "Wheat") + 
                        theme(plot.title = element_blank(), plot.subtitle = element_text( hjust = .05)), 
                      mh2 + labs(subtitle = "Hay") + 
                        theme(plot.title = element_blank(), plot.subtitle = element_text( hjust = .05)), 
                      ma2 + labs(subtitle = "Alfalfa") + 
                        theme(plot.title = element_blank(), plot.subtitle = element_text( hjust = .05)), 
                      ms2 + labs(subtitle = "Soybean") + 
                        theme(plot.title = element_blank(), plot.subtitle = element_text( hjust = .05)), 
                      common.legend = TRUE, legend = "bottom", ncol = 2, nrow =3)


ggsave("FigureSX_CropLocs_maps.jpg", width = 6, height = 6, units = "in")


# Figure S5 - Temporal Summary ----

#count of counties that exceed the 95th and 5th percentile across all metrics by year
master_lm_perc_summ <- master_lm_perc_sf %>%
  st_set_geometry(NULL) %>%
  filter(metric_type != "PRD") %>%
  distinct(GEOID, metric, YEAR, .keep_all = TRUE) %>%
  group_by(YEAR, metric) %>%
  filter(values >= quantile(values, probs = 0.95, na.rm = TRUE) | 
           values <= quantile(values, probs = 0.05, na.rm = TRUE)) %>%
  mutate(Prob = if_else(values >= quantile(values, probs = 0.95, na.rm = TRUE), 
                        "95th", 
                        if_else(values <= quantile(values, probs = 0.05, na.rm = TRUE), 
                                "5th", NA))) %>%
  ungroup() %>%
  group_by(YEAR, Prob) %>%
  summarise(county_count = n_distinct(GEOID)) %>%
  ungroup() %>%
  filter(YEAR <= 2018)

#plot of the number of counties that exceed the 95th and 5th percentile for each metric type by year
ggplot(master_lm_perc_summ %>% filter(Prob != "Other"), aes(x = YEAR, y = county_count, color = Prob)) +
  geom_point(aes(group = Prob)) +
  geom_smooth(aes(group = Prob, fill = Prob), alpha = 0.3) +
    labs(title = "Count of Counties Exceeding 95th and 5th Percentile by Year",
       x = "Year", y = "Count of Counties") +
  theme_minimal() +
  scale_fill_npg() +
  scale_color_npg()

ggsave("ALLMETRICS_tails_temporal_Trend.jpeg", height = 4, width = 6, units = "in")


#map of counties where the percentile in 2020 is more than 30% higher than in 2008

temp_map_sf <- cnty_lm_long_sf %>%
  st_set_geometry(NULL) %>%
  filter(metric_type != "PRD") %>%
  distinct(GEOID, metric, YEAR, .keep_all = TRUE) %>%
  group_by(metric) %>%
  mutate(quants = ntile(values, 10),
         Prob = quants*10) %>%
  ungroup() %>%
  filter(YEAR == 2018 | YEAR == 2008) %>%
  group_by(GEOID, metric) %>%
  arrange(YEAR) %>%
  mutate(perc_change = Prob - lag(Prob)) %>%
  ungroup() %>%
  filter(!is.na(perc_change)) %>%
  left_join(counties, ., by = c("GEOID" = "GEOID"))



hist(temp_map_sf$perc_change)

### Fig S6 ----
# get counties where percent change of any metric is greater than 30% between 2008 and 2018

temp_map_summ_sf <- temp_map_sf %>%
  filter(abs(perc_change) >= 20) %>%
  group_by(GEOID) %>%
  summarise(perc_change = mean(perc_change, na.rm = TRUE)) %>%
  mutate(per_change = if_else(perc_change > 0, "Increase", "Decrease")) %>%
  mutate(per_change = factor(per_change, levels = c("Increase", "Decrease")))


ggplot() +
  geom_sf(data = temp_map_summ_sf, 
          aes(fill = per_change), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  # ggtitle("Any metric changes more than 20% between 2008 and 2018") +
  scale_fill_npg() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

ggsave("FigS5_Temp_change_maps_071725_take2.jpeg", height = 4, width = 6, units = "in")

# How many counties are increasing vs decreasing?

temp_map_summ_sf %>%
   count(nrow(.))

temp_map_summ_sf %>%
  filter(per_change == "Increase") %>%
  count(nrow(.))

temp_map_summ_sf %>%
  filter(per_change == "Decrease") %>%
  count(nrow(.))

###Fig S7 ----
# Get places where average change over time is relatively high --> consistently high change

temp_map_summ_sf <- temp_map_sf %>%
  group_by(GEOID) %>%
  summarise(perc_change = mean(perc_change, na.rm = TRUE)) %>%
  filter(abs(perc_change) >= 20) %>%
  mutate(per_change = if_else(perc_change > 0, "Increase", "Decrease")) %>%
  mutate(per_change = factor(per_change, levels = c("Increase", "Decrease")))


ggplot() +
  geom_sf(data = temp_map_summ_sf, 
          aes(fill = per_change), color = NA) +
  geom_sf(data = state_bndry, fill = NA) +
  # ggtitle("Counties where average of metric changes is more than 20% between 2008 and 2018") +
  scale_fill_npg() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")+
  labs(fill = "")

ggsave("FigS6_Average_temp_change_maps_071725.jpeg", height = 4, width = 6, units = "in")


# Subset Maps!!! ----


counties <- tigris::counties() %>%
  filter(STATEFP < 60) %>%
  filter(STATEFP != 02 | STATEFP != 15)


cnty_lm <- left_join(counties, master_lm, by = c("GEOID" = "CNTY"))

cnty_lm_long <- master_lm %>% 
  dplyr::select(CNTY, YEAR, SDI_CDL_AG:PRD_CDL_AG, 
                SDI_CDL_BBOX_AG:PRD_CDL_BBOX_AG,
                SDI_CDL_ALL:PRD_CDL_ALL,
                SDI_CDL_BBOX_ALL:PRD_CDL_BBOX_ALL,
                SDI_RC_AG:PRD_RC_AG,
                SDI_RC_BBOX_AG:PRD_RC_BBOX_AG,
                SDI_RC_ALL:PRD_RC_ALL,
                SDI_RC_BBOX_ALL:D_RC_AG) %>%
  pivot_longer(cols = SDI_CDL_AG:D_RC_AG, names_to = "metric", values_to = "values") %>%
  mutate(metric_type = str_extract(metric, ".*(?=_CDL|_RC)"),
         metric_variety = str_extract(metric, "(CDL|RC).*$")) 

cnty_lm_long <- left_join(counties, cnty_lm_long, by = c("GEOID" = "CNTY"))

common_cnty <- readRDS(paste0("PreppedData/corn_commoncnty_ntiles_04032024.rds"))

commoncnty_lm_long <- cnty_lm_long %>%
  filter(GEOID %in% common_cnty$GEOID)

states <- tigris::states() %>% 
  filter(STATEFP <= 60 & STATEFP != "02" & STATEFP != "15")

ggplot() +
  geom_sf(data = commoncnty_lm_long %>% filter(YEAR == 2008 & metric_type == "SDI"), 
          aes(fill = values), color = NA) +
  geom_sf(data = states, fill = NA) +
  facet_wrap(~metric_variety, nrow = 4) +
  scale_fill_viridis_c() +
  ggtitle("SDI") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text = element_blank(),
        legend.position = "bottom")

ggsave("SDI_2008_commoncnty_maps.jpeg", height = 8, width = 6, units = "in")



