library(tidyverse)
library(ggmap)
library(dplyr)
library(purrr)
library(readxl)

counties <- map_data("county")

cancers <- c(
  "bladder",
  "brain & ONS",
  "breast (female)",
  "breast (female in situ)",
  "cervix",
  "colon & rectum",
  "esophagus",
  "kidney & renal pelvis",
  "leukemia",
  "liver & bile duct",
  "lung & bronchus",
  "non-hodgkin lymphoma",
  "melanoma of the skin",
  "oral cavity & pharynx",
  "ovary",
  "pancreas",
  "prostate",
  "stomach",
  "thyroid",
  "uterus"
)

dat <- purrr::map(cancers, function(x) cancer_statistics(statistic = "incidence", cancer = x) %>% mutate(cancer = x, fips = as.numeric(fips), quantile = ntile(incidence_rate, 5)))

dat <- bind_rows(dat)

test <- counties %>%
  mutate(polyname = paste(region, subregion, sep=",")) %>%
  inner_join(county.fips, by = "polyname") %>%
  inner_join(dat, by="fips")

test$cancer <- factor(test$cancer, levels = c(cancers, "superfund sites"))

npl <- read_excel("C:/Users/susmann/Downloads/epa-national-priorities-list-ciesin-mod-v2-2014.xls", sheet=6)
npl <- npl %>% filter(STATE %in% state.abb, STATE != "HI", STATE != "AK")
npl$cancer <- "superfund sites"

npl$cancer <- factor(npl$cancer, levels=c(cancers, "superfund sites"))

usa <- map_data("usa")

g1 <- ggplot(test) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = as.factor(quantile))) +
  scale_fill_manual(values = c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C")) +
  coord_map(projection = "albers", lat0 = 29.5, lat1 = 45.5) +
  geom_point(data = npl, aes(x = LONGITUDE, y = LATITUDE), size = 0.1, color = "brown", alpha = 0.2) +
  facet_wrap(~cancer, ncol = 5) +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = '#cccccc') +
  theme(legend.position = "none",
        line = element_blank(),
        rect = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 16))

 g1


g2 <- ggplot(npl) +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = NA, color = '#cccccc') +
  coord_map(projection = "albers", lat0 = 29.5, lat1 = 45.5) +
  ggtitle("national priority list sites") +
  theme(legend.position = "none",
        line = element_blank(),
        rect = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 16))

g2

library(gridExtra)
grid.arrange(g1, g2, ncol = 1, nrow = 2)



mortality <- cancer_statistics(statistic = "mortality") %>%
  select(county, fips, mortality_rate) %>%
  gather(mortality_rate, key = key, value = value) %>%
  mutate(quantile = ntile(value, 5), key = "mortality")

incidence <- cancer_statistics(statistic = "incidence") %>%
  select(county, fips, incidence_rate) %>%
  gather(incidence_rate, key = key, value = value) %>%
  mutate(quantile = ntile(value, 5), key = "incidence")

combined <- bind_rows(mortality, incidence) %>%
  mutate(fips = as.numeric(fips))

combined_with_county <- counties %>%
  mutate(polyname = paste(region, subregion, sep=",")) %>%
  inner_join(county.fips, by = "polyname") %>%
  inner_join(combined, by="fips")

ggplot(combined_with_county, aes(x = long, y = lat, group = group, fill = as.factor(quantile))) +
  geom_polygon() +
  facet_wrap(~key) +
  coord_map(projection = "albers", lat0 = 29.5, lat1 = 45.5) +
  scale_fill_manual(values = c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C")) +
  theme(legend.position = "none",
        line = element_blank(),
        rect = element_blank(),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 16))
