library(tidyverse)
library(readxl)
library(viridis)

`%notin%` <- Negate(`%in%`)

options(
    scipen = 999,
    tibble.print_max = 65,
    tibble.print_min = 30,
    width = 135,
    tibble.max_extra_cols = 10,
    pillar.min_title_chars = 8
)

style <- theme(
    text = element_text(family = "Lato", size = 20),
    title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "grey65", linewidth = 2),
    strip.background = element_blank(),
    legend.key = element_blank()
)

df <- read_excel("~/Downloads/Elantra.xlsx", sheet = 2) #%>% select(-1)


df <- df %>%
    filter(!is.na(Hyundai)) %>% #print(n = 500)
    mutate(
        column = ifelse(str_detect(Hyundai, "Mile"), "miles", "dummy"),
        column = ifelse(str_detect(Hyundai, ", "), "Location", column),
        column = ifelse(lead(column) == "miles", "title", column),
        column = ifelse(str_detect(lead(Hyundai), "on market"), "days_listed", column),
        column = ifelse(str_detect(lead(Hyundai), "est. "), "price", column),
        column = ifelse(str_detect(lead(Hyundai), "similar"), "comparison", column)
    ) %>% 
    #filter(column %in% c('title', 'miles', 'price')) %>%
    filter(column != 'dummy' & column != 'comparison') %>%     
    pivot_wider(names_from = column, values_from = Hyundai)
    
    
df <- df %>% unnest(colnames(df))
    
df <- df %>% mutate(
        miles = ifelse(miles == "0 Mile", "43194 Miles", miles),
        price = as.numeric(price),
        miles = str_remove(miles, " Miles"),
        miles = as.numeric(str_remove(miles, ","))
    ) %>%
    separate(title, into = c('year', 'make', 'model', 'model2', 'trim'), extra = 'merge') %>%
    unite(model, make, model, model2, sep = ' ')


df <- df %>% mutate(
    frac_price = (price / max(price) * 100),
    frac_miles = (min(miles) / miles * 100),
    #time_driven = 2024 - as.numeric(year),
    #miles_per_year = miles / time_driven,

    price_mile_ratio = frac_price / frac_miles
) %>% arrange(price_mile_ratio)


df %>% ggplot(aes(x = price_mile_ratio)) + geom_histogram(binwidth = 0.2)


df %>% ggplot(aes(x = price, y = price_mile_ratio, size = miles, color = miles)) + 
    #geom_hline(yintercept = c(10000, 15000), color = 'grey80') +
    geom_point() + 
    scale_color_viridis() +
    geom_smooth(method = 'gam', se = FALSE, size = 1) +
    style


