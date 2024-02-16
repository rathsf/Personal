library(readxl)
source('./auxFns/setup.R')


df <- read_excel('./Elantra_Market/Elantra.xlsx', sheet = 'Autolist')

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
    filter(column %in% c('title', 'miles', 'Location', 'days_listed', 'price')) %>% 
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


df1 <- df %>% select(-trim, -days_listed) %>% mutate(source = 'Autolist')




df <- read_excel('./Elantra_Market/Elantra.xlsx', sheet = 'OfferUp')

df <- df %>%
    filter(!is.na(Hyundai)) %>% 
    mutate(
        column = ifelse(str_detect(lead(Hyundai), "miles"), "price", 'dummy'),
        column = ifelse(str_detect(lead(Hyundai, 2), "miles"), "title", column),
        column = ifelse(str_detect(Hyundai, "miles"), "miles", column),
        column = ifelse(str_detect(Hyundai, ", "), "Location", column)
        ) %>%
        filter(column %in% c('title', 'miles', 'Location', 'price')) %>% 
    pivot_wider(names_from = column, values_from = Hyundai)

df <- df %>% unnest(colnames(df))

df <- df %>% mutate(
        price = as.numeric(price),
        miles = as.numeric(str_remove(miles, "k miles")) * 1000,
    ) %>%
    separate(title, into = c('year', 'make', 'model'), extra = 'merge') %>%
    unite(model, make, model, sep = ' ')

df2 <- df %>% distinct() %>% mutate(source = 'OfferUp')

df <- bind_rows(df1, df2)

df <- df %>% mutate(
    frac_price = (price / max(price) * 100),
    frac_miles = (min(miles) / miles * 100),
    # time_driven = 2024 - as.numeric(year),
    # miles_per_year = miles / time_driven,
    price_mile_ratio = frac_price / frac_miles
) %>% arrange(price_mile_ratio)








df %>% ggplot(aes(x = price_mile_ratio)) + geom_histogram(binwidth = 0.2)


df %>% ggplot(aes(x = price, y = price_mile_ratio, size = miles, color = miles)) +
    geom_vline(xintercept = c(10000, 16000), color = 'grey95', size = 2) +
    geom_smooth(method = "gam", se = FALSE, size = 1, color = "darkred") +
    geom_point(data = filter(df, source == "OfferUp"), shape = 1, size = 10, color = "darkorange") +
    geom_point() +
    scale_color_viridis() +
    style
