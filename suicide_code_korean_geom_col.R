library(tidyverse)
df <- read_csv("/Users/kazumichiyamamoto/Desktop/who_suicide_statistics.csv")

# rate of suicide
df <- df %>%
        mutate(percentage = suicides_no/population)

# reorder the level
df$age <- factor(df$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"))

# filter and group 4 countries
df_filtered <- df %>%
        filter(country %in% c("Japan", "Republic of Korea","United States of America", "France")) %>%
        group_by(country, year) %>%
        summarize(total_no = sum(suicides_no),
                  total_pop = sum(population),
                  no_per_100000 = total_no/total_pop*100000)

# yearly change among countries
ggplot(df_filtered, aes(x=year, y=no_per_100000, color = country))+
        geom_point()+
        geom_line()

# filter korea
df_korea <- df %>% 
        filter(country == "Republic of Korea")

# group by year and age
df_korea_grouped <- df_korea %>%
        group_by(year, age) %>%
        summarize(total_no = sum(suicides_no),
                  total_pop = sum(population),
        )
#graph geom_col
ggplot(df_korea_grouped, aes(x=year, y=total_no, fill=age))+
        geom_col()