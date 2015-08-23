library(dplyr)

load("./data/processed/russia_adm_map.RData")
load("./data/processed/data_medinc.rda")

# Data prep
under20 <- data_medinc %>%
  filter(income == "less_20000") %>%
  mutate(level = (freq / total_n)*100)

names(under20)[1] <- "NAME"

under20$NAME <- plyr::revalue(under20$NAME, c("Республика Адыгея" = "Адыгея",
                                              "Республика Алтай" = "Алтай",
                                              "Республика Башкортостан" = "Башкортостан",
                                              "Белгородская область " = "Белгородская область",
                                              "Республика Бурятия" = "Бурятия",
                                              "Республика Дагестан" = "Дагестан",
                                              "Республика Ингушетия" = "Ингушетия",
                                              "Кабардино-Балкарская Республика" = "Кабардино-Балкарская республика",
                                              "Карачаево-Черкесская Республика" = "Карачаево-Черкесская республика",
                                              "Республика Марий Эл" = "Марий Эл",
                                              "г.Москва" = "Москва",
                                              "Ненецкий авт. округ" = "Ненецкий автономный округ",
                                              "г.Санкт-Петербург " = "Санкт-Петербург",
                                              "Республика Северная Осетия - Алания" = "Северная Осетия - Алания",
                                              "Республика Татарстан (Татарстан)" = "Татарстан",
                                              "Республика Тыва" = "Тыва",
                                              "Удмуртская Республика" = "Удмуртская республика",
                                              "Ханты-Мансийский авт. округ-Югра" = "Ханты-Мансийский автономный округ - Югра",
                                              "Чеченская Республика" = "Чеченская республика",
                                              "Чувашская Республика - Чувашия" = "Чувашия",
                                              "Ямало-Ненецкий авт. округ" = "Ямало-Ненецкий автономный округ"))

med_map <- subset(russia_adm_map, NAME %in% under20$NAME)
med_map <- med_map %>%
  select(long, lat, group, NAME) %>%
  left_join(under20, by="NAME")

# Put points in region centers
cnames <-aggregate(cbind(long, lat) ~ NAME, data = med_map, FUN = function(x) mean(range(x)))
cnames$angle <-0

med_map <- med_map %>%
  select(NAME, group, level, freq)

cnames <- left_join(cnames, med_map, by="NAME")

# Plot
income_map <- ggplot(russia_adm_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(color="grey", opacity=0.5, fill="white") +
  coord_equal() + theme_opts +
  geom_polygon(data=income_map_data, aes(x = long, y = lat, group = group, 
                                         fill=as.numeric(mean_income))) +
  scale_fill_continuous(low= "#e5f5f9", high="#2ca25f",
                        guide = guide_legend(title = "Средняя з/п региона"))

med_income_map <- income_map + 
  geom_point(data=cnames, aes(x = long, y = lat, group=group, color=level)) +
  geom_path(color="grey", opacity=0.02, size=0.02) +
  scale_color_gradient(low="#efedf5", high="#756bb1", 
                       guide = guide_legend(title = "Доля с з/п <20k")) +
  theme(legend.position = "bottom") +
  ggtitle("Доля медработников с з/п <20 000 руб.")

ggsave(med_income_map, file="med_income_map.png")
