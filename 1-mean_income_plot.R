library(dplyr)

load("./data/processed/russia_adm_map.RData")
load("./data/processed/region_mean_income2015.rda")

# Data prep
names(income)[1] <- "NAME"

x <- levels(factor(russia_adm_map$NAME))
y <- as.character(income$NAME)
x[!(x %in% y)]

income$NAME <- plyr::revalue(income$NAME, c("Республика Адыгея" = "Адыгея",
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

income_map_data <- subset(russia_adm_map, NAME %in% income$NAME)
income_map_data <- income_map_data %>%
  select(long, lat, group, NAME) %>%
  left_join(income, by="NAME")

# Plot
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22)))

income_map <- ggplot(russia_adm_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(color="grey", opacity=0.5, fill="white") +
  coord_equal() + theme_opts +
  geom_polygon(data=income_map_data, aes(x = long, y = lat, group = group, 
                                         fill=as.numeric(mean_income))) +
  scale_fill_continuous(low= "#e5f5f9", high="#2ca25f",
                        guide = guide_legend(title = "Средняя з/п"))

ggsave(income_map, file="mean_income.png")

