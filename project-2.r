library(tidyverse) 
library(janitor)

library(showtext)
showtext_auto()

path <- 'D:/Master/kekonom/project/'


rus_total <- read_table(paste0(path, "Deaths_1x1.txt"), na = ".")
rus_total$Age <- as.integer(recode(rus_total$Age, "110+" = "110"))

p_total <- ggplot(rus_total, aes(x = Year, y = Age, fill = ntile(Total, 100)))   #Пример деления на страты
p_total_out <- p_total + geom_raster() +
  scale_fill_viridis_c(option = "A") +
  guides(fill = guide_legend(label.position = "bottom", title.position = "top")) +
  labs(x = "Год", y = "Возраст", fill = "Общая смертность (нормированная к 100)",
       title = "Общее количество смертей в России, 1959-2014",
       caption = "База данных: Human Mortality Database.") +
  theme(legend.position = "top",
        legend.title = element_text(size = 20))

p_total_out

ggsave("D:/Master/kekonom/project/rus_total.png", p_total_out, height = 4, width = 6)



p_rel <- ggplot(rus_total, aes(x = Year, y = Age, fill = ntile(Male / Female, 100)))
p_rel_out <- p_rel + geom_raster() +
  scale_fill_viridis_c(option = "A") +
  guides(fill = guide_legend(label.position = "bottom", title.position = "top")) +
  labs(x = "Год", y = "Возраст", fill = "Отношение мужской смеертности к женской (нормир. к 100)",
       title = "Отношение мужской смеертности к женской в России, 1959-2014") +
  theme(legend.position = "top",
        legend.title = element_text(size = 20))

p_rel_out

ggsave("D:/Master/kekonom/project/rus_rel.png", p_rel_out, height = 4, width = 6)


rus_risk <- read_table(paste0(path, "Exposures_1x1.txt"), na = ".")  
rus_risk$Age <- as.integer(recode(rus_risk$Age, "110+" = "110"))

p_risk <- ggplot(subset(rus_risk, Age < 101), aes(x = Year, y = Age, fill = ntile(Total, 100)))
p_risk_out <- p_risk + geom_raster() +
  scale_fill_viridis_c(option = "A", direction = -1) +
  scale_x_continuous(breaks = seq(1959, 2014, by = 5)) +
  guides(fill = guide_legend(nrow = 1, title.position = "top", label.position = "bottom")) +
  ylim(c(0, 100)) +
  labs(x = "Год", y = "Возраст", fill = "Количество людей с повышенным риском смерти (нормир. к 100)",
       title = "Количество людей с повышенным риском смерти в России, 1959-2014") +
  theme(legend.position = "top",
        legend.title = element_text(size = 20))

p_risk_out
ggsave("D:/Master/kekonom/project/rus_exp.png", p_risk_out, height = 4, width = 6)
