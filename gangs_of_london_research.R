install.packages('ggplot2')
install.packages('readr')
install.packages('tidyverse')

library(ggplot2)
library(readr)
library(tidyverse)

gol_research <- read.csv('gangs_of_london_phrases_n_idioms.csv')




# Статистика по типам ошибок

error_stats <- gol_research|> 
  group_by(Amedia_error_type) |> 
  summarise(
    count = n(),
    percentage = round(n() / nrow(gol_research) * 100, 1)
  ) |> 
  arrange(desc(count))

# NA означает, что ошибки не обнаружено


# Визуализация распределения ошибок

error_stats |> 
  filter(!is.na(Amedia_error_type)) |> 
  ggplot(aes(x = reorder(Amedia_error_type, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(count, " (", percentage, "%)")), 
            vjust = -0.5) +
  labs(title = "Распределение типов ошибок перевода",
       x = "Тип ошибки",
       y = "Количество случаев") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Анализ по типам фразеологизмов (orig_type)

type_stats <- gol_research |> 
  group_by(orig_type) |> 
  summarise(
    total = n(),
    with_errors = sum(!is.na(Amedia_error_type)),
    error_rate = round(with_errors / total * 100, 1)
  )

# Визуализация

type_stats |> 
  ggplot(aes(x = orig_type, y = error_rate)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_text(aes(label = paste0(error_rate, "%")), vjust = -0.5) +
  labs(title = "Процент ошибок по типам фразеологизмов",
       x = "Тип ФЕ (по Виноградову)",
       y = "Процент ошибок") +
  theme_minimal()




