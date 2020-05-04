install.packages("kokudosuuchi")
library(pacman)
p_load(kokudosuuchi,rvest,tidyverse)

ja_trains <- getKSJURL("N02", fiscalyear=2018) %>%
  select(zipFileUrl) %>%
  as.character() %>%
  getKSJData()
ja_lines <- ja_trains$`N02-18_RailroadSection`
ja_stations <- ja_trains$`N02-18_Station`
rm(ja_trains)
colnames(ja_lines) <- c("railroad_type","operator_type","line","operator","geometry")
colnames(ja_stations) <- c("railroad_type","operator_type","line","operator","station","geometry")

ja_lines %>%
  filter(operator=="福岡市") %>%
  mutate(number=row_number()) %>%
  ggplot(aes(colour=number)) +
  geom_sf() +
  scale_color_distiller(palette="Dark2")
