install.packages("kokudosuuchi")
library(pacman)
p_load(kokudosuuchi,rvest,tidyverse,sf)

theme_set(theme_gray(base_size=12, base_family="HiraKakuProN-W3"))

ja_trains <- getKSJURL("N02", fiscalyear=2018) %>%
  select(zipFileUrl) %>%
  as.character() %>%
  getKSJData()
ja_lines <- ja_trains$`N02-18_RailroadSection`
ja_stations <- ja_trains$`N02-18_Station`
rm(ja_trains)

# st_write(ja_lines, "ja_lines.shp", layer_options = "ENCODING=UTF-8")
# ja_lines <- st_read("~/Desktop/Datasets/ja_lines.shp")
# ja_stations <- st_read("~/Desktop/Datasets/ja_stations.shp")
colnames(ja_lines) <- c("railroad_type","operator_type","line","operator","geometry")
colnames(ja_stations) <- c("railroad_type","operator_type","line","operator","station","geometry")

ja_lines %>%
  filter(str_sub(operator,start=-4)=="旅客鉄道") %>%
  mutate(operator = paste0("JR",str_sub(operator,end=-5)),
         type = case_when(str_sub(line, start=-3)=="新幹線"~"新幹線", TRUE~"在来線")) %>%
  group_by(line, operator, type) %>%
  summarise(geometry = st_union(geometry)) %>%
  ggplot(aes(colour=operator,size=type,alpha=type)) +
  geom_sf() +
  scale_size_manual(values = c("新幹線"=1.5,"在来線"=0.5)) +
  scale_alpha_manual(values = c("新幹線"=.7,"在来線"=0.4)) +
  scale_color_manual(values = jr_colours)

jr_colours <- c("JR九州"="#ff0200", "JR北海道"="#2cb431",
                "JR四国"="#1ba5ca", "JR東日本"="#0b8e0f",
                "JR東海"="#f77421", "JR西日本"="#0173ca")
# brewer.pal(n = 6, name = "Dark2")
# display.brewer.pal(n = 6, name = "Dark2")

