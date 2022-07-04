# install.packages("kokudosuuchi")
library(pacman)
p_load(kokudosuuchi,rvest,tidyverse,sf,magrittr)
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

#Nationwide map
ja_lines2 <- ja_lines %>%
  group_by(line, operator) %>%
  summarize(railroad_type = head(railroad_type,1),
            operator_type = head(operator_type,1),
            line = head(line,1),
            operator = head(operator,1),
            geometry = st_union(geometry))

ja_lines %>% group_by(line, operator) %>% summarise(line = head(line,1),
                                                    geometry = st_union(geometry))


#Roughly 23 boroughs
ja_lines %>%
  st_crop(c(xmin=139.58,xmax=139.9,ymin=35.53,ymax=35.83)) %>%
  group_by(line) %>%
  summarise(geometry = st_union(geometry)) %>%
  ggplot() +
  geom_sf(aes(color=line)) +
  theme_void() +
  theme(legend.position = "none")
ggsave("tokyo_lines.svg")
  
#JR + Shinkansen
jr_colours <- c("JR九州"="#ff0200", "JR北海道"="#2cb431",
                "JR四国"="#1ba5ca", "JR東日本"="#0b8e0f",
                "JR東海"="#f77421", "JR西日本"="#0173ca")

ja_lines %>%
  filter(str_sub(operator,start=-4)=="旅客鉄道") %>%
  mutate(operator = paste0("JR",str_sub(operator,end=-5)),
         type = case_when(str_sub(line, start=-3)=="新幹線"~"新幹線", TRUE~"在来線")) %>%
  # filter(type=="新幹線") %>%
  group_by(line, operator, type) %>%
  summarise(geometry = st_union(geometry)) %>%
  ggplot(aes(colour=operator,size=type,alpha=type)) +
  geom_sf() +
  scale_size_manual(values = c("新幹線"=1.5,"在来線"=0.5)) +
  scale_alpha_manual(values = c("新幹線"=.7,"在来線"=0.4)) +
  scale_color_manual(values = jr_colours) +
  theme_void()

#only "backbone" lines
ja_lines2 %>%
  filter(line%in%c("函館線","道南いさりび鉄道線","北海道新幹線","津軽線","東北線","いわて銀河鉄道線","青い森鉄道線","東北新幹線",
                   "東海道線","東海道新幹線","山陽線","山陽新幹線","鹿児島線","肥薩おれんじ鉄道線","九州新幹線")) %>%
  mutate(operator = paste0("JR",str_sub(operator,end=-5)),
         type = case_when(str_sub(line, start=-3)=="新幹線"~"新幹線", TRUE~"在来線")) %>%
  # filter(type=="新幹線") %>%
  group_by(line, operator, type) %>%
  summarise(geometry = st_union(geometry)) %>%
  ggplot(aes(colour=operator,size=type)) +
  geom_sf() +
  scale_size_manual(values = c("新幹線"=0.9,"在来線"=0.5)) +
  # scale_alpha_manual(values = c("新幹線"=0.7,"在来線"=0.4)) +
  scale_color_manual(values = jr_colours) +
  theme_void()
ggsave("~/Desktop/ja_lines.svg")

# brewer.pal(n = 6, name = "Dark2")
# display.brewer.pal(n = 6, name = "Dark2")

#Clarify - what are the line and operator types?
#railroad_type: 10s are "railways" operated by the Railway Ministry; 
#20s are "street transit" operated by the Infra Ministry; now little difference
#11 JR / 12+21 Normal / 13 Cableway / 14+22 Suspended monorail / 15+23 Riding monorail
#16+24 AGT / 17 Grade-separated car / 25 Linimo

#operator_type: 1 Shinkansen / 2 JR / 3 Public / 4 Private / 5 3rd-Sector

theme_set(theme_classic(base_size = 9, base_family = "Hiragino Sans"))

ggplot(ja_lines2, aes(color=operator_type), size=0.01, alpha=0.5) + 
  geom_sf() +
  # geom_sf_text(aes(label=line)) +
  theme_void()

# JR Shinkansen 1
# JR Zairai 2
# Major Private 4
# Minor Private 4
# 3rd Sector 5
# Rapid Transit 3/4
# Rapid Transit (new) 3/4
