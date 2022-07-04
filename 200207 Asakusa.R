library(pacman)
p_load(rvest,tidyverse,stringr,lubridate,plotly,datetime,magrittr)
#Ginza
ginza <- jiko(277,"N",18)
#Remember: the "exc" more refers to the actual first station (in terms of directions).

#Asakusa
asakusa_new <- bind_rows(jiko(250,"N",49), jiko(251,"N",6), jiko(252,"N",4,exc=1), jiko(679,"N",8), jiko(253,"W",6),
                         jiko(222,"S",19), jiko(258,"S",5), jiko(200,"S",14),
                         jiko(682,"S",7), jiko(254,"S",42), jiko(257,"S",2), jiko(255,"S",9), jiko(256,"S",5), jiko(663,"S",1))
asakusa_etc <- asakusa_new %>% #Data cleaning
  mutate(code = str_replace_all(code,c("AF"="A")),
         station = recode(station, "羽田空港第１・第２ターミナル"="羽田第１","羽田空港第３ターミナル"="羽田第３"),
         destination = recode(destination, "羽田空港第１・第２ターミナル"="羽田第１"),
         station = case_when(station=="成田空港"&line=="京成成田スカイアクセス線" ~ "アクセス成田第２",
                             station=="成田空"&line=="京成成田スカイアクセス線" ~ "アクセス成田第１",
                             station=="成田空港"&line=="京成本線" ~ "本線成田第２",
                             station=="成田空"&line=="京成本線" ~ "本線成田第１",
                             TRUE ~ station), #1: Fix lettering
         direction = case_when(line=="京浜急行空港線" & (code %in% (filter(asakusa_new, direction=="S")%$%unique(code))) ~ "S",
                               line=="京浜急行空港線" & (code %in% (filter(asakusa_new, direction=="N")%$%unique(code))) ~ "N",
                               TRUE ~ direction)) %>% #2: FIX AIRPORT LINE
  separate(service, c("service", NA), sep= ":") #3. Service 記号の統一
asakusa_etc <- asakusa_etc %>%
  filter(!(line=="北総鉄道" & service=="アクセス特急"),
         !(line=="京成成田スカイアクセス線" & service %in% c("普通","急行","特急"))) %>% #4. Remove 北総/アクセス duplication
  mutate(line=case_when(station=="青砥"&direction=="N"&(code %in% (filter(asakusa_etc, line=="京成押上線") %$% unique(code))) ~ "京成押上線",
                        station=="京成高砂"&direction=="S"&(code %in% (filter(asakusa_etc, line=="京成押上線") %$% unique(code))) ~ "京成押上線",
                        TRUE ~ line), #5. Remove 青砥/高砂 duplication
         service=case_when(line=="都営浅草線" & service!="エアポート快特" ~ "普通",
                           TRUE ~ service)) #6. Simplify Asakusa

# asakusa_etc <- asakusa_etc %>%
#   # filter(!(line=="北総鉄道" & service=="アクセス特急"),
#   #        !(line=="京成成田スカイアクセス線" & service %in% c("普通","急行","特急"))) %>% #4. Remove 北総/アクセス duplication
#   mutate(line=case_when(station=="青砥"&direction=="N"&(code %in% (filter(asakusa_etc, line=="京成押上線",station!="青砥",station!="京成高砂") %$% unique(code))) ~ "京成本線",
#                         station=="京成高砂"&direction=="S"&(code %in% (filter(asakusa_etc, line=="京成押上線",station!="青砥",station!="京成高砂") %$% unique(code))) ~ "京成本線",
#                         TRUE ~ line))

# distinct(asakusa_etc, code, direction) %>%  ##ARE THERE ANY TRAINS WITH MULTIPLE DIRECTIONS??
#   group_by(code) %>% summarise(n=n()) %>% filter(n>1) %>% select(-n) %>%
#   left_join(distinct(asakusa_etc, code, direction)) %>% View()

# distinct(asakusa2, code, destination) %>% #ARE THERE ANY TRAINS WITH MULTIPLE DESTINATIONS??
#   group_by(code) %>% summarise(n=n()) %>% filter(n>1) %>% select(-n) %>%
#   left_join(asakusa2) %>% group_by(code) %>% View()
#   summarise(dests = toString(unique(paste(str_sub(line, -3, -2),service)))) %>% View()

# anti_join(asakusa_etc, read_csv("asakusa_etc.csv"), c("station"="station_name")) %>% distinct(station) #Prepare
asakusa_etc <- left_join(asakusa_etc, read_csv("asakusa_etc.csv"), c("station"="station_name")) #Add distances

##Sort train types
subway_filter <- filter(asakusa_etc, line=="都営浅草線") %$% unique(code)
keikyu_all_filter <- filter(asakusa_etc, str_sub(line,end=2)=="京浜") %$% unique(code)
keisei_all_filter <- filter(asakusa_etc, (str_sub(line,end=2)=="京成"|line=="芝山鉄道") & station!="青砥" & station!="京成高砂") %$% unique(code)
# rush_filter <- filter(asakusa_etc, w=="Weekday") %>%
#   filter((station=="京急蒲田"&direction=="S"&((time>34200&time<60300)|(time>70200&time<78000))) |
#            (station=="京急蒲田"&direction=="N"&((time>34200&time<62100)|(time>69300&time<80100))) |
#            (station=="品川"&direction=="S"&((time>33300&time<59400)|(time>69300&time<77100))) |
#            (station=="品川"&direction=="N"&((time>33300&time<61200)|(time>68400&time<79200))) |
#            (station=="青砥"&direction=="S"&time>39600&time<61200) |
#            (station=="青砥"&direction=="N"&time>36000&time<59400) |
#            (station=="日本橋"&direction=="S"&time>36900&time<56700) |
#            (station=="日本橋"&direction=="N"&time>36000&time<57600)) %$% unique(code)

#Add codes and timeframe (separate so that you could re-run the above filters)
asakusa2 <- mutate(asakusa_etc, code = case_when((code %in% keikyu_all_filter & !(code %in% subway_filter) & str_sub(line,end=2)=="京浜") ~ paste(code,"Keikyu"),
                                                 (code %in% keisei_all_filter & !(code %in% subway_filter) & (str_sub(line,end=2)=="京成"|line=="芝山鉄道")) ~ paste(code,"Keisei"),
                                                 TRUE ~ code),
                   code = paste(str_sub(w,1,2),code)) #Make sure codes are distinct (need expansive criteria)
                   # timeframe = case_when(code %in% rush_filter ~ "Daytime",
                   #                       TRUE ~ "Weekday rest")) #Add daytime criterion

#Add line abbreviations
# sort(unique(asakusa2$line)) #CHECK
asakusa_lines <- data.frame(line=sort(unique(asakusa2$line)),
                            abbreviation=c("千葉","千葉","北総","押上","京成","金町",
                                        "京急","大師","京急","空港","京急",
                                        "北総","京成","浅草"))

#Split days here.
asakusa_wd <- asakusa2 %>%
  filter(w=="Weekday") %>%
  left_join(asakusa_lines,"line") %>%
  mutate(lineserv = paste(abbreviation,service)) %>%
  arrange(time) %>% #2. Add origin (time order is necessary)
  group_by(code) %>%
  mutate(origin = head(station,1),
         depart = head(time,1),
         arrive0 = tail(time,1)) %>%
  ungroup() %>%
  arrange(distance) %>% #3. Each line-service to a single point
  distinct(code, abbreviation, .keep_all = TRUE) %>%
  group_by(code) %>% #4. Each line to a single point
  summarise(code_path = toString(lineserv), #THESE have to be in distance order
            dir = head(direction,1),
            south = case_when(direction=="N"~head(origin,1),TRUE~head(destination,1)),
            north = case_when(direction=="S"~head(origin,1),TRUE~head(destination,1)),
            depart = head(depart,1),
            arrive0 = tail(arrive0,1)) %>%
  filter(!str_detect(code_path,",")) %>%
  mutate(code_code = str_replace_all(code_path, c("京急 普通"="A", "京急 エアポート急行"="B", "京急 特急"="C", "京急 快特"="D", "京急 エアポート快特"="E",
                                                  "空港 普通"="F","空港 エアポート急行"="G","空港 特急"="H","空港 快特"="I","空港 エアポート快特"="J",

                                                  "京成 普通"="1","京成 通勤特急"="3","京成 特急"="4","京成 快速特急"="5","京成 快速"="2",
                                                  "北総 普通"="6","北総 急行"="7","北総 特急"="8","北総 アクセス特急"="9",
                                                  
                                                  "浅草 普通, 押上 普通"="0","浅草 普通, 押上 [快速通勤特急]+"="1","浅草 普通, 押上 アクセス特急"="1",
                                                  "浅草 エアポート快特, 押上 [快速通勤特急]+"="9","浅草 エアポート快特, 押上 アクセス特急"="9",
                                                  ", "=""))) %>%
  count(code_code) %>%
  View()
#   write.csv("~/Desktop/asakusa.csv")


keikyu_filter <- filter(asakusa2, station=="京急蒲田"|station=="品川") %$% unique(code)
keisei_filter <- filter(asakusa2, station=="京成八幡"|station=="成田湯川") %$% unique(code)
airport_filter <- filter(asakusa2, station=="羽田第３") %$% unique(code)
reverse_filter <- filter(asakusa2, station=="横浜") %$% unique(code)
asakusa2 <- mutate(asakusa2, type = case_when(((code %in% subway_filter) & !(code %in% keikyu_filter) & !(code %in% keisei_filter)) ~ "Subway only",
                                              ((code %in% subway_filter) & ((code %in% keikyu_filter)|(code %in% keisei_filter))) ~ "Subway through",
                                              ((code %in% keikyu_filter) & !(code %in% subway_filter)) ~ "Keikyu only",
                                              ((code %in% keisei_filter) & !(code %in% subway_filter)) ~ "Keisei only",
                                              ((code %in% airport_filter) & (code %in% reverse_filter)) ~ "Reverse airport",
                                              TRUE ~ "Branch")) #Classify trains (need precise criteria)

{filter(asakusa2, direction=="S",str_sub(line,end=2)=="都営",time<86400) %>%
    mutate(hour = as.time(time)) %>%
    ggplot(aes(group=code,x=hour,y=distance,colour=service,text=paste0("station: ",station," (",station_en,")\ntime: ",hour,"\ndest: ",destination))) + 
    geom_line(size=0.1) +
    geom_point(size=0.02)} %>% 
  ggplotly(tooltip=c("code","text"))




