jiko <- function (line, dir, a, min=0, exc=0) {
  station1 <- data.frame()
  for (w in 0:2) {
    for (i in min:a) {
      ok <- TRUE
      while (ok == TRUE) {
        tryCatch({
          path1 <- read_html(paste0("http://ekitan.com/timetable/railway/line-station/",line,"-",i,"/d1/?dw=",w))
          master_path <- html_nodes(path1, xpath = "/html/body/div[1]/div[2]/div/div[1]/div/form/div/div/div[2]/div/div[2]/div[2]/table")
          num1 <- html_nodes(path1, xpath = "/html/body/div[1]/div[2]/div/div[1]/div/form/div/div/div[2]/div[1]/div[2]/div[2]/table/tr/td/ul/li") %>% length()
          single <- data.frame(name=html_nodes(path1, xpath = "/html/body/div[1]/div[2]/div/div[1]/div/h2/span/text()") %>% html_text,
                               code=html_nodes(master_path, xpath = "tr/td/ul/li/a") %>% html_attr("data-tx"),
                               time=html_nodes(master_path, xpath = "tr/td/ul/li/a") %>% html_attr("data-departure"),
                               service=html_nodes(master_path, xpath = "tr/td/ul/li") %>% html_attr("data-tr-type"),
                               destination=html_nodes(master_path, xpath = "tr/td/ul/li") %>% html_attr("data-dest"),
                               w=w, i=i,
                               stringsAsFactors = FALSE) %>%
            mutate(n=row_number(), j=case_when(n<=num1 ~ 1, TRUE ~ 2))
          station1 <- bind_rows(station1, single) },
    error = function(e) { Sys.sleep(2)
      e})
        if ("error" %in% class(single)) { cat("Bad", " ") }
        else { cat(w,"-",line,"-",i, " ",sep="")
          ok <- FALSE }}}}
  dir2 <- case_when(dir=="N" ~ "S", dir=="S" ~ "N", dir=="W" ~ "E", dir=="E" ~ "W")
  return(mutate(station1, direction = case_when((j==1&i!=exc) ~ dir, TRUE ~ dir2),
                name = gsub("\\((.*?)\\)", "", name), #remove "(東京都)", etc.
                time = as.numeric(as.time(paste0(str_sub(time,end=2),":",str_sub(time,-2)))),
                time = case_when(time < 10800 ~ time + 86400, TRUE ~ time),
                w = case_when(w==0 ~ "Weekday", w==1 ~ "Saturday",
                              TRUE ~ "Sunday")) %>% #Adjust trains after midnight
           separate(code,c("code1","code2","code"),"-", extra="merge") %>%
           separate(name,c("station","line"),"　") %>%
           select(-code1,-code2, -j, -n) %>%
           mutate(station = str_sub(station,end=-2))) } #remove "駅" from end