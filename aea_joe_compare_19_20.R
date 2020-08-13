library(tidyverse)
library(rvest)
library(lubridate)
library(tidylog)

#prerequisite: save the full AEA JOE ads for 2019 and 2020 to date as HTML. 

aea_19 <- read_html("https://www.aeaweb.org/joe/listings?q=eNplT1GqAkEMu0u_faD-uQcQBO9Q6kxdR2tnaWeVRby7FVxcEd5fSNI0ecC-eCva-7baFboHFEVKrdwYOh1FFnDh6V4tozNZOkEHsFww57BUnS1nlhkW9zFOYb1cXG3-luvwVit9UZLdPyXVUZtNaNz_hDndOOOxSmbzmUykuWRqjJ6Mrgf51jNOrA2ryjRT8pmEUZsNDyHErODa-G6NFL9lGIIlkcAD9Z-45_MFRVRe-A,,")
try1 <- aea_19 %>% html_nodes(".listing-item-header-date-posted") %>% html_text()
try19_df <- data.frame(date = as.Date(gsub("Date Posted: ", "", try1), format="%m/%d/%Y")) %>% 
  group_by(date) %>% summarise(n_ads = n()) %>% ungroup() %>% mutate(total_ads = cumsum(n_ads), day_in_year = yday(date), year = as.factor(year(date)), month=month(date))

try19_df %>%
ggplot() + 
  geom_line(aes(x=date, y=total_ads))

#aea_20 <- read_html("aea_20.html")
aea_20 <- read_html("https://www.aeaweb.org/joe/listings?q=eNplj1EKwkAMRO-Sb4XSzx5AELxDiLuxrsZsSbZKEe9uBAuCf-HNZJJ5wqF4Kzr6rtoNhlwnFEVKrdwZBuhgA1deHtUyOpOlc8BgzuGoCoPOXCIbuLCsY3GfP5t913fbrg9vtTIWJdn_KanO2mxB4_FcJ8zpzhlPVTKbrzCR5pKpMXoyuh2FV8U4sTasKsuK5NsI4202PIYQrYK1-fM1UtyWaQpKXCIxTzR-416vN_ovXaE,")
try1 <- aea_20 %>% html_nodes(".listing-item-header-date-posted") %>% html_text()
try20_df <- data.frame(date = as.Date(gsub("Date Posted: ", "", try1), format="%m/%d/%Y")) %>% 
  group_by(date) %>% summarise(n_ads = n()) %>% ungroup() %>% mutate(total_ads = cumsum(n_ads), day_in_year = yday(date), year = as.factor(year(date)), month=month(date))

all_aea <- bind_rows(try19_df, try20_df)


all_aea %>% 
  filter(month%in% c(8, 9, 10)) %>%
  ggplot() + 
  geom_line(aes(x=day_in_year, y=total_ads, group=year, colour=year))+
  scale_colour_brewer(palette="Set1")+theme_bw()+
  labs(title="Cumulative Number of JOE Ads, 2019 vs. 2020 to Date")


all_aea %>% 
  filter(month%in% c(8)) %>%
  arrange(day_in_year, year) %>%
  group_by(day_in_year) %>% summarise(diff_ads = mean(total_ads - lag(total_ads), na.rm=T), date = date[1])%>%
  filter(!is.na(diff_ads)) %>% 
  ggplot() + 
  geom_line(aes(x=date, y=diff_ads))+theme_bw()+
  labs(title="Difference in Cumulative Number of JOE Ads, 2019 vs. 2020 to Date")


