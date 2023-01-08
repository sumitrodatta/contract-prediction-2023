library(tidyverse)
library(rvest)
library(janitor)
library(polite)
library(lubridate)
library(openxlsx)

realgm_bow=bow("https://basketball.realgm.com/",user_agent = "Sumitro Datta",force=TRUE)
print(realgm_bow)

salary_cap_hist<-scrape(nod(realgm_bow,path="nba/info/salary_cap")) %>% 
  html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "compact", " " ))]') %>% 
  .[[1]] %>% html_table(fill=TRUE)  %>% 
  #add correct column names (ended up as first row)
  row_to_names(1) %>% clean_names() %>% select(3:4) %>%
  #only take year and cap number, parse cap into a number (has dollar sign and commas originally)
  rename(season=luxury_tax,cap=bae) %>%
  mutate(season=as.numeric(str_sub(season,start=-4))) %>%
  mutate(cap=parse_number(cap))

write_csv(salary_cap_hist,"Data/Salary Cap History.csv")

spotrac_bow=bow("https://www.spotrac.com/",user_agent = "Sumitro Datta",force=TRUE)
print(spotrac_bow)
fa_current_yr<-scrape(nod(spotrac_bow,path="nba/free-agents/2023")) %>% html_nodes("table") %>% .[[1]] %>% html_table()

cleaned_fa_current_yr= fa_current_yr %>% 
  #filter out coaches
  filter(`Pos.` != "COA") %>% 
  rename(Player=1,Experience=Exp,sal_2023=`2022-2023 AAV`) %>% 
  select(Player,Type,Experience,sal_2023) %>% mutate(season=2023) %>% 
  #remove repeat of player's last name
  separate(Player,into=c('to_discard','player'),sep='\\s{2,100}') %>% select(-to_discard) %>%
  arrange(player) %>% clean_names() %>% 
  #add salary amounts for options
  mutate(contract_yrs=NA,
         first_year_percent_of_cap=ifelse(str_detect(type,"O"),parse_number(sal_2023),NA)) %>%
  select(-sal_2023)  %>%
  #change names to match basketball-reference data
  mutate(player=case_when(str_detect(player,"AJ Green")~"A.J. Green",
                          str_detect(player,'Bruce Brown Jr.')~'Bruce Brown',
                          str_detect(player,'Boban')~'Boban Marjanović',
                          str_detect(player,'Bogdan Bogdan')~'Bogdan Bogdanović',
                          str_detect(player,"Dario")~"Dario Šarić",
                          str_detect(player,'Goran')~'Goran Dragić',
                          str_detect(player,"Ishmael")~"Ish Smith",
                          str_detect(player,"Ishmail")~"Ish Wainright",
                          str_detect(player,"J.D. Davison")~"JD Davison",
                          str_detect(player,"Juancho")~"Juancho Hernangómez",
                          str_detect(player,"Kristaps")~"Kristaps Porziņģis",
                          str_detect(player,"Moussa")~"Moussa Diabaté",
                          str_detect(player,"Vucevic")~"Nikola Vučević",
                          str_detect(player,"PJ Wash")~"P.J. Washington",
                          str_detect(player,"RJ Hamp")~"R.J. Hampton",
                          str_detect(player,"Sviatoslav")~"Svi Mykhailiuk",
                          str_detect(player,"Theo")~"Théo Maledon",
                          str_detect(player,"Tillman")~"Xavier Tillman Sr.",
                          TRUE~player))

write_csv(cleaned_fa_current_yr,"Data/Free Agents 2023.csv")

#use pro-sports-transactions to get full picture of free agents (players waived during season)
#start from 2011-2012 season (first season under new CBA)

pro_sports_transact_bow=bow("https://www.prosportstransactions.com/",user_agent = "Sumitro Datta",force=TRUE)
print(pro_sports_transact_bow)

get_pro_sports_transact<-function(begin_date,end_date){
  session=nod(pro_sports_transact_bow,path=paste0(
    "basketball/Search/SearchResults.php?BeginDate=",begin_date,"&EndDate=",end_date,"&PlayerMovementChkBx=yes&Submit=Search"))
  pages_string=scrape(session) %>%
    html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "bodyCopy", " " ))]') %>%
    .[[3]] %>% html_text()
  number_pages=as.numeric(str_sub(pages_string,as.numeric(stringi::stri_locate_last_regex(pages_string,"\\s")[,1])))-1
  transactions=tibble()
  sapply(0:number_pages*25,function(x){
    new_session=nod(pro_sports_transact_bow,path=paste0(
      "basketball/Search/SearchResults.php?BeginDate=",begin_date,"&EndDate=",end_date,"&PlayerMovementChkBx=yes&Submit=Search&start=",
      x))
    new_transact_df=scrape(new_session) %>% 
      html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "center", " " ))]') %>%
      html_table() %>% .[[1]] %>% row_to_names(1) %>% clean_names()
    transactions<<-bind_rows(transactions,new_transact_df)
    print(x)
  })
  return(transactions)
}

transactions=get_pro_sports_transact(begin_date="2011-12-25",end_date="2021-10-18")

x2022_transact=get_pro_sports_transact(begin_date="2021-10-19",end_date="2022-10-17") #as of Jan 6 2023

clean_transacts<-function(transact_df){
  cleaned_df=transact_df %>% mutate(date=ymd(date)) %>% na_if("") %>% 
    #combine two cols into one player column
    mutate(player=coalesce(acquired,relinquished),.before="notes") %>% 
    select(-c(acquired,relinquished)) %>%
    #remove front office & coaching staff
    filter(str_detect(
      notes,"coach|GM|general manager|basketball operations|owner|fired|hired|promote|demote|resigned|renamed",negate = TRUE)) %>%
    filter(str_detect(notes,"trade|NBA draft|first round pick",negate = TRUE)) %>%
    mutate(player=str_replace(player,"• ",""))
  return(cleaned_df)
}

cleaned_transact=clean_transacts(transactions) %>% 
  #season corresponds from first day of games to day before first day of games of next season
  mutate(season=case_when(
    date %within% interval(ymd("2020-12-22"),ymd("2021-10-18"))~2021,
    date %within% interval(ymd("2019-10-22"),ymd("2020-12-21"))~2020,
    date %within% interval(ymd("2018-10-16"),ymd("2019-10-21"))~2019,
    date %within% interval(ymd("2017-10-17"),ymd("2018-10-15"))~2018,
    date %within% interval(ymd("2016-10-25"),ymd("2017-10-16"))~2017,
    date %within% interval(ymd("2015-10-27"),ymd("2016-10-24"))~2016,
    date %within% interval(ymd("2014-10-28"),ymd("2015-10-26"))~2015,
    date %within% interval(ymd("2013-10-29"),ymd("2014-10-27"))~2014,
    date %within% interval(ymd("2012-10-30"),ymd("2013-10-28"))~2013,
    date %within% interval(ymd("2011-12-25"),ymd("2012-10-29"))~2012))

cleaned_2022_transact=clean_transacts(x2022_transact) %>% mutate(season=2022)

#include option years, partial guaranteed years in counting contract years

write.xlsx(cleaned_transact,"Data/Transactions 2012-2021.xlsx")
write.xlsx(cleaned_2022_transact,"Data/Transactions 2022.xlsx")
