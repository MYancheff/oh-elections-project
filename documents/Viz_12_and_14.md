2012 and 2014 Viz
================

``` r
Ohio_df_lite <- read_csv ("data/Ohio_df_lite.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   SOS_VOTERID = col_character(),
    ##   COUNTY_ID = col_character(),
    ##   DATE_OF_BIRTH = col_date(format = ""),
    ##   REGISTRATION_DATE = col_date(format = ""),
    ##   VOTER_STATUS = col_character(),
    ##   PARTY_AFFILIATION = col_character(),
    ##   RESIDENTIAL_CITY = col_character(),
    ##   RESIDENTIAL_STATE = col_character(),
    ##   PRIMARY.03.06.2012 = col_character(),
    ##   GENERAL.11.06.2012 = col_character(),
    ##   PRIMARY.05.06.2014 = col_character(),
    ##   GENERAL.11.04.2014 = col_character(),
    ##   PRIMARY.03.15.2016 = col_character(),
    ##   GENERAL.06.07.2016 = col_character(),
    ##   PRIMARY.09.13.2016 = col_character(),
    ##   GENERAL.11.08.2016 = col_character()
    ## )

``` r
Ohio_df_lite <- mutate(Ohio_df_lite, age = 2017 - year(DATE_OF_BIRTH))
ohio_age_vote12 <- select(Ohio_df_lite, age, GENERAL.11.06.2012) %>% filter(age < 99)
ohio_age_vote14 <- select(Ohio_df_lite, age, GENERAL.11.04.2014) %>% filter(age < 99)
rm(Ohio_df_lite)
colnames(ohio_age_vote12) <- c("age", "vote")
colnames(ohio_age_vote14) <- c("age", "vote")
ohio_age_vote12 <- mutate(ohio_age_vote12, newvote = ifelse(vote %in% "X", "X", "O"))
ohio_age_vote14 <- mutate(ohio_age_vote14, newvote = ifelse(vote %in% "X", "X", "O"))
Ohio_summary12 <- ohio_age_vote12 %>% group_by(age) %>% dplyr::summarise(perc = base::mean(newvote == "X"), n())
Ohio_summary14 <- ohio_age_vote12 %>% group_by(age) %>% dplyr::summarise(perc = base::mean(newvote == "X"), n())
```

``` r
vote12complete <- read_csv("data/CCES_vote12.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   weight = col_double(),
    ##   age = col_integer(),
    ##   voted = col_character(),
    ##   party = col_character(),
    ##   registered = col_character(),
    ##   method_vote = col_character()
    ## )

``` r
vote12_noNA <- vote12complete %>% filter(registered == "Yes") %>% 
  select(-registered)

vote12_noNA <- vote12_noNA %>% 
  mutate(wtd = round(weight * 100))

vote12_long <- vote12_noNA %>% 
  mutate(ids = map(wtd, seq_len)) %>% 
  unnest()
write_csv(vote12_long, "data/vote12_long.csv")

smallviz12 <- vote12_long%>% select(age, voted) %>% group_by(age) %>% summarize(percent_voted = base::mean(voted == "yes"), n())

filteredCCES12 <- filter(smallviz12, age %in% 19:91)
filteredOhio12 <- filter(Ohio_summary12, age %in% 19:91)

merged12 <- merge(filteredCCES12, filteredOhio12, by = "age")
colnames(merged12) <-c("age", "CCESpercent", "CCESN", "VoterRegPercent", "VoterRegN")

diff12 <- mutate(merged12, CCESminusVoterReg = CCESpercent-VoterRegPercent)

write_csv(diff12, "data/diff12.csv")

SEdiff12 <- mutate(diff12, SE = sqrt((CCESpercent*(1-CCESpercent))/CCESN + ((VoterRegPercent*(1-VoterRegPercent))/VoterRegN)))

ggplot(SEdiff12, aes(age, CCESminusVoterReg, col = (CCESN + VoterRegN))) + geom_errorbar(aes(ymin = CCESminusVoterReg - SE, ymax = CCESminusVoterReg + SE))
```

![](Viz_12_and_14_files/figure-markdown_github-ascii_identifiers/data%2012-1.png)

``` r
vote14complete <- read_csv("data/CCES_vote14.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   weight = col_double(),
    ##   age = col_integer(),
    ##   voted = col_character(),
    ##   party = col_character(),
    ##   registered = col_character(),
    ##   method_vote = col_character()
    ## )

``` r
vote14_noNA <- vote14complete %>% filter(registered == "Yes") %>% 
  select(-registered)

vote14_noNA <- vote14_noNA %>% 
  mutate(wtd = round(weight * 100))

vote14_long <- vote14_noNA %>% 
  mutate(ids = map(wtd, seq_len)) %>% 
  unnest()
write_csv(vote14_long, "data/vote14_long.csv")

smallviz14 <- vote14_long%>% select(age, voted) %>% group_by(age) %>% summarize(percent_voted = base::mean(voted == "yes"), n())

filteredCCES14 <- filter(smallviz14, age %in% 19:91)
filteredOhio14 <- filter(Ohio_summary14, age %in% 19:91)

merged14 <- merge(filteredCCES14, filteredOhio14, by = "age")
colnames(merged14) <-c("age", "CCESpercent", "CCESN", "VoterRegPercent", "VoterRegN")

diff14 <- mutate(merged14, CCESminusVoterReg = CCESpercent-VoterRegPercent)

write_csv(diff14, "data/diff14.csv")

SEdiff14 <- mutate(diff14, SE = sqrt((CCESpercent*(1-CCESpercent))/CCESN + ((VoterRegPercent*(1-VoterRegPercent))/VoterRegN)))

ggplot(SEdiff14, aes(age, CCESminusVoterReg, col = (CCESN + VoterRegN))) + geom_errorbar(aes(ymin = CCESminusVoterReg - SE, ymax = CCESminusVoterReg + SE))
```

![](Viz_12_and_14_files/figure-markdown_github-ascii_identifiers/data%2014-1.png)
