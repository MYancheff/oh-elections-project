Data visualization of voting by age according to polls
================

creating summary tables for voting percentage my age with Ohio data
-------------------------------------------------------------------

``` r
 Ohio_df_lite <- read_csv("data/Ohio_df_lite.csv")
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
 #mutate to make an age variable
 
 Ohio_df_lite <- mutate(Ohio_df_lite, age = 2017 - year(DATE_OF_BIRTH))
 
 ohio_age_vote <- select(Ohio_df_lite, age, GENERAL.11.08.2016) %>% filter(age < 99)
 #remove ohio_df_lite to save space
 rm(Ohio_df_lite)

#rename column names
colnames(ohio_age_vote) <- c("age", "vote")



#renaming the NAs
ohio_age_vote1 <- mutate(ohio_age_vote, newvote = ifelse(vote %in% "X", "X", "O"))

Ohio_summary2 <- ohio_age_vote1 %>% group_by(age) %>% dplyr::summarise(perc = base::mean(newvote == "X"), n())

# https://github.com/ProjectMOSAIC/mosaic/issues/625
```

polling data : summarized voting percent by age
-----------------------------------------------

``` r
viz16 <- read_csv("data/vote16_long.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   weight = col_double(),
    ##   age = col_integer(),
    ##   voted = col_character(),
    ##   party = col_character(),
    ##   method_vote = col_character(),
    ##   wtd = col_integer(),
    ##   ids = col_integer()
    ## )

``` r
smallviz16 <- viz16 %>% select(age, voted) %>% group_by(age) %>% summarize(percent_voted = base::mean(voted == "yes"), n())

#noNAviz16 <- filter(viz16, voted %in% c("yes", "no"))
#noNAsmallviz16 <- noNAviz16 %>% select(age, voted) %>% group_by(age) %>% summarize(percent_voted_noNA = base::mean(voted == "yes"))
```

``` r
#tidying it up so we can merge


filteredCCES <- filter(smallviz16, age %in% 19:91)

filteredOhio <- filter(Ohio_summary2, age %in% 19:91)
```

``` r
#merging
merged <- merge(filteredCCES, filteredOhio, by = "age")
colnames(merged) <-c("age", "CCESpercent", "CCESN", "VoterRegPercent", "VoterRegN")


diff <- mutate(merged, CCESminusVoterReg = CCESpercent-VoterRegPercent)

write_csv(diff, "data/diff16.csv")

SEdiff <- mutate(diff, SE = sqrt((CCESpercent*(1-CCESpercent))/CCESN + ((VoterRegPercent*(1-VoterRegPercent))/VoterRegN)))

ggplot(SEdiff, aes(age, CCESminusVoterReg, col = (CCESN + VoterRegN))) + geom_errorbar(aes(ymin = CCESminusVoterReg - SE, ymax = CCESminusVoterReg + SE))
```

![](full_scale_viz_files/figure-markdown_github-ascii_identifiers/Merge%20and%20Plot-1.png)
