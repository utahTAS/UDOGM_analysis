# 
# 
# 
#           
#           data pull and report out for annual emissions.
#           
#           this is meant to pull the current data from UDOGM and then generate 
#           a report for a annual time period starting 1 year and 6 months in 
#           the past and going till 6 months in the past
# 
#           
#####
## see how long this process takes
system.time({

#####
## requried libraries

library(tidyverse)
library(lubridate)
library(writexl)

#####
##  relevant tables from UDOGM
pathy <- "D:/Users/lexiewilson/Documents/DOGM_pulls"

wells <- file.path(pathy,"old","REDO_greg_compare","Production2015to2020","Production2015to2020.csv") %>% read.csv(.,header=TRUE) %>% select(-Operator)

production <- file.path(pathy,"old","REDO_greg_compare","Wells","Wells.csv") %>% read.csv(.,header=TRUE)

#####
## set date range of interest based on the date the report was pulled from UDOGM
## these dates will go back to the beginning of the month in the case of the 
## start date and the end of the month for the end date: one full year.

date_report_ran <- Sys.Date()


# report period for 2017 annual report
start_report_window <- as.Date("2017-01-01")
end_report_window <- as.Date("2017-12-01")

#####
##  join tables and first round mutate

report_0 <- production %>%
  full_join(., wells) %>%    
  mutate(ReportPeriod = mdy(ReportPeriod),
         Received = mdy(Received),
         DaysProd = as.numeric(DaysProd),
         Oil = as.numeric(Oil),
         Gas = as.numeric(Gas),
         Water = as.numeric(Water),
         Ground_Elev = as.numeric(Ground_Elev),
         Kelly_Elev = as.numeric(Kelly_Elev),
         Floor_Elev = as.numeric(Floor_Elev),
         ConfRelDate = ymd_hms(ConfRelDate),
         TotCum_Oil = as.numeric(TotCum_Oil),
         TotCum_Gas = as.numeric(TotCum_Gas),
         TotCum_Water = as.numeric(TotCum_Water),
         tag = paste(API, WellBore, FormationName, FieldName, Operator, sep = '_'))

#####
##  filter down to the period/type of interest.
report_1 <- report_0 %>%
    filter(ReportPeriod >= start_report_window & ReportPeriod <= end_report_window,
           WellType == 'GW' | WellType == 'OW',
           WellStatus == 'S' | WellStatus == 'P')

#####
## checking if there are more than 12 months of production
## establishing well type by first well type and then
## break ties: wells showing 6 mo. prod as "OW" and 6 mo. prod as "GW" will be 
## categorized by which had more oil or gas production. gas production was
## converted to bbl equivilant. 

t1 <- report_1 %>%
    group_by(tag) %>%
    summarize(months_production = n(),
              gw = sum(WellType=='GW'),
              ow = sum(WellType=='OW'),
              gas_bbl = sum(Gas*0.1781076, na.rm = T),
              oil_bbl = sum(Oil, na.rm = T),
              well_type = ifelse(gw > ow, 'Gas Well',
                                 ifelse(ow > gw, 'Oil Well',
                                        ifelse(ow==gw & gas_bbl > oil_bbl,
                                               'Gas Well',
                                               ifelse(ow==gw & oil_bbl > gas_bbl,
                                                      'Oil Well', 
                                                      'Weird')
                                               )
                                        )
                                 )
              ) %>%
    select(tag, months_production, well_type)

#added CountyName to final report 
report_2 <- full_join(report_1, t1) %>%
    mutate(Condensate = ifelse(well_type == 'Gas Well', Oil, NA),
           Oil = ifelse(well_type == 'Oil Well', Oil, NA)) %>%
    group_by(tag, API, WellBore, FormationName, well_type, Operator, WellName, 
             CountyName, Latitude, Longitude) %>%
    summarize(annual_average_water = mean(Water, na.rm = T),
              annual_average_oil = mean(Oil, na.rm = T),
              annual_average_gas = mean(Gas, na.rm = T),
              annual_average_condensate = mean(Condensate, na.rm = T),
              total_days_production = sum(DaysProd, na.rm = T))

QA <- data.frame(`Date report was run` = as.character(date_report_ran),
                 `Report window` = paste(start_report_window, 'to',
                                         end_report_window, sep = ' '),
                 `Any facility over 12 months` = any(t1$months_production>12)) %>%
    gather()


sheets <- list(`Date Ran and QA` = QA,
               `Annual Report` = report_2) 

write_xlsx(sheets, path = paste('UDOGM Annual Average Report from', 
                                start_report_window, 'to', end_report_window, 
                                '.xlsx',
                                sep = ' '))

})























