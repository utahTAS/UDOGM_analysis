#script to process raw DOGM data
# https://oilgas.ogm.utah.gov/oilgasweb/data-center/dc-main.xhtml
# pull "Well Data" and "Production Data (2015 - 2020)"

#lexiewilson@utah.gov

library(dplyr)
library(lubridate)

path <- "D:/Users/lexiewilson/Documents/DOGM_pulls"

prod <- file.path(path,"rawdata","Production2015to2020.zip") %>% unzip() %>% read.csv(.,header=TRUE)
wells <- file.path(path,"rawdata","Wells.zip") %>% unzip() %>% read.csv(.,header=TRUE)

#convert factors to dates
dates <- mdy(prod$ReportPeriod)

#select date range (based on ReportPeriod)
  #NOTE: DOGM data is normally ~6 months delayed
prodf <- filter(prod,dates <= "2018-02-01" & dates >= "2017-03-01")
#narrow to Producing and Shut-in wells,only oil or gas wells
prodg <- filter(prodf,prodf$WellType == "GW" | prodf$WellType == "OW")
prodh <- filter(prodg,prodg$WellStatus == "S" | prodg$WellStatus == "P")

#concatenate: API, well bore, formation, and field for a unique identifier
tag <- mutate(prodh, concat = paste(prodh$API, prodh$WellBore, prodh$FormationName, sep="_"))

#join to wells info for lat/long and well name
#later - narrow down this table - it is TOO BIG
preAnalysis <- inner_join(wells,tag,by="API")

## FOR THE TIME BEING: export as csv and complete analysis in excel
write.csv(preAnalysis,file.path(path,"aug20_preAnalysis.csv"))

#check for wells with more than 12 months of production

#break ties: wells showing 6 mo. prod as "OW" and 6 mo. prod as "GW"

#Find annual averages from monthly water, oil, and gas production (from PROD not from WELLS)

#make a new column called "condensate"
#populate "condensate" with oil throughput values for GW

#final table: a.	API+WB+FORM	API	WELLBORE	FORMATION	Company	Well Name	Latitude	Longitude	WELL TYPE	Annual water (barrels)	Annual gas (mcf) Annual oil (barrels)	Annual condenate (barrels)	Days Produced


