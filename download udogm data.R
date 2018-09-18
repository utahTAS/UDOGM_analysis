# 
# 
# 
# 
#       the purpose is to downoald the udogm data 
# 
# 
# 
# 

system.time({

library(tidyverse)
library(lubridate)


udogm_data_pull <- function(url, file){    
    temp <- tempfile()
    download.file(url, temp)
    data <- read.csv(unz(temp, file),
                     stringsAsFactors = F,
                     na.strings = 'NULL',
                     colClasses = 'character')
    unlink(temp)
    as.tibble(data)
}

wells <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Wells.zip',
                         "Wells.csv")

wells_history <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/WellHistory.zip',
                                 'WellHistory.csv')

production <- do.call('rbind', 
                        
                      list(

    udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Production1984To1998.zip',
                                'Production1984To1998.csv'),

    udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Production1999To2008.zip',
                               'Production1999To2008.csv'),

    udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Production2009To2014.zip',
                                'Production2009To2014.csv'),

    udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Production2015To2020.zip',
                                'Production2015To2020.csv')
                            )
)

disposition <- do.call('rbind', 
                      
                        list(
                  
          udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Disposition1984To1998.zip',
                          'Disposition1984To1998.csv'),
          
          udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Disposition1999To2008.zip',
                          'Disposition1999To2008.csv'),
          
          udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Disposition2009To2014.zip',
                          'Disposition2009To2014.csv'),
          
          udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Disposition2015To2020.zip',
                          'Disposition2015To2020.csv')
      )
)

entities <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Entities.zip',
                                 'Entities.csv')

fields <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Fields.zip',
                            'Fields.csv')

operators <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/Operators.zip',
                          'Operators.csv')

producing_zones <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/ProducingZones.zip',
                             'ProducingZones.csv')

plant_summary <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/PlantSummary.zip',
                                   'PlantSummary.csv')

plant_prod <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/PlantProd.zip',
                                 'PlantProd.csv')

plant_well_alloc <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/PlantWellAlloc.zip',
                              'PlantWellAlloc.csv')

plant_location <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/PlantLocation.zip',
                                    'PlantLocation.csv')

plant_operators <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/PlantOperators.zip',
                                  'PlantOperators.csv')

lat_long_coordinates <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/LatLongCoordinates.zip',
                                   'LatLongCoordinates.csv')

utm_coordinates <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/UTMCoordinates.zip',
                                        'UTMCoordinates.csv')

uic_project_injection_vols <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/UICProjectVols.zip',
                                   'UICProjectVols.csv')

uic_disposal_vols <- udogm_data_pull('https://oilgas.ogm.utah.gov/pub/Database/UICWDWVols.zip',
                                    'UICWDWVols.csv')

})

current_file_name <- paste('udogm as of', Sys.Date(), sep = '_')
dput(current_file_name, 'current_file_name')

save(list = ls(), 
     file = current_file_name)







