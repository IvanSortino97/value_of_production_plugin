message(paste('Your Value of Agricultural Production Plugin has started.'))

# Loading libraries -------------------------------------------------------

message(paste('Your Value of Agricultural Production Plugin is loading libraries.'))

.libPaths("/newhome/shared/Library/3.3.3/")

suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(faoswsFlag)
  library(data.table)
  library(stringr)
  library(dplyr)
})


# Setting the environment -------------------------------------------------

if(CheckDebug()){
  
  library(faoswsModules)
  SETT <- ReadSettings("~/Agriculture Production/module/Gross Production Value/sws.yml")
  SetClientFiles(SETT[["certdir"]])
  GetTestEnvironment(baseUrl = SETT[["server"]], token = SETT[["token"]])
  source("~/Agriculture Production/module/Gross Production Value/R function/gap_filler.R")
  source("~/Agriculture Production/module/Gross Production Value/R function/discard_year_list.R")
  
}

# Dataset chosen  ---------------------------------------------------------

domain_ = swsContext.datasets[[1]]@domain
dataset_ = swsContext.datasets[[1]]@dataset

# Parameters --------------------------------------------------------------

message( paste0('Your Value of Agricultural Production Plugin is reading its parameters.'))

param_year =  eval(parse(text = swsContext.computationParams$base_year ))
param_aggregation = swsContext.computationParams$item_aggregate
param_prices = swsContext.computationParams$param_prices

selected_countries = swsContext.datasets[[1]]@dimensions[["geographicAreaM49"]]@keys

# Add leading zeroes to filter the datatable with production data
selected_countries <- str_pad(selected_countries, 3, pad = "0")

selected_element = swsContext.datasets[[1]]@dimensions[["measuredElement"]]@keys

# Get datatable data -------------------------------------------------------

message(paste('Your Value of Agricultural Production Plugin is reading datatables.'))

vop_item <- ReadDatatable("value_of_production_item")
vop_item_group <- ReadDatatable("value_of_production_item_group", columns = c( "item_group_code", "cpc_code", "factor"))
vop_country <- ReadDatatable("aproduction_country", columns = c("m49_code","start_date", "end_date" ))
vop_country_group <- ReadDatatable("aproduction_country_group", columns =  c("country_group_code","m49_code", "country_group"))




## Select session countries and production data (5510)
vop_production <- ReadDatatable("qcl_faostat_production_data", 
                                columns = c("area_code_m49", "element_code", "item_code", "year", "value"),
                                where = "element_code = '5510'"
                              # where = paste0("element_code = '5510' AND area_code_m49 IN (", paste(shQuote(selected_countries), collapse=", "), ")") # read production only for session's countries
                              #  where = paste0("element_code = '5510' AND area_code_m49 IN (", paste(shQuote(vop_country$m49_code), collapse=", "), ")")
                              )



#Indegenous meat 



# Removing leading zeroes
vop_country[, m49_code := sub( "^0+","", m49_code )]
vop_country_group[, m49_code := sub( "^0+","", m49_code )]
vop_production[, area_code_m49 := sub( "^0+","", area_code_m49 )]


## Select session countries

## Fill production Gaps ----------------------------------------------------

message( paste0( "Your Value of Agricultural Production Plugin is filling production gaps."))

suppressWarnings({ 
vop_production[, value := as.numeric(value)]
})

production_dcast <- dcast.data.table(
  vop_production,
  area_code_m49 + item_code  ~ year,
  value.var = 'value',
  fill = NA
)

missing_prod_values <- production_dcast[!complete.cases(production_dcast)] %>% as.data.frame() # select rows that contains NA

# remove rows with only NA and take the one with gaps

missing_prod_values = missing_prod_values[rowSums(missing_prod_values[, 3:length(missing_prod_values)], na.rm = T ) > 0 ,]


# run gap_filler.R

filled_values = as.data.table(gap_filler(missing_prod_values, 2))

# Remove missing data from production_dcast and then we will add the filled_values data

missing_prod_values <- data.table(missing_prod_values) # convert it to data.table

setkey(production_dcast, area_code_m49, item_code) # setkey
setkey(missing_prod_values, area_code_m49, item_code) # setkey

production_dcast_removed_na <- production_dcast[!missing_prod_values] # data without NA's

production_dcast_completed <- rbind(production_dcast_removed_na, filled_values)

years <- as.character(min(vop_production$year):max(vop_production$year))

production_melt <- melt(production_dcast_completed,
                        id.vars = c("area_code_m49","item_code"),
                        measure.vars = years)

production_melt <- setnames(production_melt, "variable", "year")



## Calculation -------------------------------------------------------------

message(paste('Your Value of Agricultural Production Plugin is calculating Gross Production Value.'))


# Selecting production data from datatables of base year selected

commodities_production <- production_melt[year %in% as.character(param_year) , .(area_code_m49, item_code, year,value)]
commodities_production <- setnames(commodities_production, "value", "production") #set names

#### Selecting Prices

if (param_prices == "ID") {
  #Gross production International dollar
  
  vop_international_dollar <- ReadDatatable("supplementary_international_dollar")
  
  commodities_prices <- vop_international_dollar[year %in% param_year, .(item_code, value, year)]
  commodities_prices <- setnames(commodities_prices, "value", "international_prices") #set names
  commodities_prices[, item_code := str_pad(item_code,4, pad="0")] #pad for fcl2cpc function
  commodities_prices[, item_code := fcl2cpc(item_code)] #convert from fao code to cpc
  
  gross_production_value <- merge(commodities_production, commodities_prices, by = 'item_code', all.x = TRUE)
  gross_production_value <- setnames(gross_production_value, c("year.x","year.y") , c("year","base_year") ) #change names
  
  # Calculate Gross production Value
  gross_production_value[ , gross_prod_value := as.numeric(international_prices) * as.numeric(production) /1000 ]
  gross_production_value[,  c('production', 'international_prices', 'base_year') := NULL]
  
  element <- "152 - international dollar" # temporary 
  
} else if (param_prices == "SLC_cu") {
    
  # Gross production SLC current price 
  vop_SLC_prices <- ReadDatatable("faostat_pp", columns =  c("area_code_m49", "item_code_cpc", "year", "value"))
  vop_SLC_prices[, area_code_m49 := sub( "^0+","", area_code_m49 )]
  
  commodities_prices <- vop_SLC_prices[ year %in% param_year,]
  commodities_prices <- setnames(commodities_prices, c("item_code_cpc", "value") , c("item_code", "SLC_prices")) #set names
  
  gross_production_value <- merge(commodities_production, commodities_prices, by = c('area_code_m49', 'item_code', 'year'), all.x = TRUE)
  
  # Calculate Gross production Value
  gross_production_value[ , gross_prod_value := as.numeric(SLC_prices) * as.numeric(production) /1000]
  gross_production_value[,  c('production', 'SLC_prices') := NULL]
  
  element <- "56 - international dollar" # temporary 
}


# setnames(gross_production_value, old = c('item_code', 'area_code_m49', 'year', 'gross_prod_value'),
#          new = c('measuredItemCPC', 'geographicAreaM49', 'timePointYears', 'Value'))

# Merging -------------------------------------------------------------------



#gross_production_value <- nameData(domain_, dataset_, gross_production_value)
# write.csv(gross_production_value, file = 'module/Gross Production Value/outcome_plugin/italy_afg_single_items.csv', row.names = FALSE)



#### Aggregate for items groups ####

message(paste('Your Value of Agricultural Production Plugin is calculating item-aggregated Gross Production Value.'))


#vop_item_group[, item_code := NULL]
setnames(vop_item_group, "cpc_code", "item_code")
vop_item_group[, item_group_code := paste0("F", item_group_code)]

gross_production_value_item_aggregate <- merge(gross_production_value, vop_item_group, by = "item_code", allow.cartesian = TRUE)

gross_production_value_item_aggregate <- gross_production_value_item_aggregate[, list(item_sum = sum(gross_prod_value, na.rm = TRUE)),
                                                                               by = c('area_code_m49', 'year', 'item_group_code')]


#### Aggregate for Countries groups ####

setnames(vop_country_group, 'm49_code', 'area_code_m49' )

gross_production_value_country_aggregate <- merge(gross_production_value, vop_country_group, by = "area_code_m49", allow.cartesian = TRUE)

discard_countries_year <- date_to_discard_list(vop_country) #create a list with all the country-years to remove: before startdate, after enddate

gross_production_value_country_aggregate <- merge(gross_production_value_country_aggregate,
                                                         discard_countries_year,
                                                         by = c("area_code_m49", "year"), all.x = T)

gross_production_value_country_aggregate <- gross_production_value_country_aggregate[ is.na(remove)] # adjust the groups for each years
gross_production_value_country_aggregate[, remove := NULL]

gross_production_value_country_aggregate <- gross_production_value_country_aggregate[, list(country_sum = sum(gross_prod_value, na.rm = TRUE)),
                                                                by = c('country_group', 'year', 'item_code')]

#### Aggregate for Item groups - Countries groups ####

gross_production_value_country_item_aggregate <- merge(gross_production_value_item_aggregate, vop_country_group, by = "area_code_m49", allow.cartesian = TRUE)


gross_production_value_country_item_aggregate <- merge(gross_production_value_country_item_aggregate,
                                                  discard_countries_year,
                                                  by = c("area_code_m49", "year"), all.x = T)

gross_production_value_country_item_aggregate <- gross_production_value_country_item_aggregate[ is.na(remove)] # adjust the groups for each years
gross_production_value_country_item_aggregate[, remove := NULL]



gross_production_value_country_item_aggregate <- gross_production_value_country_item_aggregate[, list(country_sum = sum(item_sum, na.rm = TRUE)),
                                                                                     by = c('country_group', 'year', 'item_group_code')]




## Saving data

if (param_aggregation == "single_item") {
  save_data = gross_production_value 
  setnames(save_data, c(colnames(gross_production_value)),c("measureditemcpc", "geographicaream49", "timepointyears", "value"))
  
  } else if (param_aggregation == "item_aggregate") {
    save_data = gross_production_value_item_aggregate
    setnames(save_data, c(colnames(gross_production_value_item_aggregate)),c( "geographicaream49", "timepointyears","measureditemcpc", "value"))
    
    
  } else if (param_aggregation == "country_aggregate") {
    save_data = gross_production_value_country_aggregate
    setnames(save_data, c(colnames(gross_production_value_country_aggregate)),c( "geographicaream49", "timepointyears","measureditemcpc", "value"))
    
  } else { save_data = gross_production_value_country_item_aggregate
           setnames(save_data, c(colnames(gross_production_value_country_item_aggregate)),c( "geographicaream49", "timepointyears","measureditemcpc", "value"))
          }

save_data[, measuredelement := element] 

# Temporary saving data on a Datatable 'gross_production_value'
message( paste0('Your Value of Agricultural Production Plugin is saving data into "gross_production_value" dataset'))

table <- 'gross_production_value'
changeset <- Changeset(table)  
old_data <- ReadDatatable(table, readOnly = FALSE) 

AddDeletions(changeset, old_data) # remove older data

AddInsertions(changeset, save_data)
Finalise(changeset)





#/////////////////////////////// NEED REWORK //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

# message( paste0('Your Value of Agricultural Production Plugin is saving data to the session'))
# 
# if ( param_aggregation == "single") {
#   save_data = gross_production_value[ , .( measuredItemCPC = item_code,
#                                            measuredElement = selected_element,
#                                            timePointYears = year ,
#                                            geographicAreaM49 = area_code_m49 ,
#                                            Value = gross_prod_value,
#                                            flagObservationStatus = "I",
#                                            flagMethod = "e")  ]
# 
#   save_data[, geographicAreaM49:= as.numeric(geographicAreaM49)]
#   save_data[, timePointYears:= as.character(timePointYears)]
# 
#   save_data_na <- which( is.na( save_data$Value ))
#   save_data$flagObservationStatus[save_data_na] <- NA
#   save_data$flagMethod[save_data_na] <- NA
# 
# } else {
# 
# 
#   vop_item_group[, item_code := NULL]
#   setnames(vop_item_group, "cpc_code", "item_code")
#   vop_item_group[, item_group_code := paste0("F", item_group_code)]
# 
#   gross_production_value <- merge(gross_production_value, vop_item_group, by = "item_code", allow.cartesian = TRUE)
# 
#   gross_production_value_item_aggregate <- gross_production_value[, list(sum = sum(gross_prod_value, na.rm = TRUE)),
#                                                                   by = c('area_code_m49', 'year', 'item_group_code')]
# 
#   save_data = gross_production_value_item_aggregate[ , .(measuredItemCPC = item_group_code,
#                                                          measuredElement = selected_element,
#                                                          timePointYears = year ,
#                                                          geographicAreaM49 = area_code_m49 ,
#                                                          Value = sum,
#                                                          flagObservationStatus = "I",
#                                                          flagMethod = "e")  ]
# 
#   # get names
#   save_data <- nameData(domain_, dataset_, save_data)
# 
#   save_data[, timePointYears:= as.character(timePointYears)]
#   # write.csv(save_data, file = 'module/Gross Production Value/outcome_plugin/italy_afg_item_groups.csv', row.names = FALSE)
# 
# }
# 
# 
# save <- SaveData(domain = domain_, dataset = dataset_,
#                  data = save_data, waitTimeout = 100000)
# 
# paste0("Your Value of Agricultural Production Plugin is completed successfully!",
#        save$inserted, " observations written, ",
#        save$ignored, " weren't updated, ",
#        save$discarded, " had problems.")
# 
