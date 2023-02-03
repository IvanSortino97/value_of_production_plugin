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
  
}

# Dataset chosen  ---------------------------------------------------------

domain_ = swsContext.datasets[[1]]@domain
dataset_ = swsContext.datasets[[1]]@dataset

# Parameters --------------------------------------------------------------

message( paste0('Your Value of Agricultural Production Plugin is reading its parameters.'))

param_year =  eval(parse(text = swsContext.computationParams$base_year ))  
param_aggregation = swsContext.computationParams$item_aggregate

selected_countries = swsContext.datasets[[1]]@dimensions[["geographicAreaM49"]]@keys

# Add leading zeroes to filter the datatable with production data
selected_countries <- str_pad(selected_countries, 3, pad = "0")

selected_element = swsContext.datasets[[1]]@dimensions[["measuredElement"]]@keys

# Get datatable data -------------------------------------------------------

message(paste('Your Value of Agricultural Production Plugin is reading datatables.'))

vop_item <- ReadDatatable("value_of_production_item")
vop_country <- ReadDatatable("aproduction_country")
vop_country_group <- ReadDatatable("aproduction_country_group")
vop_international_dollar <- ReadDatatable("supplementary_international_dollar")

## Select session countries and production data (5510)
vop_production <- ReadDatatable("qcl_faostat_production_data", 
                                columns = c("area_code_m49", "element_code", "item_code", "year", "value"),
                                where = paste0("element_code = '5510' AND area_code_m49 IN (", paste(shQuote(selected_countries), collapse=", "), ")"))

# Removing leading zeroes
vop_country_group[, country_code := sub( "^0+","", country_code )]
vop_production[, area_code_m49 := sub( "^0+","", area_code_m49 )]



## Select session countries

# vop_production =  vop_production[area_code_m49 %in% intersect(vop_production$area_code_m49, selected_countries ) ,]

## Fill production Gaps ----------------------------------------------------

message( paste0( "Your Value of Agricultural Production Plugin is filling production gaps."))

# reshape production datatable

# production_dcast = vop_production[, .(area_code_m49,
#                                       item_code, year,
#                                       value)]
# 
# suppressWarnings(production_dcast[, value:= as.numeric(value)] )  
# 
# production_dcast <- dcast( production_dcast,
#                            area_code_m49 + item_code  ~ year, fun = sum,
#                            value.var = 'value',
#                            fill = NA )

vop_production[, value := as.numeric(value)]

production_dcast <- dcast.data.table(
  vop_production,
  area_code_m49 + item_code  ~ year,
  value.var = 'value',
  fill = NA
  )

# if ( 'NA' %in% colnames(production_dcast) )  production_dcast[, `NA` := NULL  ] #remove column with NA name

# Find rows with NA

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
dim(production_dcast_removed_na)
dim(missing_prod_values)
dim(production_dcast)

production_dcast_completed <- rbind(production_dcast_removed_na, filled_values)
dim(production_dcast_completed)
production_dcast_completed[item_code %in% c('01191', '01342.02', '01356', '01359.01')]

# filled_rows = na.omit(match(paste(missing_prod_values$area_code_m49,
#                                     missing_prod_values$item_code),
#                              paste(production_dcast$area_code_m49, production_dcast$item_code)))
# 
# 
# 
# suppressWarnings(production_dcast[filled_rows, ] <- filled_values)

# melt back

# years <- colnames(production_dcast )[ which( colnames( production_dcast ) %in% 1900:2200) ]
years <- as.character(min(vop_production$year):max(vop_production$year))

# production_melt <- melt(production_dcast,
#                          id.vars = c("area_code_m49","item_code"),
#                          measure.vars = years)

production_melt <- melt(production_dcast_completed,
                        id.vars = c("area_code_m49","item_code"),
                        measure.vars = years)

production_melt <- setnames(production_melt, "variable", "year")



## Calculation -------------------------------------------------------------

message(paste('Your Value of Agricultural Production Plugin is calculating Gross Production Value.'))

# Selecting price and production data from datatables of base year selected

commodities_prices <- vop_international_dollar[year %in% param_year, .(item_code, value, year)]
commodities_production <- production_melt[year %in% as.character(param_year) , .(area_code_m49, item_code, year,value)]

commodities_prices <- setnames(commodities_prices, "value", "international_prices") #set names
commodities_production <- setnames(commodities_production, "value", "production") #set names

# commodities_prices$item_code <- str_pad(commodities_prices$item_code,4,pad="0") #pad for fcl2cpc function
# commodities_prices$item_code <- fcl2cpc(commodities_prices$item_code) #convert from fao code to cpc

commodities_prices[, item_code := str_pad(item_code,4, pad="0")] #pad for fcl2cpc function
commodities_prices[, item_code := fcl2cpc(item_code)] #convert from fao code to cpc

# Merging -------------------------------------------------------------------

# gross_production_value <- as.data.table(left_join(commodities_production,commodities_prices, by = c("item_code")))

gross_production_value <- merge(commodities_production, commodities_prices, by = 'item_code', all.x = TRUE)

gross_production_value <- setnames(gross_production_value, c("year.x","year.y") , c("year","base_year") ) #change names

# Calculate Gross production Value

# suppressWarnings({
gross_production_value[ , gross_prod_value := as.numeric(international_prices) * as.numeric(production)]
# })


gross_production_value[, gross_prod_value := gross_prod_value/1000]
setnames(gross_production_value, old = c('item_code', 'area_code_m49', 'year', 'gross_prod_value'),
         new = c('measuredItemCPC', 'geographicAreaM49', 'timePointYears', 'Value'))

gross_production_value[,  c('production', 'international_prices', 'base_year') := NULL]
gross_production_value <- nameData(domain_, dataset_, gross_production_value)
# write.csv(gross_production_value, file = 'module/Gross Production Value/outcome_plugin/italy_afg_single_items.csv', row.names = FALSE)

#gross_production_value[ , area_code_m49 := sub("^0+","", area_code_m49 )]


# Aggregate for countries


# countries_group = vop_country_group[ , c( 'iso2_code', 'iso3_code', 'country_code' )  := NULL ]
# setnames(countries_group, 'm49_code', 'area_code_m49' )
# 
# 
# #setdiff(countries_group$area_code_m49 , gross_production_value$area_code_m49) check differences in countries, China 1248 modified and cause problems
# 
# test <- gross_production_value %>% left_join( countries_group, by = "area_code_m49")
# test_g = test %>%  group_by( country_group, year, item_code ) %>% summarise(sum = sum(gross_prod_value, na.rm = T) ) %>%  as.data.table()
# View(test_g)
# 
# 
# test = gross_production_value %>% group_by( )


## Saving data

message( paste0('Your Value of Agricultural Production Plugin is saving data to the session'))

if ( param_aggregation == "single") {
  save_data = gross_production_value[ , .( measuredItemCPC = item_code,
                                           measuredElement = selected_element,
                                           timePointYears = year ,
                                           geographicAreaM49 = area_code_m49 ,
                                           Value = gross_prod_value,
                                           flagObservationStatus = "I",
                                           flagMethod = "e")  ]
  
  save_data[, geographicAreaM49:= as.numeric(geographicAreaM49)]
  save_data[, timePointYears:= as.character(timePointYears)]
  
  save_data_na <- which( is.na( save_data$Value ))
  save_data$flagObservationStatus[save_data_na] <- NA
  save_data$flagMethod[save_data_na] <- NA

} else {
  
  # Aggregate for items groups
  
  message(paste('Your Value of Agricultural Production Plugin is calculating item-aggregated Gross Production Value.'))
  
  vop_item_group <- ReadDatatable("value_of_production_item_group")
  
  vop_item_group[, item_code := NULL]
  setnames(vop_item_group, "cpc_code", "item_code")
  vop_item_group[, item_group_code := paste0("F", item_group_code)]
  
  # gross_production_value <- as.data.table(left_join(gross_production_value, vop_item_group, by = "item_code" )) #join
  
  gross_production_value <- merge(gross_production_value, vop_item_group, by = "item_code", allow.cartesian = TRUE)
  
  # gross_production_value_item_aggregate = gross_production_value  %>% 
  #   group_by(area_code_m49, year, item_group_code) %>% 
  #   summarise(sum = sum(gross_prod_value, na.rm = T)) %>% 
  #   as.data.table()
  
  gross_production_value_item_aggregate <- gross_production_value[, list(sum = sum(gross_prod_value, na.rm = TRUE)),
                                                                  by = c('area_code_m49', 'year', 'item_group_code')]

  # item_aggregate_NA = which(is.na(  gross_production_value_item_aggregate$item_group_code ))
  # gross_production_value_item_aggregate = gross_production_value_item_aggregate[-c(item_aggregate_NA) ]
  
  save_data = gross_production_value_item_aggregate[ , .(measuredItemCPC = item_group_code,
                                                         measuredElement = selected_element,
                                                         timePointYears = year ,
                                                         geographicAreaM49 = area_code_m49 ,
                                                         Value = sum,
                                                         flagObservationStatus = "I",
                                                         flagMethod = "e")  ]  
  
  # get names
  save_data <- nameData(domain_, dataset_, save_data)
  save_data[, Value := Value/1000]
  
  save_data[, timePointYears:= as.character(timePointYears)]
  # write.csv(save_data, file = 'module/Gross Production Value/outcome_plugin/italy_afg_item_groups.csv', row.names = FALSE)
  
}

# save_data[, geographicAreaM49:= as.numeric(geographicAreaM49)]
# save_data[, timePointYears:= as.character(timePointYears)]
# 
# save_data_na <- which( is.na( save_data$Value ))
# save_data$flagObservationStatus[save_data_na] <- NA
# save_data$flagMethod[save_data_na] <- NA

save <- SaveData(domain = domain_, dataset = dataset_, 
                  data = save_data, waitTimeout = 100000)

paste0("Your Value of Agricultural Production Plugin is completed successfully!",
       save$inserted, " observations written, ",
       save$ignored, " weren't updated, ",
       save$discarded, " had problems.")
