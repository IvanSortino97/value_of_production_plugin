##### CHINA CODE FIX ###############################################################
# Remove the following lines once the china code has been patched from 1248 to 156 #
# Remove lines also in data_production.R script
# OR set CHINA_FIX = F

CHINA_FIX = T

# If TRUE, pull production data with key 1248 and convert it to 156
# if TRUE, do not calculate region aggregate - specify corrects roots in output dataset
####################################################################################
####################################################################################

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
  library(methods)
})


# Setting the environment -------------------------------------------------

if(CheckDebug()){
  
  library(faoswsModules)
  SETT <- ReadSettings("~/Agriculture Production/module/Production Indices/sws.yml")
  SetClientFiles(SETT[["certdir"]])
  GetTestEnvironment(baseUrl = SETT[["server"]], token = SETT[["token"]])
  #source("~/Agriculture Production/module/Value of Agricultural Production/R function/gap_filler.R")
  source("~/Agriculture Production/module/Production Indices/R function/discard_year_list.R")
  source("~/Agriculture Production/module/Production Indices/R function/data_production.R")
  source("~/Agriculture Production/module/Production Indices/R function/data_prices.R")
  source("~/Agriculture Production/module/Production Indices/R function/auth_check.R")
  
}

# Dataset chosen  ---------------------------------------------------------

domain_ = swsContext.datasets[[1]]@domain
dataset_ = swsContext.datasets[[1]]@dataset

# Parameters --------------------------------------------------------------

message( paste0('Your Value of Agricultural Production Plugin is reading its parameters.'))

param_base_year = swsContext.computationParams$base_year
param_item_aggr = swsContext.computationParams$item_aggr
param_country_aggr = swsContext.computationParams$country_aggr
param_source_prod = swsContext.computationParams$source_production
param_source_prices = swsContext.computationParams$source_prices


#selected_countries = swsContext.datasets[[1]]@dimensions[["geographicAreaM49"]]@keys
selected_element = swsContext.datasets[[1]]@dimensions[["measuredElement"]]@keys
selected_years = swsContext.datasets[[1]]@dimensions[["timePointYears"]]@keys

# Check rights on datasets

message(Rights_check())

# Get support datatable data -------------------------------------------------------

message(paste('Your Value of Agricultural Production Plugin is reading datatables.'))

vop_item <- ReadDatatable("value_of_production_item", columns = c( "description","cpc_code"))
setnames(vop_item, c("cpc_code"), c("measuredItemCPC"))

vop_item_group <- ReadDatatable("value_of_production_item_group", columns = c( "item_group_code", "cpc_code", "factor"))
vop_item_group[, item_group_code := paste0("F", item_group_code)] #from FAOSTAT CODE TO SWS CODE
setnames(vop_item_group, "cpc_code", "measuredItemCPC")

# vop_indigenous <- ReadDatatable("value_of_production_indigenous_meat", columns = c("description","cpc_code"))
# setnames(vop_indigenous, c("cpc_code"), c("measuredItemCPC"))

vop_country <- ReadDatatable("aproduction_country", columns = c("m49_code","start_date", "end_date" ))
setnames(vop_country, "m49_code", "geographicAreaM49")

vop_country_group <- ReadDatatable("aproduction_country_group", columns =  c("country_group_code","m49_code"))
setnames(vop_country_group, "m49_code", "geographicAreaM49")





message(paste('Your Value of Agricultural Production Plugin is pulling data.'))

## Get production data
# Pull data from production/disseminated data

if (param_source_prod == "production"){ 
  
  domain_prod = "agriculture"
  dataset_prod = "aproduction"
  data_production <- data_prod(domain_prod,dataset_prod)
  
  } else if (param_source_prod == "disseminated"){
    
    data_production_diss = rbind(data_prod("disseminated","crops_production"),
                                 data_prod("disseminated","livestock_production"))

    
}



data_production <- data_prod(domain_prod,dataset_prod)

if ( "154" %in% selected_element ) {data_net_prod <- data_prod(domain_prod,dataset_prod, seed_feed = T)}


# Removing leading zeroes
vop_country[, geographicAreaM49 := sub( "^0+","", geographicAreaM49 )]
vop_country_group[, geographicAreaM49 := sub( "^0+","", geographicAreaM49 )]

# Get prices data

if (param_source_prices == "prices"){ 
  
  domain_prices = "prod_prices"
  dataset_prices = "annual_producer_prices_validation"
  
} else if (param_source_prices == "diss"){
  
  domain_prices = "disseminated"      #need data on disseminated domain
  dataset_prices = "annual_producer_prices_validation_diss"
}


data_price = list()

#International Dollar
if ("152" %in% selected_element) {
  #Gross production International dollar
  
  vop_international_dollar <- ReadDatatable("supplementary_international_dollar", columns = c("item_code", "year", "value"))
  setnames(vop_international_dollar, c("item_code", "year", "value"), c("measuredItemCPC", "timePointYears", "Prices"))
  
  vop_international_dollar <- vop_international_dollar[timePointYears %in% param_base_year]
  vop_international_dollar[, measuredItemCPC := str_pad(measuredItemCPC,4, pad="0")] #pad for fcl2cpc function
  vop_international_dollar[, measuredItemCPC := fcl2cpc(measuredItemCPC)] #convert from fao code to cpc
  
  International_dollar <- merge(data_production, vop_international_dollar, by = 'measuredItemCPC', all.x = TRUE)
  International_dollar[,timePointYears.y := NULL]
  setnames(International_dollar, "timePointYears.x" , "timePointYears" ) #change names
  International_dollar[, Prices := as.numeric(Prices)]
  
  International_dollar[, measuredElement := "152"] 
  
  data_price[["152"]] <- International_dollar #Temporary FAOSTAT Code
  rm(International_dollar) 
  
} 

#International Dollar - Net Prod
if ("154" %in% selected_element) {
  #Net production International dollar
  
  vop_international_dollar <- ReadDatatable("supplementary_international_dollar", columns = c("item_code", "year", "value"))
  setnames(vop_international_dollar, c("item_code", "year", "value"), c("measuredItemCPC", "timePointYears", "Prices"))
  
  vop_international_dollar <- vop_international_dollar[timePointYears %in% param_base_year]
  vop_international_dollar[, measuredItemCPC := str_pad(measuredItemCPC,4, pad="0")] #pad for fcl2cpc function
  vop_international_dollar[, measuredItemCPC := fcl2cpc(measuredItemCPC)] #convert from fao code to cpc
  
  data_net_prod <- merge(data_net_prod, vop_international_dollar, by = 'measuredItemCPC', all.x = TRUE)
  data_net_prod[,timePointYears.y := NULL]
  setnames(data_net_prod, "timePointYears.x" , "timePointYears" ) #change names
  data_net_prod[, Prices := as.numeric(Prices)]
  
  data_net_prod[, measuredElement := "154"] 
  
  prod_minus_seed_feed <- copy(data_net_prod)
  prod_minus_seed_feed[, Production := Production - Seed - Feed]
  prod_minus_seed_feed[ , c('Seed', 'Feed'):= NULL ]
  
  data_price[["154"]] <- prod_minus_seed_feed #Temporary FAOSTAT Code
  rm(prod_minus_seed_feed) 
  
} 


#Standard Local Currency
if ("56" %in% selected_element) {
  
  data_SLC <- data_prices('5531', domain_prices, dataset_prices)
  data_SLC <- merge(data_production, data_SLC, by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"), all.x = TRUE)
  data_SLC[, measuredElement := "56"] 
  
  data_price[["56"]] <- data_SLC 
  rm(data_SLC)
  
}

#Average Standard Local Currency
if ("55" %in% selected_element) {
  
  data_SLC.avg <- data_prices('5534', domain_prices,dataset_prices)
  data_SLC.avg[, timePointYears := NULL]
  data_SLC.avg <- merge(data_production, data_SLC.avg, by = c("geographicAreaM49", "measuredItemCPC"), all.x = TRUE)
  data_SLC.avg[, measuredElement := "55"] 
  
  data_price[["55"]] <- data_SLC.avg 
  rm(data_SLC.avg)
  
}

#US Dollar
if ("57" %in% selected_element) {
  
  data_USD <- data_prices("5532",domain_prices,dataset_prices)
  data_USD <- merge(data_production, data_USD, by = c("geographicAreaM49", "measuredItemCPC", "timePointYears"), all.x = TRUE)
  data_USD[, measuredElement := "57"]  #Temporary FAOSTAT Code
  
  data_price[["57"]] <- data_USD 
  rm(data_USD)
  
}

#Average US Dollar
if ("58" %in% selected_element) {
  
  data_SLC.avg <- data_prices('5535', domain_prices,dataset_prices)
  data_SLC.avg[, timePointYears := NULL]
  data_SLC.avg <- merge(data_production, data_SLC.avg, by = c("geographicAreaM49", "measuredItemCPC"), all.x = TRUE)
  data_SLC.avg[, measuredElement := "58"] 
  
  data_price[["58"]] <- data_SLC.avg 
  rm(data_SLC.avg)
  
}


## Calculation -------------------------------------------------------------

gross_production_value <- do.call("rbind", data_price)

message(paste('Your Value of Agricultural Production Plugin is calculating Gross Production Value.'))
  
gross_production_value[ , Value := Production*Prices /1000]
gross_production_value[,  c('Production', 'Prices') := NULL]
  



#### Aggregate for items groups ####
if (param_item_aggr != "item_single" ) {
  
message(paste('Your Value of Agricultural Production Plugin is calculating item-aggregated Gross Production Value.'))

  if ( "154" %in% selected_element ) { 
  # item aggregate for net Production
  
    
    gross_production_value_item_aggregate <- merge(gross_production_value, vop_item_group, by = "measuredItemCPC", allow.cartesian = TRUE)
    gross_production_value_item_aggregate <- gross_production_value_item_aggregate[ measuredElement != "154"] #aggregate for other element
    gross_production_value_item_aggregate <- gross_production_value_item_aggregate[, list(Value = sum(Value, na.rm = TRUE)),
                                                                                   by = c('measuredElement','geographicAreaM49', 'timePointYears', 'item_group_code')]
    
    
    data_net_prod <- merge(data_net_prod, vop_item_group, by = "measuredItemCPC", allow.cartesian = TRUE)
    
    # Group aggregates Cereal, Total (1717), Roots and Tubers, Total (1720), Oilcrops Primary (1730), Crops (PIN) (2041), Sugar Crops Primary (1723), Vegetables and Fruit Primary (1739), Fruit Primary (1738), Vegetables Primary (1735) only subtract seed. 
    
    data_net_prod[item_group_code %in% c('F1717',
                                         'F2041',
                                         'F1738',
                                         'F1730',
                                         'F1720',
                                         'F1723',
                                         'F1739',
                                         'F1735'), Value := (Production - Seed)*Prices/1000 ]
    
    #Meat indigenous, total (1770) and Milk, total (1780) only subtract feed. 
    
    data_net_prod[item_group_code %in% c('F1770',
                                         'F1780'), Value := (Production - Feed)*Prices/1000 ]
    
    #For the other group aggregates Livestock (PIN) (2044), Agriculture (PIN) (2051), Food (PIN) (2054) and Non Food (PIN) (2057), both seed and feed are subtracted from production quantity. 
    
    data_net_prod[item_group_code %in% c('F2051',
                                         'F1753',
                                         'F2054',
                                         'F2044',
                                         'F2057'), Value := (Production - Seed - Feed)*Prices/1000 ]
    
    data_net_prod[, c("Production","Seed","Feed","Prices"):= NULL]
                                         
    data_net_prod <- data_net_prod[, list(Value = sum(Value, na.rm = TRUE)),
                                   by = c('measuredElement','geographicAreaM49', 'timePointYears', 'item_group_code')]
    
   gross_production_value_item_aggregate <- rbind(gross_production_value_item_aggregate, data_net_prod)                                      
    
    
  } else { 
      
  gross_production_value_item_aggregate <- merge(gross_production_value, vop_item_group, by = "measuredItemCPC", allow.cartesian = TRUE)
  gross_production_value_item_aggregate <- gross_production_value_item_aggregate[, list(Value = sum(Value, na.rm = TRUE)),
                                                                                 by = c('measuredElement','geographicAreaM49', 'timePointYears', 'item_group_code')]
    }

}

#### Aggregate for Countries groups ####

#### Country Aggregate Fix ############################################################################
# Fixing country aggregate parameter to Single item since country aggregates cannot be saved in SWS ###
#######################################################################################################

if(CHINA_FIX == T){param_country_aggr <- "country_single"}  # Remove to calculate country aggregates

#######################################################################################################

if (param_country_aggr != "country_single" ) {

message(paste('Your Value of Agricultural Production Plugin is calculating country-aggregated Gross Production Value.'))

gross_production_value_country_aggregate <- merge(gross_production_value, vop_country_group, by = "geographicAreaM49", allow.cartesian = TRUE)

discard_countries_year <- date_to_discard_list(vop_country) #create a list with all the country-years to remove: before startdate, after enddate

gross_production_value_country_aggregate <- merge(gross_production_value_country_aggregate,
                                                         discard_countries_year,
                                                         by = c("geographicAreaM49", "timePointYears"), all.x = T)

gross_production_value_country_aggregate <- gross_production_value_country_aggregate[ is.na(remove)] # adjust the groups for each years
gross_production_value_country_aggregate[, remove := NULL]

gross_production_value_country_aggregate <- gross_production_value_country_aggregate[, list(Value = sum(Value, na.rm = TRUE)),
                                                                by = c("measuredElement", 'country_group_code', 'timePointYears', 'measuredItemCPC')]


#### Aggregate for Item groups - Countries groups ####

gross_production_value_country_item_aggregate <- merge(gross_production_value_item_aggregate, vop_country_group, by = "geographicAreaM49", allow.cartesian = TRUE)


gross_production_value_country_item_aggregate <- merge(gross_production_value_country_item_aggregate,
                                                  discard_countries_year,
                                                  by = c("geographicAreaM49", "timePointYears"), all.x = T)

gross_production_value_country_item_aggregate <- gross_production_value_country_item_aggregate[ is.na(remove)] # adjust the groups for each years
gross_production_value_country_item_aggregate[, remove := NULL]

gross_production_value_country_item_aggregate <- gross_production_value_country_item_aggregate[, list(Value = sum(Value, na.rm = TRUE)),
                                                                                     by = c("measuredElement", 'country_group_code', 'timePointYears', 'item_group_code')]


setnames(gross_production_value_country_aggregate, 'country_group_code', "geographicAreaM49")
setnames(gross_production_value_country_item_aggregate, c('item_group_code','country_group_code'), c("measuredItemCPC", "geographicAreaM49"))
}#end Country aggregate

### setting names for aggregates

if (param_item_aggr != "item_single" ) {
setnames(gross_production_value_item_aggregate, 'item_group_code', "measuredItemCPC")
}




## Saving data

message(paste0("Your Value of Agricultural Production Plugin is saving data."))


if ( param_item_aggr == "item_single" & param_country_aggr == "country_single") {
  save_data = gross_production_value[, c('flagObservationStatus','flagMethod') := .("E", "i")  ]

} else if ( param_item_aggr == "item_single" & param_country_aggr == "country_aggr") {
  save_data = gross_production_value_country_aggregate[, c('flagObservationStatus','flagMethod') := .("E", "i")  ]
  
} else if ( param_item_aggr == "item_single" & param_country_aggr == "country_all") {
  save_data = rbind(gross_production_value, gross_production_value_country_aggregate)
  save_data = save_data[, c('flagObservationStatus','flagMethod') := .("E", "i")  ]
   
} else if ( param_item_aggr == "item_aggr" & param_country_aggr == "country_single") {
  save_data = gross_production_value_item_aggregate[, c('flagObservationStatus','flagMethod') := .("E", "i")  ]
  
} else if ( param_item_aggr == "item_aggr" & param_country_aggr == "country_aggr") {
  save_data = gross_production_value_country_item_aggregate[, c('flagObservationStatus','flagMethod') := .("E", "i")  ]
  
} else if ( param_item_aggr == "item_aggr" & param_country_aggr == "country_all") {
  save_data = rbind(gross_production_value_item_aggregate, gross_production_value_country_item_aggregate)
  save_data = save_data[, c('flagObservationStatus','flagMethod') := .("E", "i")  ]
  
} else if ( param_item_aggr == "item_all" & param_country_aggr == "country_single") {
  save_data = rbind(gross_production_value, gross_production_value_item_aggregate)
  save_data = save_data[, c('flagObservationStatus','flagMethod') := .("E", "i")  ]
  
} else if ( param_item_aggr == "item_all" & param_country_aggr == "country_aggr") {
  save_data = rbind(gross_production_value_country_aggregate, gross_production_value_country_item_aggregate)
  save_data = save_data[, c('flagObservationStatus','flagMethod') := .("E", "i")  ]
  
} else if ( param_item_aggr == "item_all" & param_country_aggr == "country_all") {
  save_data = rbind(gross_production_value,gross_production_value_country_aggregate,gross_production_value_item_aggregate,gross_production_value_country_item_aggregate)
  save_data = save_data[, c('flagObservationStatus','flagMethod') := .("E", "i")  ]
  
}
  

save_data <- save_data[ !is.na(Value)]

## adding base year as metadata

config <- GetDatasetConfig(swsContext.datasets[[1]]@domain, swsContext.datasets[[1]]@dataset)
metadata <- save_data[, mget(config$dimensions)]

metadata[measuredElement %in% c('55','57','58','152','154'), `:=`(Metadata = "GENERAL",
                                         Metadata_Element = "COMMENT",
                                         Metadata_Language = "en",
                                         Metadata_Value = paste0("Base year: ",param_base_year))]
                           

save <- SaveData(domain = domain_, dataset = dataset_,
                 data = save_data, metadata = metadata, waitTimeout = 100000)

paste0("Your Value of Agricultural Production Plugin is completed successfully! ",
       save$inserted, " observations written, ",
       save$ignored, " weren't updated, ",
       save$discarded, " had problems.")
  