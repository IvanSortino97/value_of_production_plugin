#' This script compares the results of the plugin with FAOSTAT disseminated data
#' for Value of Agriculture Production.

# Loading libraries -------------------------------------------------------

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

param_year =  eval(parse(text = swsContext.computationParams$base_year ))  
param_aggregation = swsContext.computationParams$item_aggregate

selected_countries = swsContext.datasets[[1]]@dimensions[["geographicAreaM49"]]@keys


faostat <- fread('module/Gross Production Value/faostat/FAOSTAT_data_en_12-29-2022_Italy_2015_20.csv')
head(faostat)
dim(faostat)
faostat <- faostat[, c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'), with = FALSE]
setnames(faostat, old = c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'),
         new = c('geographicAreaM49', 'geographicAreaM49_description',
                 'measuredItemCPC', 'measuredItemCPC_description', 
                 'timePointYears', 'faostat'))

## read data produced by the plugin

country_item <- fread('module/Gross Production Value/outcome_plugin/italy_afg_single_items.csv')
country_item[, timePointYears_description := NULL]
setcolorder(country_item, c('geographicAreaM49', 'geographicAreaM49_description',
                            'measuredItemCPC', 'measuredItemCPC_description', 
                            'timePointYears', 'Value'))


country_item_group <- fread('module/Gross Production Value/outcome_plugin/italy_afg_item_groups.csv')
country_item_group[, c('flagObservationStatus', 'flagMethod','measuredElement', 
                       'measuredElement_description', 'timePointYears_description') := NULL]

setcolorder(country_item_group, c('geographicAreaM49', 'geographicAreaM49_description',
                            'measuredItemCPC', 'measuredItemCPC_description', 
                            'timePointYears', 'Value'))


results_plugin <- rbind(country_item, country_item_group)
setnames(results_plugin, 'Value', 'sws')
results_plugin <- results_plugin[geographicAreaM49 == 380]

head(results_plugin)
dim(results_plugin)

# Merge results_plugin with faostat data

sapply(faostat, class)
sapply(results_plugin, class)

data_merged <- merge(faostat, 
      results_plugin[, c('geographicAreaM49', 'measuredItemCPC','timePointYears', 'sws'), with = FALSE],
      by = c('geographicAreaM49', 'measuredItemCPC', 'timePointYears'), all.x = TRUE)

head(data_merged)
sapply(data_merged, class)

data_merged[, faostat := as.numeric(faostat)]

data_merged[, diff := round(faostat - sws)]

data_merged[diff != 0 & timePointYears == 2015] 


data_merged[is.na(sws) & !is.na(faostat) & timePointYears == 2015, sum(faostat)] 

data_merged[is.na(sws) & !is.na(faostat) & timePointYears == 2015] 

items_to_check <- data_merged[is.na(sws) & !is.na(faostat) & timePointYears == 2015, measuredItemCPC] 
items_to_check

## check if those codes exist in the production datatable and in the international dollar price

vop_production <- ReadDatatable("qcl_faostat_production_data", 
                                columns = c("area_code_m49", "element_code", "item_code", "year", "value"),
                                where = paste0("element_code = '5510' AND area_code_m49 IN (", paste(shQuote(selected_countries), collapse=", "), ")"))

# Removing leading zeroes
vop_production[, area_code_m49 := sub( "^0+","", area_code_m49 )]

# the codes are missing in the datatable but they do exist in faostat
vop_production[area_code_m49 == 380 & item_code %in% items_to_check]
vop_production[area_code_m49 == 380 & item_code %in% c('21111.01i', '21112i')]

## international dollar

vop_international_dollar <- ReadDatatable("supplementary_international_dollar")
vop_international_dollar[, item_code := str_pad(item_code, 4, pad="0")] #pad for fcl2cpc function
vop_international_dollar[, item_code := fcl2cpc(item_code)] #convert from fao code to cpc

head(vop_international_dollar)
vop_international_dollar[item_code == '0111']
vop_international_dollar[item_code %in% items_to_check & year == 2015]
