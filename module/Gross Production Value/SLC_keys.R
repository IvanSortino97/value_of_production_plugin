#SLC from disseminated dataset - SLC
# element 5531 [SLC/t]

#-------------------------------------------------------------------------------
.libPaths("/newhome/shared/Library/3.3.3/")
suppressMessages({
  library(faosws)
  library(faoswsUtil)
  library(faoswsFlag)
  library(data.table)
  library(stringr)
  library(dplyr)
})
if(CheckDebug()){
  
  library(faoswsModules)
  SETT <- ReadSettings("~/Agriculture Production/module/Gross Production Value/sws.yml")
  SetClientFiles(SETT[["certdir"]])
  GetTestEnvironment(baseUrl = SETT[["server"]], token = SETT[["token"]])
  source("~/Agriculture Production/module/Gross Production Value/R function/gap_filler.R")
  source("~/Agriculture Production/module/Gross Production Value/R function/discard_year_list.R")
  
}

#-------------------------------------------------------------------------------

faostat <- fread('module/Gross Production Value/faostat/FAOSTAT_SCL_current_single_item.csv')
faostat <- faostat[, c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'), with = FALSE]
setnames(faostat, old = c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'),
         new = c('geographicAreaM49', 'geographicAreaM49_description',
                 'measuredItemCPC', 'measuredItemCPC_description', 
                 'timePointYears', 'faostat'))


#-------------------------------------------------------------------------------

domain_slc = 'disseminated'
dataset_slc = 'annual_producer_prices_validation_diss'

keys_geo = as.character(vop_country$m49_code) 

keys_elem = "5531" #SLC

keys_item = as.character(vop_item$cpc_code)#select items of "value_of_production_item" datatable

keys_year = as.character(unique(commodities_production$year))

production_keys <- DatasetKey(
  domain = domain_slc,
  dataset =  dataset_slc,
  dimensions = list(
      Dimension(name = grep('geo',  GetDatasetConfig(domain_slc, dataset_slc)$dimensions, value = T, ignore.case = T),
                keys = keys_geo ),

      Dimension(name = grep('elem', GetDatasetConfig(domain_slc, dataset_slc)$dimensions, value = T, ignore.case = T),
                keys = keys_elem ),

      Dimension(name = grep('item', GetDatasetConfig(domain_slc, dataset_slc)$dimensions, value = T, ignore.case = T),
                keys = keys_item ),

      Dimension(name = grep('year', GetDatasetConfig(domain_slc, dataset_slc)$dimensions, value = T, ignore.case = T),
                keys = keys_year )
    )

)

# Get data ----------------------------------------------------------------

system.time(data_SLC <- GetData(production_keys))

#----------- TEMP TEST ---------------------------------------------------------

commodities_prices <- data_SLC[ timePointYears %in% param_year,]
commodities_prices <- setnames(commodities_prices, c("geographicAreaM49", "measuredItemCPC", "timePointYears", "Value") , c('area_code_m49', 'item_code', 'year', "SLC_prices_SWS")) #set names
commodities_prices[ , c('measuredElement', 'flagObservationStatus', 'flagMethod') := NULL]

vop_SLC_prices <- ReadDatatable("faostat_pp", columns =  c("area_code_m49","item", "item_code_cpc", "year", "value"))
vop_SLC_prices[, area_code_m49 := sub( "^0+","", area_code_m49 )]
commodities_prices_d <- vop_SLC_prices[ year %in% param_year,]
commodities_prices_d <- setnames(commodities_prices_d, c("item_code_cpc", "value") , c("item_code", "SLC_prices_datatable")) #set names


gross_production_value <- merge(commodities_production, commodities_prices, by = c('area_code_m49', 'item_code', 'year'), all.x = TRUE)
gross_production_value <- merge(gross_production_value, commodities_prices_d, by = c('area_code_m49', 'item_code', 'year'), all.x = TRUE)

# Calculate Gross production Value
gross_production_value[ , gross_prod_value_dt := as.numeric(SLC_prices_datatable) * as.numeric(production) /1000]
gross_production_value[ , gross_prod_value_SWS := as.numeric(SLC_prices_SWS) * as.numeric(production) /1000]


#faostat compare
{ 
faostat <- fread('module/Gross Production Value/faostat/FAOSTAT_SCL_current_single_item.csv')
faostat <- faostat[, c('Area Code (M49)',  'Item Code (CPC)',  'Year', 'Value'), with = FALSE]
setnames(faostat, old = c('Area Code (M49)',  'Item Code (CPC)',  'Year', 'Value'),
         new = c('area_code_m49', 'item_code', 'year', "faostat"))
}

gross_production_value <- merge(gross_production_value, faostat,  by = c('area_code_m49', 'item_code', 'year'), all.x = TRUE)
gross_production_value[, faostat := as.numeric(faostat)]
gross_production_value[, diff_dt := round(faostat - gross_prod_value_dt)]
gross_production_value[, diff_SWS := round(faostat - gross_prod_value_SWS)]
View(gross_production_value[diff_SWS != 0 ])
