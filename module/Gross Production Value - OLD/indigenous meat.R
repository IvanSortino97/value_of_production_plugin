vop_indigenous <- ReadDatatable("value_of_production_indigenous_meat",
                                columns = c("description","cpc_code"))

vop_indigenous$cpc_code <- gsub("i", "", vop_indigenous$cpc_code)

domain_ind = 'agriculture'
dataset_ind = 'aproduction'

keys_geo = sub("^0+","", as.character(vop_country$m49_code)) #select countries of "aproduction_country" datatable, remove all the 0 before the code
keys_elem = "55100" # ind. production [t]
keys_item = as.character(vop_indigenous$cpc_code)
keys_year = as.character(unique(commodities_production$year))

production_ind_keys <- DatasetKey(
  domain = domain_ind,
  dataset =  dataset_ind,
  dimensions = list(
    Dimension(name = grep('geo', GetDatasetConfig(domain_ind, dataset_ind)$dimensions, value = T, ignore.case = T),
              keys = keys_geo ),
    
    Dimension(name = grep('elem', GetDatasetConfig(domain_ind, dataset_ind)$dimensions, value = T, ignore.case = T),
              keys = keys_elem ),
    
    Dimension(name = grep('item', GetDatasetConfig(domain_ind, dataset_ind)$dimensions, value = T, ignore.case = T),
              keys = keys_item ),
    
    Dimension(name = grep('year', GetDatasetConfig(domain_ind, dataset_ind)$dimensions, value = T, ignore.case = T),
              keys = keys_year )
  )
  
)

# Get data ----------------------------------------------------------------

system.time(data_production_ind <- GetData(production_ind_keys))
data_production_ind[,c("measuredElement", "flagObservationStatus" ,"flagMethod"):= NULL]
data_production_ind$measuredItemCPC = paste0(data_production_ind$measuredItemCPC, "i")


