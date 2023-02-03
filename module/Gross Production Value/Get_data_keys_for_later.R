#Get production data for considered countries from aproduction dataset ------ FOR LATER

message(paste('Your Value of Agricultural Production Plugin is getting the keys.'))

domain_prod = 'agriculture'
dataset_prod = 'aproduction'

keys_geo = sub("^0+","", as.character(vop_country$m49_code)) #select countries of "aproduction_country" datatable, remove all the 0 before the code

keys_elem = "5510" #production [t]

keys_item = as.character(vop_item$cpc_code)#select items of "value_of_production_item" datatable

keys_year = as.character(swsContext.datasets[[1]]@dimensions$timePointYears@keys)#selected years of the session

production_keys <- DatasetKey(
  domain = domain_prod,
  dataset =  dataset_prod,
  dimensions = list(
      Dimension(name = grep('geo', GetDatasetConfig(domain_prod, dataset_prod)$dimensions, value = T, ignore.case = T),
                keys = keys_geo ),

      Dimension(name = grep('elem', GetDatasetConfig(domain_prod, dataset_prod)$dimensions, value = T, ignore.case = T),
                keys = keys_elem ),

      Dimension(name = grep('item', GetDatasetConfig(domain_prod, dataset_prod)$dimensions, value = T, ignore.case = T),
                keys = keys_item ),

      Dimension(name = grep('year', GetDatasetConfig(domain_prod, dataset_prod)$dimensions, value = T, ignore.case = T),
                keys = keys_year )
    )

)

# Get data ----------------------------------------------------------------

message(paste('Your Value of Agricultural Production Plugin is retrieving production data.'))


system.time(data_production <- GetData(production_keys))
