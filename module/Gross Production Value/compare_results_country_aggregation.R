faostat <- fread('module/Gross Production Value/faostat/FAOSTAT_country_aggregate.csv')
head(faostat)
dim(faostat)
faostat <- faostat[, c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'), with = FALSE]
setnames(faostat, old = c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'),
         new = c('geographicAreaM49', 'geographicAreaM49_description',
                 'measuredItemCPC', 'measuredItemCPC_description', 
                 'timePointYears', 'faostat'))

Y= unique(faostat$timePointYears)
I = unique(faostat$measuredItemCPC)

result_compare <- gross_production_value_country_aggregate_results 

setnames(result_compare, old = c("country_group","year","item_code","sum"    ),
         new = c('geographicAreaM49_description','timePointYears',
                 'measuredItemCPC', 'plugin_results'))

result_compare = result_compare[timePointYears == Y & measuredItemCPC == I]

setdiff(result_compare$geographicAreaM49_description, faostat$geographicAreaM49_description )

compare = merge(faostat, result_compare, by= c('geographicAreaM49_description',
                                               'timePointYears',
                                               'measuredItemCPC'))
compare = as.data.table(compare)
compare[, faostat := as.numeric(faostat) ]

compare[, diff :=  round(faostat - plugin_results)]
View(compare)
