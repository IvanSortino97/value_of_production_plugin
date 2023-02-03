#single item - no aggregation

faostat <- NULL
result_compare <- NULL

faostat <- fread('module/Gross Production Value/faostat/FAOSTAT_SCL_current_single_item.csv')
head(faostat)
dim(faostat)
faostat <- faostat[, c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'), with = FALSE]
setnames(faostat, old = c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'),
         new = c('geographicAreaM49', 'geographicAreaM49_description',
                 'measuredItemCPC', 'measuredItemCPC_description', 
                 'timePointYears', 'faostat'))

Y= unique(faostat$timePointYears)
I = unique(faostat$measuredItemCPC)

result_compare <- gross_production_value

colnames(result_compare)<-c('geographicAreaM49',
                            'measuredItemCPC','timePointYears', 'plugin_results')
head(result_compare)


compare <- merge(faostat, result_compare, by= c('geographicAreaM49',
                                               'timePointYears',
                                               'measuredItemCPC'))
compare <- as.data.table(compare)
compare[, faostat := as.numeric(faostat) ]

compare[, diff :=  round(faostat - plugin_results)]
View(compare[ diff != 0])

#item aggregate

faostat <- NULL
result_compare <- NULL

faostat <- fread('module/Gross Production Value/faostat/FAOSTAT_SLC_item_aggr.csv')
head(faostat)
dim(faostat)
faostat <- faostat[, c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'), with = FALSE]
setnames(faostat, old = c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'),
         new = c('geographicAreaM49', 'geographicAreaM49_description',
                 'measuredItemCPC', 'measuredItemCPC_description', 
                 'timePointYears', 'faostat'))

result_compare <- gross_production_value_item_aggregate
colnames(result_compare)<-c('geographicAreaM49','timePointYears',
                            'measuredItemCPC', 'plugin_results')

head(result_compare)

compare_item_aggr <- merge(faostat, result_compare, by= c('geographicAreaM49',
                                               'timePointYears',
                                               'measuredItemCPC'))
compare_item_aggr <- as.data.table(compare_item_aggr)
compare_item_aggr[, faostat := as.numeric(faostat) ]

compare_item_aggr[, diff :=  round(faostat - plugin_results)]
View(compare_item_aggr[ diff != 0])


