#single item - no aggregation

faostat <- NULL
result_compare <- NULL

faostat <- fread('~/Agriculture Production/module/Value of Agricultural Production/FAOSTAT/FAOSTAT_current_SCL.csv')
head(faostat)
dim(faostat)
faostat <- faostat[, c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'), with = FALSE]
setnames(faostat, old = c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'),
         new = c('geographicAreaM49', 'geographicAreaM49_description',
                 'measuredItemCPC', 'measuredItemCPC_description', 
                 'timePointYears', 'faostat'))

result_compare <- copy(gross_production_value)

setnames(result_compare, "Value", "plugin_results")
result_compare[ ,c('flagObservationStatus', 'flagMethod') := NULL]
head(result_compare)


compare <- merge(faostat, result_compare, by= c('geographicAreaM49',
                                               'timePointYears',
                                               'measuredItemCPC'))
compare <- as.data.table(compare)
compare[, faostat := as.numeric(faostat) ]

compare[, diff :=  round(faostat - plugin_results)]
View(compare)
View(compare[ diff > 1000  & diff != 0])

#item aggregate

faostat <- NULL
result_compare <- NULL

faostat <- fread('~/Agriculture Production/module/Value of Agricultural Production/FAOSTAT/FAOSTAT_current_SLC_iaggr.csv')
head(faostat)
dim(faostat)
faostat <- faostat[, c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'), with = FALSE]
setnames(faostat, old = c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'),
         new = c('geographicAreaM49', 'geographicAreaM49_description',
                 'measuredItemCPC', 'measuredItemCPC_description', 
                 'timePointYears', 'faostat'))

setnames(result_compare, "Value", "plugin_results")
setnames(result_compare, "item_group_code", "measuredItemCPC")

result_compare[ ,c('flagObservationStatus', 'flagMethod') := NULL]
head(result_compare)

compare_item_aggr <- merge(faostat, result_compare, by= c('geographicAreaM49',
                                               'timePointYears',
                                               'measuredItemCPC'))
compare_item_aggr <- as.data.table(compare_item_aggr)
compare_item_aggr[, faostat := as.numeric(faostat) ]

compare_item_aggr[, diff :=  round(faostat - plugin_results)]
View(compare_item_aggr[ diff != 0])


