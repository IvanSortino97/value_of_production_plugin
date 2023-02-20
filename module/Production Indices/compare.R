faostat = NULL
#faostat <- fread('~/Agriculture Production/module/Production Indices/FAOSTAT/FAOSTAT_aggr_total_PIN.csv')
faostat <- fread('~/Agriculture Production/module/Production Indices/FAOSTAT/FAOSTAT_afg_2018.csv')
head(faostat)
dim(faostat)
faostat <- faostat[, c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'), with = FALSE]
setnames(faostat, old = c('Area Code (M49)', 'Area', 'Item Code (CPC)', 'Item', 'Year', 'Value'),
         new = c('geographicAreaM49', 'geographicAreaM49_description',
                 'measuredItemCPC', 'measuredItemCPC_description',
                 'timePointYears', 'faostat'))

#result_compare <- copy(gross_production_value_item_aggregate)
result_compare <- copy(item_aggr.432.test)
result_compare <- result_compare[geographicAreaM49 == "4"]
result_compare <- result_compare[timePointYears == "2018"]

result_compare[ ,c('flagObservationStatus', 'flagMethod') := NULL]
head(result_compare)
compare <- merge(faostat, result_compare, by= c('geographicAreaM49',
                                                'measuredItemCPC'))
compare <- as.data.table(compare)
compare[, faostat := as.numeric(faostat) ]
 
compare[, diff :=  round(faostat - Final_value)]
compare <- compare[measuredItemCPC != 'F1735' & measuredItemCPC != 'F1738']

View(compare)
View(compare[measuredItemCPC == 'F2041'])
View(compare[ diff > 1000  & diff != 0])
