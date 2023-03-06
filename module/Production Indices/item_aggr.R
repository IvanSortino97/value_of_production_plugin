#if ( "432" %in% selected_element){
 
  item_aggr.432.avg <- merge(item_aggr.432.avg, vop_item_group, by = "measuredItemCPC", allow.cartesian = TRUE )
  item_aggr.432.avg<-item_aggr.432.avg[, list(Mean = sum(Mean, na.rm = TRUE)),by = c('geographicAreaM49', 'item_group_code')]

  item_aggr.432 <- merge(Gross_prod.432, vop_item_group, by = "measuredItemCPC", allow.cartesian = TRUE )
  item_aggr.432 <- item_aggr.432[, list(Value = sum(Value, na.rm = TRUE)),by = c('measuredElement','geographicAreaM49', 'timePointYears', 'item_group_code') ]
  
  item_aggr.432 <- merge(item_aggr.432,item_aggr.432.avg,by=c("geographicAreaM49","item_group_code"))
  item_aggr.432[, Value := Value/Mean * 100]
  item_aggr.432[, Mean := NULL]
  
  