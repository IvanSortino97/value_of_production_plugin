vop_seed_feed <- ReadDatatable("scl_seed_feed", 
                                columns = c("area_code_m49", "element_code", "item_code", "year", "value"),
                                where = paste0("element_code IN ('5525', '5520') AND area_code_m49 IN (", paste(shQuote(selected_countries), collapse=", "), ")"))


vop_seed_feed[, area_code_m49 := sub( "^0+","", area_code_m49 )]

vop_seed_feed <- dcast.data.table(
  vop_seed_feed,
  area_code_m49 + item_code + year ~ element_code,
  value.var = 'value',
  fill = 0
)

vop_seed_feed <- setnames(vop_seed_feed, c("5525", "5520" ), c("Seed", "Feed") )


# gross_production_value <- as.data.table(left_join(gross_production_value, vop_seed_feed,
#                                                   by = c("area_code_m49", "item_code", "year") )) #join
