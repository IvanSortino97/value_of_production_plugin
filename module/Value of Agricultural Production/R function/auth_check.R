Rights_check<-function(){

auth_check <- function(domain,dataset,p){
  
geo = grep('geo', GetDatasetConfig(domain, dataset)$dimensions, value = T, ignore.case = T)
elem = grep('elem', GetDatasetConfig(domain, dataset)$dimensions, value = T, ignore.case = T)
item = grep('item', GetDatasetConfig(domain, dataset)$dimensions, value = T, ignore.case = T)
year = grep('year', GetDatasetConfig(domain, dataset)$dimensions, value = T, ignore.case = T)
  
 


  production_keys <- DatasetKey(
    domain = domain,
    dataset =  dataset,
    dimensions = list(
      Dimension(name = geo,
                keys = "380" ),
      Dimension(name = elem,
                keys = if (p == "pd"){"5510"} else {"5531"}   ),
      Dimension(name = item,
                keys =  c("0111","02211")),
      Dimension(name = year,
                keys =  "2015")))
  
  GetData(production_keys)
}


tryCatch({

  if (param_source_prod == "production"){
    
    auth_check("agriculture", "aproduction", "pd")

  } else if (param_source_prod == "disseminated"){

    auth_check("disseminated", "livestock_production", "pd")
    auth_check("disseminated", "crops_production", "pd")
    
  }

  if (param_source_prices == "prices"){

    auth_check("prod_prices","annual_producer_prices_validation", "pp")

  } else if (param_source_prices == "diss"){

    auth_check("disseminated","annual_producer_prices_validation_diss", "pp")
  }

  message("User authorized for datasets")

},error=function(e) {
  #message('An Error Occurred')
  message(print(e))
  stop()
})

}
