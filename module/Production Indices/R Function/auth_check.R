Auth_check<-function(){

tryCatch({

  if (param_source_prod == "production"){ GetDatasetConfig("agriculture", "aproduction")

  } else if (param_source_prod == "disseminated"){

    GetDatasetConfig("disseminated", "livestock_production")
    GetDatasetConfig("disseminated", "crops_production")
    
  }

  if (param_source_prices == "prices"){
    
    GetDatasetConfig("prod_prices","annual_producer_prices_validation")

  } else if (param_source_prices == "diss"){

    GetDatasetConfig("disseminated","annual_producer_prices_validation_diss", "pp")
  }
  
  if ("434" %in% selected_element ){
    
    GetDatasetConfig("disseminated", "population_disseminated")
    
  }

  message("User authorized for datasets")

},error=function(e) {
  #message('An Error Occurred')
  message(print(e))
  stop()
})

}
