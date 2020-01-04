omit_missing_values = function(data,response){
  no.of.missing.values=sum(is.na(data$response));
  data = na.omit(data);
  if (no.of.missing.values == 0){
    print("Nothing to omit")
  }else{
    print("Number of missing values removed -",no.of.missing.values);
  }
  return(data)
}