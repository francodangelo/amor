get_i1 = function(rand_int, treat, predictors)
{
  v = c() # Define blank vector
  v = c(paste(c("I(",treat,")"), collapse = ""))
  while (length(v) <= rand_int)
  {
    c = paste(c("I(",sample(predictors, size = 1),")"), collapse = "")
    if (c %in% v) { v = v} else { v = c(v, c)}
  }
  
  return(paste(v, collapse = " + "))
}