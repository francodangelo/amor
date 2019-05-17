get_i2 = function(rand_int, treat, predictors)
{
  v = c()
  v = c(paste(c("I(",treat,")"), collapse = ""))
  while (length(v) <= rand_int)
  {
    # Create inter variable to randomly have an interaction or single term
    inter = sample(c(1, 2), size = 1)
    
    if (inter == 1)
    {
      c = paste(c("I(",sample(predictors, size = 1),")"), collapse = "")
      
      if (c %in% v) { v = v} else { v = c(v, c)}
    }
    else
    {
      variables = sample(predictors, size = 2)
      a = paste(c("I(",paste(variables, collapse = "*"),")"), collapse = "")
      b = paste(c("I(",paste(rev(variables), collapse = "*"),")"), collapse = "")
      
      # Create formula
      if ((a %in% v) | (b %in% v))
      { v = v } else { v = c(v, a)} # Add I() v = c(v, paste(c("I(",a,")"), collapse = ""))
    }
  }
  
  return(paste(v, collapse = " + "))
}