get_i3 = function(rand_int, treat, predictors)
{
  v = c()
  v = c(paste(c("I(",treat,")"), collapse = ""))
  while (length(v) <= rand_int)
  {
    # Create inter variable to randomly have an interaction or single term
    inter = sample(c(1, 2, 3), size = 1)
    
    if (inter == 1)
    {
      c = paste(c("I(",sample(predictors, size = 1),")"), collapse = "")
      
      if (c %in% v) { v = v} else { v = c(v, c)}
    }
    else if (inter == 2)
    {
      variables = sample(predictors, size = 2)
      a = paste(c("I(",paste(variables, collapse = "*"),")"), collapse = "")
      b = paste(c("I(",paste(rev(variables), collapse = "*"),")"), collapse = "")
      
      # Create formula
      if ((a %in% v) | (b %in% v))
      { v = v } else { v = c(v, a)} # Add I() v = c(v, paste(c("I(",a,")"), collapse = ""))
    }
    else 
    {
      variables = sample(predictors, size = 3)
      
      abc = paste(c("I(",paste(c(variables[1], variables[2], variables[3]), collapse = "*"),")"), collapse = "")
      acb = paste(c("I(",paste(c(variables[1], variables[3], variables[2]), collapse = "*"),")"), collapse = "")
      
      bac = paste(c("I(",paste(c(variables[2], variables[1], variables[3]), collapse = "*"),")"), collapse = "")
      bca = paste(c("I(",paste(c(variables[2], variables[3], variables[1]), collapse = "*"),")"), collapse = "")
      
      cab = paste(c("I(",paste(c(variables[3], variables[1], variables[2]), collapse = "*"),")"), collapse = "")
      cba = paste(c("I(",paste(c(variables[3], variables[2], variables[1]), collapse = "*"),")"), collapse = "")
      # Create formula
      if ((abc %in% v) | (acb %in% v) | (bac %in% v) | (bca %in% v) | (cab %in% v) | (cba %in% v))
      { v = v } else { v = c(v, abc)}
    }
  }
  
  return(paste(v, collapse = " + "))
}