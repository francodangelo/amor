#' Ordinary Least Squares Model Dependence Simulations
#'
#' Takes in a data set and outputs a data.frame of the estimated average treatment effects.
#' @param data Data set to perform the analysis.
#' @param dep The dependent outcome binary variable that will be used to measure the effect (e.g. "success").
#' @param treat The treatment variable (e.g. "treatment").
#' @param predictors Vector of predictor variables.
#' @param level Level of allowed interactions. Level = (1, 2, 3) and correspond to one-way, two-way and three-way interactions respectively.
#' @param n.sims Number of desired simulated models.
#' @param matched.data Additional data set of pre-processed data to compare the same random models on both data sets.
#' @return Returns a data.frame composed by the list of estimated treatment effects and the model used in each iteration. In the case of including an additional data set, the data.frame will have to columns with the respective estimates.
#' @importFrom "stats" "density" "lm" "predict"
#' @export
#' @examples \dontrun{
#' data(lalonde)
#' p <- c("treat", "age", "black", "hisp", "re75", "married", "educ")
#'
#' olsmd(lalonde, "re78", "treat", predictors=p level=2, method="odds")}
olsmd = function(data, dep, treat, predictors, level, n.sims, matched.data = NULL)
{
  if (missing(matched.data))
  {
    # Define blank vector for estimates
    Estimated_Effect = c()
    Model = c()
    # Define inputs data
    d.treated = data
    d.control = data
    d.treated[treat] = rep(1, length(data[treat]))
    d.control[treat] = rep(0, length(data[treat]))
    # Initiate iterations to 0
    i = 0
    # Iterate over number of simulations
    while (i <= n.sims)
    {
      rand_int = sample(length(predictors)-1, size=1)
      if (level == 1)
      {
        p = get_i1(rand_int, treat, predictors)
      }
      else if (level == 2)
      {
        p = get_i2(rand_int, treat, predictors)
      }
      else if(level == 3)
      {
        p = get_i3(rand_int, treat, predictors)
      }
      else
      {
        print("Level specification should be between 1 and 3.")
        break
      }
      # Generate formula
      f = paste(dep, p, sep = " ~ ")
      # Check for warnings in lm()
      modelWarning = tryCatch(lm(formula = f, data = data), warning = function(w) w)
      if (inherits(modelWarning, "warning")) next
      # Generate the model
      m = lm(formula = f, data = data)
      # Check for warnings in predict()
      predictWarning = tryCatch(predict(m, newdata = d.treated), warning = function(w) w)
      if (inherits(predictWarning, "warning")) next
      # Estimate the treatment effect
      Estimated_Effect[i] = mean(predict(m, newdata = d.treated) - predict(m, newdata = d.control))
      # Store model
      Model[i] = f
      # Continue to the next iteration
      i = i + 1
    }
    return(data.frame(Estimated_Effect, Model))
  }
  else
  {
    # Define blank vector for estimates
    Original_Effect = c()
    Matched_Effect = c()
    Model = c()
    # Define inputs OriginalData
    OriginalData = data
    d.treated = OriginalData
    d.control = OriginalData
    d.treated[treat] = rep(1, length(OriginalData[treat]))
    d.control[treat] = rep(0, length(OriginalData[treat]))
    # Define inputs MatchedData
    MatchedData = matched.data
    m.treated = MatchedData
    m.control = MatchedData
    m.treated[treat] = rep(1, length(MatchedData[treat]))
    m.control[treat] = rep(0, length(MatchedData[treat]))
    # Initiate iterations to 0
    i = 0
    # Iterate over number of simulations
    while (i <= n.sims)
    {
      rand_int = sample(length(predictors)-1, size=1)
      if (level == 1)
      {
        p = get_i1(rand_int, treat, predictors)
      }
      else if (level == 2)
      {
        p = get_i2(rand_int, treat, predictors)
      }
      else if(level == 3)
      {
        p = get_i3(rand_int, treat, predictors)
      }
      else {print("Method specification should be between 1 and 3.");break}
      # Generate formula
      f = paste(dep, p, sep = " ~ ")
      # Check for warnings in lm()
      modelWarningOD = tryCatch(lm(formula = f, data = OriginalData), warning = function(w) w)
      modelWarningMD = tryCatch(lm(formula = f, data = MatchedData), warning = function(w) w)
      if (inherits(modelWarningOD, "warning")) next
      if (inherits(modelWarningMD, "warning")) next
      # Generate the models
      dm = lm(formula = f, data = OriginalData)
      mm = lm(formula = f, data = MatchedData)
      # Check for warnings in predict()
      predictWarningOD = tryCatch(predict(dm, newdata = d.treated), warning = function(w) w)
      predictWarningMD = tryCatch(predict(mm, newdata = m.treated), warning = function(w) w)
      if (inherits(predictWarningOD, "warning")) next
      if (inherits(predictWarningMD, "warning")) next
      # Estimate the treatment effects
      Original_Effect[i] = mean(predict(dm, newdata = d.treated)) - mean(predict(dm, newdata = d.control))
      Matched_Effect[i] = mean(predict(mm, newdata = m.treated)) - mean(predict(mm, newdata = m.control))
      # Store model
      Model[i] = f
      # Continue to the next iteration
      i = i + 1
    }
    return(data.frame(Original_Effect, Matched_Effect, Model))
  }
}
