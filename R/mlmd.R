#' Maximum Likelihood Model Dependence Simulations
#'
#' Takes in a binary dependent data set and outputs a data.frame of the estimated average treatment effects.
#' @param data A binary dependent data set.
#' @param dep The dependent outcome binary variable that will be used to measure the effect (e.g. "success").
#' @param treat The treatment variable (e.g. "treatment").
#' @param predictors Vector of predictor variables.
#' @param level Level of allowed interactions. Level = (1, 2, 3) and correspond to one-way, two-way and three-way interactions respectively.
#' @param n.sims Number of desired simulated models.
#' @param method Format method to present results. The default is set to predictions of log-odds ="odds". For results on the scale of the predictors ="coeffs".
#' @param matched.data Additional data set of pre-processed data to compare the same random models on both data sets.
#' @return Returns a data.frame composed by the list of estimated treatment effects and the model used in each iteration. In the case of including an additional data set, the data.frame will have to columns with the respective estimates.
#' @importFrom "stats" "density" "glm" "predict"
#' @export
#' @examples \dontrun{
#' peacekeeping <- read.table("peacekeeping.csv")
#' p <- c("untype4", "wartype", "logcost", "wardur", "factnum", "trnsfcap", "decade")
#'
#' mlmd(peacekeeping, "pbs2s3", "untype4", predictors=p level=2, method="odds")}
mlmd = function(data, dep, treat, predictors, level, n.sims, method = "odds", matched.data = NULL)
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
      # Check for warnings in glm()
      modelWarning = tryCatch(glm(formula = f, family = 'binomial', data = data), warning = function(w) w)
      # Generate the model
      m = glm(formula = f, family = 'binomial', data = data)
      # Check for warnings in predict()
      predictWarning = tryCatch(predict(m, newdata = d.treated), warning = function(w) w)
      if (inherits(predictWarning, "warning")) next
      # Estimate the treatment effect
      if (method == "coeffs")
      {
        Estimated_Effect[i] = mean(predict(m, newdata = d.treated) - predict(m, newdata = d.control))
      }
      else if (method == "odds")
      {
        Estimated_Effect[i] =  mean(predict(m, newdata = d.treated, type = "response")) - mean(predict(m, newdata = d.control, type = "response"))
      }
      else {print("Method specification should be 'odds' or 'coeffs'.");break}
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
      # Check for warnings in glm()
      modelWarningOD = tryCatch(glm(formula = f, family = 'binomial', data = OriginalData), warning = function(w) w)
      modelWarningMD = tryCatch(glm(formula = f, family = 'binomial', data = MatchedData), warning = function(w) w)
      if (inherits(modelWarningOD, "warning")) next
      if (inherits(modelWarningMD, "warning")) next
      # Generate the models
      dm = glm(formula = f, family = 'binomial', data = OriginalData)
      mm = glm(formula = f, family = 'binomial', data = MatchedData)
      # Check for warnings in predict()
      predictWarningOD = tryCatch(predict(dm, newdata = d.treated), warning = function(w) w)
      predictWarningMD = tryCatch(predict(mm, newdata = m.treated), warning = function(w) w)
      if (inherits(predictWarningOD, "warning")) next
      if (inherits(predictWarningMD, "warning")) next
      # Estimate the treatment effects
      if (method == "coeffs")
      {
        Original_Effect[i] = mean(predict(dm, newdata = d.treated)) - mean(predict(dm, newdata = d.control))
        Matched_Effect[i] = mean(predict(mm, newdata = m.treated)) - mean(predict(mm, newdata = m.control))
      }
      else if (method == "odds")
      {
        Original_Effect[i] =  mean(predict(dm, newdata = d.treated, type = "response")) - mean(predict(dm, newdata = d.control, type = "response"))
        Matched_Effect[i] =  mean(predict(mm, newdata = m.treated, type = "response")) - mean(predict(mm, newdata = m.control, type = "response"))
      }
      else {print("Method specification should be 'odds' or 'coeffs'.");break}
      # Store model
      Model[i] = f
      # Continue to the next iteration
      i = i + 1
    }
    return(data.frame(Original_Effect, Matched_Effect, Model))
  }
}
