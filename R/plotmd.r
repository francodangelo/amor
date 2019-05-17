#' Data Visualization Tool for Model Dependence Simulations
#'
#' A tool to turn the outputted data.frame from mlmd() and olsmd() into a density plot. The function automatically recognizes whether an additional data set (matched.data) was included when running mlmd() or olsmd() and genereates the plot accordingly.
#' @param data The data.frame returned by mlmd() or olsmd().
#' @return Returns a density plot of the estimates calculated by mlmd() or olsmd().
#' @importFrom "graphics" "legend" "lines" "plot"
#' @export
plotmd = function(data)
{
  if ("Estimated_Effect" %in% colnames(data))
  {
    final = data.frame(Original = data$Estimated_Effect)
    dens = apply(final, 2, density)
    plot(NA, ylab="Density", xlab="Estimated Effect", xlim=range(sapply(dens, "[", "x")), ylim=range(sapply(dens, "[", "y")))
    mapply(lines, dens, lty=c(1), lwd=1, col=c("black"))
  }
  else if ("Original_Effect" %in% colnames(data))
  {
    final = data.frame(Original = data$Original_Effect, Matched = data$Matched_Effect)
    dens = apply(final, 2, density)
    plot(NA, ylab="Density", xlab="Estimated Effect", xlim=range(sapply(dens, "[", "x")), ylim=range(sapply(dens, "[", "y")))
    mapply(lines, dens, lty=c(1, 2), lwd=1, col=c("black", "grey"))
    legend("topright", legend=names(dens), lty=c(1, 2), col=c("black", "grey"))
  }
  else {print("Please check the input data.")}
}
