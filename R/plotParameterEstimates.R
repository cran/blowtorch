plotParameterEstimates <- function(X, lambda, delta, cols=1, layout=NULL) {
  params <- rbind(X,lambda)
  N <- ncol(X)
  
  plotList <- lapply(rownames(params), FUN=function(parName) {
    X <- Y <- NULL # remove note in R CMD check
    df <- data.frame(X=seq.int(N), Y=params[parName,])
    plotTitle <- parse(text=parName)
    ggplot(data=df, aes(x=X, y=Y)) +
      geom_line() +
      stat_smooth(method='loess', se=FALSE, color='red') +
      labs(x="iteration", y=NULL, title=plotTitle) +
      theme_bw()
  })
  
  iters <- h <- NULL # remove note in R CMD check
  
  hPlot <- ggplot(data=data.frame(iters=seq.int(length(delta)), h=delta),
                  aes(x=iters, y=h)) +
    geom_line() +
    stat_smooth(method='loess', se=FALSE, color='red') +
    labs(x="iteration", y=NULL, title=parse(text="h(X,lambda)")) +
    theme_bw()
  
  plotList <- c(plotList, list(hPlot))
  
  multiplot(plotlist=plotList, cols=cols, layout=layout)
}