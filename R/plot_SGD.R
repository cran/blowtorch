#' @export
#' @title Plot method for SGD object
#' @description plot the value of \code{h} across iterations.
#' @param x an object of class \code{SGD}
#' @param y does nothing
#' @param ... does nothing
#' 
plot.SGD <- function(x, y=NULL, ...) {
  if (!is.null(x$delta)) delta <- x$delta else stop("Invalid SGD object")
  
  ggplot(data=data.frame(delta=delta),
         aes(x=seq.int(length(delta)), y=delta)) +
    geom_line(alpha=0.25) +
    geom_point(alpha=0.45, shape=1) +
    stat_smooth(method='loess', se=FALSE, color='red') +
    geom_hline(yintercept=x$convTol, color='green', lty=2) +
    theme_bw() +
    labs(y=expression(h(theta)), x="iteration", title=NULL)
}