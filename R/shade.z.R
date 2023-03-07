#' Plot and Shade A Normal Denisty Function
#'
#' Will plot a normal density function and shade in the selected direction.
#' @param mean The mean of the data set. The default is 0.
#' @param sd The standard deviation of the data set.  The default is 1.
#' @param min The x-value of the minimum shaded region.  The default is -Infinity.
#' @param max The x-value of the maximum shaded region.  The default is Infinity.
#' @return A shaded normal denisty plot, area of shaded region.
#' @export
#' @importFrom graphics abline axis lines plot polygon text
#' @importFrom stats pnorm
shade.z = function(mean=0, sd=1, min=-Inf, max=Inf) {
  if(min==-Inf) {
    area=pnorm(max,mean=mean,sd=sd)
    } else {
    if(max==Inf) {
      area=(1-pnorm(min,mean=mean,sd=sd))
    } else {
      area=(pnorm(max,mean=mean,sd=sd)-pnorm(min,mean=mean,sd=sd))
    }
  }
  numpoints = 100
  max.scale = mean+3*sd
  min.scale = mean-3*sd
  if(max > mean+3*sd) {max=max.scale}
  if(min < mean-3*sd) {min=min.scale}
  if((max.scale-min.scale)>6) {numpoints = 100 + 100*((max.scale-min.scale)%/%6)}
  x = seq(from=min.scale, to=max.scale, length=numpoints)
  y = dnorm(x,mean=mean,sd=sd)
  plot(x,y,type="l",xaxt="n",ylab="Density")
  axis(side=1, at=c(min.scale,mean-2*sd,mean-sd,mean,mean+sd,mean+2*sd,max.scale))
  abline(h=0)
  poly.x=c(min,x[x>=min & x<=max],max)
  poly.y=c(0,y[x>=min & x<=max],0)
  polygon(poly.x,poly.y,density=10)
  text(mean-2*sd,max(y)*.9,label=c("Area of shaded region = \n\n",round(area,digits=4)))
}
