# A plot function for the "point" class of objects
assign("plot.point",
function (point.obj,a,axes=F,xlab='',ylab='',legend.pos=0,...) {
# Be careful to plot in a square region.  We can't distort the earth!
# I can't seem to force Splus to use the same scaling on both axes!
# But we must find a way!
  old.par <- par(pty='s')
  xdiff _ max(point.obj$x) - min(point.obj$x)
  ydiff _ max(point.obj$y) - min(point.obj$y)
  if (xdiff < ydiff) {
#   Set up our limits so that there are ydiff units on x and y...
    ylimits _ c(min(point.obj$y),max(point.obj$y))
    xlimits _ c((min(point.obj$x) + xdiff/2) - ydiff/2,
                (min(point.obj$x) + xdiff/2) + ydiff/2)
  }
  else {
    xlimits _ c(min(point.obj$x),max(point.obj$x))
    ylimits _ c((min(point.obj$y) + ydiff/2) - xdiff/2,
                (min(point.obj$y) + ydiff/2) + xdiff/2)
  }
  if (!missing(a)) {
    a.name<-a
    a <- point.obj[[match(a,names(point.obj))]]
#    colors <- cut(a,c(min(a)-1,quantile(a,c(.25,.5,.75,1))))
    a.q<-quantile(a,c(.25,.5,.75,1))	
    qcol<-c(3,7,2,6) # green, yellow, red, cyan
    colors <- cut(a,c(min(a)-1,a.q),labels=c(1:4))
    plot(point.obj$x,point.obj$y, axes=axes,xlab=xlab,ylab=ylab,type='n',
         xlim=xlimits,ylim=ylimits)
    for (i in as.numeric(unique(colors))){
      points(point.obj$x[colors==i],point.obj$y[colors==i],col=qcol[i],...)
}
    if (legend.pos!=0){
      l.x<-switch(legend.pos,
		  xlimits[1],xlimits[2],
		  xlimits[2],xlimits[1])
      l.xj<-switch(legend.pos,0,1,1,0)
      l.y<-switch(legend.pos,
		  ylimits[1],ylimits[1],
		  ylimits[2],ylimits[2])
      l.yj<-switch(legend.pos,0,0,1,1)
      legend(l.x,
	     l.y,
	     c(paste("[",min(a),",",a.q[1],"]"),
	       paste("(",a.q[1],",",a.q[2],"]"),
	       paste("(",a.q[2],",",a.q[3],"]"),
	       paste("(",a.q[3],",",max(a),"]")),
	     qcol,
	     xjust=l.xj,yjust=l.yj)
    }
    title(deparse(substitute(point.obj)))
    
  }
  else {
    plot(point.obj$x,point.obj$y, axes=axes,xlab=xlab,ylab=ylab,
         xlim=xlimits,ylim=ylimits)
    title(deparse(substitute(point.obj)))
  }
  invisible(par(old.par))
})
