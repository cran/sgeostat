################
#  Create a plotting routine for variograms...
assign("plot.variogram",	
function(variogram.obj,var.mod.obj=NULL,title.str=NULL,ylim=NULL,type='c',N=F) {

#  oldpar <- par()
#  par(mfrow=c(2,1),lab=c(12,5,7), #lab=c(length(variogram.obj$lag),5,7),
#      mar=c(4,12,4,12)+.1)

if (!inherits(variogram.obj,'variogram')) stop('variogram.obj must be of class "variogram"')
if (!missing(var.mod.obj))
  if (!inherits(var.mod.obj,'variogram.model')) stop('variogram.obj must be of class "variogram.model"')

if(type!='c'&type!='r'&type!='m') stop('type must be "c", "r", or "m".\n')

  if(type=='c'){
    ylabel _ 'Classical semi-variogram estimator'
    y _ variogram.obj$classic
  }
  if(type=='r'){
    ylabel _ 'Robust semi-variogram estimator'
    y _ variogram.obj$robust
  }
  if(type=='m'){
    ylabel _ 'Median semi-varigram estimator'
    y _ variogram.obj$med
  }
  y _ y/2


  if(is.null(ylim)) ylim <- c(0,max(variogram.obj$classic/2,variogram.obj$robust/2,na.rm=T))
  plot(variogram.obj$bins,y,
       ylim=ylim,
       xlim=c(0,max(variogram.obj$bins)),
       xlab="Lag",ylab=ylabel,
       type="p")

  if(N)
    text(variogram.obj$bins,y,variogram.obj$n)

  if(is.null(title.str))
    title(paste("Variogram estimator:",deparse(substitute(variogram.obj))))
  else
    title(title.str)
				    
# See if we need to plot a fitted variogram...
  if(!is.null(var.mod.obj)) {
    if (is.null(attr(var.mod.obj,"class"))) stop('var.mod.obj must be of class, "variogram.model".\n')
    else if (attr(var.mod.obj,"class") != 'variogram.model') stop('var.mod.obj must be of class, "variogram.model".\n')
    h <- seq(from=0.0001,to=max(variogram.obj$bins),length=50)
    lines(h,var.mod.obj$model(h,var.mod.obj$parameters))
  }

#  plot(variogram.obj$lag,variogram.obj$med,
#       ylim=c(0,max(variogram.obj$classic,variogram.obj$robust,variogram.obj$med,na.rm=T)),
#       xlab="Lag",ylab="Median estimator",
#       type="h")

#  plot(variogram.obj$bins,variogram.obj$robust,
#       ylim=c(0,max(variogram.obj$classic,variogram.obj$robust,variogram.obj$med,na.rm=T)),
#       xlim=c(0,max(variogram.obj$bins)),
#       xlab="Lag",ylab="Robust estimator",
#       type="p")


#  par(mfrow=c(1,1))
#  invisible(par(oldpar))
})
