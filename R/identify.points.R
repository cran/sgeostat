# A function to "identify" points in a "point" object...
# This function allows the user to identify points graphically.
assign("identify.point",
function(point.obj,a,...) {

  a <- point.obj[[match(a,names(point.obj))]]

  identify(point.obj$x,point.obj$y,a,...)
})
