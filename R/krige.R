"krige" <-
function (s, point.obj, at, var.mod.obj, maxdist = NULL, extrap = F) 
{
	    if (!inherits(point.obj, "point")) 
	    	    stop("point.obj must be of class, \"point\".\n")
	    # perform kriging on all given points:
	    if (!inherits(var.mod.obj, "variogram.model")) 
	    	    stop("var.mod.obj must be of class, \"variogram.model\".\n")
	    s$do <- c(rep(T, length(s$x)))
	    # do nothing outside the convex hull?
	    # pull out the attribute vector...
	    if (!extrap) {
	    	    s$do <- in.chull(s$x,s$y,point.obj$x,point.obj$y) 
	    }
	    at <- point.obj[[match(at, names(point.obj))]]
	    # if a maxdist hasn't been entered, then use all of the points...
	    if (is.null(maxdist)) 
	    	    krige.all(s, point.obj, at, var.mod.obj)
	    else krige.maxdist(s, point.obj, at, var.mod.obj, maxdist)
}
