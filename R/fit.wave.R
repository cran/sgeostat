assign("fit.wave",
function(v.object,c0=NULL,cw=NULL,aw=NULL,type='c',iterations=10,
         tolerance=.00001,echo=F,plot.it=F,weighted=T){
  # This program fits a univariate periodical model to an empirical variogram
  # estimate.  The SEMI variogram model is the model fit...

  if (!inherits(v.object,"variogram")) stop('v.object must be of class, "variogram".\n')

  if (is.null(c0) & is.null(cw) & is.null(aw)) estimate.initial <- T
  else estimate.initial <- F

  if ( !estimate.initial & (is.null(c0) | is.null(cw) | is.null(aw)))
    stop('c0, cw, and aw must all be entered.\n')

# Interpret the "type" argument...
  if (type == 'c') empgamma <- v.object$classic /2
  else if (type == 'r') empgamma <- v.object$robust /2
  else if (type == 'm') empgamma <- v.object$medain /2
  else stop("type must be 'c', 'r', or 'm'.\n")

# Set up the first derivative functions for each of the parameters...
# dc0 = 1
  dcw <- function(h,aw) { return(1-(aw*sin(h/aw)/h)) }

  daw <- function(h,cw,aw) { return((cw/h)*(sin(h/aw) - (h/aw * cos(h/aw))))}

# Set up the variogram function...
  wave.v <- function(h,parameters)
                   ifelse(h==0,0,
                   parameters[1] + parameters[2]*(1-(parameters[3]*sin(h/parameters[3])/h)))

# Get the number of observations and the bins (h's)...
  numobs <- v.object$n
  h      <- v.object$bins

# If any numobs is 0, get rid of that lag...
  empgamma <- empgamma[numobs>0]
  h <- h[numobs>0]
  numobs <- numobs[numobs>0]

# Start the sums of squares at 0...
  rse <- 0

# Begin iterations...
  parameters <- c(c0,cw,aw)
  cat('Initial parameter estimates: ',parameters,'\n')
  loop <- T
  i <- 1
  while (loop) {
    cat('Iteration:',i,'\n')
# establish the Y vector...
    y <- (empgamma - wave.v(h,parameters))

# establish the x matrix...
    xmat <- cbind(rep(1,length(h)),
                  dcw(h,parameters[3]), 
                  daw(h,parameters[2],parameters[3]))

# establish the weights (Cressie, p. 99)...
    if(weighted) {
      w <- numobs/(wave.v(h,parameters))^2
      }
    else {
#      w <- (1:length(numobs))
      w <- rep(1,length(numobs))
      }
#    w <- numobs
   
    if(echo) cat('  X matrix:\n')
    if(echo) print(cbind(y,xmat,w))
    if(echo) cat('\n\n')    

    fit <- lsfit(xmat,y,wt=w,intercept=F)

# calculate the new parameter estimates...
    parameters.old <- parameters
    parameters <- fit$coef + parameters
    parameters <- ifelse(parameters>0,parameters,.000001)
    cat('Gradient vector: ',fit$coef,'\n')
    cat('New parameter estimates: ',parameters,'\n\n')

# Check for convergence, see if the sum of squares has converged
    rse.old <- rse
    rse <- sum(fit$residuals^2)
    rse.dif <- rse-rse.old

# Check for convergence of parmeters...
    parm.dist <- sqrt(sum((parameters-parameters.old)^2))

    cat('rse.dif = ',rse.dif,'(rse =',rse,')  ;  parm.dist = ',parm.dist,'\n\n')
#    cat('rse.dif = ',rse.dif,'(rse =',rse,')\n\n')
#    if(rse.dif < tolerance & parm.dist < tolerance) {
    if(abs(rse.dif) < tolerance) {
      loop <- F
      converge <- T
      cat('Convergence achieved by sums of squares.\n')
    }

    i <- i+1
    if (i>iterations) {
      loop <- F
      converge <- F
    }
    v.m.object <- list(parameters=parameters,
                       model= wave.v
                      )
    attr(v.m.object,'class') <- 'variogram.model'
    attr(v.m.object,'type') <- 'wave'
    if (plot.it)
      plot(v.object,v.m.object,type=type)
  }
  if (converge) 
    cat('Final parameter estimates: ',parameters,'\n\n')
  else
    cat('Convergence not achieved!\n')
  

  v.m.object <- list(parameters=parameters,
                     model= wave.v
                    )
  attr(v.m.object,'class') <- 'variogram.model'
  attr(v.m.object,'type') <- 'wave'

  return(v.m.object)
  
})
