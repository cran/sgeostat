# print.pair prints out INFORMATION ABOUT an object of class "pair"
assign("print.pair",
function(pair.obj) {

  cat(paste('\nPair object:',deparse(substitute(pair.obj)),'\n'))

  if (!is.null(attributes(pair.obj)$type))
    cat('\n      Type:            ',attributes(pair.obj)$type)
  if (!is.null(attributes(pair.obj)$theta))
    cat('\n      Theta:           ',attributes(pair.obj)$theta)
  if (!is.null(attributes(pair.obj)$dtheta))
    cat('\n      Dtheta:          ',attributes(pair.obj)$dtheta)
  cat('\n      Number of pairs: ',length(pair.obj$from))
  cat('\n      Number of lags:  ',length(unique(pair.obj$lags)))
  cat('\n      Max h:           ',max(pair.obj$dist))
  cat('\n\n')


})
