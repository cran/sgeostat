# print.point prints out INFORMATION ABOUT an object of class "point"
assign("print.point",
function(point.obj) {

  cat(paste('\nPoint object:',deparse(substitute(point.obj)),'\n'))

  cat(paste('\n   Locations: ',length(point.obj$x),sep=''))
  cat(paste('\n\n   Attributes:\n      ',paste(names(point.obj),
                    collapse='\n      '),sep=''))
  cat('\n\n')


})
