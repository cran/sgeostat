.First.lib <- function(lib, pkg) {
  library.dynam("sgeostat", pkg, lib)
  require(mva)
}
  
