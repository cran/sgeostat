.First.lib <- function(lib, pkg) {
  library.dynam("sgeostat", pkg, lib)
  if(version$major<="1" && version$minor<"9.0")
    require(mva)
  else
    require(stats)
}
  
