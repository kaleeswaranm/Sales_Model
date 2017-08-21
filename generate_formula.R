 # Cuisines, cftrange, Area, vrb, Total_Orders
 # x <- colnames()

ffi <- function(x) {
  a <- grep("Area", x) ; b <- grep("cftrange", x) ; c <- grep("Cuisines", x) ; d <- grep("vrb", x)
  w <- vector() ; y <- vector() ; z <- vector()
  for(i in 1:length(a)) {
    for(j in 1:length(b)) {
      if(i == 1 & j == 1) {
        w <- paste(w, paste(x[a[i]], x[b[j]], sep = " * "), sep = "")
      }
      else {
        w <- paste(w, paste(x[a[i]], x[b[j]], sep = " * "), sep = " + ")
      }
    }
    for(k in 1:length(c)) {
      if(i == 1 & k == 1) {
        y <- paste(y, paste(x[a[i]], x[c[k]], sep = " * "), sep = "")
      }
      else {
        y <- paste(y, paste(x[a[i]], x[c[k]], sep = " * "), sep = " + ")
      }
    }
    for(l in 1:length(d)) {
      if(i == 1 & l == 1) {
        z <- paste(z, paste(x[a[i]], x[d[l]], sep = " * "), sep = "")
      }
      else {
        z <- paste(z, paste(x[a[i]], x[d[l]], sep = " * "), sep = " + ")
      }
    }
  }
  return(paste(w,y,z,sep = " + "))
}

fv  <- ffi(colnames(rhsadv))
fv2 <- ffi(colnames(rhsamv))
writeLines(fv, "formula.txt")

ffc <- function(x) {
  a <- grep("Area", x) ; b <- grep("cftrange", x) ; d <- grep("vrb", x)
  w <- vector() ; z <- vector()
  for(i in 1:length(a)) {
    for(j in 1:length(b)) {
      if(i == 1 & j == 1) {
        w <- paste(w, paste(x[a[i]], x[b[j]], sep = " * "), sep = "")
      }
      else {
        w <- paste(w, paste(x[a[i]], x[b[j]], sep = " * "), sep = " + ")
      }
    }
    for(l in 1:length(d)) {
      if(i == 1 & l == 1) {
        z <- paste(z, paste(x[a[i]], x[d[l]], sep = " * "), sep = "")
      }
      else {
        z <- paste(z, paste(x[a[i]], x[d[l]], sep = " * "), sep = " + ")
      }
    }
  }
  return(paste(w,z,sep = " + "))
}

fv3   <- ffc(colnames(rhsamv))
fv3   <- paste(fv3, paste(colnames(rhsamv)[grep("Cuisines", colnames(rhsamv))], collapse = " + "), sep = " + ")
fvdg  <- ffi(colnames(rhsamvdg))
fvdg  <- paste(fvdg, paste(colnames(rhsamvdg)[grep("Cuisines", colnames(rhsamvdg))], collapse = " + "), sep = " + ")

ffa <- function(x) {
  a <- grep("Area", x) ; b <- grep("cftrange", x) ; c <- grep("Cuisines", x) ; d <- grep("vrb", x)
  w <- vector() ; y <- vector() ; z <- vector() ; v <- vector()
  for(i in 1:length(a)) {
    print(i)
    for(j in 1:length(b)) {
      if(i == 1 & j == 1) {
        w <- paste(w, paste(x[a[i]], x[b[j]], sep = " * "), sep = "")
      }
      else {
        w <- paste(w, paste(x[a[i]], x[b[j]], sep = " * "), sep = " + ")
      }
    }
    for(k in 1:length(c)) {
      if(i == 1 & k == 1) {
        y <- paste(y, paste(x[a[i]], x[c[k]], sep = " * "), sep = "")
      }
      else {
        y <- paste(y, paste(x[a[i]], x[c[k]], sep = " * "), sep = " + ")
      }
    }
    for(l in 1:length(d)) {
      if(i == 1 & l == 1) {
        z <- paste(z, paste(x[a[i]], x[d[l]], sep = " * "), sep = "")
      }
      else {
        z <- paste(z, paste(x[a[i]], x[d[l]], sep = " * "), sep = " + ")
      }
    }
  }
  for(m in 1:length(b)) {
    print(m)
    for(n in 1:length(d)) {
      if(m == 1 & n == 1) {
        v <- paste(v, paste(x[b[m]], x[d[n]], sep = " * "), sep = "")
      }
      else {
        v <- paste(v, paste(x[b[m]], x[d[n]], sep = " * "), sep = " + ")
      }
    }
  }
  return(paste(w,y,z,v,sep = " + "))
}

tempf <- ffa(colnames(ssmdv))
writeLines(tempf, "tempf.txt")




