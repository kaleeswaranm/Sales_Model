# Required packages

library(stringr)
library(ggplot2)

# override string as factors

options(stringsAsFactors = F)

# z function

z <- function(frame) {
  total_restos <- nrow(frame)
  total_orders <- sum(frame$Total.orders)
  restos       <- frame[,c(1:8)]
  appendcms40  <- vector()
  for(i in 1:nrow(frame)) {
    appendcms40 <- append(appendcms40, unlist(strsplit(frame$Cuisine[i], ",")))
  }
  cuisinesplitms40 <- as.data.frame(table(appendcms40))[as.data.frame(table(appendcms40))$Freq != 0,]
  cuisinesplitms40$range <- unique(frame$cftrange)
  cuisinesplitms40$area  <- unique(frame$area)
  cuisinesplitms40$order <- 0
  for(i in 1:nrow(cuisinesplitms40)) {
    for(j in 1:nrow(frame)) {
      uc <- vector()
      uc <- unlist(strsplit(frame$Cuisine[j], ","))
      for(k in 1:length(uc)) {
        if(uc[k] == cuisinesplitms40$appendcms40[i]) {
          cuisinesplitms40$order[i] <- cuisinesplitms40$order[i] + floor(frame$Total.orders[j] / length(uc))
        }
      }
    }
  }
  return(list(a = total_restos, b = total_orders, c = restos, d = cuisinesplitms40))
}

# abc function

abc <- function(ms, quant = 40) {
  ms <- ms[order(ms$Total.orders, decreasing = TRUE),]
  ms40 <- ms[ms$Total.orders >= quantile(ms$Total.orders, 1 - quant/100),]
  #1
  cftms40 <- as.data.frame(table(ms$CFT))[as.data.frame(table(ms$CFT))$Freq != 0,]
  appendc <- vector()
  for(i in 1:nrow(ms40)) {
    appendc <- append(appendc, unlist(strsplit(ms40$Cuisine[i], ",")))
  }
  #2
  cuisinems40 <- as.data.frame(table(appendc))[as.data.frame(table(appendc))$Freq != 0,]
  for(i in 1:nrow(ms40)) {
    if(ms40$CFT[i] <= 250) {
      ms40$cftrange[i] <- "<= 250"
    }
    else if(ms40$CFT[i] > 250 & ms$CFT[i] <= 500) {
      ms40$cftrange[i] <- "> 250 & <= 500"
    }
    else if(ms40$CFT[i] > 500 & ms$CFT[i] <= 1000) {
      ms40$cftrange[i] <- "> 500 & <= 1000"
    }
    else if(ms40$CFT[i] > 1000 & ms$CFT[i] <= 1500) {
      ms40$cftrange[i] <- "> 1000 & <= 1500"
    }
    else if(ms40$CFT[i] > 1500 & ms$CFT[i] <= 2500) {
      ms40$cftrange[i] <- "> 1500 & <= 2500"
    }
    else {
      ms40$cftrange[i] <- "> 2500"
    }
  }
  m40  <- do.call(rbind, lapply(split(ms40, ms40$cftrange), FUN = z))
  #4
  g <- ggplot(do.call(rbind, c(m40[1:length(unique(ms40$cftrange)),4])), aes(appendcms40, order, group = range, fill = range)) +
       geom_bar(colour = "black", stat = "identity", size = 0.2) + facet_grid(~range, scales = "free_x") +
       theme(text = element_text(size=7), axis.text.x = element_text(angle=90, hjust=1)) + xlab("Cuisines")
  return(list(cftms40, cuisinems40, m40, g))
}

ms  <- read.csv("ms.csv")
ms$CFT <- as.numeric(ms$CFT)
ms <- ms[complete.cases(ms),]
aa <- lapply(split(ms, ms$area), FUN = abc)
aa$`Ashok Vihar`









