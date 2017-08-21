qos            <- read.csv("qos_july.csv")

qosor          <- qos[,c(1,2,4,6,17,18,19,22,31)]

qosorl         <- qosor
qosorl$Cuisine <- as.character(qosorl$Cuisine)

qosorl         <- cSplit(qosor, "Cuisine", ", ", direction = "Long")
qosorl         <- cSplit(qosor, "Cuisine", ",",  direction = "Long")
qosorl$Cuisine <- as.character(qosorl$Cuisine)

for(i in 1:nrow(qosorl)) {
  if(qosorl$CFT[i] <= 250) {
    qosorl$cftrange[i] <- "<= 250"
  }
  else if(qosorl$CFT[i] > 250 & qosorl$CFT[i] <= 500) {
    qosorl$cftrange[i] <- "> 250 & <= 500"
  }
  else if(qosorl$CFT[i] > 500 & qosorl$CFT[i] <= 1000) {
    qosorl$cftrange[i] <- "> 500 & <= 1000"
  }
  else if(qosorl$CFT[i] > 1000 & qosorl$CFT[i] <= 1500) {
    qosorl$cftrange[i] <- "> 1000 & <= 1500"
  }
  else if(qosorl$CFT[i] > 1500 & qosorl$CFT[i] <= 2500) {
    qosorl$cftrange[i] <- "> 1500 & <= 2500"
  }
  else {
    qosorl$cftrange[i] <- "> 2500"
  }
  if(qosorl$Cuisine[i] %in% c("pizza", "Pizza", "Pizzas")) {
    qosorl$Cuisine[i] <- "Pizza"
  }
  else if(qosorl$Cuisine[i] %in% c("Juice", "Juices")) {
    qosorl$Cuisine[i] <- "Juice"
  }
}

colnames(qosorl) <- c("Rest_id", "Rest_name","Cuisines", "Area", "votesratings", "Ratings", "Votes", "CFT", "L/N", "cftrange")

qosorlc           <- qosorl[!(qosorl$Cuisines %in% setdiff(qosorl$Cuisines, rhsaq$Cuisines)),]
qosorlc           <- qosorlc[!(qosorlc$Area %in% setdiff(qosorlc$Area, rhsaq$Area))]
qosorlc           <- qosorlc[!(qosorlc$cftrange %in% setdiff(qosorlc$cftrange, rhsaq$cftrange))]

qosorlc$Area      <- as.factor(qosorlc$Area)
qosorlc$Cuisines  <- as.factor(qosorlc$Cuisines)
qosorlc$cftrange  <- as.factor(qosorlc$cftrange)

qosorlc$Area      <- droplevels(qosorlc$Area)
qosorlc$Cuisines  <- droplevels(qosorlc$Cuisines)
qosorlc$cftrange  <- droplevels(qosorlc$cftrange)

wocgp <- function(x) {
  z <- 0
  for(i in 1:nrow(x)) {
    z <- z + cvrdd$fvr[cvrdd$Cuisines %in% x$Cuisines[i]]
  }
  for(i in 1:nrow(x)) {
    x$votesratings[i] <- x$votesratings[i] * cvrdd$fvr[cvrdd$Cuisines %in% x$Cuisines[i]] / z
  }
  return(x)
}

duplisip <- unique(qosorlc$Rest_id[duplicated(qosorlc$Rest_id)])

qd       <- lapply(split(qosorlc[qosorlc$Rest_id %in% duplisip,], qosorlc$Rest_id[qosorlc$Rest_id %in% duplisip]), FUN = wocgp)
samcta   <- NULL
for(i in 1:length(qd)) {
  print(i)
  samcta <- rbind(samcta, qd[[i]])
}

qd                <- rbind(samcta, qosorlc[!(qosorlc$Rest_id %in% duplisip),])

qd$votesratings   <- floor(qd$votesratings)

for(i in 1:nrow(qd)) {
  print(i)
  if(qd$votesratings[i] >= quantile(rhsaq$votesratings, 0)          & qd$votesratings[i] < quantile(rhsaq$votesratings, 0.40) |
     qd$votesratings[i] == 0) {
    qd$vrb[i] <- 1
  }
  else if(qd$votesratings[i] >= quantile(rhsaq$votesratings, 0.40)  & qd$votesratings[i] < quantile(rhsaq$votesratings, 0.65)) {
    qd$vrb[i] <- 2
  }
  else if(qd$votesratings[i] >= quantile(rhsaq$votesratings, 0.65)  & qd$votesratings[i] < quantile(rhsaq$votesratings, 0.8)) {
    qd$vrb[i] <- 3
  }
  else if(qd$votesratings[i] >= quantile(rhsaq$votesratings, 0.8)   & qd$votesratings[i] < quantile(rhsaq$votesratings, 0.875)) {
    qd$vrb[i] <- 4
  }
  else if(qd$votesratings[i] >= quantile(rhsaq$votesratings, 0.875) & qd$votesratings[i] < quantile(rhsaq$votesratings, 0.925)) {
    qd$vrb[i] <- 5
  }
  else if(qd$votesratings[i] >= quantile(rhsaq$votesratings, 0.925) & qd$votesratings[i] < quantile(rhsaq$votesratings, 0.975)) {
    qd$vrb[i] <- 6
  }
  else {
    qd$vrb[i] <- 7
  }
}

qd$vrb           <- as.factor(qd$vrb)

qd$Cuisines      <- droplevels(qd$Cuisines)
qd$Area          <- droplevels(qd$Area)
qd$cftrange      <- droplevels(qd$cftrange)

cu <- as.data.frame(levels(rhsaq$Cuisines))
cf <- as.data.frame(levels(rhsaq$cftrange))
ar <- as.data.frame(levels(rhsaq$Area))
cu$lvl <- 1:nrow(cu)
cf$lvl <- 1:nrow(cf)
ar$lvl <- 1:nrow(ar)

cu$`levels(rhsaq$Cuisines)` <- as.character(cu$`levels(rhsaq$Cuisines)`)
cf$`levels(rhsaq$cftrange)` <- as.character(cf$`levels(rhsaq$cftrange)`)
ar$`levels(rhsaq$Area)`     <- as.character(ar$`levels(rhsaq$Area)`)

qdlm <- qd

qdlm$Cuisines <- as.character(qdlm$Cuisines)
qdlm$cftrange <- as.character(qdlm$cftrange)
qdlm$Area     <- as.character(qdlm$Area)

for(i in 1:nrow(qdlm)) {
  print(i)
  qdlm$Cuisines[i] <- cu$lvl[cu$`levels(rhsaq$Cuisines)` %in% qdlm$Cuisines[i]]
  qdlm$cftrange[i] <- cf$lvl[cf$`levels(rhsaq$cftrange)` %in% qdlm$cftrange[i]]
  qdlm$Area[i]     <- ar$lvl[ar$`levels(rhsaq$Area)` %in% qdlm$Area[i]]
}

rhsadvt          <- model.matrix(~.+0, data = qdlm[,c(3,4,10,11), with = F])
rhsadvt          <- as.data.frame(rhsadvt)
rhsadvt[,setdiff(colnames(rhsamv), colnames(rhsadvt))] <- 0

qd$pre           <- predict(lm1, as.data.frame(rhsadvt))

xgb.DMatrix(rhsadvt)

qd$pre           <- predict(fitxg, xgb.DMatrix(rhsadvt))

wicgp <- function(x) {
  z <- x[1,]
  z$votesratings <- sum(x$votesratings)
  z$pre          <- sum(x$pre)
  z$Cuisines     <- NULL
  for(i in 1:nrow(x)) {
    z$Cuisines   <- paste(z$Cuisines, x$Cuisines[i], sep = ",")
  }
  return(z)
}

qds  <- lapply(split(qd[qd$Rest_id %in% duplisip,], qd$Rest_id[qd$Rest_id %in% duplisip]), FUN = wicgp)

samcta   <- NULL
for(i in 1:length(qds)) {
  print(i)
  samcta <- rbind(samcta, qds[[i]])
}

qds      <- rbind(samcta, qd[!(qd$Rest_id %in% duplisip),])
qds      <- qds[order(qds$pre, decreasing = TRUE),]
write.csv(qds, "lm1pre.csv")











