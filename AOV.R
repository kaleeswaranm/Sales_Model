library(splitstackshape)

sss           <- read.csv("sss.csv")
sss           <- sss[,c(1,2,3,5,8,9,10,16,17,18)]
colnames(sss) <- c("Rest_id", "Rest_name", "Area", "E/D", "AO", "MO", "JO", "AAOV", "MAOV", "JAOV")
sss$AO        <- as.numeric(sss$AO)
sss$MO        <- as.numeric(sss$MO)
sss$JO        <- as.numeric(sss$JO)
sss$AAOV      <- as.numeric(sss$AAOV)
sss$MAOV      <- as.numeric(sss$MAOV)
sss$JAOV      <- as.numeric(sss$JAOV)
sss1          <- sss[sss$AO > 1 & sss$MO > 1 & sss$JO > 1,]
sss1$wa       <- 3 / 6 * sss1$JAOV + 2 / 6 * sss1$MAOV + 1 / 6 * sss1$AAOV
sssm          <- merge(sss1,samcd[,c(1,3,4,6,9,10)],by="Rest_id")

lapply(sssm, class)
sssm          <- cSplit(sssm, "Cuisines", "," ,direction = "Long")
sssm$Cuisines <- as.character(sssm$Cuisines)
sssm$CFT      <- as.numeric(sssm$CFT)
sssm$vr       <- sssm$Zomato_Ratings * sssm$Zomato_total_votes

for(i in 1:nrow(sssm)) {
  if(sssm$CFT[i] <= 250) {
    sssm$cftrange[i] <- "1"
  }
  else if(sssm$CFT[i] > 250 & sssm$CFT[i] <= 500) {
    sssm$cftrange[i] <- "2"
  }
  else if(sssm$CFT[i] > 500 & sssm$CFT[i] <= 1000) {
    sssm$cftrange[i] <- "3"
  }
  else if(sssm$CFT[i] > 1000 & sssm$CFT[i] <= 1500) {
    sssm$cftrange[i] <- "4"
  }
  else if(sssm$CFT[i] > 1500 & sssm$CFT[i] <= 2500) {
    sssm$cftrange[i] <- "5"
  }
  else {
    sssm$cftrange[i] <- "6"
  }
  if(sssm$Cuisines[i] %in% c("pizza", "Pizza", "Pizzas")) {
    sssm$Cuisines[i] <- "Pizza"
  }
  else if(sssm$Cuisines[i] %in% c("Juice", "Juices")) {
    sssm$Cuisines[i] <- "Juice"
  }
}

duplisia <- unique(sssm$Rest_id[duplicated(sssm$Rest_id)])

waov  <- function(x) {
  z   <- 0
  w   <- 0
  for(i in 1:nrow(x)) {
    z <- z + cvrdd$fto[cvrdd$Cuisines %in% x$Cuisines[i]]
    w <- w + cvrdd$fvr[cvrdd$Cuisines %in% x$Cuisines[i]]
  }
  for(i in 1:nrow(x)) {
    x$Total_Orders[i] <- x$Total_Orders[i] * cvrdd$fto[cvrdd$Cuisines %in% x$Cuisines[i]] / z
    x$vr[i]           <- x$vr[i] * cvrdd$fvr[cvrdd$Cuisines %in% x$Cuisines[i]] / w
  }
  return(x)
}

ssm      <- lapply(split(sssm[sssm$Rest_id %in% duplisia,], sssm$Rest_id[sssm$Rest_id %in% duplisia]), FUN = waov)
samcta   <- NULL
for(i in 1:length(ssm)) {
  print(i)
  samcta <- rbind(samcta, ssm[[i]])
}

ssm      <- rbind(samcta, sssm[!(sssm$Rest_id %in% duplisia),])

ssm$vr   <- floor(ssm$vr)

for(i in 1:nrow(ssm)) {
  if(ssm$vr[i] >= quantile(rhsaq$votesratings, 0)          & ssm$vr[i] < quantile(rhsaq$votesratings, 0.40) |
     ssm$vr[i] == 0) {
    ssm$vrb[i] <- 1
  }
  else if(ssm$vr[i] >= quantile(rhsaq$votesratings, 0.40)  & ssm$vr[i] < quantile(rhsaq$votesratings, 0.65)) {
    ssm$vrb[i] <- 2
  }
  else if(ssm$vr[i] >= quantile(rhsaq$votesratings, 0.65)  & ssm$vr[i] < quantile(rhsaq$votesratings, 0.8)) {
    ssm$vrb[i] <- 3
  }
  else if(ssm$vr[i] >= quantile(rhsaq$votesratings, 0.8)   & ssm$vr[i] < quantile(rhsaq$votesratings, 0.875)) {
    ssm$vrb[i] <- 4
  }
  else if(ssm$vr[i] >= quantile(rhsaq$votesratings, 0.875) & ssm$vr[i] < quantile(rhsaq$votesratings, 0.925)) {
    ssm$vrb[i] <- 5
  }
  else if(ssm$vr[i] >= quantile(rhsaq$votesratings, 0.925) & ssm$vr[i] < quantile(rhsaq$votesratings, 0.975)) {
    ssm$vrb[i] <- 6
  }
  else {
    ssm$vrb[i] <- 7
  }
}

ssm$vrb        <- as.factor(ssm$vrb)
ssm$Cuisines   <- as.factor(ssm$Cuisines)
ssm$cftrange   <- as.factor(ssm$cftrange)
ssm$Area       <- droplevels(ssm$Area)

sms            <- ssm

levels(sms$Cuisines)[1:length(levels(sms$Cuisines))] <- 1:length(levels(sms$Cuisines))
levels(sms$Area)[1:length(levels(sms$Area))]         <- 1:length(levels(sms$Area))

ssmdv          <- cbind(sms[,11], model.matrix(~.+0, data = sms[,c(3,12,18,19)]))

formaov        <- as.formula(paste("wa", tempf, sep = " ~ "))

lmaov          <- lm(formaov, data = ssmdv)

ssmt           <- sms
ssmt$pre       <- predict(lmaov, ssmdv)

qosa           <- read.csv("qos_july.csv")
qosar          <- qosa[,c(1,2,4,6,17,18,19,22,31)]

qosarl         <- cSplit(qosar, "Cuisine", ", ", direction = "Long")
qosarl         <- cSplit(qosar, "Cuisine", ",",  direction = "Long")

qosarl$Cuisine    <- as.character(qosarl$Cuisine)
colnames(qosarl)  <- c("Rest_id", "Rest_name", "Cuisines", "Area", "vr", "r", "v", "CFT", "L/N")

for(i in 1:nrow(qosarl)) {
  print(i)
  if(qosarl$CFT[i] <= 250) {
    qosarl$cftrange[i] <- "1"
  }
  else if(qosarl$CFT[i] > 250 & qosarl$CFT[i] <= 500) {
    qosarl$cftrange[i] <- "2"
  }
  else if(qosarl$CFT[i] > 500 & qosarl$CFT[i] <= 1000) {
    qosarl$cftrange[i] <- "3"
  }
  else if(qosarl$CFT[i] > 1000 & qosarl$CFT[i] <= 1500) {
    qosarl$cftrange[i] <- "4"
  }
  else if(qosarl$CFT[i] > 1500 & qosarl$CFT[i] <= 2500) {
    qosarl$cftrange[i] <- "5"
  }
  else {
    qosarl$cftrange[i] <- "6"
  }
  if(qosarl$Cuisines[i] %in% c("pizza", "Pizza", "Pizzas")) {
    qosarl$Cuisines[i] <- "Pizza"
  }
  else if(qosarl$Cuisines[i] %in% c("Juice", "Juices")) {
    qosarl$Cuisines[i] <- "Juice"
  }
}

rdf     <- NULL
rdf$c   <- as.character(levels(ssm$Cuisines))
rdf$n   <- as.numeric(levels(sms$Cuisines))
rdf     <- as.data.frame(rdf)

rda     <- NULL
rda$a   <- as.character(levels(ssm$Area))
rda$n   <- as.numeric(levels(sms$Area))
rda     <- as.data.frame(rda)

qosarlz <- qosarl

qosarlz <- qosarlz[!(qosarlz$Cuisines %in% setdiff(unique(qosarlz$Cuisines), cvrdd$Cuisines)),]

dupli <- unique(qosarlz$Rest_id[duplicated(qosarlz$Rest_id)])

waoz  <- function(x) {
  w   <- 0
  for(i in 1:nrow(x)) {
    w <- w + cvrdd$fvr[cvrdd$Cuisines %in% x$Cuisines[i]]
  }
  for(i in 1:nrow(x)) {
    x$vr[i]           <- x$vr[i] * cvrdd$fvr[cvrdd$Cuisines %in% x$Cuisines[i]] / w
  }
  return(x)
}

osm      <- lapply(split(qosarlz[qosarlz$Rest_id %in% dupli,], qosarlz$Rest_id[qosarlz$Rest_id %in% dupli]), FUN = waoz)

samcta   <- NULL
for(i in 1:length(osm)) {
  print(i)
  samcta <- rbind(samcta, osm[[i]])
}

osm      <- rbind(samcta, qosarlz[!(qosarlz$Rest_id %in% dupli),])

asm          <- osm[!(osm$Area %in% setdiff(osm$Area,rda$a)),]
asm          <- asm[!(asm$Cuisines %in% setdiff(asm$Cuisines,rdf$c)),]

for(i in 1:nrow(asm)) {
  print(i)
  asm$Cuisines[i] <- rdf$n[rdf$c %in% asm$Cuisines[i]]
  asm$Area[i]     <- rda$n[rda$a %in% asm$Area[i]]
}

asm$vr            <- floor(asm$vr)

for(i in 1:nrow(asm)) {
  print(i)
  if(asm$vr[i] >= quantile(rhsaq$votesratings, 0)          & asm$vr[i] < quantile(rhsaq$votesratings, 0.40)
    |asm$vr[i] == 0) {
    asm$vrb[i] <- 1
  }
  else if(asm$vr[i] >= quantile(rhsaq$votesratings, 0.40)  & asm$vr[i] < quantile(rhsaq$votesratings, 0.65)) {
    asm$vrb[i] <- 2
  }
  else if(asm$vr[i] >= quantile(rhsaq$votesratings, 0.65)  & asm$vr[i] < quantile(rhsaq$votesratings, 0.8)) {
    asm$vrb[i] <- 3
  }
  else if(asm$vr[i] >= quantile(rhsaq$votesratings, 0.8)   & asm$vr[i] < quantile(rhsaq$votesratings, 0.875)) {
    asm$vrb[i] <- 4
  }
  else if(asm$vr[i] >= quantile(rhsaq$votesratings, 0.875) & asm$vr[i] < quantile(rhsaq$votesratings, 0.925)) {
    asm$vrb[i] <- 5
  }
  else if(asm$vr[i] >= quantile(rhsaq$votesratings, 0.925) & asm$vr[i] < quantile(rhsaq$votesratings, 0.975)) {
    asm$vrb[i] <- 6
  }
  else {
    asm$vrb[i] <- 7
  }
}

fsm <- asm

fsm <- fsm[!(fsm$Cuisines %in% setdiff(fsm$Cuisines,sms$Cuisines)),]
fsm <- fsm[!(fsm$Area %in% setdiff(fsm$Area,sms$Area)),]
fsm <- fsm[!(fsm$cftrange %in% setdiff(fsm$cftrange,sms$cftrange)),]
fsm <- fsm[!(fsm$vrb %in% setdiff(fsm$vrb,sms$vrb)),]

fsm$Cuisines <- as.factor(fsm$Cuisines)
fsm$Area     <- as.factor(fsm$Area)
fsm$cftrange <- as.factor(fsm$cftrange)
fsm$vrb      <- as.factor(fsm$vrb)

osmdv   <- model.matrix(~.+0, data = fsm[,c("Cuisines","Area","vrb","cftrange")])
setdiff(colnames(osmdv), colnames(ssmdv))
osmdv   <- as.data.frame(osmdv[,!(colnames(osmdv) %in% setdiff(colnames(osmdv), colnames(ssmdv)))])
osmdv$Area38 <- 0
osmdv$Area1  <- 0
osmdv$Area41 <- 0
osmdv$Cuisines16 <- 0
osmdv$Cuisines19 <- 0
osmdv$Cuisines20 <- 0
osmdv$Cuisines22 <- 0
osmdv$Cuisines38 <- 0
osmdv$Cuisines54 <- 0
osmdv$Cuisines56 <- 0
osmdv$Cuisines60 <- 0
osmdv$Cuisines62 <- 0
osmdv$Cuisines65 <- 0
osmdv$Cuisines69 <- 0
osmdv$Cuisines70 <- 0
osmdv$Cuisines73 <- 0
psm     <- fsm
psm$pre <- predict(lmaov, osmdv)
write.csv(psm,)
setdiff(colnames(osmdv), colnames(ssmdv))

qsm <- psm
qsm$Cuisines <- as.character(qsm$Cuisines)
qsm$Area <- as.character(qsm$Area)
for(i in 1:nrow(qsm)) {
  print(i)
  qsm$Cuisines[i] <- rdf$c[rdf$n %in% qsm$Cuisines[i]]
  qsm$Area[i]     <- rda$a[rda$n %in% qsm$Area[i]]
}
write.csv(qsm,"aovpre.csv")

