options(stringsAsFactors = F)

install.packages("caret")
install.packages("xgboost")
library(xgboost)
library(mboost)
library(gbm)
library(splitstackshape)
library(ggplot2)
library(caret)

sam <- read.csv("Data_Request_Format.csv")
rj  <- read.csv("rhi_june.csv")

lapply(sam, class)

sam$Total_Orders[sam$Total_Orders %in% "-"]             <- 0
sam$Late_Night_Orders[sam$Late_Night_Orders %in% "-"]   <- 0
sam$Zomato_Ratings[sam$Zomato_Ratings %in% "-"]         <- 0
sam$Zomato_total_votes[sam$Zomato_total_votes %in% "-"] <- 0

samc                    <- sam
samc$Priority           <- as.factor(sam$Priority)
samc$Zomato_Ratings     <- as.numeric(sam$Zomato_Ratings)
samc$Zomato_total_votes <- as.numeric(sam$Zomato_total_votes)
samc$Late_Night_Orders  <- as.numeric(sam$Late_Night_Orders)
samc$Locality           <- as.factor(sam$Locality)
samc$CFT                <- as.numeric(as.character(sam$CFT))
samc                    <- samc[complete.cases(samc),]

lapply(samc, class)

duplicid <- samc$Rest_id[duplicated(samc$Rest_id)]
samcd    <- samc[!(samc$Rest_id %in% duplicid),]

duplicfu <- function(x) {
  z                   <- x[1,]
  z$Total_Orders      <- sum(x$Total_Orders)
  z$Late_Night_Orders <- sum(x$Late_Night_Orders)
  return(z)
}

samcd    <- rbind(samcd, do.call(rbind, lapply(split(samc[samc$Rest_id %in% duplicid,], samc$Rest_id[samc$Rest_id %in% duplicid]), FUN = duplicfu)))

samcs          <- cSplit(samcd, "Cuisines", "," ,direction = "Long")
samcs$Cuisines <- as.character(samcs$Cuisines)
samcs          <- samcs[!(samcs$Cuisines %in% c("-", "Special Discount from (Denzong Kitchen)", "Special Discount from Denzong Kitchen")),]

for(i in 1:nrow(samcs)) {
  if(samcs$CFT[i] <= 250) {
    samcs$cftrange[i] <- "<= 250"
  }
  else if(samcs$CFT[i] > 250 & samcs$CFT[i] <= 500) {
    samcs$cftrange[i] <- "> 250 & <= 500"
  }
  else if(samcs$CFT[i] > 500 & samcs$CFT[i] <= 1000) {
    samcs$cftrange[i] <- "> 500 & <= 1000"
  }
  else if(samcs$CFT[i] > 1000 & samcs$CFT[i] <= 1500) {
    samcs$cftrange[i] <- "> 1000 & <= 1500"
  }
  else if(samcs$CFT[i] > 1500 & samcs$CFT[i] <= 2500) {
    samcs$cftrange[i] <- "> 1500 & <= 2500"
  }
  else {
    samcs$cftrange[i] <- "> 2500"
  }
  if(samcs$Cuisines[i] %in% c("pizza", "Pizza", "Pizzas")) {
    samcs$Cuisines[i] <- "Pizza"
  }
  else if(samcs$Cuisines[i] %in% c("Juice", "Juices")) {
    samcs$Cuisines[i] <- "Juice"
  }
}

samcs$votesratings    <- samcs$Zomato_Ratings * samcs$Zomato_total_votes

wonf <- function(x) {
  for(i in 1:nrow(x)) {
    x$Total_Orders[i]       <- x$Total_Orders[i] / nrow(x)
    x$Late_Night_Orders[i]  <- x$Late_Night_Orders[i] / nrow(x)
    x$votesratings[i]       <- x$votesratings[i] / nrow(x)
  }
  return(x)
}

duplisid <- unique(samcs$Rest_id[duplicated(samcs$Rest_id)])
samct    <- lapply(split(samcs[samcs$Rest_id %in% duplisid,], samcs$Rest_id[samcs$Rest_id %in% duplisid]), FUN = wonf)

samcta   <- NULL
for(i in 1:length(samct)) {
  print(i)
  samcta <- rbind(samcta, samct[[i]])
}
samct    <- rbind(samcta, samcs[!(samcs$Rest_id %in% duplisid),])


wocf     <- function(x) {
  z                   <- NULL
  z$Cuisines          <- unique(x$Cuisines)
  z$Total_Orders      <- sum(x$Total_Orders)
  z$Late_Night_Orders <- sum(x$Late_Night_Orders)
  z$votesratings      <- sum(x$votesratings)
  return(as.data.frame(z))
}

cvrdd      <- do.call(rbind, lapply(split(samcs, samcs$Cuisines), FUN = wocf))

cvrdd$fto  <- cvrdd$Total_Orders / sum(cvrdd$Total_Orders)
cvrdd$flno <- cvrdd$Late_Night_Orders / sum(cvrdd$Late_Night_Orders)
cvrdd$fvr  <- cvrdd$votesratings / sum(cvrdd$votesratings)

wocg <- function(x) {
  z  <- 0
  y  <- 0
  w  <- 0
  for(i in 1:nrow(x)) {
    z <- z + cvrdd$fto[cvrdd$Cuisines %in% x$Cuisines[i]]
    y <- y + cvrdd$flno[cvrdd$Cuisines %in% x$Cuisines[i]]
    w <- w + cvrdd$fvr[cvrdd$Cuisines %in% x$Cuisines[i]]
  }
  for(i in 1:nrow(x)) {
    x$Total_Orders[i]      <- x$Total_Orders[i] * cvrdd$fto[cvrdd$Cuisines %in% x$Cuisines[i]] * nrow(x) / z
    x$Late_Night_Orders[i] <- x$Late_Night_Orders[i] * cvrdd$flno[cvrdd$Cuisines %in% x$Cuisines[i]] * nrow(x) / y
    x$votesratings[i]      <- x$votesratings[i] * cvrdd$fvr[cvrdd$Cuisines %in% x$Cuisines[i]] * nrow(x) / w
  }
  return(x)
}

samctad  <- lapply(split(samct[samct$Rest_id %in% duplisid,], samct$Rest_id[samct$Rest_id %in% duplisid]), FUN = wocg)
samcta   <- NULL
for(i in 1:length(samctad)) {
  print(i)
  samcta <- rbind(samcta, samctad[[i]])
}

samctad  <- rbind(samcta, samct[!(samct$Rest_id %in% duplisid),])

rhsa     <- merge(samctad, rj[,c(1,5,6)], by = "Rest_id")

rhsa$Cuisines <- as.factor(rhsa$Cuisines)
rhsa$cftrange <- as.factor(rhsa$cftrange)
rhsa$Area     <- as.factor(rhsa$Area)
rhsa$City     <- as.factor(rhsa$City)

rhsac <- rhsa

rhsac$Total_Orders      <- ceiling(rhsac$Total_Orders)
rhsac$Late_Night_Orders <- ceiling(rhsac$Late_Night_Orders)
rhsac$votesratings      <- floor(rhsac$votesratings)

rhsao <- rhsac[rhsac$votesratings != 0 & rhsac$Total_Orders > 10,]

boxplot.stats(rhsao$Total_Orders)
boxplot.stats(rhsao$votesratings)

ggplot(rhsao, aes(x = votesratings)) + geom_histogram(bins = 1000)

quantile(rhsao$votesratings, c(seq(0, 1, by = 0.1)))

qbf <- function(x) {
  z <- seq(0, 1, by = 0.1)
  y <- vector()
  for(i in 1:(length(z) - 2)) {
  y[i] <- nrow(x[x$votesratings >= quantile(x$votesratings, z[i]) & x$votesratings < quantile(x$votesratings, z[i+1]),])
  }
  y[length(z) - 1] <- nrow(x[x$votesratings >= quantile(x$votesratings, z[length(z) - 1]) & x$votesratings <= quantile(x$votesratings, z[length(z)]),])
  return(y)
}

qd    <- qbf(rhsao)

rhsaq <- rhsao

for(i in 1:nrow(rhsaq)) {
  if(rhsaq$votesratings[i] >= quantile(rhsaq$votesratings, 0)          & rhsaq$votesratings[i] < quantile(rhsaq$votesratings, 0.40)) {
    rhsaq$vrb[i] <- 1
  }
  else if(rhsaq$votesratings[i] >= quantile(rhsaq$votesratings, 0.40)  & rhsaq$votesratings[i] < quantile(rhsaq$votesratings, 0.65)) {
    rhsaq$vrb[i] <- 2
  }
  else if(rhsaq$votesratings[i] >= quantile(rhsaq$votesratings, 0.65)  & rhsaq$votesratings[i] < quantile(rhsaq$votesratings, 0.8)) {
    rhsaq$vrb[i] <- 3
  }
  else if(rhsaq$votesratings[i] >= quantile(rhsaq$votesratings, 0.8)   & rhsaq$votesratings[i] < quantile(rhsaq$votesratings, 0.875)) {
    rhsaq$vrb[i] <- 4
  }
  else if(rhsaq$votesratings[i] >= quantile(rhsaq$votesratings, 0.875) & rhsaq$votesratings[i] < quantile(rhsaq$votesratings, 0.925)) {
    rhsaq$vrb[i] <- 5
  }
  else if(rhsaq$votesratings[i] >= quantile(rhsaq$votesratings, 0.925) & rhsaq$votesratings[i] < quantile(rhsaq$votesratings, 0.975)) {
    rhsaq$vrb[i] <- 6
  }
  else {
    rhsaq$vrb[i] <- 7
  }
}

rhsaq$vrb        <- as.factor(rhsaq$vrb)

rhsaq$Area       <- droplevels(rhsaq$Area)
rhsaq$Cuisines   <- droplevels(rhsaq$Cuisines)
rhsaq$cftrange   <- droplevels(rhsaq$cftrange)
rhsaq$vrb        <- droplevels(rhsaq$vrb)

rhsam            <- rhsaq
levels(rhsam$Cuisines)[1:length(levels(rhsam$Cuisines))] <- 1:length(levels(rhsam$Cuisines))
levels(rhsam$Area)[1:length(levels(rhsam$Area))]         <- 1:length(levels(rhsam$Area))
levels(rhsam$cftrange)[1:length(levels(rhsam$cftrange))] <- 1:length(levels(rhsam$cftrange))

ggplot(rhsao, aes(x = City, y = Total_Orders, fill = City)) + geom_boxplot()
ggplot(rhsao, aes(x = City, y = votesratings, fill = City)) + geom_boxplot()

rhsamv   <- cbind(rhsam[,6], model.matrix(~.+0, data = rhsam[,c(3,11,13,15), with = F]))
rhsamvdg <- rhsam[rhsam$City %in% c("Delhi","Gurgaon"),]
rhsamvdg <- droplevels(rhsamvdg)
rhsamvdg <- cbind(rhsamvdg[,6], model.matrix(~.+0, data = rhsamvdg[,c(3,11,13,15), with = F]))

rhsadv   <- cbind(rhsaq[,6], model.matrix(~.+0, data = rhsaq[,c(3,11,13,15), with = F]))
rhsadvd  <- cbind(rhsaq[rhsaq$City %in% c("Delhi", "Gurgaon"),6], model.matrix(~.+0, data = rhsaq[rhsaq$City %in% c("Delhi", "Gurgaon"),c(3,11,13,15), with = F]))

lapply(rhsadv, class)

smxg     <- xgb.DMatrix(as.matrix(rhsadv[,-"Total_Orders"]), label = rhsadv$Total_Orders)

par      <-  list(booster = "gbtree",
                  objective = "reg:linear",
                  min_child_weight = 6,
                  gamma = 2,
                  subsample = 0.85,
                  colsample_bytree = 0.75,
                  max_depth = 6,
                  verbose = 1,
                  scale_pos_weight = 1)

xgbcv    <- xgb.cv(params = par, data = smxg, nrounds = 250, nfold = 5, showsd = T, stratified = T,
                   print.every.n = 10, early.stop.round = 20, maximize = F)

fitxg    <- xgboost(params = par, data = smxg, nrounds = 76)

fi1      <- rhsadv
fi1$pre  <- predict(fitxg, smxg)
fi1[,c("Total_Orders", "pre")]

form     <- paste("Total_Orders", fv3, sep = " ~ ")
writeLines(form, "form.txt")
formu    <- as.formula(form)

formdg   <- paste("Total_Orders", fvdg, sep = " ~ ")
formudg  <- as.formula(formdg)

lm1      <- lm(formu, data = rhsamv)
lmdg     <- lm(formudg, data = rhsamvdg)

glm1     <- glmboost(formu, data = rhsamv)

rhsal1        <- rhsam
rhsal1$pre    <- predict(lm1, rhsamv)
rhsal1$preglb <- predict(glm1, rhsamv)
write.csv(rhsal1, "lmi1.csv")
