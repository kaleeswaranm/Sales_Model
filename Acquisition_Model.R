options(stringsAsFactors = F)

# Required packages

install.packages("gbm")
install.packages("mboost")
install.packages("outliers")
install.packages("biglm")
install.packages("biganalytics")
library(biganalytics)
library(bigmemory)
library(biglm)
library(outliers)
library(mboost)
library(gbm)
library(splitstackshape)
library(ggplot2)
library(forecast)

# Read the data

sam <- read.csv("Data_Request_Format.csv")
rj  <- read.csv("rhi_june.csv")

# Clean the data

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

# Data processing

  # Removing duplicates

duplicid <- samc$Rest_id[duplicated(samc$Rest_id)]
samcd    <- samc[!(samc$Rest_id %in% duplicid),]

duplicfu <- function(x) {
  z                   <- x[1,]
  z$Total_Orders      <- sum(x$Total_Orders)
  z$Late_Night_Orders <- sum(x$Late_Night_Orders)
  return(z)
}

samcd    <- rbind(samcd, do.call(rbind, lapply(split(samc[samc$Rest_id %in% duplicid,], samc$Rest_id[samc$Rest_id %in% duplicid]), FUN = duplicfu)))

  # Clean again after splitting on cuisines

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

samcs$cftrange <- as.factor(samcs$cftrange)
samcs$Cuisines <- as.factor(samcs$Cuisines)

  # Working on cuisines

wonf <- function(x) {
  for(i in 1:nrow(x)) {
    x$Total_Orders[i] <- x$Total_Orders[i] / nrow(x)
    x$Late_Night_Orders[i] <- x$Late_Night_Orders[i] / nrow(x) 
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

samct$Total_Orders      <- ceiling(samcta$Total_Orders)
samct$Late_Night_Orders <- ceiling(samcta$Late_Night_Orders)

wocf <- function(x) {
  z                   <- NULL
  z$Cuisines          <- unique(x$Cuisines)
  z$Total_Orders      <- sum(x$Total_Orders)
  z$Late_Night_Orders <- sum(x$Late_Night_Orders)
  return(as.data.frame(z))
}

cdd      <- do.call(rbind, lapply(split(samct, samct$Cuisines), FUN = wocf))

wocf <- function(x) {
  z                   <- NULL
  z$Cuisines          <- unique(x$Cuisines)
  z$Total_Orders      <- sum(x$Total_Orders)
  z$Late_Night_Orders <- sum(x$Late_Night_Orders)
  return(as.data.frame(z))
}

for(i in 1:nrow(cdd)) {
  cdd$fto[i]  <- cdd$Total_Orders[i] / sum(cdd$Total_Orders)
  cdd$flno[i] <- cdd$Late_Night_Orders[i] / sum(cdd$Late_Night_Orders)
}

wocg <- function(x) {
  z <- 0
  y <- 0
  for(i in 1:nrow(x)) {
    z <- z + cdd$fto[cdd$Cuisines %in% x$Cuisines[i]]
    y <- y + cdd$flno[cdd$Cuisines %in% x$Cuisines[i]]
  }
  for(i in 1:nrow(x)) {
    x$Total_Orders[i]      <- x$Total_Orders[i] * cdd$fto[cdd$Cuisines %in% x$Cuisines[i]] * nrow(x) / z
    x$Late_Night_Orders[i] <- x$Late_Night_Orders[i] * cdd$flno[cdd$Cuisines %in% x$Cuisines[i]] * nrow(x) / y
  }
  return(x)
}

samctad <- lapply(split(samct[samct$Rest_id %in% duplisid,], samct$Rest_id[samct$Rest_id %in% duplisid]), FUN = wocg)
samcta   <- NULL
for(i in 1:length(samctad)) {
  print(i)
  samcta <- rbind(samcta, samctad[[i]])
}

samctad                   <- rbind(samcta, samct[!(samct$Rest_id %in% duplisid),])
samctad$Total_Orders      <- ceiling(samctad$Total_Orders)
samctad$Late_Night_Orders <- ceiling(samctad$Late_Night_Orders)

lapply(samctad, class)

samctad$cftrange     <- as.factor(samctad$cftrange)
samctad$Cuisines     <- as.factor(samctad$Cuisines)
samctad$votesratings <- samctad$Zomato_Ratings * samctad$Zomato_total_votes

samctadm             <- merge(samctad, rj[,c(1,5,6)], by = "Rest_id")
samctadm$Area        <- as.factor(samctadm$Area)
samctadm$City        <- as.factor(samctadm$City)

wovrf <- function(x) {
  z                   <- NULL
  z$Cuisines          <- unique(x$Cuisines)
  z$vr                <- sum(x$votesratings)
  return(as.data.frame(z))
}

vrdd                 <- do.call(rbind, lapply(split(samctadm, samctadm$Cuisines), FUN = wovrf))
vrdd$vrf             <- vrdd$vr / sum(vrdd$vr)

wovr  <- function(x) {
  y <- 0
  for(i in 1:nrow(x)) {
    y <- y + vrdd$vrf[vrdd$Cuisines %in% x$Cuisines[i]]
  }
  for(i in 1:nrow(x)) {
    x$votesratings[i]      <- x$votesratings[i] * vrdd$vrf[vrdd$Cuisines %in% x$Cuisines[i]] / y
  }
  return(x)
}

samctavr <- lapply(split(samctadm[samctadm$Rest_id %in% duplisid,], samctadm$Rest_id[samctadm$Rest_id %in% duplisid]), FUN = wovr)
samcta   <- NULL
for(i in 1:length(samctavr)) {
  print(i)
  samcta <- rbind(samcta, samctavr[[i]])
}

samctavr <- rbind(samcta, samctadm[!(samctadm$Rest_id %in% duplisid),])

 #outlier analysis

samctadmno           <- samctadm[!(samctadm$Total_Orders %in% boxplot.stats(samctadm$Total_Orders)$out),]
samctadmno           <- samctadmno[!(samctadmno$votesratings %in% boxplot.stats(samctadmno$votesratings)$out),]
samctadmno           <- samctadmno[samctadmno$votesratings != 0,]
ggplot(samctadmno, aes(x = City, y = Total_Orders)) + geom_boxplot()
ggplot(samctadmno, aes(x = City, y = votesratings, fill = City)) + geom_boxplot(outlier.shape = 1, outlier.alpha = 0.5)

 #modelling

fg1  <- Total_Orders ~ Area * cftrange + Area * Cuisines + votesratings
fg2  <- Total_Orders ~ .

fit1                 <- gbm(fg2, data = samctadmno[,c(3,6,11,12,13)], distribution = "gaussian", interaction.depth = 4, 
                            cv.folds = 5, n.trees = 50000, shrinkage = 0.0005, train.fraction = 0.5, n.cores = 2, verbose = TRUE)
best.iter            <- gbm.perf(fit1, method = "cv")
summary(fit1, n.trees=best.iter)
print(pretty.gbm.tree(fit1, fit1$n.trees))

samctadmnofit1       <- samctadmno[,c(3,6,11,12,13)]
samctadmnofit1$pre   <- predict.gbm(fit1, samctadmnofit1, n.trees = best.iter)
1 - (sum((samctadms$Total_Orders - samctadms$pre)^2) / sum((samctadms$Total_Orders - mean(samctadms$Total_Orders))^2))

fit2                 <- lm(fg2, data = samctadmno[,c(3,6,11,12,13)])
samctadmnofit2       <- samctadmno[,c(3,6,11,12,13)]
samctadmnofit2$pre   <- predict(fit2)

fit4                 <- lm(fg2, data = samctadm[,c(3,6,11,12,13)])
samctadmfit4         <- samctadm[,c(3,6,11,12,13)]
samctadmfit4$pre     <- predict(fit4)

fit3                 <- glmboost(fg2, samctadmno[,c(3,6,11,12,13)], control = boost_control(mstop = 1000))
samctadmnofit3       <- samctadmno[,c(3,6,11,12,13)]
samctadmnofit3$pre   <- fitted(fit3)

fit5                 <- glmboost(fg2, data = samctadm[,c(3,6,11,12,13)], control = boost_control(mstop = 10000))
samctadmfit5         <- samctadm[,c(3,6,11,12,13)]
samctadmfit5$pre     <- predict(fit5)

samctadms            <- samctadm[,c(3,6,11,12,13)]
samctadms[,4]        <- scale(samctadms[,4], scale = FALSE)
fit6                 <- lm(fg2, data = samctadms)
samctadms$pre        <- predict(fit6)

