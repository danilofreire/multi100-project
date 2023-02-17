
### R-Code to estimate Synthetic Weights 
### March 2015
### Journal of Conflict Resolution
### The Beheading of Criminal Organizations and the Dynamics of Violence in Mexico ***
### Calderón: Banco de México, Stanford University (email: gabriela.calderon@banxico.org.mx); 
### Robles: Stanford University (email: grobles@stanford.edu); 
### Díaz-Cayeros: Stanford University (email: albertod@stanford.edu); 
### Magaloni: Stanford University (email: magaloni@stanford.edu).

## Clear all
rm(list=ls())

## Working Directory
setwd("/Users/guspeiro/Documents/Poverty Governance Lab/Paper Beheadings/Paper beheadings 2013/regresiones base publica/Replication Materials")

## Load packages
library("Synth")
library("foreign")
library("plyr")

## Read data
data1<-read.dta("replication.data.beheadings.march.15.dta")
data1$idunico <- as.numeric(data1$idunico)
data1$date <- as.numeric(data1$date)

## List of unique numeric identifiers
idunicos<-as.numeric(levels(factor(data1$idunico)))

## We identify the municipalities where a leader was captured and the date of the capture.
## In case that two or more leaders are captured in the same municipality, we take as reference the date of the first capture. 
## Leaders were captured in 14 municipalities. 
date.treat.direct.lider <- data1[which(data1$leader_pub==1),c("idunico","date")]
date.treat.direct.lider <- ddply(date.treat.direct.lider, ~ idunico, function(x) c(idunico = min(x$idunico), date = min(x$date)))
dim(date.treat.direct.lider)

## We identify the municipalities where a lieutenant was captured and the date of the capture.
## In case that two or more lieutenants are captured in the same municipality, we take as reference the date of the first capture. 
## Lieutenants were captured in 70 municipalities.
date.treat.direct.lieu <- data1[which(data1$lieutenant_pub==1),c("idunico","date")]
date.treat.direct.lieu <- ddply(date.treat.direct.lieu, ~ idunico, function(x) c(idunico = min(x$idunico), date = min(x$date)))
dim(date.treat.direct.lieu)

## We identify the neighboring municipalities to those where a leader was captured and the date of the capture.
## In case that two or more leaders are captured in neighboring municipalities, we take as reference the date of the first capture. 
## There are 75 neighboring municipalities to those where a leader was captured.
date.treat.indirect.lider <- data1[which(data1$leader_cont_pub==1),c("idunico","date")]
date.treat.indirect.lider <- ddply(date.treat.indirect.lider, ~ idunico, function(x) c(idunico = min(x$idunico), date = min(x$date)))
dim(date.treat.indirect.lider)

## We identify the neighboring municipalities to those where a lieutenant was captured and the date of capture.
## In case that two or more lieutenants are captured in neighboring municipalities, we take as reference the date of the first capture. 
## There are 339 neighboring municipalities to those where a lieutenant was captured.
date.treat.indirect.lieu <- data1[which(data1$lieutenant_cont_pub==1),c("idunico","date")]
date.treat.indirect.lieu <- ddply(date.treat.indirect.lieu, ~ idunico, function(x) c(idunico = min(x$idunico), date = min(x$date)))

## We create lists of municipalities' numeric identifiers according to the type of treatment (leader vs lieutenant, treated (direct) vs neighboring (indirect)).
treated.direct.lider <- date.treat.direct.lider$idunico
treated.direct.lieu <- date.treat.direct.lieu$idunico
treated.indirect.lider <- date.treat.indirect.lider$idunico
treated.indirect.lieu <- date.treat.indirect.lieu$idunico

## 2049 municipalities do not belong to any of these lists. These municipalities will serve as our control group.
control <- setdiff(idunicos,union(union(union(treated.direct.lider,treated.direct.lieu),treated.indirect.lider),treated.indirect.lieu))
length(control)

### We estimate 8 vectors of synthetic weights across the following dimensions:
# 	1. Captures of leaders vs. Captures of lieutenants	
#	2. Treated municipalities (direct) vs. neighboring municipalities (indirect)							
#	3. Homicides of males between 15 and 39 yo (objective/dto) vs. Homicides among the rest of the population (rest)

### Given computational and time constraints, we estimated each vector of weights separately.
### The following parameters NEED TO BE CHANGED to estimate set of weights.
### No other changes to the code are required.

###################################
### PARAMETERS TO BE SET ##########
###################################

treated 			<- 	treated.direct.lider  				# Numeric identifiers of treated units.
date.treated 		<- date.treat.direct.lider				# Table of numeric identifiers and dates of first captures
colnames(data1)[which(colnames(data1)=="d_homi_male_15_39")] <- "homicidios" # Homicides series
referencia  		<- "weights_treat_lead_dto"				# Reference label
casos 				<- 14									# Number of cases

### The reference labels should be 1) "weights_treat_lead_dto" 2) "weights_treat_lead_rest" 3) "weights_treat_lieu_dto" 4) "weights_treat_lieu_rest" 5) "weights_neigh_lead_dto" 6) "weights_neigh_lead_rest" 7) "weights_neigh_lieu_dto" 8) "weights_neigh_lieu_rest"

### The number of cases should be 1) 14 2) 14 3) 70 4) 70 5) 75 6) 75 7) 339 8) 339

### Homicides series should be 1) "d_homi_male_15_39" 2) "d_homi_not_male_15_39" 3) "d_homi_male_15_39" 4) "d_homi_not_male_15_39" 5) "d_homi_male_15_39" 6) "d_homi_not_male_15_39" 7) "d_homi_male_15_39" 8) "d_homi_not_male_15_39"

### Numeric identifiers should be 1) "treated.direct.lider" 2) "treated.direct.lider" 3) "treated.direct.lieu" 4) "treated.direct.lieu" 5) "treated.indirect.lider" 6) "treated.indirect.lider" 7) "treated.indirect.lieu" 8) "treated.indirect.lieu"

### Tables of first captures should be 1) "date.treat.direct.lider" 2) "date.treat.direct.lider" 3) "date.treat.direct.lieu" 4) "date.treat.direct.lieu" 5) "date.treat.indirect.lider" 6) "date.treat.indirect.lider" 7) "date.treat.indirect.lieu" 8) "date.treat.indirect.lieu"


###################################
###################################
###################################

## Notes:

# For each "treated" municipality, the following loop finds the optimal weights such that the synthetic control municipality has pre-treatment trends in homicides that are maximally similar to those of the treated municipality.
# Eleven predictors were chosen to reflect the short and long term trends in violence in each treated municipality before a capture. The predictors were the monthly number of homicides in the six previous months before the intervention and the average number of homicides in each of the 5 years before this 6-month period.
# Given computational constraints, for each of the 11 pre-treatment periods we identified the 30 municipalities in the set of potential control municipalities with the closest levels of violence in absolute value to the ones of the treated municipality. 
# The control group for each treated municipality was then formed by the union of the 11 sets with 30 municipalities each. 
# A single synthetic control unit for each treated municipality was estimated as an optimal weighted average of the corresponding control group. 
# Finally, after estimating a vector of optimal weights for each treated municipality, we added the weights across vectors and conducted our analysis using the resulting vector of sums.

## For each treated municipality, the following loop:
## 1. Estimates the 11 predictors (pre-treatment periods) for the treated municipality. This vector is stored as "v.referencia".
## 2. Estimates the 11 predictors for each control municipality. This dataset is stored as "base.predictors.controles".
## 3. For each predictor, it finds the 30 control municipalities with the closest levels in absolute value to the ones of the treated municipality.
## 4. Defines the relevant control group for treated municipality "i" as the union of the eleven sets with 30 municipalities each. This vector of numeric identifiers is stored as "controls.i"
## 5. Estimates the vector of optimal weights for each treated municipality using the package "synth". This vector is stored as "weights.i"
## 6. Appends the vector of each iteration to the full matrix of weights ("weights.matrix").

## In few cases, the package "synth" could not find the optimal weights given that the matrix of predictors was nearly singular or at least one predictor had no variation across control units. We used the following criteria to estimate the optimal weights for these cases:
## 1. Estimate weights using 11 predictors: the number of homicides in each of the six previous months before the intervention and the average number of homicides in each of the 5 years before this 6-month period.
## 2. If not possible, estimate weights using 8 predictors: the average number of homicides in each of the 3 previous 2-month periods before the intervention and the average number of homicides in each of the 5 years before this 6-month period.
## 3. If not possible, estimate weights using 7 predictors: the average number of homicides in each of the 2 previous 3-month periods before the intervention and the average number of homicides in each of the 5 years before this 6-month period.
## 4. If not possible, estimate weights using 6 predictors: the average number of homicides in the previous 6-month period before the intervention and the average number of homicides in each of the 5 years before this 6-month period.
## 5. If not possible, give up.
## We were able to estimate synthetic weights for all treated municipalities using this criteria.

## Before the loop starts, we create an empty matrix of weights
## Dimension: Number of control municipalities (2049) x (1 + number of treated municipalities)
weights.matrix <-data.frame(matrix(NA,length(control),1+casos))
weights.matrix[1] <- control
colnames(weights.matrix)[1] <- "control"

## We also create the following lists to keep track of special cases:
ids.try2 <- data.frame(i = NULL, treated = NULL)
ids.try3 <- data.frame(i = NULL, treated = NULL)
ids.try4 <- data.frame(i = NULL, treated = NULL)
ids.try5 <- data.frame(i = NULL, treated = NULL)

###########################
### THE LOOP STARTS HERE ##
###########################

for (i in c(1:length(treated))){
	print(i)

##	1. Estimate the 11 predictors (pre-treatment periods) for the treated municipality. This vector is stored as "v.referencia".
	
### Date of treatment
date.referencia <- date.treated$date[which(date.treated$idunico==treated[i])]

### Homicides series for municipality "i"
data.referencia <- data1[which(data1$idunico== treated[i]),c("idunico","date","homicidios")]

### Predictors
v.referencia <- data.frame(idunico= treated[i],
t1=data.referencia$homicidios[which(data.referencia$date==date.referencia)-1],
t2=data.referencia$homicidios[which(data.referencia$date==date.referencia)-2],
t3=data.referencia$homicidios[which(data.referencia$date==date.referencia)-3],
t4=data.referencia$homicidios[which(data.referencia$date==date.referencia)-4],
t5=data.referencia$homicidios[which(data.referencia$date==date.referencia)-5],
t6=data.referencia$homicidios[which(data.referencia$date==date.referencia)-6],
t1y=mean(data.referencia$homicidios[(which(data.referencia$date==date.referencia)-18):(which(data.referencia$date==date.referencia)-7)]),
t2y=mean(data.referencia$homicidios[(which(data.referencia$date==date.referencia)-30):(which(data.referencia$date==date.referencia)-19)]),
t3y=mean(data.referencia$homicidios[(which(data.referencia$date==date.referencia)-42):(which(data.referencia$date==date.referencia)-31)]),
t4y=mean(data.referencia$homicidios[(which(data.referencia$date==date.referencia)-54):(which(data.referencia$date==date.referencia)-43)]),
t5y=mean(data.referencia$homicidios[(which(data.referencia$date==date.referencia)-66):(which(data.referencia$date==date.referencia)-55)])
)

## 2. Estimate the 11 predictors for each control municipality. This dataset is stored as "base.predictors.controles".
base.predictors.controles <- NULL

for (h in 1:length(control)){
data.referencia.control <- data1[which(data1$idunico==control[h]),c("idunico","date","homicidios")]
v.referencia.control <- data.frame(idunico=control[h],
t1=data.referencia.control$homicidios[which(data.referencia.control$date==date.referencia)-1],
t2=data.referencia.control$homicidios[which(data.referencia.control$date==date.referencia)-2],
t3=data.referencia.control$homicidios[which(data.referencia.control$date==date.referencia)-3],
t4=data.referencia.control$homicidios[which(data.referencia.control$date==date.referencia)-4],
t5=data.referencia.control$homicidios[which(data.referencia.control$date==date.referencia)-5],
t6=data.referencia.control$homicidios[which(data.referencia.control$date==date.referencia)-6],
t1y=mean(data.referencia.control$homicidios[(which(data.referencia.control$date==date.referencia)-18):(which(data.referencia.control$date==date.referencia)-7)]),
t2y=mean(data.referencia.control$homicidios[(which(data.referencia.control$date==date.referencia)-30):(which(data.referencia.control$date==date.referencia)-19)]),
t3y=mean(data.referencia.control$homicidios[(which(data.referencia.control$date==date.referencia)-42):(which(data.referencia.control$date==date.referencia)-31)]),
t4y=mean(data.referencia.control$homicidios[(which(data.referencia.control$date==date.referencia)-54):(which(data.referencia.control$date==date.referencia)-43)]),
t5y=mean(data.referencia.control$homicidios[(which(data.referencia.control$date==date.referencia)-66):(which(data.referencia.control$date==date.referencia)-55)])
)
base.predictors.controles <- rbind(base.predictors.controles,v.referencia.control)		
}
### Note, 2049 controls and 11 predictors

## 3. For each predictor, find the 30 control municipalities with the closest levels in absolute value to the ones of the treated municipality.

sets.control<-NULL
for (j in 2:dim(base.predictors.controles)[2]) {
control.temp <-data.frame(control=c(control))
control.temp$predictor.j <- base.predictors.controles[j]
elec.obs <- as.numeric(v.referencia[j])
control.temp$dif <- abs(control.temp$predictor.j - elec.obs)
control.temp <-data.frame(control.temp[order(control.temp$dif),])
sets.control<- cbind(sets.control,control.temp$control[c(1:30)])
}

## 4. Define the relevant control group for treated municipality "i" as the union of the eleven sets with 30 municipalities each. This vector of numeric identifiers is stored as "controls.i"

controls.i <- as.numeric(levels(factor(as.vector(sets.control))))

## 5. Estimate the vector of optimal weights for each treated municipality using the package "synth". This vector is stored as "weights.i"

## dataprep() creates a dataset that the function synth() uses to estimate optimal weights.

dataprep.try1 <- dataprep(foo = data1,
time.predictors.prior = 456:(date.referencia-1),
special.predictors = list(
list("homicidios", (date.referencia-1), "mean"),
list("homicidios", (date.referencia-2), "mean"),
list("homicidios", (date.referencia-3), "mean"),
list("homicidios", (date.referencia-4), "mean"),
list("homicidios", (date.referencia-5), "mean"),
list("homicidios", (date.referencia-6), "mean"),
list("homicidios", (date.referencia-18):(date.referencia-7), "mean"),
list("homicidios", (date.referencia-30):(date.referencia-19), "mean"),
list("homicidios", (date.referencia-42):(date.referencia-31), "mean"),
list("homicidios", (date.referencia-54):(date.referencia-43), "mean"),
list("homicidios", (date.referencia-66):(date.referencia-55), "mean")),
dependent = "homicidios",
unit.variable = "idunico",
time.variable = "date",
treatment.identifier = treated[i],
controls.identifier = c(controls.i),
time.optimize.ssr = (date.referencia-66):(date.referencia-1),
time.plot = 456:611)

## 5.1. Estimate weights using 11 predictors: the number of homicides in each of the 6 previous months before the intervention and the average number of homicides in each of the 5 years before this 6-month period.
## If not possible, keep track of the numeric identifier for "i" and add it to the list "ids.try2"

synth.out.try1  <- try(synth(data.prep.obj = dataprep.try1,optimxmethod=c('BFGS','Nelder-Mead','nlm')))
if(synth.out.try1[1] == "Error in svd(c) : infinite or missing values in 'x'\n" | synth.out.try1[1] == "Error in synth(data.prep.obj = dataprep.try1, optimxmethod = c(\"BFGS\",  : \n  \n At least one predictor in X0 has no variation across control units. Please remove this predictor.\n"){
	ids.try2.i <- data.frame( i = i , treated = treated[i])  
	ids.try2   <- rbind(ids.try2,ids.try2.i)
	}else{
	synth.tables.try1 <- synth.tab(dataprep.res = dataprep.try1,synth.res = synth.out.try1)
	weights.i <- data.frame(idunico=controls.i,weight=synth.tables.try1$tab.w$w.weights)
}

## 5.2. If 5.1. not possible, estimate weights using 8 predictors: the average number of homicides in each of the 3 previous 2-month periods before the intervention and the average number of homicides in each of the 5 years before this 6-month period.

if(i %in% ids.try2$i){
dataprep.try1 <- dataprep(foo = data1,
time.predictors.prior = 456:(date.referencia-1),
special.predictors = list(
list("homicidios", (date.referencia-2):(date.referencia-1), "mean"),
list("homicidios", (date.referencia-4):(date.referencia-3), "mean"),
list("homicidios", (date.referencia-6):(date.referencia-5), "mean"),
list("homicidios", (date.referencia-18):(date.referencia-7), "mean"),
list("homicidios", (date.referencia-30):(date.referencia-19), "mean"),
list("homicidios", (date.referencia-42):(date.referencia-31), "mean"),
list("homicidios", (date.referencia-54):(date.referencia-43), "mean"),
list("homicidios", (date.referencia-66):(date.referencia-55), "mean")),
dependent = "homicidios",
unit.variable = "idunico",
time.variable = "date",
treatment.identifier = treated[i],
controls.identifier = c(controls.i),
time.optimize.ssr = (date.referencia-66):(date.referencia-1),
time.plot = 456:611)

## If not possible, keep track of the numeric identifier for "i" and add it to the list "ids.try3"

synth.out.try1 <- try(synth(data.prep.obj = dataprep.try1,optimxmethod=c('BFGS','Nelder-Mead','nlm')))
if(synth.out.try1[1] == "Error in svd(c) : infinite or missing values in 'x'\n" | synth.out.try1[1] == "Error in synth(data.prep.obj = dataprep.try1, optimxmethod = c(\"BFGS\",  : \n  \n At least one predictor in X0 has no variation across control units. Please remove this predictor.\n"){
	ids.try3.i <- data.frame( i = i , treated = treated[i])  
	ids.try3   <- rbind(ids.try3,ids.try3.i)
	}else{
	synth.tables.try1 <- synth.tab(dataprep.res = dataprep.try1,synth.res = synth.out.try1)
	weights.i <- data.frame(idunico=controls.i,weight=synth.tables.try1$tab.w$w.weights)
}
}

## 5.3. If 5.2. not possible, estimate weights using 7 predictors: the average number of homicides in each of the 2 previous 3-month periods before the intervention and the average number of homicides in each of the 5 years before this 6-month period.

if(i %in% ids.try3$i){
dataprep.try1 <- dataprep(foo = data1,
time.predictors.prior = 456:(date.referencia-1),
special.predictors = list(
list("homicidios", (date.referencia-3):(date.referencia-1), "mean"),
list("homicidios", (date.referencia-6):(date.referencia-4), "mean"),
list("homicidios", (date.referencia-18):(date.referencia-7), "mean"),
list("homicidios", (date.referencia-30):(date.referencia-19), "mean"),
list("homicidios", (date.referencia-42):(date.referencia-31), "mean"),
list("homicidios", (date.referencia-54):(date.referencia-43), "mean"),
list("homicidios", (date.referencia-66):(date.referencia-55), "mean")),
dependent = "homicidios",
unit.variable = "idunico",
time.variable = "date",
treatment.identifier = treated[i],
controls.identifier = c(controls.i),
time.optimize.ssr = (date.referencia-66):(date.referencia-1),
time.plot = 456:611)

## If not possible, keep track of the numeric identifier for "i" and add it to the list "ids.try4"

synth.out.try1 <- try(synth(data.prep.obj = dataprep.try1,optimxmethod=c('BFGS','Nelder-Mead','nlm')))
if(synth.out.try1[1] == "Error in svd(c) : infinite or missing values in 'x'\n" | synth.out.try1[1] == "Error in synth(data.prep.obj = dataprep.try1, optimxmethod = c(\"BFGS\",  : \n  \n At least one predictor in X0 has no variation across control units. Please remove this predictor.\n"){
	ids.try4.i <- data.frame( i = i , treated = treated[i])  
	ids.try4   <- rbind(ids.try3,ids.try3.i)
	}else{
	synth.tables.try1 <- synth.tab(dataprep.res = dataprep.try1,synth.res = synth.out.try1)
	weights.i <- data.frame(idunico=controls.i,weight=synth.tables.try1$tab.w$w.weights)
}
}

## 5.4. If 5.3. not possible, estimate weights using 6 predictors: the average number of homicides in the previous 6-month period before the intervention and the average number of homicides in each of the 5 years before this 6-month period.

if(i %in% ids.try4$i){
dataprep.try1 <- dataprep(foo = data1,
time.predictors.prior = 456:(date.referencia-1),
special.predictors = list(
list("homicidios", (date.referencia-6):(date.referencia-1), "mean"),
list("homicidios", (date.referencia-18):(date.referencia-7), "mean"),
list("homicidios", (date.referencia-30):(date.referencia-19), "mean"),
list("homicidios", (date.referencia-42):(date.referencia-31), "mean"),
list("homicidios", (date.referencia-54):(date.referencia-43), "mean"),
list("homicidios", (date.referencia-66):(date.referencia-55), "mean")),
dependent = "homicidios",
unit.variable = "idunico",
time.variable = "date",
treatment.identifier = treated[i],
controls.identifier = c(controls.i),
time.optimize.ssr = (date.referencia-66):(date.referencia-1),
time.plot = 456:611)

## If not possible, keep track of the numeric identifier for "i" and add it to the list "ids.try5"

synth.out.try1 <- try(synth(data.prep.obj = dataprep.try1,optimxmethod=c('BFGS','Nelder-Mead','nlm')))
if(synth.out.try1[1] == "Error in svd(c) : infinite or missing values in 'x'\n" | synth.out.try1[1] == "Error in synth(data.prep.obj = dataprep.try1, optimxmethod = c(\"BFGS\",  : \n  \n At least one predictor in X0 has no variation across control units. Please remove this predictor.\n"){
	ids.try5.i <- data.frame( i = i , treated = treated[i])  
	ids.try5   <- rbind(ids.try3,ids.try3.i)
	weights.i  <- data.frame(idunico=controls.i,weight=NA)
	}else{
	synth.tables.try1 <- synth.tab(dataprep.res = dataprep.try1,synth.res = synth.out.try1)
	weights.i <- data.frame(idunico=controls.i,weight=synth.tables.try1$tab.w$w.weights)
}
}

## 5.5. If 5.4. not possible, give up. This was never the case.

## 6. Append the vector of weights of each iteration ("weights.i") to the full matrix of weights ("weights.matrix").
weights.matrix[,i+1] <- weights.i$weight[match(weights.matrix$control,weights.i$idunico)]
weights.matrix[,i+1][which(is.na(weights.matrix[,i+1])==TRUE)]<-0

### Change the corresponding column's name using i's numeric identifier preceded by the letter "m"
title <- paste("m",treated[i],sep = "")
colnames(weights.matrix)[i+1]<-title

}

###########################
### THE LOOP ENDS HERE ####
###########################

### Exclude the vector of weights for Ciudad Juarez, if any, from the sample
aux.matrix.weights <- weights.matrix
names <- colnames(aux.matrix.weights)
names <- setdiff(names, "m8037")
aux.matrix.weights <-aux.matrix.weights[,names]

### Create a vector of weights for the full sample (treated + control municipalities)
#		 Weights for treated municipalities are equal to 1.
#		 Weights for control municipalities are equal to the sum of weights across vectors (columns).
#		 Weights for neither treated or control municipalities are equal to 0.

### Appending rows of treated and neighboring municipalities to the matrix of weights.
weights.all <-  data.frame(idunico=idunicos)
weights.all <-  merge(weights.all, aux.matrix.weights, by.x = "idunico", by.y= "control", all=TRUE)

### Replacing NA's with zeroes
for (j in 1:dim(weights.all)[2]){
weights.all[which(is.na(weights.all[1:dim(weights.all)[1],j])==TRUE),j]<-0
check[j]<-sum(weights.all[1:dim(weights.all)[1],j])
}

### Adding weights across vectors (columns).
weights.all$weights <-rowSums(weights.all[,2:dim(weights.all)[2]])

### Weights for treated municipalities are equal to 1.
for (k in 1:length(treated)){
	weights.all$weights[which(weights.all$idunico== treated[k])]<- 1
}

### Ciudad Juarez is excluded from the sample and has a weight of 0.
weights.all$weights[which(weights.all$idunico==8037)] <- 0

### Extract and save the final vector of weights
vector.final <- weights.all[,c("idunico","weights")]

### Change the name of the column
colnames(vector.final)[2] <- referencia
title3 <- paste0(referencia,".dta")

### Export the vector of weights to Stata.
write.dta(vector.final,title3)

