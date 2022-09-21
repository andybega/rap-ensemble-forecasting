#
#		Replication for PITF Irregular Regime Change 
#		21 August 2014
#		Andreas Beger (adbeger@gmail.com)
#
#   The "replication" folder, alongside with this script, contains 2 R packages
#   that are needed but not available on CRAN, several functions, and several
#   data files. The data files include the main data, "irc.data", as well 
#   as intermediate steps in the results like saved theme model estimates, 
#   the ensemble model data object and ensemble model estimates themselves.
# 
#   We have verified that it runs on R xxx on Windows 7
#   Mid-2012 MacBook Pro 2.5 GHz Intel Core i5 OS X 10.9.4 with R 3.1.1
# 
#   Results may differ slightly by system, but we have included intermediate
#   estimates/data needed to exactly replicate the results reported.
#
#   Changelog
#
#   2015-01-30: original replication materials on GitHub
#   2019-04-11: update/fix replication issues
#     - use the CRAN version of spduration
#     - fix inconsistencies in ensembleForecast() and predDate() relating
#       to how the input given to weighInputs() was structured.
#     - Add .tex output for the two tables to output/version control.
#       Note that Table 1, the fit comparison, does not exactly replicate, 
#       in part because back in the day I did not save the logit and 
#       full spduration model fit. 
#

##------------------------------------------------------------------------------
##
##	1. Set up directories and install/load needed packages.
##	
##  You may have to install missing packages from CRAN manually.
##
##------------------------------------------------------------------------------

##
##	CHANGE: path to replication folder
##

# Set to path containing replication folder
library("here")
setwd(here::here("replication"))

# Figures will be saved to a graphics directory. Create if not exists.
if (!file.exists("graphics")) {
	message("Creating 'graphics' subdirectory for figures")
	dir.create("graphics")
}

# Load required libraries
library(EBMAforecast)
library(ggplot2)
library(maptools)
library(plyr)
library(pROC)
library(RColorBrewer)
library(ROCR)
library(sbgcop)
library(spduration)
library(xtable)
library(forecast)


# Script with functions to create ensemble predictions
source("R/ensemble_forecast.R")


##------------------------------------------------------------------------------
##
##	Table 1: Top 10 forecasts for ILC between April and September 2014 
##	(6 months) using March 2014 data.
##
##	To generate the forecasts we take the following steps:
##	  1. Set up data for modeling 
##	  2. Estimate thematic models
##	  3. Calibrate ensemble
##	  4. Calculate forecasts
##
##------------------------------------------------------------------------------

# load imputed data
load("data/irc_data_mod.rda")

# Dates:
# train = data used to estimate models
# calib + test = test data for theme model estimates
# calib = calibration period for ensemble
# test  = test period for ensemble
train.start <- as.Date("2001-03-01")
train.end   <- as.Date("2009-12-31")
calib.start <- as.Date("2010-01-01")
calib.end   <- as.Date("2012-04-30")
test.start  <- calib.end+1
test.end    <- as.Date("2014-03-31")

# Tabulate failures
table(irc.data$failure[with(irc.data, date>=train.start & date<=train.end)])
table(irc.data$failure[with(irc.data, date>=calib.start & date<=calib.end)])
table(irc.data$failure[with(irc.data, date>=test.start & date<=test.end)])


##
##		2. Estimate thematic models
##

# If you want to format the spdur results to play nicely with xtable
# in order to produce latex tables, use table.spdur() 
#source("R/utilities/table.spdur.R")

##
##		Leader characteristics
##

model1 <- spduration::spdur(
	"duration ~ 1 + log10(i.matl.conf.DIStGOV.l1+1) + 
		log10(i.matl.coop.GOVtGOV.l1+1)",
	"atrisk ~ 1 + ldr.irregular + ldr.foreign + log10(mths.in.power+1)",
	data=train)

##
##		Public interactions
##

model2 <- spduration::spdur(
	"duration ~ 1 + log10(i.verb.coop.GOVtGOV.l1+1) + 
		log10(i.verb.conf.GOVtDIS.l1+1) + log10(i.verb.conf.DIStGOV.l1+1) + 
  		log10(i.protest.tGOV.l1+1)",
  	"atrisk ~ 1 + IT.NET.USER.P2.l1 + IT.CEL.SETS.P2.l1 + log10(exclpop.l1+1)",
  	data=train)

##
##		Global instability models
##   

model3 <- spduration::spdur(
	"duration ~ 1 + W.knn4.std.ins.l.count.both.l1 + SP.DYN.LE00.FE.IN.l1 + 
		PARCOMP.l1 ",
	"atrisk ~ 1 + log10(NY.GDP.PCAP.KD.l1) + exclpop.l1",
	data=train)

##
##		Protest model
##

model4 <- spduration::spdur(
	"duration ~ 1 + eth.rel.l.count.l1 + reb.l.count.both.l1 + protest.tALL.l1 + 
	W.gower.pol.reb.l.count.both.l1",
	"atrisk ~ 1+ dom.cris.i.count.l1 + log10(MS.MIL.XPND.GD.ZS.l1)",
	data=train)

##
##		Contagion model
##

model5 <- spduration::spdur(
	"duration ~ 1 + log10(W.centdist.std.opp_resistance.l1+1) +
  		log10(W.centdist.std.repression.l1+1)",
	"atrisk ~ 1 + Amnesty.l1 + log10(ProxElection.l1+1) +
  		log10(opp_resistance.l1+1) + log10(SP.POP.TOTL.l1)",
  	data=train)

##
##		Internal conflict
##

model6 <- spduration::spdur(
  "duration ~ 1 +   log(intratension.l1 *i.protest.tGOV.l1*IT.CEL.SETS.P2.l1 + 1 )+
  log(intratension.l1+1) +
  log(i.protest.tGOV.l1+1) +
  log(IT.CEL.SETS.P2.l1+1) ",
  "atrisk ~ 1 + log10(NY.GDP.PCAP.KD.l1)  +ProxElection.l1 + AUTOC.l1 ",
  data=train)

##
##		Financial stability
##

model7 <- spduration::spdur(
 "duration ~  log(1+ifs__cpi.i) + log(1+ifsinrsv.i)",
 "atrisk ~ 1 + log10(NY.GDP.PCAP.KD.l1) + Amnesty.l1 + log10(ProxElection.l1+1) +
   	log10(opp_resistance.l1+1) + log10(SP.POP.TOTL.l1)",
 data=train)

# Optionally, save model estimates. Can also load Duke model estimates if 
# you want to skip the model estimation above.
#save(model1, model2, model3, model4, model5, model6, model7, 
#	file="data/model_estimates.rda")
#load("data/model_estimates.rda")


##
##		3. Calibrate ensemble
##

# Set number of models and their names. We will need this several times
# for the code below.
n.models <- 7
model.names <- c("Leader char.", "Public disc.", "Global Inst.", "Protest", 
	"Contagion", "Internal conflict", "Financial")


##		Calculate theme model predictions
pr.out  <- data.frame(observed=test$failure)
for (i in 1:n.models) {
	a.model <- get(paste0("model", i))
	p.name  <- paste0("pred.", i)
	print(p.name)
	pr.out[, p.name] <- predict(a.model, newdata=test,  stat="conditional hazard")
	# fix exact 0's to slightly above 0, otherwise EBMA will not work
	pr.out[, p.name] <- replace(pr.out[, p.name], pr.out[, p.name]<=0, 1e-19)
}

# Subset calibration and test period predictions
pr.calib <- pr.out[test$date>=calib.start & test$date<=calib.end, ]
pr.test  <- pr.out[test$date>=test.start  & test$date<=test.end,  ]

# Create ensemble data object
ensemble.data <- makeForecastData(
	.predCalibration 	= pr.calib[, 2:(n.models+1)],
	.outcomeCalibration = pr.calib[, "observed"],
	.predTest 			= pr.test[, 2:(n.models+1)],
	.outcomeTest 		= pr.test[, "observed"],
	.modelNames=model.names
	)

# Optionally, save or load ensemble data object
#save(ensemble.data, file="data/ensemble_data.rda")
#load("data/ensemble_data.rda")

# Calibrate ensemble model
ensemble <- calibrateEnsemble(ensemble.data, model="logit", maxIter=25000, 
	exp=2, const=0.0001)

# This table will show the fit statitics for the ensemble and component models,
# as well as the model weights. The constant and slope parameters are 
# optionally used in EBMA to aggregate theme model predictions, however we 
# do not use them here.
summary(ensemble)

# Optionally, save or load fitted ensemble object
#save(ensemble.data, ensemble, file="data/ensemble.rda")
#load("data/ensemble.rda")


##
##		4. Calculate forecasts
##

# How many months out to predict?
n.ahead <- 6

# Ensemble forecasts for n.ahead months
fcast <- ensembleForecast(n.ahead, n.models=7)
fcast <- fcast[order(fcast[, 1], decreasing=TRUE), ]
head(fcast)

# Add cumulative total
total <- data.frame(
	gwcode=test[format(test$date, "%Y-%m")==format(test.end, "%Y-%m"), ]$ccode,
	total=(1 - apply(1 - ensembleForecast(n.ahead, n.models=7), 1, prod))
	)

# List of top forecasts
top.list <- head(total[order(total$total, decreasing=TRUE), ], 10)
top.list <- data.frame(Country=rownames(top.list), Probability=top.list$total)
tbl <- print(xtable(top.list, caption="Top 10 forecasts for IRC between April and September 2014 (6 months) using March 2014 data", 
	label="tab:forecast"), include.rownames=FALSE)
writeLines(tbl, "tables/table2-top10.tex")


# 2022-09-21:
# save the actual forecasts, not just the top
tbl <- total[order(total$total, decreasing = TRUE), ]
tbl$country <- rownames(tbl)
rownames(tbl) <- NULL
colnames(tbl)[colnames(tbl)=="total"] <- "ebma"
tbl <- tbl[, c("country", "gwcode", "ebma")]
write.csv(tbl, here::here("forecasts-201404-to-201409.csv"), row.names = FALSE)



##------------------------------------------------------------------------------
##
##	Comparison to baseline models
##
##	We will estimate a garbage can logit model with all covariates,
##	and a spdur model with the covariates from the two theme models
##	that have the highest weights in the ensemble.
##
##------------------------------------------------------------------------------

##	Convenience functions that will calculate the AUC and maximum balanced 
##	F-Score for a model

maxF <- function(pred, obs, data=NULL) {
	# Wrapper for ROCR that returns maximum F score as numeric
	if (!is.null(data)) {
		pred <- eval(substitute(pred), envir=data)
		obs  <- eval(substitute(obs), envir=data)
	}
	rocr.df <- prediction(pred, obs)
	perf <- performance(rocr.df, "f")
	res  <- max(perf@y.values[[1]], na.rm=T)
	return(res)
}

auc2 <- function(pred, obs) {
	# Wrapper for ROCR that returns AUC as numeric
	df <- prediction(pred, obs)
	perf <- performance(df, "auc")
	res  <- perf@y.values[[1]]
	return(res)
}

# Empty results table
fit.tab <- data.frame(Model=c("Ensemble", "Logit", "Split-duration"),
	AUC1=NA, F1=NA, AUC2=NA, F2=NA)

# Drop calibration period for ensemble out-of-sample data
test.df <- test[with(test, date>=test.start & date<=test.end), ]

##
##		Calculate ensemble predictions and fit
##

# Calculate predictions
in.sample.preds <- predData(rbind(train, test[with(test, date>=calib.start & date<=calib.end), ]))
oo.sample.preds <- predData(test.df)

fit.tab[1, 2] <- auc2(in.sample.preds$Ensemble, in.sample.preds$obs)
fit.tab[1, 3] <- maxF(in.sample.preds$Ensemble, in.sample.preds$obs)

fit.tab[1, 4] <- auc2(oo.sample.preds$Ensemble, test.df$failure)
fit.tab[1, 5] <- maxF(oo.sample.preds$Ensemble, test.df$failure)


##
##		Logit model with all covariates from the thematic models
##

logit.all <- glm(failure ~ log10(i.matl.conf.DIStGOV.l1+1) + 
	log10(i.matl.coop.GOVtGOV.l1+1) + ldr.irregular + ldr.foreign + 
	log10(mths.in.power+1) + log10(i.verb.coop.GOVtGOV.l1+1) + 
	log10(i.verb.conf.GOVtDIS.l1+1) + log10(i.verb.conf.DIStGOV.l1+1) + 
  	 IT.NET.USER.P2.l1 + IT.CEL.SETS.P2.l1 + 
  	log10(exclpop.l1+1) + + W.knn4.std.ins.l.count.both.l1 + SP.DYN.LE00.FE.IN.l1 + 
	PARCOMP.l1 + log10(NY.GDP.PCAP.KD.l1) + eth.rel.l.count.l1 + reb.l.count.both.l1 + protest.tALL.l1 + 
	W.gower.pol.reb.l.count.both.l1 + dom.cris.i.count.l1 + log10(MS.MIL.XPND.GD.ZS.l1) +
	log10(W.centdist.std.opp_resistance.l1+1) + log10(W.centdist.std.repression.l1+1) +
	Amnesty.l1 + log10(ProxElection.l1+1) + log10(opp_resistance.l1+1) + log10(SP.POP.TOTL.l1) +
  	log10(W.centdist.std.opp_resistance.l1+1) +
  	log10(W.centdist.std.repression.l1+1) + Amnesty.l1 + 
  	log10(ProxElection.l1+1) + log10(opp_resistance.l1+1) + 
  	log10(SP.POP.TOTL.l1) + log(intratension.l1 *i.protest.tGOV.l1*IT.CEL.SETS.P2.l1 + 1 ) +
  	log(intratension.l1+1) + log(i.protest.tGOV.l1+1) + log(IT.CEL.SETS.P2.l1+1)  +
    log10(NY.GDP.PCAP.KD.l1) + ProxElection.l1 + AUTOC.l1, 
	data=train, family="binomial")

summary(logit.all)

# Now let's calculate in-sample AUC and maximum F-score
pred.train <- predict(logit.all, newdata=train, type="response")
fit.tab[2, 2] <- auc2(pred.train, train$failure)
fit.tab[2, 3] <- maxF(pred.train, train$failure)

# Out of sample (test) AUC and maximum F-score
# Although this model doesn't use data from the calibration period,
# we don't use it since the ensemble doesn't use it for OO sample.
pred.test <- predict(logit.all, newdata=test.df, type="response")
fit.tab[2, 4] <- auc2(pred.test, test.df$failure)
fit.tab[2, 5] <- maxF(pred.test, test.df$failure)



##
##		Split-duration model with all covariates
##

spdur.all <- spduration::spdur(
	"duration ~ 1 + log10(W.centdist.std.opp_resistance.l1+1) +
  		log10(W.centdist.std.repression.l1+1) + 
  		log(intratension.l1 *i.protest.tGOV.l1*IT.CEL.SETS.P2.l1 + 1 ) +
  		log(intratension.l1+1) + log(i.protest.tGOV.l1+1) + 
  		log(IT.CEL.SETS.P2.l1+1)",
	"atrisk ~ 1 + Amnesty.l1 + log10(ProxElection.l1+1) +
  		log10(opp_resistance.l1+1) + log10(SP.POP.TOTL.l1) + 
  		log10(NY.GDP.PCAP.KD.l1)  + ProxElection.l1 + AUTOC.l1",
	data=train)

summary(spdur.all)

# Now let's calculate in-sample AUC and maximum F-score
pred.train <- predict(spdur.all, newdata=train, stat="conditional hazard")
fit.tab[3, 2] <- auc2(pred.train, train$failure)
fit.tab[3, 3] <- maxF(pred.train, train$failure)

# Out of sample (test) AUC and maximum F-score
test.df <- test[with(test, date>=test.start & date<=test.end), ]
pred.test <- predict(spdur.all, newdata=test.df, stat="conditional hazard")
fit.tab[3, 4] <- auc2(pred.test, test.df$failure)
fit.tab[3, 5] <- maxF(pred.test, test.df$failure)


tbl <- print(xtable(fit.tab, digits=3))
writeLines(tbl, "tables/table1-fit-comparison.tex")


# 2022-09-21:
# write this out for the Readme in markdown
fit.tab |>
  setNames(c("Model", "AUC_is", "F_is", "AUC_oos", "F_oos")) |>
  knitr::kable(format = "markdown", digits = 3) 


##------------------------------------------------------------------------------
##
##    	Figure 1: Forecast model ROC curves.
##
##------------------------------------------------------------------------------


##
##		Calculate AUC, accuracy, prec., recall, ROC for train/calib/test
##		for all models and ensemble
##

# predData() is from ensemble_forecast.R

train.preds <- predData(train)
calib.preds <- predData(test[with(test, date>=calib.start & date<=calib.end), ])
test.preds  <- predData(test[with(test, date>=test.start & date<=test.end), ])

##
##		Annualize predictions
##

annualizePredData <- function(pred.data) {
	require(lubridate)

	preds <- pred.data
	
	# Create country-year id
	ids <- strsplit(preds$id, " ")
	ids <- data.frame(do.call(rbind, ids), stringsAsFactors=FALSE)
	ids[, 1] <- as.Date(ids[, 1])
	preds$cyear<- paste(year(ids[, 1]), ids[, 2])

	# Create country-year data frame and merge observed events
	preds.cy <- data.frame(cyear=unique(preds$cyear))
	failure.cy <- by(preds$obs, preds$cyear, max)
	failure.cy <- data.frame(cyear=names(failure.cy), obs=as.vector(failure.cy))
	preds.cy <- join(preds.cy, failure.cy)

	# Identify non-id columsn in country-month data
	want <- ! names(preds) %in% c("id", "obs", "cyear")
	preds.mod <- preds[, want]

	# For each want column, aggregate probabilities
	for (i in 1:ncol(preds.mod)) {
		pred.cy <- by(preds.mod[, i], preds$cyear, function(x) 1 - prod(1 -x))
		pred.cy <- data.frame(cyear=names(pred.cy), pred=as.vector(pred.cy))
		colnames(pred.cy) <- c("cyear", colnames(preds.mod)[i])
		preds.cy <- join(preds.cy, pred.cy)
	}

	return(preds.cy)
}

train.preds.cy <- annualizePredData(train.preds)
calib.preds.cy <- annualizePredData(calib.preds)
test.preds.cy  <- annualizePredData(test.preds)


##
##		ROC curves
##


# Generate roc objects (pROC)
calib.cm.roc <- roc(calib.preds$obs, calib.preds$Ensemble)
calib.cy.roc <- roc(calib.preds.cy$obs, calib.preds.cy$Ensemble)
test.cm.roc  <- roc(test.preds$obs, test.preds$Ensemble)
test.cy.roc  <- roc(test.preds.cy$obs, test.preds.cy$Ensemble)

# Plot ROC curve
png("graphics/ensl_roc.png", height=600, width=600)
plot.new()
par(usr=c(1, 0, 0, 1), cex.axis=1.5, cex.main=1.7, cex.lab=1.5)
grid(nx=5)
ticks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
axis(1, at=ticks, labels=rev(as.character(ticks)), col="gray50", cex.axis=1.5)
axis(2, at=ticks, labels=as.character(ticks), col="gray50", las=1, 
	cex.axis=1.5)
box(which="plot", col="gray50", lwd=3)
abline(a=1, b=-1, lty=3, col="darkred", lwd=3)
# Add ROC curves; 
plot.roc(calib.cy.roc, add=TRUE, legacy.axes=TRUE, lwd=3, col="#1f78b4", lty=3)
plot.roc(calib.cm.roc, add=TRUE, legacy.axes=TRUE, lwd=3, col="#a6cee3", lty=3)
plot.roc(test.cy.roc, add=TRUE, legacy.axes=TRUE, lwd=4, col="#a6cee3")
plot.roc(test.cm.roc, add=TRUE, legacy.axes=TRUE, lwd=4, col="#1f78b4")
title(xlab="False positive rate", line=2.5)
title(ylab="True positive rate", main="")
legend(0.45, 0.29, cex=1.5, bty="n",
	legend=c("Random guess", "Test CM", "Test CY", "Calib. CM", "Calib. CY"),
	lty=c(2, 1, 1, 3, 3), lwd=c(3, 4, 4, 3, 3), 
	col=c("darkred", "#1f78b4", "#a6cee3", "#1f78b4", "#a6cee3"))
text(0.05, 0.149, cex=1.5, labels=c(".50\n.84\n.96\n.89\n.91"))
dev.off() 



##------------------------------------------------------------------------------
##
## 		Figure of Archigos irregular leadership changes, March 2001 through 
##		March 2014.
##
##------------------------------------------------------------------------------

# load original, unimputed data
load(file="data/irc-data-v3.rda")

# Source helper function
source("R/worldMap.R")

# Aggregate by country
irc.by.country <- subset(irc.data, select=c('ccode','irr.t'))
irc.by.country <- irc.by.country[complete.cases(irc.by.country),]
irc.by.country <- with(irc.by.country, aggregate(irr.t, by=list(ccode), FUN=sum))
colnames(irc.by.country)<- c('ccode', 'irr.t')

# Plot
png("graphics/map_ILC.png", width=960, height=540)
worldMap('irr.t', 'ccode', irc.by.country, col=colors, cex=1)
dev.off()


##
##		Done
##