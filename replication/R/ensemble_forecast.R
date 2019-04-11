##
##		Functions for EBMA-based forecasts
##		12 May 2014
##		Andreas Beger
##

.makeAdj <- function(x, exp){
	# Bias adjustment for raw input probabilities
    .adjPred <- qlogis(x)
    .negative <- .adjPred<0
	.pos <- .adjPred>1
	.adjPred <- ((1+abs(.adjPred))^(1/exp))-1
	.miss <- is.na(.adjPred)
	.negative[.miss] <- FALSE
	.adjPred[.negative] <- .adjPred[.negative]*(-1)
	#.adjPred[.pos] <- NA
	.adjPred[.miss] <- NA
	.adjPred
}

affineTransform <- function(x, a, b) {
	# Affine transform of y = a + x*b
	y <- a + x * b
	y
}

weighInputs <- function(raw.inputs, ensemble, adjust=FALSE) {
	# Create weighted ensemble model predictions from matrix of raw
	# input probabilities. 
		# Use model parameters or not?
	if (adjust==FALSE) {
		inputs <- raw.inputs
	} else if (adjust==TRUE) {
		useModelParams <- TRUE

		# Adjust model predictions
		exp <- ensemble@exp
		inputs.adj <- lapply(inputs.raw, .makeAdj, exp)
		
		# Model parameter transformaton
		inputs.model <- NULL
		for (i in 1:n.models) {
			model.params <- ensemble@modelParams[, i, ]
			inputs.model[[i]] <- sapply(inputs.adj[[i]], affineTransform, 
			model.params[1], model.params[2])
		}
		inputs <- inputs.model
	}
	
	# Multiply by model weights
	weights <- ensemble@modelWeights

	# 2019-04-11: replace block below with these two lines
	input_preds <- do.call(cbind, inputs)
	ebma_preds <- (input_preds %*% weights)[, 1]
	
	# # Initialize results matrix
	# pred <- matrix(NA, nrow=nrow(inputs[[1]]), ncol=ncol(inputs[[1]]))
	# for (m in 1:ncol(inputs[[1]])) {
	# 	# Matrix of predictions from inputs for one month
	# 	month <- lapply(1:length(inputs), function(i) inputs[[i]][, m])
	# 	month <- do.call(cbind, month)
	# 	pred[, m] <- month %*% weights
	# }
	
	if (adjust==TRUE) {
	  # Convert to probabilities
	  ebma_preds <- plogis(ebma_preds)
	}
	
	return(ebma_preds)
}

# function to create data frame of id, pred, obs for train/calib/test
predData <- function(data.subset, input.names=model.names, stat="conditional hazard") {
	# Implicit input: models[1:7], ensemble
	# List of input model predictions
	inputs.raw <- lapply(1:n.models, function(x) 
		predict(get(paste0("model", x)), newdata = data.subset, stat=stat))
	names(inputs.raw) <- input.names

	# Calculate ensemble prediction
	pred <- weighInputs(inputs.raw, ensemble, adjust=FALSE)

	# Combine input and ensemble predictions with id
	inputs.df <- do.call(cbind, inputs.raw)
	colnames(inputs.df) <- names(inputs.raw)
	res <- data.frame(
		id=data.subset[, "id"], 
		obs=data.subset[, "failure"],
		Ensemble=pred,
		inputs.df,
		stringsAsFactors=FALSE)
	return(res)
}


##
##		Below functions are strictly for actuals forecasts that are based on
##		the last month with observed data.
##

modelForecast <- function(model, n.ahead, pred.data, stat="conditional hazard") {
	# Produce n.aheah months forecasts using model
	# model   - spdur object
	# n.ahead - how many months to forecast ahead
	# date    - which data to use for forecasting?
	data  <- pred.data
	pred  <- forecast(model, pred.data=data, n.ahead=n.ahead, 
		stat=stat)
	rownames(pred) <- data$country
	months <- seq.Date(test.end+1, by="month", length.out=n.ahead)
	months <- format(months, "%Y-%m")
	colnames(pred) <- months
	pred
}

ensembleForecast <- function(n.ahead=6, n.models=7, adjust=FALSE) {
	# Produce n.ahead months forecasts using the ensemble model
	# Implicity input: test, test.end
	# adjust - use EBMA parameters or raw transform using model weights only?
	data <- test[format(test$date, "%Y-%m")==format(test.end, "%Y-%m"), ]
	inputs.raw <- lapply(1:n.models, function(x) 
		modelForecast(get(paste0("model", x)), n.ahead=n.ahead, pred.data=data))
	names(inputs.raw) <- model.names[1:n.models]

	# 2019-04-05: 
	# inputs.raw originally was/is a list(7) where each element is a 164x6 matrix
	# with country rows and h/yearmonth columns
	# make this a list of vectors to match the other changes in weighInputs
	# cram back to this matrix list structure after pred so the rest doesn't break
	cc <- rownames(inputs.raw[[1]])
	ym <- colnames(inputs.raw[[1]])
	inputs.raw <- lapply(inputs.raw, as.vector)
	
	pred <- weighInputs(inputs.raw, ensemble, adjust=adjust)
	
	# 2019-04-05: 
	# cram back into matrix
	pred <- matrix(pred, nrow = length(cc), byrow = FALSE)
	
	# Pretty formatting
	rownames(pred) <- cc
	colnames(pred) <- ym
	
	pred
}