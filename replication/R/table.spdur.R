# Format split-duration results so they play nice with xtable

table.spdur <- function(model) {
	# Format spdur estimates summary for pretty printing with xtable
	dur   <- summary(model)$duration[, c(1, 4)]
	risk  <- summary(model)$split[, c(1, 4)]
	alpha <- summary(model)$alpha[, c(1, 4)]
	res <- data.frame(rbind(dur, alpha, risk))
	res
}