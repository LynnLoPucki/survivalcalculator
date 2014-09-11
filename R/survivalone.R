#' survival
#' 
#' Test survival function
#' 
#' @export
#' @param saleintended2var. Optional.
#' @param ebitbeforedummyvar. Optional.
survivalone <- function(saleintended2var = NaN) {
	if (saleintended2var == NaN) {
		stop("You must enter a value for at least one variable.")
	} else {
		mylogit <- glm(emerge ~ saleintended2, data=successdata, family=binomial)
		newdata1 <- with(successdata, data.frame(saleintended2=saleintended2var))
	}
	newdata2 <- predict(mylogit, newdata=newdata1, type="response", se.fit=TRUE)
	efit = newdata2$fit
	lfit = efit - (newdata2$se.fit * 1.96)
	hfit = efit + (newdata2$se.fit * 1.96)
	list(efit=efit, hfit=hfit, lfit=lfit)
}
