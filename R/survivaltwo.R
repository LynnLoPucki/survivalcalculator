#' survival
#' 
#' Test survival function
#' 
#' @export
#' @param saleintended2var. Optional.
#' @param ebitbeforedummyvar. Optional.
survivaltwo <- function(saleintended2var = NaN, ebitbeforedummyvar = NaN) {
	if (saleintended2var == NaN && ebitbeforedummyvar == NaN) {
		stop("You must enter a value for at least one variable.")
	} else if (ebitbeforedummyvar == NaN) {
		mylogit <- glm(emerge ~ saleintended2, data=successdata, family=binomial)
		newdata1 <- with(successdata, data.frame(saleintended2=saleintended2var))
	} else if (saleintended2var == NaN) {
		mylogit <- glm(emerge ~ ebitbeforedummy, data=successdata, family=binomial)
		newdata1 <- with(successdata, data.frame(equitybefore=ebitbeforedummyvar))
	} else {
		mylogit <- glm(emerge ~ saleintended2 + ebitbeforedummy, data=successdata, family=binomial)
		newdata1 <- with(successdata, data.frame(saleintended2=saleintended2var, equitybefore=ebitbeforedummyvar))
	}
	newdata2 <- predict(mylogit, newdata=newdata1, type="response", se.fit=TRUE)
	efit = newdata2$fit
	lfit = efit - (newdata2$se.fit * 1.96)
	hfit = efit + (newdata2$se.fit * 1.96)
	list(efit=efit, hfit=hfit, lfit=lfit)
}
