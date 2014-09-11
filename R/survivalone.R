#' survival
#' 
#' Test survival function
#' 
#' @export
#' @param saleintended2var. Optional.
#' @param ebitbeforedummyvar. Optional.
survivalone <- function(saleintended2var = "") {
	if (saleintended2var == "") {
		stop("You must enter a value for at least one variable.")
	} else {
		mylogit <- glm(emerge ~ saleintended2, data=successdata, family=binomial)
		newdata1 <- with(successdata, data.frame(saleintended2=1))
	}
	newdata2 <- predict(mylogit, newdata=newdata1, type="response", se.fit=TRUE)
	efit = newdata2$fit
	lfit = efit - (newdata2$se.fit * 1.96)
	hfit = efit + (newdata2$se.fit * 1.96)
	list(efit=efit, hfit=hfit, lfit=lfit)
}
