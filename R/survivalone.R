#' survival
#' 
#' Test survival function
#' 
#' @export
#' @param saleintended2var. Optional.
survivalone <- function(input) {
	inputdata <- as.data.frame(input)
	mylogit <- glm(emerge ~ names(inputdata), data=successdata, family=binomial)
	newdata1 <- with(successdata, inputdata)
	newdata2 <- predict(mylogit, newdata=newdata1, type="response", se.fit=TRUE)
	efit = newdata2$fit
	lfit = efit - (newdata2$se.fit * 1.96)
	hfit = efit + (newdata2$se.fit * 1.96)
	list(efit=efit, hfit=hfit, lfit=lfit, stuff=names(inputdata))
}
