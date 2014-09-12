#' Survival
#' 
#' Bankruptcy survival function
#' 
#' @export
#' @param input. Required.
survival <- function(formula, values) {
	useformula <- as.formula(formula)
	usevalues <- as.data.frame(values)
	mylogit <- glm(useformula, data=successdata, family=binomial)
	newdata1 <- with(successdata, usevalues)
	newdata2 <- predict(mylogit, newdata=newdata1, type="response", se.fit=TRUE)
	efit = newdata2$fit
	lfit = efit - (newdata2$se.fit * 1.96)
	hfit = efit + (newdata2$se.fit * 1.96)
	list(efit=efit, hfit=hfit, lfit=lfit)
}
