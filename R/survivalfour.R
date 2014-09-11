#' survival
#' 
#' Test survival function
#' 
#' @export
#' @param saleintended2. Optional.
survivalfour <- function(saleintended2var = NaN, ebitbeforedummyvar = NaN, equitybeforevar = NaN, manufacturingvar = NaN) {

	if (saleintended2var == NaN && ebitbeforedummyvar == NaN && equitybeforevar == NaN && manufacturingvar == NaN) {
		stop("You must enter a value for at least one variable.")
	} else if (saleintended2var == NaN && ebitbeforedummyvar == NaN && equitybeforevar == NaN && manufacturingvar == NaN) {
		mylogit <- glm(emerge ~ saleintended2 + ebitbeforedummy + equitybefore +  manufacturing, data=successdata, family=binomial)
		newdata1 <- with(successdata, data.frame(saleintended2=saleintended2var, equitybefore=ebitbeforedummyvar, ebitbeforedummy=equitybeforevar, manufacturing=manufacturingvar))
	} else if (saleintended2var == NaN && ebitbeforedummyvar == NaN && equitybeforevar == NaN && manufacturingvar == NaN) {
		mylogit <- glm(emerge ~ saleintended2 + ebitbeforedummy + equitybefore +  manufacturing, data=successdata, family=binomial)
		newdata1 <- with(successdata, data.frame(saleintended2=saleintended2var, equitybefore=ebitbeforedummyvar, ebitbeforedummy=equitybeforevar, manufacturing=manufacturingvar))
	} else if (saleintended2var == NaN && ebitbeforedummyvar == NaN && equitybeforevar == NaN && manufacturingvar == NaN) {
		mylogit <- glm(emerge ~ saleintended2 + ebitbeforedummy + equitybefore +  manufacturing, data=successdata, family=binomial)
		newdata1 <- with(successdata, data.frame(saleintended2=saleintended2var, equitybefore=ebitbeforedummyvar, ebitbeforedummy=equitybeforevar, manufacturing=manufacturingvar))
	} else if (saleintended2var == NaN && ebitbeforedummyvar == NaN && equitybeforevar == NaN && manufacturingvar == NaN) {
		mylogit <- glm(emerge ~ saleintended2 + ebitbeforedummy + equitybefore +  manufacturing, data=successdata, family=binomial)
		newdata1 <- with(successdata, data.frame(saleintended2=saleintended2var, equitybefore=ebitbeforedummyvar, ebitbeforedummy=equitybeforevar, manufacturing=manufacturingvar))
	}

	newdata2 <- predict(mylogit, newdata=newdata1, type="response", se.fit=TRUE)
	efit = newdata2$fit
	lfit = efit - (newdata2$se.fit * 1.96)
	hfit = efit + (newdata2$se.fit * 1.96)

	list(efit=efit, hfit=hfit, lfit=lfit)
}
