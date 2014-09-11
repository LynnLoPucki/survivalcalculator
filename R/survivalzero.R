survival <- function() {
	mylogit <- glm(emerge ~ saleintended2 + ebitbeforedummy + equitybefore +  manufacturing, data=successdata, family=binomial)
	newdata1 <- with(successdata, data.frame(saleintended2=0, equitybefore=0, ebitbeforedummy=1, manufacturing=1))
	newdata2 <- predict(mylogit, newdata=newdata1, type="response", se.fit=TRUE)
	efit = newdata2$fit
	lfit = efit - (newdata2$se.fit * 1.96)
	hfit = efit + (newdata2$se.fit * 1.96)
	list(efit=efit, hfit=hfit, lfit=lfit)
}
