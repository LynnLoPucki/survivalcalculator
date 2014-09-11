#' Success
#' 
#' Test success function
#' 
#' @export
#' @param saleintended2. Optional.
hello <- function(saleintended2var = 1) {
# The variables names after "~" come from the calculator input.
	mylogit <- glm(emerge ~ saleintended2 + ebitbeforedummy + equitybefore +  manufacturing, data=successdata, family=binomial)
#	summary(mylogit)

# The values after "=" come from the calculator input.
	newdata1 <- with(successdata, data.frame(saleintended2=0, equitybefore=0, ebitbeforedummy=1, manufacturing=1))
	newdata2 <- predict(mylogit, newdata=newdata1, type="response", se.fit=TRUE)

# This is the predicted probability of Success
	efit = newdata2$fit
# These are the 95% lower and upper confidence intervals of efit
	lfit = efit - (newdata2$se.fit * 1.96)
	hfit = efit + (newdata2$se.fit * 1.96)

# These are the values that should be returned to the user.
#	efit # This is the predicted value
#	lfit # This is the lower bound
#	hfit # This is the upper bound

	list( message = paste(efit, ",", hfit, ",", lfit) )
}
