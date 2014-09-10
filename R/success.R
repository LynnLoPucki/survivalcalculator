#' Hello World
#' 
#' Basic hello world function to be called from the demo app
#' 
#' @export
#' @param myname your name. Required.
hello <- function(myname = ""){
  if(myname == ""){
    stop("Tell me your name!")
  }
  list(
    message = paste("hello", myname, "! This is", R.Version()$version.string)
  )
}


mydata <- read.csv("data/successdata.csv")

#The variables names after "~" come from the calculator input.
mylogit <- glm(emerge ~ saleintended2 + ebitbeforedummy + equitybefore +  manufacturing, data=mydata, family=binomial)
summary(mylogit)

#The values after "=" come from the calculator input.
newdata1 <-with(mydata, data.frame(saleintended2=0, equitybefore=0, ebitbeforedummy = 1, manufacturing=1))
newdata2 <- predict(mylogit, newdata=newdata1, type="response", se.fit=TRUE)

#This is the predicted probability of Success
efit = newdata2$fit
#These are the 95% lower and upper confidence intervals of efit
lfit = efit - (newdata2$se.fit * 1.96)
hfit = efit + (newdata2$se.fit * 1.96)

#these are the values that should be returned to the user.
efit # This is the predicted value
lfit # This is the lower bound
hfit # This is the upper bound
