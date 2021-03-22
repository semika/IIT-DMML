install.packages("neuralnet")

TKS = c(20, 10, 30, 20, 80, 30)
CSS = c(50, 56, 40, 90, 80, 60)
placed = c(1,0,0,1,1,1)

df = data.frame(TKS, CSS, placed)
df

require(neuralnet)

nn = neuralnet(placed~TKS+CSS, data=df, hidden=4, act.fct = "logistic", linear.output = FALSE)
plot(nn)

#installed.packages()[1:5,]

#?installed.packages()

?neuralnet
?paste0
?compute
?rbind
?metrics
?kable
?rollmean
