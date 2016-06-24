data = read.csv("data.csv")
X <- data[,c('deltagerNr', 'Complete','Tilfredshed', 'PerBøn')]
# filtering
idx <- X$Complete < 3;
X = X[idx,]
s <- unique(X[,1])
# max cross corr function
max_ccf<- function(a,b)
{
  d <- ccf(a, b, plot = FALSE)
  cor = d$acf[,,1]
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  res_max = res[which.max(abs(res$cor)),]
  return(res_max)
}
# apply to data
cor <- rep(0,length(s))
lag <- rep(0,length(s))
res.df <- data.frame(cor,lag)
for (i in 1:length(s)){
#  i = 2
  x <- X[X[,1] == s[i],3]
  y <- X[X[,1] == s[i],4]
  if (var(x) == 0 || var(y) == 0){
    res.df[i,] <- c(0,0)
  }else{
    res.df[i,] <- max_ccf(x,y)
  }
}
# filter 0 variance
res.mat <- as.matrix(res.df)
res.mat[rowSums(res.mat) != 0,]
# one sample t-test
mdl <- t.test(res.mat[,2],mu = 0)
