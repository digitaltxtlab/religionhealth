X<-subset(data,Complete<3 & is.na(PosAffekt)==FALSE,select =c("deltagerNr","Complete","PosAffekt","IGudLøs"))
data[,'deltagerNr']
# halvfærdige besvarelser frasorteret med is.na(PosAffekt)==F
# filtering
# X<-data[,c("deltagerNr", "Complete","PosAffekt", "perPrayer")]

idx <- X$Complete < 3;
X = X[idx,]
s <- unique(X[,1]) # s består af unikke deltager numre
# max cross corr function
max_ccf<- function(a,b)
{
  d <- ccf(a, b, plot = FALSE)
  cor = d$acf[,,1]
  lag = d$lag[,,1]
  res = data.frame(cor,lag) #dataframe med resultaterne fra ccf
  res_max = res[which.max(abs(res$cor)),] #  matrix med max absolutte cor-værdier
  return(res_max)
}
# apply to data
cor <- rep(0,length(s)) 
lag <- rep(0,length(s))
res.df <- data.frame(cor,lag) #dataframe med samme længde som antallet af (inkluderede) deltagere
for (i in 1:length(s)){
  #  i = 2
  x <- X[X[,1] == s[i],3] # for hver gennemløb af loopet udvælges subset x af X til at være en deltager med tilhørende værdi for den ene variabel der skal krydskorreleres
  y <- X[X[,1] == s[i],4] # for hver gennemløb af loopet udvælges subset y af X til at være en deltager med tilhørende værdi for den anden variabel der skal krydskorreleres
  if (var(x) == 0 || var(y) == 0){
    res.df[i,] <- c(0,0) #Hvis variansen af x eller y =0, så skriv 0 for cor og lag i dataframe res.df
  }else{
    res.df[i,] <- max_ccf(x,y) 
  }
}
res.mat <- as.matrix(res.df) 
res.mat[rowSums(res.mat) != 0,] # her udvælges de rækker hvor summen af cor og lag er forskellig fra 
# one sample t-test
mdl <- t.test(res.mat[,2],mu = 0) #t test: er lag forskellig fra 0. 

mdl 
sd(res.mat[,2])
hist(res.mat[,2])

mdl2<- t.test(res.mat[,1],mu = 0)
mdl2
ggplot(res.df,aes(res.mat[,1]))+geom_histogram(binwidth = 0.1)
