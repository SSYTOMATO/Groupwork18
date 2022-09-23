#Exercise 2.01

#-------------------------------------------------------------
#(i) Store outcomes in object R
#-------------------------------------------------------------

#Any one of the following

R <- 0:36

R <- seq(0,36)      
R <- seq(0,36,1)    
R <- seq(0,36,by=1)

#could use R <- c(0,1,2,3,4,...,36) but would have to enter all 37 numbers...

#-------------------------------------------------------------
#(ii) Calculating probabilities
#-------------------------------------------------------------

#(a) P(R <= 20)

length(R[R<20])/length(R)

#answer: 0.5405405

#(b) P(R >= 10)

length(R[R>=10])/length(R)

#answer: 0.7297297

#(c) P(3<R<=9)

length(R[R>3&R<=9])/length(R)

#answer: 0.1621622

#-------------------------------------------------------------
#(iii) Simulation
#-------------------------------------------------------------

set.seed(37)
S1 <- sample(R, 1000, replace=TRUE)

#other equivalent versions include:

set.seed(37)
sample(R, size=1000, replace=TRUE)

set.seed(37)
sample(R, 1000, TRUE)

#-------------------------------------------------------------
#(iv) Table
#-------------------------------------------------------------

table(S1)

#-------------------------------------------------------------
#(v) Histogram
#-------------------------------------------------------------

hist(S1,breaks=(-0.5:40.5))#可以多尝试几个数，反正能装下就好，以0.5开始就是在中间 如果想让一一对应就有多少个数 分多少个空就好 在example201中有
              

#The above covers the whole range even though some values did not occur in our simulation

#-------------------------------------------------------------
#(vi) Empirical probabilities
#-------------------------------------------------------------

#(a) P(R <= 20)

length(S1[S1<20])/length(S1)

#answer: 0.563
#version 3.6 onwards answer: 0.552

#(b) P(R >= 10)

length(S1[S1>=10])/length(S1)

#answer: 0.704
#version 3.6 onwards answer: 0.729

#(c) P(3<R<=9)

length(S1[S1>3&S1<=9])/length(S1)

#answer: 0.175
#version 3.6 onwards answer: 0.153

#-------------------------------------------------------------
#(vii) Empirical moments
#-------------------------------------------------------------

mean(S1)

#answer: 17.424
#version 3.6 onwards answer: 17.865

median(S1)

#answer: 17
#version 3.6 onwards answer: 18

sd(S1)

#answer: 10.73636
#version 3.6 onwards answer: 10.6193

skew <- sum((S1-mean(S1))^3)/length(S1)

skew/sd(S1)^3

#or

skew/var(S1)^1.5

#answer: 0.04215146
#version 3.6 onwards answer: 0.01744707