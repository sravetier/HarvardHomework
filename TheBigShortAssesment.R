library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

#Questions 1 and 2: Insurance rates, part 1
#Use the information below as you answer this 6-part question.
#An insurance company offers a one-year term life insurance policy that pays $150,000 in the event of death within one year.
#The premium (annual cost) for this policy for a 50 year old female is $1,150. Suppose that in the event of a claim,
#the company forfeits the premium and loses a total of $150,000, and if there is no claim the company gains the premium amount
#of $1,150. The company plans to sell 1,000 policies to this demographic.

#Question 1a
#The death_prob data frame from the dslabs package contains information about the estimated probability of death within 1 year
#(prob) for different ages and sexes.
#Use death_prob to determine the death probability of a 50 year old female, p.
p<-death_prob%>%
  filter(age==50,sex=="Female")%>%
  pull(prob)
p

#Question 1b
#The loss in the event of the policy holder's death is -$150,000 and the gain if the policy holder
#remains alive is the premium $1,150.
#What is the expected value of the company's net profit on one policy for a 50 year old female?
loss<- -150000
gain<- 1150
mu<-p*loss+(1-p)*gain
mu
  
#Question 1c
#Calculate the standard error of the profit on one policy for a 50 year old female.
se<-abs(gain-loss)*sqrt((1-p)*p)
se

#Question 1d
#What is the expected value of the company's profit over all 1,000 policies for 50 year old females?
n=1000
avg_n=mu*n
avg_n

#Question 1e
#What is the standard error of the sum of the expected value over all 1,000 policies for 50 year old females?
se_n<-sqrt(n)*abs(gain-loss)*sqrt((1-p)*p)
se_n

#Question 1f
#Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of 1,000 policies.
p<-pnorm(0,avg_n,se_n)

#50 year old males have a different probability of death than 50 year old females. We will calculate a profitable premium for
#50 year old males in the following four-part question.
#Question 2a
#Use death_prob to determine the probability of death within one year for a 50 year old male.
head(death_prob)
p_death_m<-death_prob%>%
  filter(age==50,sex=="Male")%>%
  pull(prob)
p_death_m

#Question 2b
#Suppose the company wants its expected profits from 1,000 50 year old males with $150,000 life insurance policies to be $700,000.
#Use the formula for expected value of the sum of draws with the following values and solve for the premium  b :
#E[S]=??S=700000 
#n=1000 
#p=death probability of age 50 male 
#a=150000 
#b=premium 
#What premium should be charged?
n<- 1000
loss<- -150000
target<- 700000
b <- ((target/n) - loss*p_death_m)/(1-p_death_m)
b

#Question 2c
#Using the new 50 year old male premium rate, calculate the standard error of the sum of 1,000 premiums.
se_n<-sqrt(n)*abs(b-loss)*sqrt((1-p_death_m)*p_death_m)
se_n

#Question 2d
#What is the probability of losing money on a series of 1,000 policies to 50 year old males?
avg<-p_death_m*loss+(1-p_death_m)*b
avg_n<-avg*n
p<-pnorm(0,avg_n,se_n)
p

#Questions 3 and 4: insurance rates, part 2
#Life insurance rates are calculated using mortality statistics from the recent past.
#They are priced such that companies are almost assured to profit as long as the probability of death remains similar.
#If an event occurs that changes the probability of death in a given age group, the company risks significant losses.

#In this 6-part question, we'll look at a scenario in which a lethal pandemic disease increases the probability of
#death within 1 year for a 50 year old to .015. Unable to predict the outbreak, the company has sold 1,000 $150,000 life
#insurance policies for $1,150.

#Question 3a
#What is the expected value of the company's profits over 1,000 policies?
n= 1000
p_death<- 0.015
prime<- 1150
penalty<- -150000
mu<-p_death*penalty+(1-p_death)*prime
mu_n<-mu*n
mu_n

#Question 3b
#What is the standard error of the expected value of the company's profits over 1,000 policies?
se<-sqrt(n)*abs(prime-penalty)*sqrt(p_death*(1-p_death))
se

#Question 3c
#What is the probability of the company losing money?
p<-pnorm(0,mu_n,se)
p

#Question 3d
#Suppose the company can afford to sustain one-time losses of $1 million, but larger losses will force it to go out of business.
#What is the probability of losing more than $1 million?
pnorm(-1000000,mu_n,se)

#Question 3e
#Investigate death probabilities p <- seq(.01, .03, .001).
#What is the lowest death probability for which the chance of losing money exceeds 90%?
p<-seq(.01,.03,.001)
f <- function(x){
  mu <- n * (penalty*x + prime*(1-x))
  err <- sqrt(n) * abs(penalty - prime) * sqrt(x*(1 - x))
  pnorm(0, mu, err)
}
test<-sapply(p, FUN=f)
tab<-data.frame(p,test)
tab

#Question 3f
#Investigate death probabilities p <- seq(.01, .03, .0025).
#What is the lowest death probability for which the chance of losing over $1 million exceeds 90%?
p <- seq(.01, .03, .0025)
f <- function(x){
  mu <- n * (penalty*x + prime*(1-x))
  err <- sqrt(n) * abs(penalty - prime) * sqrt(x*(1 - x))
  pnorm(-1000000, mu, err)
}
test<-sapply(p, FUN=f)
tab<-data.frame(p,test)
tab

#Question 4, which has two parts, continues the scenario from Question 3.
#Question 4a
#Define a sampling model for simulating the total profit over 1,000 loans with probability of claim p_loss = .015, loss
#of -$150,000 on a claim, and profit of $1,150 when there is no claim. Set the seed to 25, then run the model once.
#(IMPORTANT! If you use R 3.6 or later, you will need to use the command set.seed(x, sample.kind = "Rounding")
#instead of set.seed(x).

#What is the reported profit (or loss) in millions (that is, divided by  10^6 )?
p_loss<-.015
loss<- -150000
prime<- 1150
n=1000

set.seed(25)
X<-sample(c(loss,prime),n,replace=T,prob=c(p_loss,1-p_loss))
sum(X)/10^6

#Set the seed to 27, then run a Monte Carlo simulation of your sampling model with 10,000 replicates to simulate
#the range of profits/losses over 1,000 loans.
#(IMPORTANT! If you use R 3.6 or later, you will need to use the command set.seed(x, sample.kind = "Rounding")
#instead of set.seed(x). Your R version will be printed at the top of the Console window when you start RStudio.)
#What is the observed probability of losing $1 million or more?
B=10000

set.seed(27)
prob<-replicate(B,{
  X<-sample(c(prime,loss),n,replace=T,prob=c(1-p_loss,p_loss))
  sum(X)/10^6  
})
sum(prob<=-1)/B

#Questions 5 and 6: Insurance rates, part 3
#Question 5, which has 4 parts, continues the pandemic scenario from Questions 3 and 4.
#Suppose that there is a massive demand for life insurance due to the pandemic, and the company wants to find a
#premium cost for which the probability of losing money is under 5%, assuming the death rate stays stable at  p=0.015 .


#Question 5a
#Calculate the premium required for a 5% chance of losing money given  n=1000  loans, probability of death  p=0.015,
#and loss per claim  l=???150000 . Save this premium as x for use in further questions.
p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

#Question 5b
#What is the expected profit per policy at this rate?
esp<-(1-p)*x+p*l
esp

##Question 5c
#What is the expected profit over 1,000 policies?
n*esp

#Question 5d
#Run a Monte Carlo simulation with B=10000 to determine the probability of losing money on 1,000 policies given the new premium x,
#loss on a claim of $150,000, and probability of claim  p=.015 . Set the seed to 28 before running your simulation.
#What is the probability of losing money here?
B=10000
set.seed(28)
prob<-replicate(B,{
  y<-sample(c(x,l),n,replace=T,prob=c(1-p,p))
  sum(y)
})
sum(prob<=0)/B

#Question 6a
#The company cannot predict whether the pandemic death rate will stay stable. Set the seed to 29,
#then write a Monte Carlo simulation that for each of  B=10000  iterations:
  
#randomly changes  p  by adding a value between -0.01 and 0.01 with sample(seq(-0.01, 0.01, length = 100), 1)
#uses the new random  p  to generate a sample of  n=1,000  policies with premium x and loss per claim  l=???150000 
#returns the profit over  n  policies (sum of random variable)

#The outcome should be a vector of  B  total profits. Use the results of the Monte Carlo simulation to answer the
#following three questions.

#(Hint: Use the process from lecture for modeling a situation for loans that changes the probability of default for
#all borrowers simultaneously.)

#What is the expected value over 1,000 policies?
set.seed(29)
B<-10000
n<-1000
l<- -150000
p<-0.015
x<-3268

prob<-replicate(B,{
  new_p<-p+sample(seq(-0.01, 0.01, length = 100), 1)
  y<-sample(c(x,l),n,replace=T,prob=c(1-new_p,new_p))
  sum(y)
})
mean(prob)

set.seed(29)
B<-10000
n<-1000
l<- -150000
p<-0.015
x<-3268.063

prob<-replicate(B,{
  new_p<-p+sample(seq(-0.01, 0.01, length = 100), 1)
  y<-sample(c(x,l),n,replace=T,prob=c(1-new_p,new_p))
  sum(y)
})
mean(prob)

#Question 6b
#What is the probability of losing money?
sum(prob<0)/B

#Question 6c
#What is the probability of losing more than $1 million?
mean(prob< -1000000)