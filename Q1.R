#we create a function for calculating the price
price = function(C,face,n,y){
P = 0 #create Price value
e = c(rep(0,n)) # create a vector of 0
if(length(y) <= n){
y = append(y,c(rep(0,n-length(y))),after = length(y)) # add 0 to the interest rate vector if it is not existed in the time of current payment.
} else {}
  for (i in 1:n) {
    if(y[i] !=0){ # check if the interest rate at that payment is existed or not
    e[i] = C*exp(-y[i]*i) # store result in the vector
    } else{
    y[i] = y[(i-1)] #if the interest rate for that payment is not existed, then take the last interest rate
    e[i] = C*exp(-y[i]*i) # store result in the vector
    }
  }
E = sum(e)  
P = E + (face*exp(-y[n]*n))
cat("the Price should be", P)
} 
# Price of the bond over the period
# this code required user input value of coupon C, face value F
# total number of coupon payments n
# and the vector of interest rates y which contains all the interest rate in time of each payments

#We take an example with C = 1, F = 200, 5 payments and the interest rate record for the first 3 payments is y =(0.01,0.02,0.04)
price(1,200,5,c(0.01,0.02,0.04))