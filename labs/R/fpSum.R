# Task: Write an R function called fpSum that adds two numeric vectors and returns a vector that has been rounded to digits significant digits. You can use signif in your code.

fpSum = function(x , y, digits){
  
  return(signif(x + y, digits))
  
}

x = 1.351
y = 2.534
fpSum(x, y, 2 )


k = 7
x = 1.351*10^k
y = 2.534*10^k

fpSum(x ,y, 2)

# We generate some uniform numbers
set.seed(2313)
x1 = runif (5000)
y1 = runif (5000)

# We calculate the absolue differences with digits=2 and plot the density plot
diff1 = abs((x+y)-fpSum(x,y,2))
hist (diff1 )

max(diff1)

# We calculate the absolue differences with digits=3 and plot the density plot
diff1 = abs((x+y)-fpSum(x,y,3))
hist (diff1)
# We calculate the absolue differences with digits=4 and plot the density plot
diff1 = abs((x+y)-fpSum(x,y,4))
hist (diff1)

## Machine epsilon

# beta = 10
# d = digits
# u = 0.5 * beta ^( 1 - d )


# TASK: Now simulate the same number of uniform random values and store them in a vector z.
set.seed(32131)
z1 = runif(5000)

diff2 = abs(fpSum(fpSum(x1,y1,2),z1,2)-(x1+y1+z1))
hist(diff2)

max(diff2)

diff2 = abs(fpSum(fpSum(x1,y1,3),z1,3)-(x1+y1+z1))
hist(diff2)

diff2 = abs(fpSum(fpSum(x1,y1,4),z1,4)-(x1+y1+z1))
hist(diff2)

# Can you guess at a bound for the maximum absolute error for a sequence of k such additions?
# (k/2)*10*(1-d)

