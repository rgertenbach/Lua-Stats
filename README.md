# Lua-Stats
Statistical functions for Lua
# Reference
## Generic Functions
Function|Description
---|---
integral(*f, start, stop, delta, ...*)|Calculates the area under a function *f* from *start* to *stop* in *delta*-sized steps. The optional arguments *...* are passed on to the function to integrate
integralUntil(f, a, start = 0, delta = 0.001, ...)|Integrates a function *f* from *start* at *delta* steps until the absolute area is *a*
factorial(*x*)|Calculates the factorial recursively for integer *x* or using the gamma function if *x* is a float
findX(*y, f, accuracy = 0.001, ...*)|Finds the x needed for *f* to return *y* at a certain *accuracy*, given the optional arguments *...*
gamma(*x*)|the probability density function of the gamma distribution for *x*
beta(*x,y*)|The probability density function of the beta distibution
pValue(*q, f, ...*)| Calculates the p-value of a test statistic coming from the CDF *f*. The optional arguments *...* are passed on to *f*.
map(*t, f*)|Applies a function *f* to every value of a table *t*.
reduce(*t, f*)|Reduces a table *t* using a function *f* that takes two arguments.
in_table(*value, t, key = false*)|True if *value* is in table *t*. Searches for a key if *key* is true
unify(*...*)|Takes an arbitrary number of values and tables and concatenates them into one big table
unique(*t*)|Returns unique entities in table *t*
multiSubscript(*i, ...*)|Returns an Array containing all elements of the provided tables at key *i*

## Arithmetic functions
All functions taking only arbitrary optional arguments *...* concatenate all supplied values into a table and calculate from that.  

Function|Description
---|---
sum(*...*)|Sum
count(*...*)|Count of values
mean(*...*)|Average of values
sumSquares(*...*)|Sum of the squared difference from the mean
var(*...*)|Variance
varPop(*...*)|Variance for the population
sd(*...*)|Standard deviation
sdPop(*...*)|Standard Deviation for the population
cov(*t1, t2*)|Returns the Covariance between tables *t1* and *t1*
cor(*t1, t2, method = "Pearson"*)|Returns the correlaion between two arrays *t1* and *t2*
min(*...*)|Minimum value
max(*...*)|Maximum value
frequency(*t*)|Returns the number of occurrences of each element in a table *t*
sdPooled(*sd1, n1, sd2, n2*)|Calculates the pooled standard deviation
quantile(*t, q*)|Gets the *q*-th quantile of sequence *t*, where 0 >= *t* >= 1
median(*t*)|Median for table *t*
quartile(*t, i*)|Wrapper for quantile returning<br>0: Minimum<br>1: 1st quartile<br>2: Median<br>3: 3rd quartile<br>4: Maximum

## Normal Distribution Functions
Function|Description
---|---
dNorm(*x, mu = 0, sd = 1*)|Probability density function for the normal distribution with mean *mu* and standard deviation *sd* at point *x*
pNorm(*x, mu = 0, sd = 0, accuracy = 0.001*)|Cumulative distribution function for the Normal distribution with mean *mu* and standard deviation *sd* at point *x*
qNorm(*q, accuracy = 0.01*)| Calculates the z-value for *q* for N(0|1)

# T-distribution functions
Function|Description
---|---
dT(*x, df*)|probability density function of the t-distribution at *x* with *df* degrees of freedom
pT(*q, df, accuracy = 0.01*)|CDF of the d-distribution at *x* with *df* degrees of freedom
qT(*p, df*)|t-value corresponding to the *p*-value provided at *df* degrees of freedom

# Chi-square distribution functions
Function|Description
---|---
dChisq(*x, df*)|Probability density function of the chi-square distribution at *x* with *df* degrees of freedom.
pChisq(*q, df*)|CDF of the chi-square distribution at *q* with *df* degrees of freedom
qChisq(*p, df, accuracy = 0.01*)|Chi square value corresponding to the *p* value provided at *df* degrees of freedom down to *accuracy*'s accuracy.

# F distribution functions
Function|Description
---|---
dF(*x, df1, df2*)|Probability density function of *x* at *df1* and *df2* degrees of freedom.
pF(*x, df1, df2*)|CDF of the F distribution of *x* at *df1* and *df2* degrees of freedom.
qF(*p, df1, df2, accuracy = 0.01*)|F-statistic corresponding to the *p*-value at *df1* and *df2* degrees of freedom.

# Tests
Function|Description
---|---
assertTables(*...*)|Asserts that all provided arguments are tables
zValue(*y1, sd1, n1 [, y2, sd2, n2, sameVar = false]*)|Performs a z-test on one or two means
zTest(*t1, t2, sameVar = false*)|Performs a z-test on two sequences of numbers *t1* and *t2*
zTestP(*t1, t2, sameVar = false*)|Returns the p-value of a z-test on *t1* and *t2*
tValue(*y1, sd1, n1 [, y2, sd2, n2, sameVar = false]*)|Performs a t-test on one or two means.
tTest(*t1, t2, sameVar = false*)|Returns the t-value of the comparison of *t1* against *t2*
tTestP(*t1, t2, sameVar = false*)|Returns the p-value of the t-statistic of the comparison of *t1* against *t2*
fValue(*s1, s2*)|Returns the F-value of two variances *s1* and *s2*
fTest(*t1, t2*)|Returns the F-value of the comparison of *t1* against *t2*
fTestP(*t1, t2*)|Returns the p-value of the F-statistic of the comparison of *t1* against *t2*
