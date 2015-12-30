--[[
TODO: optimize findX for chi square
TODO: Make findX automaitcally find bounds
TODO: Type checking
TODO: Sample Size
TODO: F-Distribution and F-Test
TODO: Chi-Square test
TODO: Add more quantile algorithms 
      use: http://127.0.0.1:11774/library/stats/html/quantile.html
]]


--[[ Generics ]]--

-- Integrates a function from start to stop in delta sized steps
local function integral(f, start, stop, delta, ...)
    local delta = delta or 1e-5
    local area = 0
    for i = start, stop, delta do
      area = area + f(i, unpack({...})) * delta
    end
    return area
  end


-- Calculates the factorial of a number n recursively
local function factorial(x)
  if (x == 1) then
    return x
  elseif (x % 1 == 0) then
    return x * (factorial(x - 1))
  else 
    return gamma(x - 1)
  end
end


-- finds the x neded for a givnen function f
-- Need to find a way to pass min and max bounds for estimator
local function findX(y, f, accuracy, ...)
  assert(y ~= nil, "No y value provided")
  assert(f ~= nil, "No function provided")
  accuracy = accuracy or 0.001

  local minX, maxX, yVal, xVal = -100, 100, 0, 0

  while (maxX - minX > accuracy) do 
    yVal = f(xVal, unpack({...}))
    if (yVal > y) then
      maxX = xVal
    else
      minX = xVal
    end 
    xVal = (maxX + minX) / 2
  end 
  return xVal
end 


-- I have no idea what the hell is going on here. Taken from:
-- http://rosettacode.org/wiki/Gamma_function#Lua
local function gamma(x)
  local gamma =  0.577215664901
  local coeff = -0.65587807152056
  local quad  = -0.042002635033944
  local qui   =  0.16653861138228
  local set   = -0.042197734555571

  local function recigamma(z)
    return z + gamma * z^2 + coeff * z^3 + quad * z^4 + qui * z^5 + set * z^6
  end
   
  local function gammafunc(z)
    if z == 1 then return 1
    elseif math.abs(z) <= 0.5 then return 1 / recigamma(z)
    else return (z - 1) * gammafunc(z-1)
    end
  end

  return gammafunc(x)
end 


-- p-value of a quantile q of a probability function f
local function pValue(q, f, ...)
  if (q == nil) then
    return nil
  else
    return math.abs(1 - math.abs(f(q, unpack({...})) - f(-q, unpack({...}))))
  end
end


-- Simple map function
local function map(t, f)
  local output = t

  for i, e in ipairs(t) do
    output[i] = f(e)
  end
  return output
end 


-- Simple reduce function
local function reduce(t, f)
  local result

  for i, value in ipairs(t) do
    if i == 1 then
      result = value
    else
      result = f(result, value)
    end 
  end
  return result
end 


-- Concatenates tables and scalars into one list
local function unify(...)
  local output = {}
  for i, element in ipairs({...}) do
    if type(element) == 'number' then
      table.insert(output, element)
    elseif type(element) == 'table' then
      for j, row in ipairs(element) do
        table.insert(output, row)
      end
    end 
  end 
  return output
end 


--[[ Basic Arithmetic functions needed for aggregate functions ]]--
local function sum(...) 
  return reduce(unify(...), function(a, b) return a + b end)
end 


local function count(...) 
  return #unify(...) 
end 


local function mean(...) 
  return sum(...) / count(...) 
end 


local function sumSquares(...)
  local data = unify(...)
  local mu = mean(data)

  return sum(map(data, function(x) return (x - mu)^2 end))  
end 


local function var(...)
  return sumSquares(...) / (count(...) - 1)
end


local function varPop(...)
  return sumSquares(...) / count(...)
end 


local function sd(...)
  return math.sqrt(var(...))
end 


local function sdPop(...)
  return math.sqrt(varPop(...))
end


local function min(...)
  local data = unify(...)
  
  table.sort(data)
  return data[1]
end 


local function max(...)
  local data = unify(...)
  table.sort(data)
  return data[#data]
end 


-- Calculates the quantile
-- Currently uses the weighted mean of the two values the position is inbetween
local function quantile(t, q)
  assert(q >= 0 and q <= 1, "Quantile must be between 0 and 1")
  table.sort(t)
  local position = #t * q + 0.5
  local mod = position % 1

  if position < 1 then 
    return t[1]
  elseif position > #t then
    return t[#t]
  elseif mod == 0 then
    return t[position]
    else
      return mod * t[math.ceil(position)] +
             (1 - mod) * t[math.floor(position)] 
  end 
end 


local function median(t)
  return quantile(t, 0.5)
end


local function quartile(t, i)
  local quartiles = {0, 0.25, 0.5, 0.75, 1}
  if i == 1 then 
    return min(t)
  elseif i== 5 then
    return max(t)
  else 
    return quantile(t, quartiles[i])
  end 
end 


--[[ Normal Distribution Functions ]]--

-- Probability Density function of a Normal Distribution
local function dNorm(x, mu, sd)
  local mu = mu or 0
  local sd = sd or 1

  return (1 / 
         (sd * math.sqrt(2 * math.pi))) * 
         math.exp(-(((x - mu) * (x - mu)) / (2 * sd^2)))
end


-- CDF of a normal distribution
local function pNorm(q, mu, sd, accuracy)
  mu = mu or 0
  sd = sd or 1
  accuracy = accuracy or 1e-3

  return 0.5 + 
    (q > 0 and 1 or -1) * integral(dNorm, 0, math.abs(q), accuracy, mu, sd)
end

-- Quantile function of the Normal distribution
-- Calculates the Z-Score based on the cumulative probabiltiy
local function qNorm(p, accuracy)
  accuracy = accuracy or 0.01
  return findX(p, pNorm, accuracy)
end 


--[[ T-Distribution Functions ]]--

-- Probability Density function of a T Distribution
local function dT(x, df)
  return gamma((df + 1) / 2) / 
         (math.sqrt(df * math.pi) * gamma(df / 2)) * 
         (1 + x^2 / df)^(-(df + 1) / 2)
end 


-- CDF of the T-Distribution
local function pT(q, df, accuracy)
  assert(df > 0, "More at least 1 degree of freedom needed")
  accuracy = accuracy or 1e-4

  return 0.5 + (q > 0 and 1 or -1) * integral(dT, 0, math.abs(q), accuracy, df)
end 

-- Finds T-Ratio for a given p-value.
local function qT(p, accuracy)
  accuracy = accuracy or 0.01
  return findX(p, pT, accuracy)
end 


--[[ Chi-Square Distribution Functions ]]--

-- Probability density of the chi square distribution.
local function dChisq(x, df)
  return 1 / (2^(df / 2) * gamma(df / 2)) * x^(df / 2 - 1) * math.exp(-x / 2)
end


-- CDF of the Chi square distribution.
local function pChisq(q, df)
  return integral(dChisq, 0, q, 1e-4, df)
end 


-- Quantile function of the Chi-Square Distribution.
local function qChisq(p, df, accuracy)
    accuracy = accuracy or 0.01
    return findX(p, pChisq, accuracy, df)
end 


--[[ Tests ]]--

-- Calculates the Z-Score for one or two samples. 
-- Assumes non equivalent Variance.
local function zValue(y1, sd1, n1, y2, sd2, n2)
  assert(sd1 > 0, "Standard Deviation has to be positive")
  assert(n1  > 1, "Sample Size has to be at least 2")

  local y2 = y2 or 0
  local z
  if (n2 == nil) then
    z = (y1 - y2) / (sd1 / math.sqrt(n1))
  else
    z = (y1 - y2) / math.sqrt(sd1 / n1 + sd2 / n2)
  end

  return z
end


-- Performs a z-test on two tables and returns z-statistic.
local function zTest(t1, t2)
  return (mean(t1) - mean(t2)) / 
         math.sqrt(var(t1) / count(t1) + var(t2) / count(t2))
end 


-- Calculates the p-value of a two sample zTest.
local function zTestP(t1, t2)
  return pValue(zTest(t1, t2), pNorm)
end


-- Calculaes the t-value of one or two means, assuming non equivalent variance.
local function tValue(y1, sd1, n1, y2, sd2, n2)
  return zValue(v1, sd1, n1, v2, sd2, n2)
end


-- Performs a t-test on two tables and returns t-statistic.
local function tTest(t1, t2)
  return zTest(t1, t2)
end 


-- Calculates the p-value of a two sample tTest.
local function tTestP(t1, t2)
  return pValue(zTest(t1, t2), pT, count(t1, t2) - 2)
end
