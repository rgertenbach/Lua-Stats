--[[
TODO: qNorm
TODO: Type checking
TODO: Sample Size
]]

-- Integrates a function and calculates the area under the function
-- from start to stop in delta sized steps
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


-- I have no idea what the hell is going on here. Taken from:
-- http://rosettacode.org/wiki/Gamma_function#Lua
local function gamma(x)
  local gamma = 0.577215664901
  local coeff = -0.65587807152056
  local quad  = -0.042002635033944
  local qui   = 0.16653861138228
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


--[[
Normal Distribution Functions
]]

-- Probability Density function of a Normal Distribution
local function dNorm(x, mu, sd)
  local mu = mu or 0
  local sd = sd or 1

  return (1 / 
         (sd * math.sqrt(2 * math.pi))) * 
         math.exp(-(((x - mu) * (x - mu)) / (2 * sd^2)))
end


-- Cumulative Probability Density Function of a normal distribution
local function pNorm(q, mu, sd)
  if (q > 0) then  
    return 0.5 + integral(dNorm, 0, q, 1e-5, mu, sd)
  else
    return 0.5 - integral(dNorm, 0, -q, 1e-5, mu, sd)
  end
end

-- Quantile function of the Normal distribution
-- Calculates the Z-Score based on the cumulative probabiltiy
-- a.k.a. Inverse Normal distribution
-- Here goes qNorm


--[[
T-Distribution Functions
]]

-- Probability Density function of a T Distribution
local function dT(x, df)
  return gamma((df + 1) / 2) / 
         (math.sqrt(df * math.pi) * gamma(df / 2)) * 
         (1 + x^2 / df)^(-(df + 1) / 2)
end 


-- CDF of the T-Distribution
local function pT(q, df)
  if (q > 0) then  
    return 0.5 + integral(dT, 0, q, 1e-5, mu, sd)
  else
    return 0.5 - integral(dT, 0, -q, 1e-5, mu, sd)
  end
end 

-- Quantile function goes here


-- Calculates the Z-Score for one or two samples
local function zScore(y1, sd1, n1, y2, sd2, n2)
  assert(sd1 > 0, "Standard Deviation has to be positive")
  assert(n1 > 1, "Sample Size has to be at least 2")

  local y2 = y2 or 0
  local z
  if (n2 == nil) then
    z = (y1 - y2) / (sd1 / math.sqrt(n1))
  else
    z = (y1 - y2) / math.sqrt(sd1 / n1 + sd2 / n2)
  end

  return z
end


-- Returns the p-value of a quantile q of a probability function f
local function pValue(q, f)
  if (q == nil) then
    return nil
  else
    return math.abs(1 - math.abs(f(q) - f(-q)))
  end
end
