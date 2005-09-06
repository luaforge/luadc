local pkg = require "luadc"

local meta = {}
meta.__index = meta

local function emptybasket()
  return setmetatable({}, meta)
end

local Zero = 0 * scalar
local function nonzero(q) return q ~= 0 and q ~= Zero and q or nil end

function meta:addto(other)
  -- if the other is also a basket, we combine
  -- otherwise, we distribute and hope for the best
  if getmetatable(other) == meta then
    for dim, qty in other do
      local q = self[dim]
      if q then self[dim] = nonzero(q + qty)
           else self[dim] = qty
      end
    end
  else
    local dim = D(other)
    local q = self[dim]
    if q then self[dim] = nonzero(q + other)
         else self[dim] = other
    end
  end
  return self
end

-- save the trouble of negating; major code duplication
function meta:takefrom(other)
  if getmetatable(other) == meta then
    for dim, qty in other do
      local q = self[dim]
      if q then self[dim] = nonzero(q - qty)
           else self[dim] = -qty
      end
    end
  else
    local dim = D(other)
    local q = self[dim]
    if q then self[dim] = nonzero(q - other)
         else self[dim] = -other
    end
  end
  return self
end

local function copy(self)
  local rv = emptybasket()
  for dim, qty in self do rv[dim] = qty end
  return rv
end

meta.copy = copy

function meta:__add(other)
  if getmetatable(self) == meta then
    return copy(self):addto(other)
  else
    return copy(other):addto(self)
  end
end

function meta:__sub(other)
  if getmetatable(self) == meta then
    return copy(self):addto(other)
  else
    return (-other):addto(self)
  end
end

function meta:__unm()
  local rv = emptybasket()
  for dim, qty in pairs(self) do rv[dim] = -qty end
  return rv
end

function meta:__mul(other)
  if getmetatable(self) ~= meta then self, other = other, self
  elseif getmetatable(other) == meta then error("Cannot multiply a Basket by a Basket")
  end
  local rv = emptybasket()
  for _, qty in pairs(self) do rv:addto(qty * other) end
  return rv
end

function meta:__div(other)
  local rv = emptybasket()
  if getmetatable(self) ~= meta then 
    for _, qty in pairs(other) do rv:addto(self / qty) end
  elseif getmetatable(other) ~= meta then
    for _, qty in pairs(self) do rv:addto(qty / other) end
  else
    error("Cannot divide a Basket by a Basket")
  end
  return rv
end

function meta:__tostring()
  local t = {}
  for _, qty in pairs(self) do 
    table.insert(t, tostring(qty))
  end
  return table.concat(t, " + ")
end

function pkg.Basket(...)
  local rv = emptybasket()
  for _, v in ipairs(arg) do
    rv:addto(v)
  end
  return rv
end

pkg.EmptyBasket = emptybasket

-- An example: shopping baskets

-- Create a commodity. This creates both a Dimension ("Commodity:name") and a Measure (name)
function pkg.Commodity(name)
  return Dimension("Commodity:"..name, name)
end

-- A PriceList relates commodities to prices. It's just a table, really, but
-- the interface would be awkward without this. Use the unit measure created
-- as a key; it will work out.
do
  local meta = {}
  meta.__index = meta
  
  function pkg:PriceList() return setmetatable({}, meta) end

  pkg.each = scalar
  -- eg: pl:add(apple, .65 * Sol, kg)
  function meta:add(measure, price, unit)
    local um = unit * measure
    self[D(um)] = price / um
  end

  meta.set = meta.add  -- :)

  -- Get the price for a Basket
  -- For fun, multicurrency
  function meta:compute(basket)
    local rv = pkg:EmptyBasket()
    for dim, qty in pairs(basket) do
      local price = self[dim]
      if not price then error("No price for "..tostring(dim)) end
      rv = rv + price * qty
    end
    local d, q = next(rv)
    if d then
      if next(rv, d) then return rv
      else return q
      end
    else
      return 0
    end
  end
  
end
