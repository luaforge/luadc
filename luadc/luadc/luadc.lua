-- The Lua Dimension Calculator.
-- Copyright 2005 Rici Lake. 
-- You may use this package under the same terms as Lua 5.0, or any subsequent version
-- of Lua at your option.

-- Great, got that over with.
-- TODO
--   Document the innards a bit
--   Refactor quantities and measures into separate objects, maybe
--   Memoise measure operations instead of creating a new domain every time
--   Other efficiency hacks as seem appropriate.

-- That said, it seems to work, so go for it.

local next, tostring, tonumber, setmetatable, pairs =
      next, tostring, tonumber, setmetatable, pairs
local insert, concat, sort =
      table.insert, table.concat, table.sort
  
local pkg = {}
-- Create a type for (abstract) dimensions

-- The good old standards

local function newtable() return {} end

local memoise
do
  local function makemeta(func)
    return {
      __index = function(self, k)
        local v = func(k)
        self[k] = v
        return v
      end
    }
  end
  
  local metacache = {[makemeta] = makemeta(makemeta)}
  function memoise(tab, func)
    return setmetatable(tab, metacache[func])
  end
  memoise(metacache, makemeta)
end

-- todo: memoise this :)
local function memofactory(func, self)
  return memoise({}, function(other) return func(self, other) end)
end
local function memoise2(func)
  return memoise({}, function(self) return memofactory(func, self) end)
end

local function exponentRep(d, e)
  if e == 1 then
    return d
  else
    return d .. "^" .. tostring(e)
  end
end
  
local function normalise_dimension(self)
  local display = {}
  for d, e in pairs(self or display) do
    if e ~= 0 then insert(display, exponentRep(d, e)) end
  end
  if next(display) then
    sort(display)
    return concat(display, ' * ')
  else
    return "Scalar"
  end
end

-- we'll fill this in later
local dmeta = {}
local function newdimension(t)
  return setmetatable(t, dmeta)
end

local Scalar = newdimension{}
local name2dimension = {Scalar = Scalar}
local dimension2name = {[Scalar] = "Scalar"}
local dimensioncache = setmetatable({[Scalar] = Scalar}, {
  __index = function(self, dim)
    local norm = normalise_dimension(dim)
    local inter = name2dimension[norm]
    if inter then
      return inter
    else
      name2dimension[norm] = newdimension(dim)
      dimension2name[dim] = norm
      self[dim] = dim
      return dim
    end
  end
})
pkg.Scalar = Scalar

local Alias = memoise({}, newtable)

-- Make or find a unitary dimension. Doesn't work if e == 0.
local function unitDimension(name, e)
  local ename = name
  if e then
    ename = exponentRep(name, e)
  else
    e = 1
  end
  local dim = name2dimension[ename]
  if not dim then
    dim = newdimension{[name] = e}
    name2dimension[ename] = dim
    dimension2name[dim] = ename
    dimensioncache[dim] = dim
  end
  return dim
end
  
-- Now we can fill in the dimension metatable
do
  local meta = dmeta
  meta.__index = meta
  
  -- On the assumption that there will never be a lot of types
  -- of measures, we cache operations on dimensions.

  local function mul(self, other)
    if self and other then
      local rv = {}
      for d, e in pairs(self) do rv[d] = e end
      for d, e in pairs(other) do
        rv[d] = (rv[d] or 0) + e
      end
      return dimensioncache[rv]
    else
      return (self ~= Scalar and self) or other
    end
  end
  local mulcache = memoise2(mul)
  function meta:__mul(other) return mulcache[self][other] end
  
  local function invert(self)
    if self then
      local rv = {}
      for d, exponent in pairs(self) do rv[d] = -exponent end
      return dimensioncache[rv]
    end
  end

  local function div(self, other)
    return mul(self, invert(other))
  end
  local divcache = memoise2(div)
  function meta:__div(other) return divcache[self][other] end
  
  -- ideally, we'd use rational numbers for exponents. Instead, we
  -- just fuzz them a bit if they're close to being integers.
  -- It might actually be better to ban non-integer exponents.
  local function toint(n)
    return math.mod(math.abs(n), 1) < 1e-10
             and math.floor(n + 0.1)
             or n
  end
  
  local function pow(self, i)
    local i = assert(tonumber(i), "number expected")
    if i == 1 then return self
    elseif i ~= 0 then
      local rv = {}
      for d, e in pairs(self) do rv[d] = toint(e * i) end
      return dimensioncache[rv]
    end
  end
  local powcache = memoise2(pow)
  function meta:__pow(i) return powcache[self][i] end
  
  -- Add and subtract only do error checking and always return self
  function meta:__add(other)
    if self == other then
      return self
    else
      error ("Incompatible dimensions `"..tostring(self).."' and `"..tostring(other).."'")
    end
  end

  meta.__sub = meta.__add
  
  function meta:__tostring()
    if next(Alias[self]) then
      return dimension2name[self] .. " (" .. concat(Alias[self], ", ") .. ")"
    else
      return dimension2name[self]
    end
  end

  -- Just a concept for now. If you index a Dimension by a unitary dimension
  -- then it gives you the exponent or 0
  function meta:__index(dim)
    if getmetatable(dim) == meta then
      local u = next(dim)
      if u then
        if dim[u] ~= 1 or next(dim, u) then
          error("Attempt to index dimension with non-unitary dimension `"..tostring(dim).."'")
        end
        return self[u] or 0
      else
        return 0
      end
    end
  end
  
end

-- A Measurement is a numeric quantity (n), a Dimension (d)
-- and may also be annotated with a name (name) and a domain (domain)
--
-- TODO: 
-- Measurements should be treated as immutable. Probably
-- the immutability should be enforced and appropriate getters
-- should be defined.

-- There is some wierdness with handling of Scalars. They're turned
-- into regular numbers unless they have a name or a domain. That
-- prevents named/domained scalars from being compared with regular
-- scalars. How serious is this? I don't know.

do

  -- construct and unify are inverses, but they're a bit clunky.
  local meta = {}
  meta.__index = meta

  local function construct(n, d, domain, name)
    local v
    if name then
      v = {magnitude = n, dimension = d, name = name}
      v.domain = {[d] = v}
    elseif d == Scalar and (not domain or not domain[Scalar]) then
      return n
    else
      v = {magnitude = n, dimension = d, domain = domain}
    end
    return setmetatable(v, meta)
  end

  local function unify(v)
    if type(v) == "number" then
      return v, Scalar
    elseif getmetatable(v) == meta then
      return v.magnitude, v.dimension, v.domain, v.name
    else
      error("Expected measure, got "..type(v))
    end
  end

  -- In comparisons, we can assume that both self and other are Measures
  function meta:__eq(other)    
    if self.magnitude ~= 0 then
      return self.magnitude == other.magnitude and self.dimension == other.dimension
    else
      return other.magnitude == 0 and (self.dimension == other.dimension
                                       or self.dimension == Scalar
                                       or other.dimension == Scalar)
    end
  end

  function meta:__lt(other)
    -- throw an error if not comparable
    local _ = self.dimension + other.dimension 
    return self.magnitude < other.magnitude
  end

  function meta:__le(other)
    local _ = self.dimension + other.dimension 
    return self.magnitude <= other.magnitude
  end

  function meta:__unm()
    local n, d, dom = unify(self)
    return construct(-n, d, dom)
  end
  
  -- Otherwise, we need to use unify
  function meta:__add(other)
    local selfn, selfd, selfdom = unify(self)
    local othern, otherd, otherdom = unify(other)
    return construct(selfn + othern, selfd + otherd, selfdom or otherdom)
  end

  function meta:__sub(other)
    local selfn, selfd, selfdom = unify(self)
    local othern, otherd, otherdom = unify(other)
    return construct(selfn - othern, selfd - otherd, selfdom or otherdom)
  end
 
  -- To allow for stuff like: velocity .. km / h
  -- We try to come up with a good name if named measures are multiplied,
  -- divided, or powered. The algorithm is really simple-minded, although
  -- we could do a better one on the same model as Dimensions. This one
  -- is pure string manipulation.
  local function ispure(a)
    return a and not string.find(a, "[ /^]")
  end
  local function isproduct(a)
    return a and not string.find(a, "/")
  end

  function meta:__mul(other)
    local selfn, selfd, selfdom, selfname = unify(self)
    local othern, otherd, otherdom, othername = unify(other)
    local name = isproduct(selfname) and isproduct(othername) 
                 and (selfname .. " " .. othername)
    return construct(selfn * othern, selfd * otherd,
                     selfdom or otherdom, name)
  end

  function meta:__div(other)
    local selfn, selfd, selfdom, selfname = unify(self)
    local othern, otherd, otherdom, othername = unify(other)
    local name = isproduct(selfname) and isproduct(othername)
                 and (selfname .. " / " .. othername)
    return construct(selfn / othern, selfd / otherd,
                     selfdom or otherdom, name)
  end
 
  -- pow is different
  function meta:__pow(e)
    e = assert(tonumber(e), "Expected number")
    if e == 0 then
      return 1
    elseif e == 1 then
      return self
    else
      -- we can assume self is a Measure.
      local n, d, dom, selfname = unify(self)
      local name = ispure(selfname) and exponentRep(selfname, e)
      return construct(n^e, d^e, dom, name)
    end
  end
  
  -- Create a new Dimension, providing the Unit measure.
  -- Returns the Unit measure
  -- eg: meter = Dimension("Length", "meter")
  -- both the Dimension and the unit are added to the pkg.
  
  local Units = {}
  function pkg.Dimension(name, unitname)
    if pkg[name] then
      error("`"..name.."' is already defined")
    end
    local dim = unitDimension(name)
    unit = construct(1, dim, nil, unitname)
    pkg[name] = dim
    pkg[unitname] = unit
    Units[dim] = unit
    return unit
  end
  
  -- Give a compound Dimension an alias. We use a Measure as a
  -- proxy for the Compound Dimension, to avoid introducing
  -- dimensions into userspace (although they are, in fact, there)
  function pkg.CompoundDimension(alias, measure)
    if pkg[alias] then
      error("`"..alias.."' is already defined")
    end
    pkg[alias] = measure.dimension
    insert(Alias[measure.dimension], alias)
  end

  -- TODO: Check for redefinition
  -- Create a new measurement, given a value and a name
  --   cm = Measure("cm", .01 * meter)
  --   l = Measure("l", 1000 * cm^3)
  -- The measurement is added to the package if it is a simple
  -- word.
  function pkg.Measure(name, value)
    local n, d  = unify(value)
    local m = construct(n, d, nil, name)
    if string.find(name, "^%a%w*$") then
      pkg[name] = m
    end
    return m
  end

  -- Convenience function. 
  function pkg.Unit(name)
    return pkg.Measure(name, 1)
  end

  -- Given a domain, a dimension and an exponent, find a
  -- representation in the domain. If the representation
  -- has a positive exponent, put it in pos; if it has a
  -- negative exponent put it in neg; return the multiplier.
  -- If no representation is found, return nil.
  -- TODO cache d^e lookups

  local function getOneDimension(pos, neg, domain, d, e)
    -- Try exact match
    local m = domain[unitDimension(d, e)]
    if m then
      insert(pos, m.name)
      return m.magnitude
    end
    if e < 0 then 
      -- If e is negative, also try -1; otherwise, do a recursive
      -- call to try the positive exponent
      if e < -1 then
        local m = domain[unitDimension(d, -1)]
        if m then
          insert(pos, exponentRep(m.name, -e))
          return m.magnitude^-e
        end
      end
      local n = getOneDimension(neg, pos, domain, d, -e)
      return n and 1/n
    elseif e > 1 then
      -- If e is positive, also try 1 (unless we already have)
      local m = domain[unitDimension(d, 1)]
      if m then
        insert(pos, exponentRep(m.name, e))
        return m.magnitude^e
      end
    end
  end
  
  -- simplified version for use with Units
  local function getUnitRep(pos, neg, d, e)
    local name = Units[name2dimension[d]].name
    if e > 0 then
      insert(pos, exponentRep(name, e))
    elseif e < 0 then
      insert(neg, exponentRep(name, -e))
    end
    return 1
  end
  
  -- print a value using a measure (which must be compatible
  -- with the value)
  local function showas(self, measure)
    local selfn, selfd = unify(self)
    local n, d, name = measure.magnitude, measure.dimension, measure.name
    local _ = selfd + d -- check compatibility
    return tostring(selfn / n).." "..name
  end
  
  -- print a value in accordance with a domain, falling back
  -- to the default domain.
  local function show(self, domain)
    local function concat(t, j)
      local rv = {}
      for i, v in ipairs(t) do rv[i] = tostring(v) end
      return table.concat(rv, j)
    end
    local selfn, selfd = unify(self)
    local default = pkg.BaseDomain
    local d1 = domain or default
    local d2 = domain and domain ~= default and default
    local m = d1[selfd] or (d2 and d2[selfd])
    if m then return showas(self, m) end
    -- Now it's heuristic time. We'll do it the simple way for
    -- now: pick it apart and use either the domain or the base
    -- domain to look up each Dimension in turn. 
    -- We do not insert the results into the domain. Perhaps we
    -- should
    local pos, neg, n = {1}, {}, 1
    for d, e in pairs(selfd) do
      n = n * (
                getOneDimension(pos, neg, d1, d, e)
                or (d2 and getOneDimension(pos, neg, d2, d, e))
                or getUnitRep(pos, neg, d, e)
              )
    end
    pos[1] = tostring(selfn / n)
    local p = concat(pos, ' ')
    if next(neg) then
      p = p .. ' / ' .. concat(neg, ' ')
    end
    return p
  end
  
  -- This should work with either a domain or a measure,
  -- but if it's a measure, it needs to be compatible.
  function meta:__concat(domain)
    if type(self) == "string" then 
      self = assert(tonumber(self), "Expected quantity, got string")
    end
    local selfn, selfd = unify(self)
    if getmetatable(domain) == meta then
      local _, otherd = unify(domain)
      local d = selfd + otherd
      assert(domain.name, "Expected measurement, got quantity")
      return construct(selfn, d, {[d] = domain})
    else
      return construct(selfn, selfd, domain)
    end
  end
  pkg.AS = meta.__concat

  function meta:__tostring()
    return show(self, self.domain)
  end
end

-- Make a dimension-indexed type. The argument is the backup table,
-- typically the same as the meta table itself.

function pkg.dimensionIndex(meta)
  return function(self, key)
    local rv = meta[key]
    if rv then return rv end
    if type(key) == "table" then
      local dim = key.dimension
      return dim and self[dim]  -- could recurse!
    end
  end
end
 
do
  local meta = {}
  meta.__index = meta
  local function construct(t)
    return setmetatable(t, meta)
  end

  -- Constructor takes a list of measures
  -- 5.0.2 syntax!
  local function addMeasures(self, measures)
    for _, measure in ipairs(measures) do
      assert(measure.name, "Domain member has no name")
      self[measure.dimension] = measure
    end
    return self
  end
  
  function pkg.Domain(...)
    return construct(addMeasures({}, arg))
  end

  -- just like the constructor but adds things.
  function meta:add(...)
    return addMeasures(self, arg)
  end

  function meta:show()
    local idx = {}
    for dim, m in pairs(self) do
      insert(idx, tostring(dim)..' =\t'..tostring(m))
    end
    sort(idx)
    return concat(idx, "\n")
  end
  meta.__tostring = meta.show

  -- add the __concat meta from measures, just in case they
  -- say 3 .. domain and domain includes a scalar
  meta.__concat = pkg.AS
end

pkg.BaseDomain = pkg.Domain()
function pkg.BaseMeasure(name, value)
  local m = pkg.Measure(name, value)
  pkg.BaseDomain:add(m)
  return m
end

-- Extract the magnitude of a scalar
function pkg.Q(value)
  return pkg.AS(value + 0, nil)
end

-- Extract the dimension of a quantity
local scalard = pkg.Domain(pkg.Measure("scalar", 1))
function pkg.D(quant)
  return pkg.AS(quant, scalard).dimension
end

return pkg
