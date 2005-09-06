-- Base data file for the Lua Dimensional Calculator
-- This file is released into the public domain.
-- It's all from public sources, anyway.
--
-- If you change it, please document your sources. I'll try to
-- do that better for mine as we go along. Also, consider using
-- separate datafiles for separate domains.

local pkg = require"luadc"
-- This will attempt to completely pollute your namespace, but it
-- won't be able to under Lua 5.1; you'll have to do it yourself.
--
-- Reading in this file will use up virtually every one-letter symbol and a
-- great number of two letter symbols. But you really do *not* want to be
-- typing 3 * pkg.kg * pkg.m / pkg.s^2
-- (Do you?)
--
-- I would have put in more constants, but the namespace issue was getting out
-- of hand, and also I was getting bored.
--

-- Notes and Sources:
-- http://www.bipm.fr/en/si/
--   The official SI site
-- http://www.unc.edu/~rowlett/units/index.html
--   Lots of useful information and trivia about measurements.
--   The allegation about chipmunks and Maryland, which I've seen in
--   several places, appears to be historical or mythical; the current
--   Maryland code says "the amount of wood contained in a space of 128
--   cubic feet, when the wood is ranked and well stowed."
-- http://en.wikipedia.org/
--   Lots more useful information
-- http://physics.nist.gov/cuu/Constants/index.html
--   Comprehensive list of physical constants, in case you need
--   static values of things.
-- http://physics.nist.gov/cuu/pdf/sp811.pdf
--   US style guide, the only one I could find. Some notes:
--     "...there are three cases where the final vowel of an SI prefix is
--      commonly omitted: megohm (not megaohm), kilohm (not kiloohm),
--      and hectare (not hectoare). In all other cases where the unit name
--      begins with a vowel, both the final vowel of the prefix and the vowel
--      of the unit name are retained and both are pronounced."
--   This would imply that you would say "kilohm" but "picoohm", which
--   seems odd. I've never seen either "megare" nor "megaare", but megamp
--   and megampere are not uncommon. Anyway, names are cheap, so I just
--   defined a number of incorrect names.
--
--   NIST accepts but discourages multiple prefix symbols and prefix
--   symbols in denominators, although it specifically allows "/ kg":
--   "For example, 0.13 mmol / g is *not* considered preferable
--    to 0.13 mol / kg."
--   That makes sense to me, but I'm not yet sure how to handle it.
--   Other authorities ban prefix symbols altogether in denominators.
--   There seems a general consensus that you do not say "g / cm^2"
--   but rather "Mg / m^2" or, better, multiply by 1000 and use
--   "kg / m^2". (kPa would be better in any event). Only the first
--   symbol in a multiplicative list should have a prefix, so you
--   would say, for example, "mA s" and not "A ms". (Presumably
--   "ms A" is also acceptable, but I think "mA s" is better, myself.)
--   Such issues really only apply to generated measures, and I'm still
--   trying to figure out the best way to do that, anyway.
--   
-- The definition of "jitter" and "jifi" is entirely my own, but if you
-- feel that I've inadvertently stolen them from somewhere, please let
-- me know and I'll fix it. "jiffy" is used variously in different domains;
-- The closest definition (in size, not in meaning) is the one attributed
-- to Gilbert Newton Lewis, which is the time it takes light to travel one
-- cm (1 cm / c). However, that is not related to actual time measurement.
-- Another very small unit is the chronon or Planck time, which NIST says is
-- 5.391 21(40) x 10-44 s. Unfortunately, the metric prefixes end too soon
-- to make this value useful.

do

  -- We need this from time to time, so let's just grab it now.
  local pi = math.pi
  pkg.pi =pi
  
  -- This is only here to make the prefix tables a bit more readable
  local function pfxsplit(t)
    local symbs, pfxs = {}, {}
    for symbpfx, mult in pairs(t) do
      local _, _, symb, pfx = string.find(symbpfx, "(%a+)_*(%a+)")
      symbs[symb], pfxs[pfx] = mult, mult
    end
    return symbs, pfxs
  end
    
  local msym, metricprefix = pfxsplit {
    y__yocto = 1E-24,
    z__zepto = 1E-21,
    a__atto  = 1E-18,
    f__femto = 1E-15,
    p__pico  = 1E-12,
    n__nano  = 1E-9,
    u__micro = 1E-6,
    m__milli = 1E-3,
    c__centi = 1E-2,
    d__deci  = 1E-1,
    da_deka  = 1E1,
    h__hecto = 1E2,
    k__kilo  = 1E3,
    M__mega  = 1E6,
    G__giga  = 1E9,
    T__tera  = 1E12,
    P__peta  = 1E15,
    E__exa   = 1E18,
    Z__zetta = 1E21,
    Y__yotta = 1E24,
  }

  -- I knew you'd be expecting this
  local isym, ieciprefix = pfxsplit {
    -- that's read: icky prefix. Thanks to the IEC
    Ki__kibi = 2^10,
    Mi__mebi = 2^20,
    Gi__gibi = 2^30,
    Ti__tebi = 2^40,
    Pi__pebi = 2^50,
    Ei__exbi = 2^60,
  }
  
  -- TODO alias does not name things. It probably should.
  local function alias(base, long, fullname)
    pkg[fullname] = base
    for pfx, mult in long do
      pkg[pfx..fullname] = mult * base
      -- Handle (some) exceptions, a bit liberally
      -- It doesn't yet get megalergs right.
      if string.sub(pfx, -1) == string.sub(fullname, 1, 1) then
        pkg[pfx..string.sub(fullname, 2)] = mult * base
      end
    end
  end
  
  local function SIprefix(unit, fullname)
    local name = unit.name
    for pfx, mult in pairs(msym) do
      Measure(pfx..name, mult * unit)
    end
    if fullname then
      alias(unit, metricprefix, fullname)
    end
  end

  local function IECprefix(unit, fullname)
    local name = unit.name
    for pfx, mult in pairs(isym) do
      Measure(pfx..name, mult * unit)
    end
    if fullname then
      alias(unit, ieciprefix, fullname)
    end
  end
  
  -- hook globals for simplicity
  setmetatable(getfenv(), {__index = pkg})


  Dimension("Length", "m")
  CompoundDimension("Area", m^2)
  CompoundDimension("Volume", m^3)

  SIprefix(m, "metre")
  SIprefix(m, "meter")
  
  Measure("are", 100 * m^2)
  Measure("ha", 100 * are)
  
  Measure("l", 1E-3 * m^3)
  SIprefix(l, "litre")
  
  Dimension("Mass", "g")
  SIprefix(g, "gram")
  
  -- A second is "the duration of 9 192 631 770 periods of the radiation
  -- corresponding to the transition between the two hyperfine levels of
  -- the ground state of the caesium 133 atom." So what do you call one
  -- transition? Anyone who knows, email me. For now, it's an jiffy, and
  -- the corresponding frequency is a jitter. That makes a second about
  -- 9.2 gigajiffies. Later, we'll learn how to express CPU speeds in
  -- metric jitters.
  Dimension("Time", "s")
  SIprefix(s, "second")
  pkg.sec = second -- just for me
  
  Measure("minute", 60*s)
  Measure("hour", 60*minute)
  Measure("day", 24*hour)
  -- There are lots of ways of measuring years, and all of them are subject to
  -- variation. This one is a tropical year; it will do.
  Measure("year", 31556925.9747 * s)

  Dimension("Current", "A")
  SIprefix(A, "ampere")
  pkg.amp = ampere
  
  Dimension("Temperature", "K")

  Dimension("Intensity", "cd")
  SIprefix(cd, "candela")

  CompoundDimension("Velocity", km / s)
  CompoundDimension("Acceleration", km / s^2)

  -- Hertz is measured in per-seconds, but you don't say 
  -- "I was walking at 2 meter-hertz".
  Measure("Hz", 1/s)
  CompoundDimension("Frequency", Hz)
  SIprefix(Hz, "hertz")
  
  Measure("N", kg * m / s^2)
  CompoundDimension("Force", N)
  SIprefix(N, "newton")

  Measure("Pa", N / m^2)
  CompoundDimension("Pressure", Pa)
  CompoundDimension("Stress", Pa)
  SIprefix(Pa, "pascal")

  -- The old CGS unit for energy was the erg, which is dyne * cm
  -- where a dyne is g * cm
  -- Consequently, a joule (J) is 1E7 ergs or 10 megalergs.
  -- I had to say that because I just love the word megalerg.
  -- Also, ten newtons are a megadyne. 
  Measure("J", N * m)
  CompoundDimension("Energy", J)
  CompoundDimension("Work", J)
  SIprefix(J, "joule")

  Measure("W", J / s)
  CompoundDimension("Power", W)
  SIprefix(W, "watt")

  Measure("C", A * s)
  CompoundDimension("ElectricCharge", C)
  SIprefix(C, "coulomb")

  Measure("V", W / A)
  CompoundDimension("ElectromotiveForce", V)
  SIprefix(V, "volt")

  Measure("F", C / V)
  CompoundDimension("Capacitance", F)
  SIprefix(F, "farad")

  -- Omega isn't ISO-8859-1. Nor is it a valid "letter".
  -- And W is already taken.
  Measure("O", V / A)
  CompoundDimension("Resistance", O)
  SIprefix(O, "ohm")

  Measure("S", A / V)
  CompoundDimension("Conductance", S)
  SIprefix(S, "siemens")

  Measure("Wb", V / s)
  CompoundDimension("MagneticFlux", Wb)
  SIprefix(Wb, "weber")

  Measure("T", Wb / m^2)
  CompoundDimension("MagneticFluxDensity", T)
  SIprefix(T, "tesla")

  Measure("H", Wb / A)
  CompoundDimension("Inductance", H)
  SIprefix(H, "henry")

  -- This is not really a dimension
  Dimension("Angle", "rad")

  -- Neither is this
  Dimension("SolidAngle", "sr")

  Measure("lm", cd * sr)
  CompoundDimension("LuminousFlux", lm)
  SIprefix(lm, "lumen")

  Measure("lx", lm / m^2)
  CompoundDimension("Illuminance", lx)
  SIprefix(lx, "lux")

  -- Missing:
  --   mole (mol) 
  --     "The mole is the amount of substance of a system which contains as
  --     many elementary entities as there are atoms in 0.012 kilogram of
  --     carbon 12. When the mole is used, the elementary entities must be
  --     specified and may be atoms, molecules, ions, electrons, other
  --     particles, or specified groups of such particles."
  --     (Why didn't they say 12 gram? The two 12's are not coincidental.)
  --   becquerel (Bq) "Activity" /second
  --   gray (Gy) "absorbed dose" J/kg
  --   sievert (Sv) "dose equivalent" qualitative measurement, related to Gy
  --   katal (kat) "catalytic activity" mol/s  What is the dimension of mol?

  -- Note: Hz is not in this list. That was deliberate; it avoids unexpected
  -- appearance of m Hz instead of m/s. 
  pkg.SI = Domain(m, kg, s, A, K, cd, ha, l, N, Pa, J, W, C, V, F, ohm,
                  S, Wb, T, H, rad, sr, lm, lx)
  
  Dimension("Information", "b")
  BaseMeasure("B", 8 * b)
  --TODO
  IECprefix(b, "bit")
  IECprefix(B, "byte")

  -- Common (and other) non-SI measurements
  Measure("inch", 2.54 * cm)
  Measure("foot", 12 * inch)
  Measure("yard", 3 * foot)
  Measure("link", 7.92 * inch)
  Measure("chain", 100 * link)
  Measure("furlong", 10 * chain)
  Measure("mile", 8 * furlong)
  Measure("acre", furlong * chain)
  Measure("section", mile * mile)
  Measure("quarter", section / 4)

  -- US gallon, which I think is the only one still in common use
  -- I sometimes miss Canadian gallons. Oh, well.
  Measure("gallon", 231 * inch^3)
  -- The liquid quart. I haven't put dry quarts in yet.
  Measure("quart", .25 * gallon)

  Measure("pound", 453.59237 * g)
  Measure("ounce", pound / 16)
  Measure("stone", 14 * pound)
  Measure("cwt", 8 * stone)
  Measure("ton", 2000 * pound) -- US ton
  Measure("longton", 20 * cwt) -- history is wonderful
  

  pkg.US = Domain(inch, inch^2, quart, pound, mile / hour, s, A, W, V, ohm)
  -- Could have an Imperial domain, too.
  -- Perhaps even a Nostalgic domain.
  
  -- Some scalars
  Measure("percent", 1/100)
  Unit("count")
  Measure("dozen", 12 * count)

  -- Stuff from here down are mostly used for examples.



  -- There are a couple of ways to do currency. You could erroneously
  -- assume that exchange rates are fixed; that's simple but it
  -- doesn't capture reality. You could redefine the exchange rates
  -- every so often, but your old values would already have been
  -- converted according to the "rate of the day"; accounting usually
  -- does this, but it loses historical information useful for
  -- analytics.
  --
  -- I think the right way to do this is to define every currency
  -- as a separate dimension, but that's a little irritating for
  -- doing exchange rate calculations interactively. In those cases,
  -- you want the "rate of the day" definition. Of course, you could
  -- have both.
  --
  -- Anyway, my examples all use the Peruvian Sol so you will want to
  -- change this :)
  Dimension("PeruvianCurrency", "Sol")
  Measure("USD", 3.24 * Sol)

  -- If a square cm is the area of a square 1 cm across, surely a round cm
  -- must be the area of a circle 1 cm across, no?
  function pkg.square(measure) return measure^2 end
  function pkg.round(measure) return (pi/4) * measure^2 end
  -- So...
  function pkg.cube(measure) return measure^3 end
  function pkg.spherical(measure) return (pi/6) * measure^3 end
  -- It would be cuter if you could say:
  -- >  area .. round(inch)
  -- >  area .. square(inch)
  -- 
  -- Note that bushel is actually always measured by weight, although it
  -- really is a volume. The weight varies from grain to grain, usually 50-60 pounds.
  --
  -- Rowlett says:
  -- "The volume of a cylinder 18.5 inches in diameter and 8 inches in height"
  -- But its actually defined in terms of dry quarts (which are not the same
  -- as liquid quarts, by the way.) I used the cylinder definition mostly to
  -- show off.

  Measure("bushel", round(18.5 * inch) * (8 * inch))
  -- CompoundDimension("CropYield", bushel / acre)

  pkg.Farm = Domain(mile, acre, bushel)

  pkg.feet = foot -- :)
  -- A stack of firewood 4 feet wide, 8 feet long and 4 feet high,
  -- "packed tight enough so that a chipmunk cannot run through it".
  -- The chipmunk is not included in this package.
  -- Note to Canucks: what I grew up thinking of as a cord is actually
  -- a cord-foot, which is apparently 4 feet x 1 foot x 4 feet, or cord/8.
  -- Although I've always thought of it as having dimensions 2 x 4 x 8,
  -- or roughly the capacity of a 1956 Ford half-ton.
  Measure("cord", 4 * feet * 8 * feet * 4 * feet)
  -- You cannot convert boardfeet into cords, at least not accurately,
  -- because cords include air even when there are no chipmunks.
  Measure("boardfoot", 1 * inch * 12 * inch * 1 * foot)
  Measure("MBF", 1000 * boardfoot)
  
  pkg.Wood = Domain(cord) 
  
  -- Fuel "efficiency"
  Measure("mpg", mile / gallon)
  CompoundDimension("FuelEfficiency", mpg)

  -- Fun stuff
  Measure("overacre", 1 / acre)
    for pfx, mult in pairs(metricprefix) do
      Measure(pfx.."overacre", mult * overacre)
    end
  
  Measure("Jf", second / 9192631770)
  SIprefix(Jf, "jifi")
  
  Measure("Ji", 1/Jf)
  SIprefix(Ji, "jitter")
  
  pkg.Fun = Domain(Jf, Ji, gigaoveracre) 
  -- Some physical constants with units
  
  -- The speed of light
  pkg.c = 299792.458 * (km / s)
  -- Newton's constant (of gravitation)
  pkg.G = 6.6742e-11 * (m^3 / (kg * s^2))
  -- Planck's constant.
  pkg.h = 6.6260693E-34 * (J * s)
  -- But often used as h-bar
  pkg.hbar = h / (2 * pi)
  -- From which we can derive
  pkg.chronon = (hbar * G / c^5)^(1/2)
  
  pkg.lightyear = year * c
  pkg.jiffy = cm / c
end
