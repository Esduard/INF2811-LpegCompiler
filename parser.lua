local lpeg = require "lpeg"

--  function (n) return {tag = "number", num = n} end)
--  node("number", "num") --> function (...) 

local function node (tag, ...)
  local labels = {...}
  return function (...)
    local values = {...}
    local t = {tag = tag}
    for i = 1, #labels do
      t[labels[i]] = values[i]
    end
    return t
  end
end

local function fold (t)
  local res = t[1]
  for i = 2, #t, 2 do
    res = {tag = "binarith", e1 = res, op = t[i], e2 = t[i + 1]}
  end
  return res
end

local function compare(t)
  local res = t[1]
  for i = 2, #t, 2 do
    res = {tag = "bincomp", e1 = res, op = t[i], e2 = t[i + 1]}
  end
  return res
end

local function foldCast(t)
  local res = t[1]
  
  for i=2, #t, 1 do
    res = {tag = "cast", e = res, type = t[i]}
  end

  return res
end

local function varToExp(var)
return {tag = "varExp", var = var}
end


local function foldVar(t)
  local res = {tag = "id", id = t[1]}
  
  for i=2, #t, 1 do
    res = {tag = "indexed", array = varToExp(res), index = t[i]}
  end

  return res
end

local function matchPreIncDec(p1,p2)
  return {tag = "incDec", var = p2, opIncDec = p1, use_after_incDec = true}
end

local function matchPostIncDec(p1,p2)
  return {tag = "incDec", var = p1, opIncDec = p2, use_after_incDec = false}
end

local S = lpeg.V"S"

local OP = "(" * S
local CP = ")" * S
local OAB = "{" * S -- angle brackets
local CAB = "}" * S
local OBB = "[" * S -- box brackets
local CBB = "]" * S
local SC = ";" * S
local C = ":" * S
local Coma = "," * S
local Prt = "@" * S
local Eq = "=" * S

local digit = lpeg.R"09"
local alpha = lpeg.R("az", "AZ", "__")
local alphanum = alpha + digit

local reservedwords = {}
local function Rw (id)
  reservedwords[id] = true
  return lpeg.P(id) * -alphanum * S
end

local function RwC (id)
  reservedwords[id] = true
  return lpeg.C(lpeg.P(id)) * -alphanum * S
end


local integer = (digit^1) / tonumber * S
local double = (digit^1) * lpeg.S('.') * (digit^1) / tonumber * S
local opA = lpeg.C(lpeg.S("+-")) * S
local opM = lpeg.C(lpeg.S("*/")) * S
local opUn = lpeg.C("-") * S
local opInc = (lpeg.C("++")  + lpeg.C("--")) * S

local opC = lpeg.C(lpeg.S(">") * lpeg.S("=") + lpeg.S("<") * lpeg.S("=") + 
  lpeg.S("=") * lpeg.S("=") + lpeg.S("!") * lpeg.S("=") + lpeg.S(">") + lpeg.S("<")) * S

local function notRw(id, position, id_matched)
  if reservedwords[id_matched] ~= nil then
    return false
  else
    return true, id_matched
  end
end

I = lpeg.P(function (s,debug_index)
  print(debug_index)
  return true
  end)

local Id = lpeg.Cmt(alpha * alphanum^0, notRw) * S

local primary = lpeg.V"primary"
local factor = lpeg.V"factor"
local expM = lpeg.V"expM"
local expC = lpeg.V"expC"
local exp = lpeg.V"exp"
local stat = lpeg.V"stat"
local stats = lpeg.V"stats"
local block = lpeg.V"block"
local def = lpeg.V"def"
local Type = lpeg.V"Type"
local call = lpeg.V"call"
local var = lpeg.V"var"
local arguments = lpeg.V"arguments"
local parameter = lpeg.V"parameter"
local new = lpeg.V"new"
local postfix = lpeg.V"postfix"
local postfixCast = lpeg.V"postfixCast"
local inc = lpeg.V"inc"
local varExp = lpeg.V"varExp"


local SyntaxAnalyser = {lastpos = 0}

local prog = lpeg.P{"defs";
  defs = lpeg.Ct(def^1);
  def = Rw"fun" *  Id * OP * (lpeg.Ct(parameter * (Coma * parameter)^0) + lpeg.C(""))  * CP * ((C * Type) + lpeg.Cc("void")) * block / node("func" , "name", "parameters" , "typeRet" , "body");
  stats = stat * (SC * stats)^-1 * SC^-1 / function (st, pg)
	  return pg and {tag="seq", s1 = st, s2 = pg} or st
	end;
  block = OAB * stats * CAB / node("block", "body");
  stat = block
       + Prt * exp / node("print", "e")
       + Rw"var" * Id * C * Type *  (Eq * (exp))^-1 / node("var", "id", "typeVar" , "e")
       + Rw"if" * exp * block * (Rw"else" * block)^-1 / node("if", "cond", "th", "el")
       + Rw"while" * exp * block / node("while", "cond", "body")
       + var * Eq * exp / node("ass", "lhs", "e")
       + Rw"return" * (exp +  lpeg.Cc("void"))/ node("ret", "e")
       + exp / node("exp", "e");
  new = Rw"new" * Type * OP * exp * CP / node("newArray", "type", "size");
  parameter = Type * Id / node("parameter", "typeVar" , "id");
  primary = double / node("number double", "num")
      + integer / node("number int", "num")
      + OP * lpeg.V"exp" * CP
      + inc
      + varExp
      + new;
  varExp = var / node("varExp", "var");
  inc = ((var * opInc) / matchPostIncDec + (opInc * var) / matchPreIncDec);

  var = lpeg.Ct((Id) * (OBB  * exp * CBB)^0) * S / foldVar;
  call = Id * OP * (arguments)^-1 * CP / node("call", "name", "arguments");
  postfix = call + primary;
  postfixCast = lpeg.Ct((postfix * (Rw"as" * Type)^0)) / foldCast;
  factor = postfixCast
        + opUn * factor / node("unarith", "op", "e");
  arguments = lpeg.Ct(exp * (Coma * exp)^0);
  expM = lpeg.Ct(factor * (opM * factor)^0) / fold;
  expC = lpeg.Ct(expM * (opC * expM)^0) / compare;
  expA = lpeg.Ct(expC * (opA * expC)^0) / fold;
  exp = lpeg.V"expA";
  S = lpeg.S(" \n\t")^0 *
        lpeg.P(function (_,p)
          SyntaxAnalyser.lastpos = math.max(SyntaxAnalyser.lastpos, p); return true end);
  Type =  OBB * Type * CBB / node("arrayType", "array")
  + (RwC"int" + RwC"double" + RwC"void" )  / node("primitiveType", "type");
}

SyntaxAnalyser.prog = prog * -1


return{ node = node, RwC = RwC, SyntaxAnalyser=SyntaxAnalyser}