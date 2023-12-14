local lpeg = require "lpeg"
local parser = require("parser")

local Compiler = { tempCount = 0; vars = {}; funcs = {}; 
funcTypeTables = {}; funcsParametersTypeTables = {}; 
functionTypeTable = nil; returnCheck = false;}

function Compiler:CompilerError(errorString)
  io.stderr:write(errorString)
  os.exit()
end

function Compiler:newTemp ()
  local temp = string.format("%%T%d", self.tempCount)
  self.tempCount = self.tempCount + 1
  return temp
end

function Compiler:newLabel ()
  local temp = string.format("L%d", self.tempCount)
  self.tempCount = self.tempCount + 1
  return temp
end

local binAOps = {
  ["i32"] = {
    ["+"] = "add",
    ["-"] = "sub",
    ["*"] = "mul",
    ["/"] = "sdiv",
  },
  ["double"] = {
    ["+"] = "fadd",
    ["-"] = "fsub",
    ["*"] = "fmul",
    ["/"] = "fdiv",
  }
}

local binCOps = {
  ["i32"] = {
    [">="] = "sge",
    ["<="] = "sle",
    ["=="] = "eq",
    ["!="] = "ne",
    [">"] = "sgt",
    ["<"] = "slt",
  },
  ["double"] = {
    [">="] = "oge",
    ["<="] = "ole",
    ["=="] = "oeq",
    ["!="] = "une",
    [">"] = "ogt",
    ["<"] = "olt",
  }
  
}

local binCtypes = {
  ["i32"] = "icmp",
  ["double"] = "fcmp",

}

local castTypes = {
  ["i32"] = {
    ["double"] = "sitofp"
  },
  ["double"] = {
    ["i32"] = "fptosi"
  },
}

function Compiler:getLLVMCode(type)
  local llvmCode
  if type == 'int' then
    llvmCode = 'i32'
  elseif type == 'i32' then
    llvmCode = 'i32'
  elseif type == "double" then
    llvmCode = 'double'
  elseif type == "void" then
    llvmCode = 'void'
  else
    llvmCode = "ptr"
  end
  return llvmCode
end

local Utils = {}

function Utils:printTable(t, indent)
  indent = indent or 0

  local function printIndentation()
      return string.rep("  ", indent)
  end

  for k, v in pairs(t) do
      -- Print the key and value
      if type(v) == "table" then
          print(printIndentation() .. k .. ":")
          self:printTable(v, indent + 1) -- Recursively call printTable for sub-tables
      else
          print(printIndentation() .. k .. ": " .. tostring(v))
      end
  end
end

function Utils:deepCompare(t1, t2)
  -- Check if both are tables
  if type(t1) ~= "table" or type(t2) ~= "table" then
      return t1 == t2
  end

  -- Check if both tables have the same length
  if #t1 ~= #t2 then
      return false
  end

  -- Check each key-value pair in t1 and compare with t2
  for k, v in pairs(t1) do
      if not Utils:deepCompare(v, t2[k]) then
        return false
      end
  end
  return true
end

function Compiler:typeTableToString(table)
  if table.tag == 'primitiveType' then
    return table.type
  end
  return '[' .. self:typeTableToString(table.array) .. ']'
end

function Compiler:stringToTypeTable(string)

  local S = lpeg.V"S"
  local Type = lpeg.V"Type"
  local OBB = "[" * S -- box brackets
  local CBB = "]" * S
  
  local node = parser.node
  local RwC = parser.RwC
  
  local tablePattern = lpeg.P{"initial";
  initial = Type;
  Type = OBB * Type * CBB * S / node("arrayType", "array")
        + RwC"void" / node("primitiveType", "type")
        + "i32" * lpeg.Cc"int" * S / node("primitiveType", "type")
        + RwC"int" / node("primitiveType", "type")
        + RwC"double" / node("primitiveType", "type");
  S = lpeg.S(" \n\t")^0;
  }

  local table = tablePattern:match(string)

  return table
end

function Compiler:findVar(id)
  local vars = self.vars
  for i = #vars, 1, -1 do
  if vars[i].id == id then
    return {val = vars[i].reg, type = vars[i].varType}
  end
  end
  self:CompilerError(string.format("variable not found : %s\n", id))
  
end

function Compiler:codeCall(llvmCode, name, regArguments)
  io.write(string.format("call %s @%s(", llvmCode, name))

  if regArguments ~= '' then
    for i = 1, #regArguments do
      local regValue = regArguments[i].val
      local regLLVMCode = self:typeTabletoLLVMCode(regArguments[i].type)
      io.write(string.format("%s %s",regLLVMCode, regValue))
      if (i < #regArguments) then
        io.write(string.format(","))
      end
    end
  end

  io.write(string.format(")\n"))
end

function Compiler:getFunctionArguments(exp)
  local list_arguments_to_exps = {}
  if self.funcsParametersTypeTables[exp.name] then
    if exp.arguments == nil then
      self:CompilerError(string.format("function call \"%s\" missing positional arguments\n", exp.name))
    end
    if #exp.arguments ~= #self.funcsParametersTypeTables[exp.name] then
      self:CompilerError(string.format("function call \"%s\" has the wrong ammount of positional arguments: used %d arguments but needs %d\n", exp.name, #exp.arguments, #self.funcsParametersTypeTables[exp.name]))
    end

    for i = 1, #exp.arguments do
      local tableArgument = self:codeExp(exp.arguments[i])
      local parameterTypeTable = self.funcsParametersTypeTables[exp.name][i]
      if parameterTypeTable == nil then
        self:CompilerError(string.format("function call \"%s\" parameter position \"%s\" does not exist\n", exp.name, i))
      end
      
      tableArgument = self:verify_and_emit_cast(tableArgument,parameterTypeTable)

      list_arguments_to_exps[i] = tableArgument
    end
  end
  return list_arguments_to_exps
end

function Compiler:verifyFunctionExists(name)
  if not self.funcs[name] then
    io.stderr:write(string.format("unknown function : %s\n",name))
    os.exit()
  end
end

function Compiler:codeExp_int(exp)
  return {val = string.format("%d", exp.num) , type = self:stringToTypeTable("i32") }
end

function Compiler:codeExp_double(exp)
  return {val = string.format("%.16e", exp.num) , type = self:stringToTypeTable("double") }
end

function Compiler:codeExp_varExp(exp)
  local regETable = self:codeExp(exp.var)

  local res = self:newTemp()

  local llvmCode = self:typeTabletoLLVMCode(regETable.type)

  io.write(string.format("%s = load %s, ptr %s\n", res, llvmCode, regETable.val))

  return {val = res, type = regETable.type }
end

function Compiler:codeExp_indexed(exp)
  --solve array
  local regVTable = self:codeExp(exp.array)

  local indexingExpTable = self:codeExp(exp.index)

  if self:typeTableToString(indexingExpTable.type) ~= 'int' then
    self:CompilerError("index of array operation is not int\n")
  end

  local i64Indexing = self:newTemp()

  io.write(string.format("%s = sext i32 %s to i64\n",i64Indexing, indexingExpTable.val ))

  local indexedArray = self:newTemp()

  local indexedValueTypeCode = self:typeTabletoLLVMCode(regVTable.type.array)

  io.write(string.format("%s = getelementptr inbounds %s, ptr %s, i64 %s\n", indexedArray, indexedValueTypeCode, regVTable.val, i64Indexing))
  
  return {val = indexedArray; type = regVTable.type.array}
end

function Compiler:codeExp_id(exp)
  return self:findVar(exp.id)
end

function Compiler:verify_and_emit_cast(tableE,castingTypeTable)

  local originalTypeLLVM = self:typeTabletoLLVMCode(tableE.type)

  local castingTypeLLVM = self:typeTabletoLLVMCode(castingTypeTable)

  -- dont emit cast if types are equal already
  if Utils:deepCompare(tableE.type,castingTypeTable) then
    return tableE
  end

  if tableE.type.tag == "arrayType" then
    self:CompilerError("array casting not permitted\n")
  end

  if castingTypeTable.tag == "arrayType" then
    self:CompilerError("casting element to array type not permitted\n")
  end

  if castingTypeLLVM == 'void' or tableE.type == 'void' then
    self:CompilerError("casting void not permitted\n")
  end

  local castCode = castTypes[originalTypeLLVM][castingTypeLLVM]

  local res = self:newTemp()

  io.write(string.format("%s = %s %s %s to %s\n", res,castCode, originalTypeLLVM ,tableE.val, castingTypeLLVM))
  return {val = res, type = self:stringToTypeTable(castingTypeLLVM) }
  
end

function Compiler:codeExp_cast(exp)
  local tableE = self:codeExp(exp.e)
  local castingType = self:typeTabletoLLVMCode(exp.type)

  return self:verify_and_emit_cast(tableE,exp.type)

end

function Compiler:codeExp_unarith(exp)
  local tableE = self:codeExp(exp.e)

  if tableE.type.tag == "arrayType" then
    self:CompilerError("unarith operation on array not permitted")
  end


  local llvmCode = self:typeTabletoLLVMCode(tableE.type)
  local subType = binAOps[llvmCode]["-"]

  --cast zero to the expression type if it is not i32 already
  local zeroResCast = {val = '0', type = self:stringToTypeTable('i32')}
  if self:typeTableToString(tableE.type) ~= 'i32' then
    local zeroExp = {tag = "number int", num = 0 }
    local zeroExpcast = { op = tableE.type, e = zeroExp};
    zeroResCast = self:codeExp_cast(zeroExpcast)
  end
  local res = self:newTemp()
  io.write(string.format("%s = %s %s %s, %s\n", res, subType, tableE.type, zeroResCast.val, tableE.val))
  return {val = res, type = tableE.type }
end

function Compiler:verifyTypeMismatch(typeR1,typeR2)
  if not Utils:deepCompare(typeR1,typeR2) then
    io.stderr:write(string.format("mismatch type between binarith operators\n"))
    os.exit()
  end
end

-- function to help with binary operators
function Compiler:castToMostPrecise(tableR1,tableR2)
  local llvmCodeR1 = self:typeTabletoLLVMCode(tableR1.type)
  local llvmCodeR2 = self:typeTabletoLLVMCode(tableR2.type)

  if llvmCodeR1 == llvmCodeR2 then
    return tableR1, tableR2
  end

  local types = {'double', 'i32'}

  for _, type in ipairs(types) do
    if llvmCodeR1 == type then
      local castTableR2 = self:verify_and_emit_cast(tableR2,tableR1.type)
      return tableR1, castTableR2
    end

    if llvmCodeR2 == type then
      local castTableR1 = self:verify_and_emit_cast(tableR1,tableR2.type)
      return castTableR1, tableR2
    end
  end

  self:CompilerError("unknown type to cast")

end

function Compiler:codeExp_binarith(exp)
  local tableR1 = self:codeExp(exp.e1)
  local tableR2 = self:codeExp(exp.e2)
  local res = self:newTemp()

  local typeTableR1 = tableR1.type
  local typeTableR2 = tableR2.type

  if typeTableR1.tag == 'arrayType' or typeTableR2.tag == 'arrayType' then
    self:CompilerError("binarith operation on arrays not permitted")
  end
  
  --binarith implicit casting
  tableR1,tableR2 = self:castToMostPrecise(tableR1,tableR2)
  
  local llvmCodeR1 = self:typeTabletoLLVMCode(tableR1.type)
  local llvmCodeR2 = self:typeTabletoLLVMCode(tableR2.type)

  io.write(string.format("%s = %s %s %s, %s\n",
              res, binAOps[llvmCodeR1][exp.op], llvmCodeR1, tableR1.val, tableR2.val))
              return {val = res, type = tableR1.type }
end

function Compiler:codeExp_bincomp(exp)
  local tableR1 = self:codeExp(exp.e1)
  local tableR2 = self:codeExp(exp.e2)
  local res = self:newTemp()
  local conv = self:newTemp()

  local typeTableR1 = tableR1.type
  local typeTableR2 = tableR2.type

  if typeTableR1.tag == 'arrayType' or typeTableR2.tag == 'arrayType' then
    self:CompilerError("bincomp operation on arrays not permitted")
  end
  
  -- bincomp implicit casting
  tableR1,tableR2 = self:castToMostPrecise(tableR1,tableR2)

  local llvmCodeR1 = self:typeTabletoLLVMCode(tableR1.type)
  local llvmCodeR2 = self:typeTabletoLLVMCode(tableR2.type)

  io.write(string.format("%s = %s %s %s %s, %s\n",
              res,binCtypes[llvmCodeR1], binCOps[llvmCodeR1][exp.op],llvmCodeR1 ,tableR1.val, tableR2.val))
  io.write(string.format("%s = zext i1 %s to i32\n",
  conv, res))
  return {val = conv, type = self:stringToTypeTable('i32') }
end

function Compiler:codeExp_call(exp)
  self:verifyFunctionExists(exp.name)
  local funcTypeTable = self.funcTypeTables[exp.name]

  local llvmCode = self:typeTabletoLLVMCode(funcTypeTable)

  if llvmCode == 'void' then
    io.stderr:write(string.format("called void function as expression\n"))
    os.exit()
  end

  local list_arguments_to_exps = self:getFunctionArguments(exp)

  local reg = self:newTemp()
  io.write(string.format("%s = ", reg))
  self:codeCall(llvmCode, exp.name, list_arguments_to_exps)
  return {val = reg, type = funcTypeTable }
end

function Compiler:getTypeByteSize(typeNode)
  if typeNode.array.tag == 'primitiveType' and typeNode.array.type == 'int' then
    return 4
  end
  return 8
end

function Compiler:codeExp_newArray(exp)
  
  local sizeExpTable = self:codeExp(exp.size)

  if exp.type.tag ~= "arrayType" then
    self:CompilerError(string.format("new array must be of array type, not %s\n", self:typeTableToString(exp.type)))
  end

  if self:isVoidType(exp.type) then
    self:CompilerError(string.format("cannot declare array of type %s \n", self:typeTableToString(exp.type)))
  end
  --implicit casting to int 
  sizeExpTable = self:verify_and_emit_cast(sizeExpTable,self:stringToTypeTable('i32'))

  local i64SizeExpTable = self:newTemp()

  io.write(string.format("%s = sext i32 %s to i64\n", i64SizeExpTable, sizeExpTable.val))

  local i64MallocSize = self:newTemp()

  local typeSizeInBytes = self:getTypeByteSize(exp.type)
  
  io.write(string.format("%s = mul i64 %s, %s\n", i64MallocSize, i64SizeExpTable, typeSizeInBytes))

  local ptrArray = self:newTemp()

  io.write(string.format("%s = call noalias ptr @malloc(i64 noundef %s)\n", ptrArray, i64MallocSize))

  return {val = ptrArray, type = exp.type}
end

function Compiler:codeExp_inc(st)
  local lhsTable = self:codeExp(st.var)

  local varExp = self:codeExp({tag = "varExp", var = st.var})

  local llvmCode = self:typeTabletoLLVMCode(varExp.type)

  -- op on varExp
  local resOp = self:newTemp()

  local op = string.sub(st.opInc, 1, 1)

  local opLLVM = binAOps[llvmCode][op]

  io.write(string.format("%s = %s %s %s, 1\n", resOp, opLLVM, llvmCode, varExp.val))

  -- store value on lhsTable
  io.write(string.format("store %s %s, ptr %s\n", llvmCode, resOp, lhsTable.val))

  if st.use_before_inc then
    return {val = varExp.val, type = varExp.type}
  else
    return {val = resOp, type = varExp.type}
  end

end


function Compiler:codeExp(exp)
  local tag = exp.tag
  if tag == "number int" then return self:codeExp_int(exp)
  elseif tag == "number double" then return self:codeExp_double(exp)
  elseif tag == "varExp" then return self:codeExp_varExp(exp)
  elseif tag == "indexed" then return self:codeExp_indexed(exp)
  elseif tag == 'id' then return self:codeExp_id(exp)
  elseif tag == "cast" then return self:codeExp_cast(exp)
  elseif tag == "unarith" then return self:codeExp_unarith(exp)
  elseif tag == "binarith" then return self:codeExp_binarith(exp)
  elseif tag == "bincomp" then return self:codeExp_bincomp(exp)
  elseif tag == "call" then return self:codeExp_call(exp)
  elseif tag == "newArray" then return self:codeExp_newArray(exp)
  elseif tag == "inc" then return self:codeExp_inc(exp)
  else
    io.stderr:write(string.format("%s : expression not yet implemented\n", tag))
    os.exit()
  end
end

local printType = {
  ["i32"] = "printI",
  ["double"] = "printD",
  ["ptr"] = "printP",
}

function Compiler:codeCond(exp, Ltrue, Lfalse)
  local regTable = self:codeExp(exp)
  local llvmCode = self:typeTabletoLLVMCode(regTable.type)

  local aux = self:newTemp()
  io.write(string.format("%s = %s %s %s %s, 0\nbr i1 %s, label %%%s, label %%%s\n", 
  aux, binCtypes[llvmCode], binCOps[llvmCode]["!="], llvmCode, 
  regTable.val, aux, Ltrue, Lfalse))
end

function Compiler:codeLabel (label)
  io.write(string.format("%s:\n", label))
end

function Compiler:codeJmp (label)
  io.write(string.format("br label %%%s\n", label))
end

function Compiler:codeStat_seq (st)
  self:codeStat(st.s1)
  self:codeStat(st.s2)
end

function Compiler:codeStat_block (st)
  local vars = self.vars
  local level = #vars
  self:codeStat(st.body)
  for i = #vars, level + 1, -1 do
    table.remove(vars)
  end
end

function Compiler:codeStat_if(st)
  local Lthen = self:newLabel()
  local Lend = self:newLabel()
  local Lelse = self:newLabel()
  self:codeCond(st.cond, Lthen, st.el and Lelse or Lend)
  self:codeLabel(Lthen)
  self:codeStat(st.th)
  self:codeJmp(Lend)
  if st.el then
    self:codeLabel(Lelse)
    self:codeStat(st.el)
    self:codeJmp(Lend)
  end
  self:codeLabel(Lend)
end

function Compiler:codeStat_while(st)
  local Lcond = self:newLabel()
  local Lbody = self:newLabel()
  local Lend = self:newLabel()
  self:codeJmp(Lcond)
  self:codeLabel(Lcond)
  self:codeCond(st.cond, Lbody, Lend)
  self:codeLabel(Lbody)
  self:codeStat(st.body)
  self:codeJmp(Lcond)
  self:codeLabel(Lend)
end

function Compiler:codeStat_print(st)
  local regTable = self:codeExp(st.e)

  local llvmCode = self:typeTabletoLLVMCode(regTable.type)
  local currPrintType = printType[llvmCode]
  if currPrintType == nil then
    self:CompilerError("unknown type to print")
  end
  io.write(string.format("call void @%s(%s %s)\n", currPrintType, llvmCode, regTable.val))
end

function Compiler:codeStat_call(st)
  self:verifyFunctionExists(st.name)
  local list_arguments_to_exps = self:getFunctionArguments(st)
  local funcTypeTable = self.funcTypeTables[st.name]
  local llvmCode = self:typeTabletoLLVMCode(funcTypeTable)
  self:codeCall(llvmCode, st.name, list_arguments_to_exps)
end

function Compiler:codeStat_exp(st)
  self:codeExp(st.e)
end

function Compiler:createVar(id, type, reg)
  -- verify if variable is already used by comparing id value in the table with function argument id
  for i = 1, #self.vars do
    if self.vars[i].id == id then
      self:CompilerError(string.format("variable already declared : %s\n", id))
    end
  end

  self.vars[#self.vars + 1] = {id = id, varType = type, reg = reg}
end

function Compiler:codeEmptyVar(id, reg, varType)
  local varllvmCode = self:typeTabletoLLVMCode(varType)

  io.write(string.format("%s = alloca %s", reg,varllvmCode))
  self:createVar(id, varType, reg)
end

-- makea function that verifys if the primitiveType is a void type.
-- if it is return true
-- if its an arrayTYpe recursively call this function
function Compiler:isVoidType(typeTable)
  if typeTable.tag == 'primitiveType' then
    if typeTable.type == 'void' then
      return true
    end
    return false
  end
  return self:isVoidType(typeTable.array)
end

function Compiler:codeStat_var(st)

  local regV = self:newTemp()

  if st.e == nil then
    self:codeEmptyVar(st.id, regV, st.typeVar)
    return
  end

  if self:isVoidType(st.typeVar) then
    self:CompilerError(string.format("cannot declare variable of type %s \n", self:typeTableToString(st.typeVar)))
  end

  local varllvmCode = self:typeTabletoLLVMCode(st.typeVar)

  local expTable = self:codeExp(st.e)

  expTable = self:verify_and_emit_cast(expTable,st.typeVar)

  io.write(string.format("%s = alloca %s\nstore %s %s, ptr %s\n", regV, varllvmCode, varllvmCode, expTable.val, regV))

  self:createVar(st.id, st.typeVar, regV)
end

function Compiler:codeStat_ass(st)
  local regETable = self:codeExp(st.e)
  local regVTable = self:codeExp(st.lhs)

  regETable = self:verify_and_emit_cast(regETable,regVTable.type)

  local llvmCodeE = self:typeTabletoLLVMCode(regETable.type)

  io.write(string.format("store %s %s, ptr %s\n", llvmCodeE, regETable.val, regVTable.val))
end

function Compiler:codeStat_ret(st)
  self.returnCheck = true
  local functionTypeTable = self.functionTypeTable
  local llvmCode = self:typeTabletoLLVMCode(functionTypeTable)

  local regETable

  if st.e == 'void' then
    regETable = {type = self:stringToTypeTable(st.e)}
  else
    regETable = self:codeExp(st.e)
  end

  -- filter case of void in returned expression or in the function return mismatch
  local returnIsVoid = Utils:deepCompare(regETable.type,self:stringToTypeTable('void'))

  if returnIsVoid and llvmCode ~= 'void' or 
  (not returnIsVoid) and llvmCode == 'void' then
    
    self:CompilerError(string.format("mismatch type between returned expression type: %s and function return: %s\n",  self:typeTableToString(regETable.type), self:typeTableToString(functionTypeTable)))
  end

  -- filter case of void return 
  if llvmCode == 'void' then
    io.write(string.format("ret void\n"))
    return
  end

  -- make casting only if both are primitives 
  if regETable.type.tag == 'primitiveType' and functionTypeTable.tag == 'primitiveType' then
    regETable = self:verify_and_emit_cast(regETable,functionTypeTable)
    io.write(string.format("ret %s %s\n", llvmCode, regETable.val))
    return
  end
  
  -- regETable.type.tag == 'arrayType' or functionTypeTable.tag == 'arrayType'
  if not Utils:deepCompare(regETable.type,functionTypeTable) then
    self:CompilerError(string.format("mismatch type between returned expression type: %s and function return: %s\n", self:typeTableToString(regETable.type), self:typeTableToString(functionTypeTable)))
  end
  
  -- both are arrays of the same type
  io.write(string.format("ret %s %s\n", llvmCode, regETable.val))
end

function Compiler:codeStat (st)
  self.returnCheck = false
  local tag = st.tag
  if tag == "seq" then self:codeStat_seq(st)
  elseif tag == "block" then self:codeStat_block(st)
  elseif tag == "if" then self:codeStat_if(st)
  elseif tag == "while" then self:codeStat_while(st)
  elseif tag == "print" then self:codeStat_print(st)
  elseif tag == "call" then self:codeStat_call(st)
  elseif tag == "exp" then self:codeStat_exp(st)
  elseif tag == "var" then self:codeStat_var(st)
  elseif tag == "ass" then self:codeStat_ass(st)
  elseif tag == "ret" then self:codeStat_ret(st)
  elseif tag == "inc" then self:codeStat_inc(st)
  else
    io.stderr:write(string.format("%s: statement not yet implemented\n",tag))
    os.exit()
  end
end

local poscode = [[
}
]]

function Compiler:typeTabletoLLVMCode(typeTable)
  local string = self:typeTableToString(typeTable)
  return self:getLLVMCode(string)
end

function Compiler:codeParam(func_name,paramsIdList)
  local list_param_to_reg = {}
  self.funcsParametersTypeTables[func_name] = {}
  for i = 1, #paramsIdList do
    local regParam = self:newTemp()
    local paramId = paramsIdList[i].id
    local paramTypeTable = paramsIdList[i].typeVar
    if Compiler:isVoidType(paramTypeTable) then
      self:CompilerError(string.format("cannot declare parameter of type %s \n", self:typeTableToString(paramTypeTable)))
    end
    local paramLLVMCode = self:typeTabletoLLVMCode(paramTypeTable)
    self.funcsParametersTypeTables[func_name][i] = paramTypeTable
    list_param_to_reg[paramId] = regParam
    io.write(string.format("%s noundef %s", paramLLVMCode, list_param_to_reg[paramId]))
    if (i < #paramsIdList) then
      io.write(string.format(","))
    end
  end
  io.write(string.format(") {\n"))

  for i = 1, #paramsIdList do
    local regV = self:newTemp()
    local paramId = paramsIdList[i].id
    local paramTypeTable = self.funcsParametersTypeTables[func_name][i]
    local paramLLVMCode = self:typeTabletoLLVMCode(paramTypeTable)
    local regParam = list_param_to_reg[paramId]
    io.write(string.format("%s = alloca %s\nstore %s %s, ptr %s\n", 
    regV,paramLLVMCode, paramLLVMCode, regParam, regV))
    self:createVar(paramId, paramTypeTable, regV)
  end

end


function Compiler:codeFunc(func)
  self.funcs[func.name] = true
  local functionTypeTable = func.typeRet

  if functionTypeTable == 'void' then
    functionTypeTable = {type = 'void';tag = 'primitiveType';};
  end
  self.functionTypeTable = functionTypeTable
  self.funcTypeTables[func.name] = self.functionTypeTable

  local funcllvmCode = self:typeTabletoLLVMCode(self.functionTypeTable)

  io.write(string.format("define %s @%s(",funcllvmCode, func.name))

  if func.parameters ~= '' then
    self.funcsParametersTypeTables[func.name] = true
    self:codeParam(func.name,func.parameters)
  else
    self.funcsParametersTypeTables[func.name] = false
    io.write(string.format(") {\n"))
  end
  
  self:codeStat(func.body)
  if not self.returnCheck then
    if func.typeRet == 'void' then
      io.write(string.format("ret void\n"))
    else
      io.stderr:write(string.format("non-void function does not have a 'return' statement\n"))
      os.exit()
    end
  end
  io.write(poscode)
end


function Compiler:codeProg (prog)
  for i = 1, #prog do
    self:codeFunc(prog[i])
  end
  if not self.funcs["main"] then
    io.stderr:write(string.format("missing main function\n"))
    os.exit()
  end
end

return Compiler