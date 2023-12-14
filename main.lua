local pt = require "pt"
local parser = require("parser")
local Compiler = require("compiler")

local premable = [[
  @.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
  @.str.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1
  @.str.2 = private unnamed_addr constant [4 x i8] c"%p\0A\00", align 1
  
  ; Function Attrs: noinline nounwind optnone uwtable
  define dso_local void @printI(i32 noundef %0) #0 {
    %2 = alloca i32, align 4
    store i32 %0, ptr %2, align 4
    %3 = load i32, ptr %2, align 4
    %4 = call i32 (ptr, ...) @printf(ptr noundef @.str, i32 noundef %3)
    ret void
  }
  
  declare i32 @printf(ptr noundef, ...) #1
  
  ; Function Attrs: noinline nounwind optnone uwtable
  define dso_local void @printD(double noundef %0) #0 {
    %2 = alloca double, align 8
    store double %0, ptr %2, align 8
    %3 = load double, ptr %2, align 8
    %4 = call i32 (ptr, ...) @printf(ptr noundef @.str.1, double noundef %3)
    ret void
  }

  ; Function Attrs: noinline nounwind optnone uwtable
  define dso_local void @printP(ptr noundef %0) #0 {
    %2 = alloca ptr, align 8
    store ptr %0, ptr %2, align 8
    %3 = load ptr, ptr %2, align 8
    %4 = call i32 (ptr, ...) @printf(ptr noundef @.str.2, ptr noundef %3)
    ret void
  }

  ; Function Attrs: nounwind allocsize(0)
  declare noalias ptr @malloc(i64 noundef) #1

]]

--local file = io.open("text.txt", "r")
--local input = file:read("*a")
--file:close()

local input = io.read("*a")
local tree = parser.SyntaxAnalyser.prog:match(input)
if not tree then
  io.write("syntax error near <<" ..
    string.sub(input, parser.SyntaxAnalyser.lastpos - 10, parser.SyntaxAnalyser.lastpos - 1) .. "|" ..
    string.sub(input, parser.SyntaxAnalyser.lastpos, parser.SyntaxAnalyser.lastpos + 10), ">>\n")
  os.exit(1)
end
io.write(premable)
--do print(pt.pt(tree)); return end
local e = Compiler:codeProg(tree)