local lfs = require("lfs")

local function startsWith(file, word)
    local f = io.open(file, "r")
    if f then
        local content = f:read("*all")
        f:close()
        return content:find("^%s*" .. word) ~= nil
    end
    return false
end

local function compareFiles(file1, file2)
    local f1 = io.open(file1, "r")
    local f2 = io.open(file2, "r")
    
    if f1 and f2 then
        local content1 = f1:read("*a")
        local content2 = f2:read("*a")
        f1:close()
        f2:close()
        return content1 == content2
    end
    
    return false
end

function printFileContents(filename)
    -- Open the file in read mode
    local file = io.open(filename, "r")

    if file then
        -- Read the entire content of the file
        local content = file:read("*all")

        -- Close the file
        file:close()

        -- Print the content
        print(content)
    else
        print("File not found or unable to open")
    end
end

-- Function to check if a file is empty
local function isFileEmpty(filePath)
    local f = io.open(filePath, "r")
    if f then
        local content = f:read("*a")
        f:close()
        return content == nil or content == ""
    end
    return true
end

-- Function to execute the command and store output and error
local function executeCommand(command)
    local handle = io.popen(command)
    local result = handle:read("*a")
    handle:close()
    return result
end

function compareFilesIgnoringPointers(file1, file2)
    local pattern = "0x%x+" -- Pattern for hexadecimal values
    local f1 = io.open(file1, "r")
    local f2 = io.open(file2, "r")

    if f1 and f2 then
        local content1 = f1:read("*a")
        local content2 = f2:read("*a")

        f1:close()
        f2:close()

        local lines1 = {}
        for line in content1:gmatch("[^\r\n]+") do
            if not string.match(line, pattern) then
                lines1[#lines1 + 1] = line
            end
        end

        local lines2 = {}
        for line in content2:gmatch("[^\r\n]+") do
            if not string.match(line, pattern) then
                lines2[#lines2 + 1] = line
            end
        end

        local modifiedContent1 = table.concat(lines1, "\n")
        local modifiedContent2 = table.concat(lines2, "\n")

        return modifiedContent1 == modifiedContent2
    else
        return false
    end
end

-- Function to perform steps for each directory and .txt file
local function iterateDirectories(rootDirectory)
    local counter_wrong = 0
    for entry in lfs.dir(rootDirectory) do
        if entry ~= "." and entry ~= ".." then
            local fullPath = rootDirectory .. "/" .. entry
            local fileMode = lfs.attributes(fullPath, "mode")
            
            if fileMode == "directory" then
                print("Processing directory:", fullPath)
                
                -- Iterate through .txt files in the current directory
                for txtFile in lfs.dir(fullPath) do
                    if txtFile:match("%.txt$") then
                        local txtFilePath = fullPath .. "/" .. txtFile
                        local errFilePath = txtFilePath:gsub("%.txt$", "") .. ".err"
                        local resFilePath = txtFilePath:gsub("%.txt$", "") .. ".res"
                        
                        -- Execute steps for each .txt file
                        print("Processing TXT file:", txtFilePath)
                        executeCommand("rm -f main.ll a.out main.s main.err")
                        local command = string.format("lua5.1 main.lua < '%s' > main.ll 2> main.err", txtFilePath)
                        local output = executeCommand(command)

                        -- Check main.ll for syntax alert
                        if startsWith("main.ll", "syntax") then
                            print("FAIL: main.ll has syntax error'")
                            counter_wrong = counter_wrong + 1
                        else
                            -- Compare main.err with dynamically generated .err file
                            if not compareFiles("main.err", errFilePath) then
                                print("FAIL: main.err and generated .err file contents are not equal.")
                                counter_wrong = counter_wrong + 1
                            else
                                print("main.err and generated .err file contents are equal.")
                                -- Check if errFilePath has content
                                if not isFileEmpty(errFilePath) then
                                    print("errFilePath has content. Continuing to the next file.")
                                else
                                    -- Execute additional commands
                                    executeCommand("llc-15 main.ll -opaque-pointers")
                                    executeCommand("clang-15 -no-pie main.s")
                                    executeCommand("./a.out > main.res")

                                    if not compareFilesIgnoringPointers("main.res", resFilePath) then
                                        print("FAIL: main.res and generated .res file contents are not equal.")
                                        printFileContents("main.res")
                                        print("-----------")
                                        printFileContents(resFilePath)
                                        counter_wrong = counter_wrong + 1
                                    else
                                        print("main.res and generated .res file contents are equal.")
                                    end
                                end
                            end

                            

                        end
                    end
                end
            end
        end
    end
    print("Number of wrong tests: ", counter_wrong)
end

-- Provide the root directory here
local rootDirectory = lfs.currentdir() .. "/tests"
iterateDirectories(rootDirectory)