# cd to project root
cd $PSScriptRoot\..

# run
Write-Output "word1 word5" "word2 word6" "word3 word7" "word4 word8" | `
runhaskell -i"$PSScriptRoot\..\src\" -i"$PSScriptRoot\..\src\Chart" "$PSScriptRoot\..\src\Main" -h -d " "

# run again
runhaskell -i"$PSScriptRoot\..\src\" -i"$PSScriptRoot\..\src\Chart" "$PSScriptRoot\..\src\Main" -h -d " " "util/test0" "util/test1"
