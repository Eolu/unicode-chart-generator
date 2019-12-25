# cd to project root
Set-Location $PSScriptRoot\..

# compile
ghc -O1 -i"src" -i"src\Chart" "src\Main"

# cleanup intermediate files
Remove-Item -path src\* -include *.o,*.hi -recurse

# move executable to bin
Move-Item -Force -Path src\Main.exe -Destination bin\chart.exe
