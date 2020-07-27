@echo off
cls

dotnet tool restore
dotnet fake run build.fsx %*
