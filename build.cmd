@echo off
cls

dotnet tool restore
dotnet paket restore
dotnet fake run build.fsx %*
