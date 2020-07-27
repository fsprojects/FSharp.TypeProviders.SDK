@echo off
cls

dotnet tool restore
dotent paket restore
dotnet fake run build.fsx %*
