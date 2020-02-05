#!/bin/bash
# On Linux (or at least, Ubuntu), update the libunwind8 package so .NET Core can run, see https://github.com/dotnet/cli/issues/3390
if [ $(uname -s) = 'Linux' ]; then
    sudo apt-get install libunwind8
fi

which mono
which msbuild

dotnet tool restore
dotnet paket restore
dotnet fake run build.fsx $@
