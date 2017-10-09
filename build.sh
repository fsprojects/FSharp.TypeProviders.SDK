#!/bin/bash
if test "$OS" = "Windows_NT"
then
  # use .Net
  .paket/paket.exe restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx
else
  # use mono
  # see https://github.com/dotnet/netcorecli-fsc/wiki/.NET-Core-SDK-1.0#using-net-framework-as-targets-framework-the-osxunix-build-fails
  export FrameworkPathOverride=$(dirname $(which mono))/../lib/mono/4.5/
  export RuntimeIdentifier=$(dotnet --info | grep "RID" | awk '{print $2}')


  #sudo apt-get -y install msbuild mono-complete mono-devel fsharp

  which mono
  which dotnet
  which msbuild

  mono .paket/paket.exe restore
  exit_code=$?
  if [ $exit_code -ne 0 ]; then
  	exit $exit_code
  fi

  mono packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx
fi
