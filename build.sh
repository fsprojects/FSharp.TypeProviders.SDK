#!/bin/bash
mono .nuGet/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
mono .nuGet/NuGet.exe install Nunit.Runners -OutputDirectory packages -ExcludeVersion -Version 2.6.4
mono packages/FAKE/tools/FAKE.exe build.fsx $@
