#!/bin/bash
mono .nuGet/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
mono .nuGet/NuGet.exe install Nunit.Runners -OutputDirectory packages -ExcludeVersion
mono packages/FAKE/tools/FAKE.exe build.fsx $@
