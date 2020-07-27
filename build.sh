#!/bin/bash

dotnet tool restore
dotnet fake run build.fsx $@
