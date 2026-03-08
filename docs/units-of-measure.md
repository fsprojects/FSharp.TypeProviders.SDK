---
title: Units of Measure
category: Documentation
categoryindex: 1
index: 5
---
# Units of Measure in F# Type Providers

Use `ProvidedMeasureBuilder` to annotate numeric types exposed by a type provider with F# units of measure (e.g. `float<kg>`).

## Standard SI Units

Use `ProvidedMeasureBuilder.SI` to reference standard units from
`Microsoft.FSharp.Data.UnitSystems.SI`. Pass either the unit symbol (e.g. `"kg"`) or the
unit name in lowercase (e.g. `"kilogram"`):

```fsharp
let kg     = ProvidedMeasureBuilder.SI "kg"       // UnitSymbols.kg
let m      = ProvidedMeasureBuilder.SI "m"        // UnitSymbols.m
let s      = ProvidedMeasureBuilder.SI "s"        // UnitSymbols.s
let kelvin = ProvidedMeasureBuilder.SI "kelvin"   // UnitNames.kelvin
```

## Annotating Types with Units

Use `ProvidedMeasureBuilder.AnnotateType` to produce annotated numeric types such as `float<kg>`:

```fsharp
let kg = ProvidedMeasureBuilder.SI "kg"
let floatKg = ProvidedMeasureBuilder.AnnotateType(typeof<float>, [ kg ])
// floatKg represents float<kg>

let weightProp =
    ProvidedProperty("WeightKg", floatKg, isStatic = true,
        getterCode = fun _args -> <@@ 70.0 @@>)
myType.AddMember weightProp
```

## Compound Units

`ProvidedMeasureBuilder` provides combinators for composing units:

```fsharp
let kg = ProvidedMeasureBuilder.SI "kg"
let m  = ProvidedMeasureBuilder.SI "m"
let s  = ProvidedMeasureBuilder.SI "s"

// Product: kg·m
let kgTimesM = ProvidedMeasureBuilder.Product(kg, m)

// Ratio: kg/m
let kgPerM = ProvidedMeasureBuilder.Ratio(kg, m)

// Square: m²
let mSquared = ProvidedMeasureBuilder.Square(m)

// Acceleration: m/s²
let accel = ProvidedMeasureBuilder.Ratio(m, ProvidedMeasureBuilder.Square(s))

// Inverse: 1/s (frequency in Hz)
let perSecond = ProvidedMeasureBuilder.Inverse(s)

// Dimensionless
let one = ProvidedMeasureBuilder.One
```

## Non-Standard (Custom) Units

To expose a custom unit of measure (not from the SI library), define an erased
`ProvidedTypeDefinition` and use it directly as a measure argument to `AnnotateType`:

```fsharp
// Define a custom unit "USD" (US dollars)
let dollar = ProvidedTypeDefinition(asm, ns, "USD", None, isErased = true)
this.AddNamespace(ns, [ dollar ])

// Use the custom unit to annotate a type
let floatDollar = ProvidedMeasureBuilder.AnnotateType(typeof<float>, [ dollar ])

let priceProp = ProvidedProperty("Price", floatDollar, getterCode = fun _ -> <@@ 9.99 @@>)
myType.AddMember priceProp
```

## Multiple Unit Arguments (Generic Types)

`AnnotateType` also supports generic types with multiple type arguments. For example, to produce
`Vector<float, kg>`:

```fsharp
let kg = ProvidedMeasureBuilder.SI "kg"
// Suppose vectorType is the generic definition of Vector<'T, 'U>
let annotated = ProvidedMeasureBuilder.AnnotateType(vectorType, [ typeof<float>; kg ])
```
