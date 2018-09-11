#if INTERACTIVE
#r @"../test/StressProvider.dll"
#endif

module StressProvider.Tests


let x = File1.providedTags.Tag1
let x2a = File1.providedTags2a.Tag1
let x2empty = File1.providedTags2empty.Tag1
