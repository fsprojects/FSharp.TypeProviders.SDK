module File1


open StressProvider

type Provided = Provider<"">
let providedTags = Provided.Tags


type Provided2a = Provider2<"a">
let providedTags2a = Provided2a.Tags

type Provided2empty = Provider2<"">
let providedTags2empty = Provided2empty.Tags

type Provided3  = Provider3<"three">

//type Provided2missing = Provider2< >
//let providedTags2missing = Provided2missing.Tags

type Generated1 = GenerativeProvider<3>
type Generated3 = GenerativeProvider3<"three">

