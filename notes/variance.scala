// everthing needs to be in an onbject, trait, ...
// Nothing has no inhabitants - ??? - used for teaching

// since string is subtype of any,
// list of string is a list of any
// list is defined co-variantly

// List[+A] => Covarient
// List[-A] => contravarient


object variance {
    // val list: List[Any] = ???
    val list: List[Any] = List("sdf")
    // val func2: Any => String = { s: String => "a string" }  // barfs, expects and any
    val func: String => String = { s: Any => "a string" }  // works
}

// List[+A] is defined covariently
// Function[-A, +B]: first one is contravariently
// When i require a function that requires a bigger set (Any)., and you give me a function with subset (wont work),  but if i require a subsett (string) and you give me whole set, works

// Lots of people really good at fp dont use subtyping

trait MyList[A] {
    def contains(a: A): Boolean
}

// contravarient = takes supertype
// covarient = takes subtype
// varience gets in the way of inference

// trait MyList2[+A] {
//     // This barfs, because i may not get a, i might get a higher type
//     def contains(a: A): Boolean
// }


// can do implementation hiding - mix in tons of trait, but pin it to a type, so a value can have a lot of minxins, but be casted.

// biggest difference between type theoy and set theory
// keynote by eugenea change
// talks about category theory - people who reject "this" :
// people to get this, and people who this contradicts what theyve learned in math

// in type theory - multuple values of 1, 1 thats an int, 1 thats a float, ....

// invarient list => ilist
