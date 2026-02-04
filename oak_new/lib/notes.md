## Extension types.

We have `Is(Int, Type) : Kind`, and

- In the future, we might want `Is(Int, Type) : Type` which is what f-ing modules does.
- The rule is, `Is(e, t)` where `t : U_i` has type `U_(i - 1)`, unless `i = 0` and it has type `U_0`

`Is(M, Int -> Int) : Type` when `a : Type` `b : Type`

`In` and `Out` are both introduce and eliminate this type.

This type makes conversion checking type directed.

When we write `Is(Int)`, the elaborator will infer `Int` as `Type` and elaborate it to `Is(Int, Type)`. In the type checker, we always write the kind.

## Universes

`Type : Kind : Sig`

No cumulativity for now. See if coercive cumulativity can work.

## Modal types

Also known as types that have no identity, or can be completely ignored.

if `t` is modal then `t ~= Pack t`

`(a : Type) -> B(a)` is modal if `B(a)` is modal.
If `a : Type` then `a` modal.
This means that `Pack S : Type` where `S : Sig` is modal

`sig { let x : t1; let y : t2 }` is modal if both `t1` and `t2` are modal.

For modal types maybe we can use coercive subtyping to transport along that isomorphism?

## Pack type

`Pack S : Type` where `S : Sig`.

The introduction rule `pack e` must be inferred in checking mode.

This makes `Type` a reflective subuniverse of `Sig`.

We also have bind operator
```
bind x = e
...
```
This suffers from the avoidance problem, so only allow this expression in checking mode for now.
Also, the body must return a modal type.
- Future, since the body must be modal, maybe we can always infer it by just normalizing the result type, and checking if the type refers to the bound variable. If it does, then error.

Pack type does not interact with subtyping at all. Pack types will only compare the two inner signatures for equivalence.

## Subtyping

- Use coercive subtyping
- Since we have extension types, we need type conversion checking.
- This is similar to the eta law problem for units, which need typed conversion checking.
    - Otherwise, it is hard to prove `forall x : unit, x === tt`. For example consider the case with two variables `x === y`.
    - We can only prove that `tt === y` or `y === tt` when we have `tt` on either side.
- Just follow the old rules that you used, `equivalent, structure_equivalent, structure_equivalent_path`
- For `equivalent` in the case when the `ty` is abstract, we must call `structure_equivalent_path`, because no other value can give an abstract type.

## Mutual recursion

general mutual recursion only works for function types that are all modal, and all have type signatures

```
rec {
  // both are modal types, also, each function is not in scope for the type signature
  let f : fun (a : type) -> a -> a
  let f : fun (a b : Int) Int -> Int
  
  let g : fun (a : type) -> b -> b
  
  // this is not modal
  let g : fun type -> type
}
```

## Data types

All data types are structural.

```
let T = struct {
  first : int;
  second : int;
}
```

```
let Option ('A : Type) = enum {
  Some A
  None
}
```

Recursive data types are elaborated into higher kinded iso recursive types.

```
let types = family {
  Tree ('A : Type) = struct {
    value : A
    forest : Forest 'A 
  }
  
  Forest ('A : Type) = List (Tree 'A)
}

let Tree = types.Tree
let Forest = types.Forest
```

To check equivalence of data type families, first sort the declarations by name and then check each declaration one by one for exact equivalence.

## Conversion Checking
 
- We must use typed conversion checking, because values at any type `A` where `A : Type` are equal. This is not possible to do with untyped conversion checking.
- Alternative: annotate variables with types.
  - actually, this doesn't work, consider `f(a) === g(b)`. If `f : int -> unit` and `g : bool -> unit`, then these types are still equal. Thus, we still need typed conversion to know the type that we are checking at.

## Singleton kinds

For `S(A, B)` we must have `B : U_i`. However, we require that `U_i > 0`, because singletons have subtyping and we don't want anyn subtyping at universe `0`.
