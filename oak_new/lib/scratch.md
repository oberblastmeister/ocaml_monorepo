(* Ty : (S(Type, Kind) : Sig) *)
(*
  S(Int, Type) : Kind
  S(Int, Out_Type(Ty)) : Kind
  
  S(Int, Type) : Type
  S(Int, Out_type(Ty)) : Type
*)
(* S(Int, Ty) *)

(* hello world *)


(*
first : (S(Int, Type) : Kind)
first = In_(Int)(Int)

second : S(Int, Type)
second = In_(Int)(Int)

unbox x === unbox y

Out_(Int)(first) : Type === Out_(Int)(second) : Type

first : S(Int, Type)

(In_(first)(first) : S(first, S(Int, Type)))

(first <: S(Int, Type))

first <: Type

Out_(first)(first) : S(Int, Type)

Out_(Int)(Out_(first) (first)) : Type

In_(Int)( Out_(Int) )
In_(Int)( Out_(Int) )

first === Int

(first <: S(fun (x) { x }, )

(Out_(Int) (first) <: Type)

Int_(Int)(Out_(Int)(first)) === Int_(Int) (Out_(Int)(second))

first === second


another : (S(first, S(Int, Type)) : Kind)
another = In_(first)(first)

wow : (Out_(Int)(Out_(first)(another)) : Type)
wow = 0

Int : Type === Out_(Int)(Out_(first)(another)) : Type

In_(Int) (Int) : S(Int, Type) === Out_(first)(another) : S(Int, Type)

In_(first) (In_(Int) (Int)) === another
*)

Rules for elaborating singleton types. When singleton type is on the right, for example

(int_alias <: S(Int, Type))

first we do

coe1 = (int_alias <: Type)

Then apply the coercion and check definitional equality.

(coe1 int_alias) === Int

In_(Int)(Out_(Int)(first)) === In_(Int) (Out_(Int)(second))


(In(first, first) <: S(second, Type))

In(first, first)

S(first, S(Int, Type)) <= S(second, S(Int, Type))
