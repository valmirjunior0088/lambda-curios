# lambda-curios

λCurios: The new typechecker for Curios.

## Primer

```
x, y ∈ identifiers

a, b, c, f, A, B ::=
  | x
  
  | Type
  
  | (x: A) -> B
  | x => b
  | f a
  
  | (x: A) !> B
  | a, b
  | split a {|x, y| b}

  | {x y}
  | :x
  | match a {|x| b; |y| c;}

Γ ::=
  | ∅
  | Γ; x : A
  | Γ; x = a
```

## Explanation

The central idea to λCurios is that inductive types are not formed by adding new axioms to the type system (like in the Calculus of Inductive Constructions). Instead, the type system defines dependent pair types and label types, and its eliminators add knowledge about the scrutinee to the context. This allows new inductive data types to be introduced by encoding them using a combination of these two constructs (alongside the usual dependent function types). 

It is important to note that there is no syntactical difference between terms and types. A different metavariable is used when a term plays the role of a type (i.e. `a` for terms and `A` for types), but types are terms, and can be manipulated as such.

The following sections go into more detail about different aspects of the type system, with remarks about the implementation.

### Identifiers

```
x, y ∈ identifiers
```

Valid identifiers (variables or labels) are composed of `['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']`.

### Terms

```
a, b, c, f, A, B ::=
  | x
  
  | Type
  
  | (x: A) -> B
  | x => b
  | f a
  
  | (x: A) !> B
  | a, b
  | split a {|x, y| b}

  | {x y}
  | :x
  | match a {|x| b; |y| c;}
```

- The type of all types is `Type`.

- Function types are written `(x: A) -> B`. When `x` is not free in `B`, it can be abbreviated to `A -> B`.

- Functions are written `x => b`.

- Application is just juxtaposition, i.e. `f a`.

- Pair types are written `(x: A) !> B`. When `x` is not free in `B`, it can be abbreviated to `A !> B`.

- Pairs are written `a, b`.

- Split expressions, used to eliminate pairs, are written `split a {|x, y| b}`.

- Label types are written `{x y}`.

- Labels are written `:x`.

- Match expressions, used to eliminate labels, are written `match a {|x| b; |y| c;}`.

### Contexts

```
Γ ::=
  | ∅
  | Γ; x : A
  | Γ; x = a
```

Contexts start out empty (i.e. `∅`) and are formed by a sequence of declarations (i.e. `Γ; x : A`) and definitions (i.e. `Γ; x = a`).

## Rules

The type system is a Pure Type System a la Henk Barendregt, but with `Type : Type`. This was chosen mainly because even though this is a type system with dependent types, it is meant to be used in the core of a programming language (in contrast with the fact that most type systems with dependent types are used in the core of theorem provers). General recursion is considered an essential part of any programming language, and since general recursion introduces inconsistency to the type system (with regards to theorem proving), `Type : Type` was introduced for simplicity.

This does not mean that this type system is unfit for theorem proving. The only thing it means is that termination checking is optional, and when such a functionality is implemented, it will be disabled by default.

Function types, functions and applications behave as usual: functions are typed by dependent function types, and applications propagate their right-hand side to both the body of the function and the body of the dependent function type in order to eliminate the function.

Just like function types, pair types are also quantifiers, but pairs and split expressions behave a little differently from their function counterparts. Like pair types suggest, pairs propagate the term at the left-hand side to the pair type. Split expressions, apart from eliminating pairs, also perform dependent elimination: if the scrutinee of a split expression reduces to a variable, the context is augmented with the knowledge that the variable stands for a pair.

Label types are also known as finite types, and they introduce a set of scopeless labels. The only requirement for a label type to be considered a valid type is that the labels it introduces are distinct from each other. Like split expressions, match expressions also perform dependent elimination if the scrutinee reduces to a variable: the context in each branch is augmented with the knowledge that the variable is a label.

## Example

```
id : (A: Type) -> A -> A;
id = A => a =>
  a;

Unit : Type;
Unit =
  {unit};

unit : Unit;
unit =
  :unit;

Nat : Type;
Nat =
  (label: {zero succ}) !> match label {
    |zero| Unit;
    |succ| Nat;
  };

zero : Nat;
zero =
  :zero, unit;

succ : Nat -> Nat;
succ = value =>
  :succ, value;

add : Nat -> Nat -> Nat;
add = one => other =>
  split one {|label, one| match label {
    |zero| other;
    |succ| succ (add one other);
  }};

Stream : Type -> Type;
Stream = A =>
  A !> (Unit -> Stream A);

from : Nat -> Stream Nat;
from = initial =>
  initial, (unit => from (succ initial));

List : Type -> Type;
List = A =>
  (label: {null cons}) !> match label {
    |null| Unit;
    |cons| A !> List A;
  };

null : (A: Type) -> List A;
null = A =>
  :null, unit;

cons : (A: Type) -> A -> List A -> List A;
cons = A => a => rest =>
  :cons, a, rest;

take : (A: Type) -> Nat -> Stream A -> List A;
take = A => quantity => stream =>
  split quantity {|label, quantity| match label {
    |zero| null A;
    |succ| split stream {|a, rest| cons A a (take A quantity (rest unit))};
  }};

Vec : Type -> Nat -> Type;
Vec = A => size =>
  split size {|label, size| match label {
    |zero| Unit;
    |succ| A !> Vec A size;
  }};

empty : (A: Type) -> Vec A zero;
empty = A =>
  unit;

push : (A: Type) -> (size: Nat) -> A -> Vec A size -> Vec A (succ size);
push = A => size => a => rest =>
  a, rest;

head : (A: Type) -> (size: Nat) -> Vec A (succ size) -> A;
head = A => size => vec =>
  split vec {|a, _| a};

list : List Nat;
list =
  take Nat (succ (succ (succ zero))) (from zero);
```

## Sources

This type system was implemented with ideas from:

- [ΠΣ: Dependent Types without the Sugar](http://www.cs.nott.ac.uk/~psztxa/publ/pisigma-new.pdf)

ΠΣ uses dependent pairs and finite types to encode inductive data types just like λCurios does, but ΠΣ also introduced the concept of "box" (and an additional construct which allowed boxes to participate in types) with the intent of dealing with recursion. In λCurios, the equirecursion check from Yatima is used, and thus, we avoid the need for these constructs: variables can be used in recursive positions as long as they have a (weak) head-normal form.

If a variable without a (weak) head-normal form (such as `bad : Type; bad = bad;`) appears in a type, then the type checker loops during reduction. It is important to recall that type systems featuring dependent types rely on evaluation during type checking, and evaluation is essentially reduction.

Additionally, in ΠΣ, the concept of "box" is also used in the definition of lazy data types such as `Stream`. But in λCurios, a lazy value can be encoded with `(A: Type) -> Unit -> A`. In the previous section, it is shown how the `Stream` data type (a inductive and lazy data type) can be encoded in λCurios. The choice of using a function that takes a `Unit` value might be revisited in the future, but for proof-of-concept purposes, it is more than sufficient.

- [Yatima](https://github.com/yatima-inc/yatima-lang-alpha)

Yatima is a project that stems from the creators of [Formality](https://github.com/Soonad/Formality-Core), and both of these languages use the concept of equirecursive types: a recursive type and the infinite tree it represents are considered the same i.e. the recursive type is equal to its unfolding.

The concept of equirecursive types is not new, and has been a staple in object-oriented languages such as Java and C#, but in Yatima/Formality it is used in an innovative way: to check recursive occurrences of variables in functional programs. This is innovative because the de facto way of checking recursive occurrences of variables in functional programming languages has always been through isorecursive types, where recursion is isomorphic but not equal to its unfolding.

The use of equirecursive types in λCurios also allowed the type system to forego the use of the concept of "box" (used in ΠΣ to control recursion) with the downside that if a variable without a (weak) head-normal form (such as `bad : Type; bad = bad;`) appears in a type, then the type checker loops during reduction.

This is expected from the equirecursive approach, since it is requires less syntactical machinery to work. But it is important to note that, while the it requires less syntactical machinery to work, it requires a much more involved algorithm to be correctly checked. The implementation of beta-equality in λCurios needs to keep track of every step of the tree to know when to consider two types equal, making it not as straightforward as the implementation of beta-equality in the isorecursive approach (which is the approach used by ΠΣ).

Another important detail to note is that the equirecursive approach to recursion also makes other higher-level features such as type inference also not so straightforward to implement.