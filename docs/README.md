# pong

## Language implementation

### Types

```
data Type
  = TUnit
  | TBool
  | TInt
  | TFloat
  | TDouble
  | TChar
  | TString
  | TCon Name [Type]
  | TArr Type Type
  | TVar Var
  | TRec Type
  | RNil
  | RExt Name Type Type
```

#### Primitive types

| Constructor   | Type                                    | Keyword  |
| ------------- | --------------------------------------- | -------- |
| `TUnit`       | Unit type                               | `unit`   |
| `TBool`       | Boolean                                 | `bool`   |
| `TInt`        | Integer                                 | `int`    |
| `TFloat`      | Single-precision floating point number  | `float`  |
| `TDouble`     | Double-precision floating point number  | `double` |
| `TChar`       | Char                                    | `char`   |
| `TString`     | String                                  | `string` |

These types correspond, in a one-to-one manner, to the built-in language primitives (described [here](#language-primitives)).

#### Composite types

| Constructor   | Type                                    | Notation                |
| ------------- | --------------------------------------- | ----------------------- |
| `TCon`        | Algebraic data type                     | `Con s t r u ...`       |
| `TArr`        | Function type                           | `s → t`                 |
| `TVar`        | Type variable                           | `'0`, `a`               |

##### Algebraic data types

##### Type schemes

Type schemes denote polymorphic types &mdash; types parameterized by one or more type variables. These type variables are said to be *bound* in the scheme under consideration.

| Type scheme                  | Bound variables | Type rep. (Haskell expression)                                               |
| ---------------------------- | --------------- | ---------------------------------------------------------------------------- |
| `List a → Int`               | `a`             | `TCon "List" [TVar "a"] ~> Int`                                              |
| `(a → b) → List a → List b`  | `a`, `b`        | `(TVar "a" ~> TVar "b") ~> TCon "List" [TVar "a"] ~> TCon "List" [TVar "b"]` |

#### Record and row types

| Constructor   | Type                                    | Kind                 | Remarks             |
| ------------- | --------------------------------------- | -------------------- | ------------------- |
| `TRec`        | Record type constructor                 | `row → type`         |                     |
| `RNil`        | The empty row                           | `row`                |                     |
| `RExt`        | Row extension                           | `type → row → row`   |                     |

A *row* is a structure whose purpose is to encode the type of a record. At the implementation level, it is a [cons list](https://en.wikipedia.org/wiki/Cons)-like chain of labeled type-fields. Inductively defined, a row is either
- empty; or
- the extension of an existing row, formed by adding (consing) an extra label-type pair on to it.

| Record                                | Type                               | Type rep. (Haskell expression)                       |
| ------------------------------------- | ---------------------------------- | ---------------------------------------------------- |
| `{ name = "Scooby Doo", dog = true }` | `{ name : string, dog : bool }`    | `TRec (RExt "name" TString (RExt "dog" TBool RNil))` |

We use the notation $()$ for the empty row, and $( l : t \ | \ r )$ for the row $r$ extended by a label $l$ and type $t$.

##### Row equality and normalization

Since records are unordered, it follows that we consider rows equivalent up to permutation of labels. This can be more formally expressed as an equivalence relation.

$$ () \cong () $$

$$
\frac{
    l_0 \ne l_1
  } {
    ( l_0 : t_0 \mid ( l_1 : t_1 \mid r )) \cong ( l_1 : t_1 \mid ( l_0 : t_0 \mid r ))
  }
$$

##### Open rows

Furthermore, a row can be either *open* or *closed*. A closed row consists of a sequence ending with the empty row, whereas an open row is one in which the final element is a type variable:

```
{ id : int, name : string | a }
```

### Language primitives

```
data Prim
  = PBool Bool
  | PInt Int
  | PFloat Float
  | PDouble Double
  | PChar Char
  | PString Text
  | PUnit
```

| Constructor   | Type     | Values                                                   | Remarks                                 |
| ------------- | -------- | -------------------------------------------------------- | --------------------------------------- |
| `PBool`       | `bool`   | `false` \| `true`                                        |                                         |
| `PInt`        | `int`    | `-2^63`, &hellip;, `-1`, `0`, `1`, &hellip;, `2^63-1`    | Signed 64-bit integer                   |
| `PFloat`      | `float`  | `/\d+\.\d+f/`                                            | Single-precision floating point number  |
| `PDouble`     | `double` | `/\d+\.\d+/`                                             | Double-precision floating point number  |
| `PChar`       | `char`   | &hellip;, `'a'`, `'b'`, `'c'`, &hellip;                  | A single ASCII character                |
| `PString`     | `string` | `""`, `"a"`, `"aa"`, `"aaa"`, &hellip;, `"b"`, &hellip;  |                                         |
| `PUnit`       | `unit`   | `()`                                                     |                                         |

### Unary operators

```
data Op1
  = ONot
  | ONeg
```

| Constructor   | Operator            | Symbol  | Remarks                                                                             |
| ------------- | ------------------- | ------- | ----------------------------------------------------------------------------------- |
| `ONot`        | Logical NOT         | `not`   | Logical complement: `not true == false`, and `not false == true`                    |
| `ONeg`        | Arithmetic negation | `-`     | Produces the negative of its operand, which must be an `int`, `float`, or `double`. |

### Binary operators

```
data Op2
  = OEq
  | ONEq
  | OLt
  | OGt
  | OLtE
  | OGtE
  | OAdd
  | OSub
  | OMul
  | ODiv
  | OLogicOr
  | OLogicAnd
```

| Constructor   | Operator              | Symbol  | Precedence | Associativity       | Remarks             |
| ------------- | --------------------- | ------- | ---------- | ------------------- | ------------------- |
| `OEq`         | Equal to              | `==`    | 4          | None                |                     |
| `ONEq`        | Not equal to          | `/=`    | 4          | None                |                     |
| `OLt`         | Less than             | `<`     | 4          | None                |                     |
| `OGt`         | Greater than          | `>`     | 4          | None                |                     |
| `OLtE`        | Less than or equal    | `<=`    | 4          | None                |                     |
| `OGtE`        | Greater than or equal | `>=`    | 4          | None                |                     |
| `OAdd`        | Addition              | `+`     | 6          | Left                |                     |
| `OSub`        | Subtraction           | `-`     | 6          | Left                |                     |
| `OMul`        | Multiplication        | `*`     | 7          | Left                |                     |
| `ODiv`        | Division              | `/`     | 7          | Left                |                     |
| `OLogicOr`    | Logical OR            | `\|\|`  | 2          | Right               |                     |
| `OLogicAnd`   | Logical AND           | `&&`    | 3          | Right               |                     |

### Expression syntax

```
data Expr
  = EVar Label
  | ECon Label
  | ELit Prim
  | EIf Expr Expr Expr
  | ELet Label Expr Expr
  | EApp Expr [Expr]
  | ELam [Label] Expr
  | ECall Label [Expr]
  | EOp1 Op1 Expr
  | EOp2 Op2 Expr Expr
  | EPat Expr [Clause]
  | ENil
  | EExt Name Expr Expr
  | ERes [Label] Expr Expr
```

```
type Label = (Type, Name)
```

```
type Clause = ([Label], Expr)
```

#### `EVar` &ndash; Variable

#### `ECon` &ndash; Data constructor

#### `ELit` &ndash; Literal value

#### Records

Records are usually defined as unordered containers of labeled *fields* (name-value pairs). Our implementation deviates slightly from this, in that the same label is allowed to appear more than once in a record. A field is therefore not just a key-value pair, but rather a key associated with an ordered sequences of values. The reasons for this are discussed in [x].

$$
\begin{align*}
 &\{ \\
 & &l_0 &= [v_{(0, 0)}, v_{(0, 1)}, \dots, v_{(0, m_0)}], \\
 & &l_1 &= [v_{(1, 0)}, v_{(1, 1)}, \dots, v_{(1, m_1)}], \\
 & & &\dots, \\
 & &l_n &= [v_{(n, 0)}, v_{(n, 1)}, \dots, v_{(n, m_n)}] \\
 &\} \\
\end{align*}
$$

##### Examples

##### Extension

##### Restriction
