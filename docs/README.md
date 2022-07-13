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

#### Composite types

| Constructor   | Type                                    | Notation                | Remarks             |
| ------------- | --------------------------------------- | ----------------------- | ------------------- |
| `TCon`        | Algebraic data type                     | `Con s t r u ...`       |                     |
| `TArr`        | Function type                           | `s → t`                 |                     |
| `TVar`        | Type variable                           | `'0`, `a`               |                     |

##### Algebraic data types

##### Type schemes

#### Record and row types

A row is a sequence of labeled fields that encode the type of a record.

| Constructor   | Type                                    | Kind                 | Remarks             |
| ------------- | --------------------------------------- | -------------------- | ------------------- |
| `TRec`        | Record type constructor                 | `row → type`         |                     |
| `RNil`        | The empty row                           | `row`                |                     |
| `RExt`        | Row extension                           | `type → row → row`   |                     |

| Record                                | Type                               | Row                                           |
| ------------------------------------- | ---------------------------------- | --------------------------------------------- |
| `{ name = "Scooby Doo", dog = true }` | `{ name : string, dog : bool }`    | `RExt "name" TString (RExt "dog" TBool RNil)` |

##### Row equality and normalization

Since records are unordered, it is natural to considered rows equivalent up to permutation of distinct labels. This can be more formally expressed as an equivalence relation.

##### Open rows

A row can be either *open* or *closed*. Whereas a closed row has a sequence which ends with the empty row, an open row is one in which the final element is a variable:

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
| `PFloat`      | `float`  | `/\d+\.\d+f/`                                            | Single-precision floating point numbers |
| `PDouble`     | `double` | `/\d+\.\d+/`                                             | Double-precision floating point numbers |
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

Records are unordered containers of labeled fields.

##### Duplicate fields

Due to reasons discussed in [x],

##### Examples

##### Extension

##### Restriction
