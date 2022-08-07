# Taiyaki

<p>&nbsp;</p>
<p align="center">
  <img src="https://raw.githubusercontent.com/laserpants/area-51/dev/taiyaki/docs/taiyaki.png" width="250" />
</p>

## Language implementation

## Compilation strategy

After parsing, type checking, and other preliminary steps, the compiler
proceeds through a series of transformations applied to the syntax tree. The
final goal of this process is to arrive at an adaptation of the input,
semantically identical, but based on a much smaller expression grammar.
Compared to the source language, this representation is more suitable for code
generation and evaluation. The code generation stage is important enough that
we treat this grammar as its own language entirely. There are a number of
benefits of doing so:

- Less cognitive overhead

```mermaid
flowchart LR;
    A --> B;
    B --> C;
    C --> D;
```

##### Normalize lambda abstractions

###### Expansion of multiple arguments

```
(x, y, z) ⇒ e
```

```
lam(x) ⇒
  lam(y) ⇒
    lam(z) ⇒
      e
```

###### Pattern elimination

```
(P[...]) ⇒ e
```

```
lam($v) ⇒
  match $v {
    P[...] ⇒ e
  }
```

##### Translate let-bindings

Let-bindings are still permitted, but only those that bind to a variable,
that is, are of the form `let v = expr`, where `v` is a variable.

###### Simple lets

```
let v = expr
```

```
fix v = expr
```

###### Function bindings

```
let f(x, y) = expr
```

```
fix f =
  lam(x) ⇒
    lam(y) ⇒
      expr
```

###### Pattern bindings

```
let P[...] = expr in body
```

```
match expr {
  | P[...] ⇒ body
}
```

##### Translate `match` expressions to simple `case` pattern matching

TODO

##### Desugar tuples, records, and list literals

###### List literals

```
[1, 2, 3]
```

```
1 :: 2 :: 3 :: []
```

```
((::) 1 ((::) 2 ((::) 3 [])))
```

###### Tuples

```
('a', 1)
```

```
(,) 'a' 1
```

###### Records

```
{ foo = "baz", bar = 1 }
```

```
#Record ({foo} "baz" ({bar} 1 {}))
```

##### Transform type class constraints to records

TODO

### Patterns

###### Literal patterns

```
  | 5 => true
```

```
  | $a when $a == 5 => true
```

```
  | [1, 2, 3] => true
```

```
  | [$a, $b, $c]
      when $a == 1 && $b == 2 && $c == 3 =>
        true
```

###### Or-patterns

```
  | [x, _] or [x, _, _] => true
```

```
  | [x, _]    => true
  | [x, _, _] => true
```

###### As-patterns

```
  match ys {
    | [x, _, _] as xs => e1
    | _               => e2
  }
```

```
  match ys {
    | xs =>
        match xs {
          | [x, _, _] => e1
          | _         => e2
        }
  }
```

A slightly more complex example

```
  match ys {
    | ((1 :: 2 :: _) as xs, (1 :: 2 :: _) as ys) => xs <> ys
    | _ => e2
  }
```

```
  match ys {
    | (xs , ys) =>
        match xs {
          | 1 :: 2 :: _ =>
              match ys {
                | 1 :: 2 :: _ => xs <> ys
                | _           => e2
              }
          | _ => e2
        }
  }
```

###### Any-patterns

```
  | _ => true
```

```
  | $_ => true
```

#### Exhaustiveness checking

#### Compilation

## Etymology

Taiyaki (鯛焼き) is a Japanese fish-shaped cake, commonly sold as street food.

### Attribution

<small>
  <a href="https://www.flaticon.com/free-icons/taiyaki" title="taiyaki icons">Taiyaki icons created by Freepik - Flaticon</a>
</small>
