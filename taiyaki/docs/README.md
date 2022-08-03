# Taiyaki

<p>&nbsp;</p>
<p align="center">
  <img src="https://raw.githubusercontent.com/laserpants/area-51/dev/taiyaki/docs/taiyaki.png" width="250" />
</p>

## Language implementation

## Compilation strategy

The compiler proceeds through a series of transformation steps applied to the
syntax tree. The final goal of this process is to arrive at a tree based on a
reduced expression grammar. Compared to the source language, this target AST is
a much smaller and simpler representation which is more suitable for code
generation and evaluation.

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

#### Translate non-trivial let-bindings

Let-bindings are still permitted, but only those that bind to a variable,
that is, are of the form `let v = expr`, where `v` is a variable.

##### Translate `match` expressions to simple `case` pattern matching

TODO

##### Desugar tuples, records, and list literals

TODO

##### Transform type class constraints to records

TODO

### Patterns

#### Exhaustiveness checking

#### Compilation

## Etymology

Taiyaki (鯛焼き) is a Japanese fish-shaped cake, commonly sold as street food.

### Attribution

<small>
  <a href="https://www.flaticon.com/free-icons/taiyaki" title="taiyaki icons">Taiyaki icons created by Freepik - Flaticon</a>
</small>
