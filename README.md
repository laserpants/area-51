# pong

def 
  main(n : int) : List int = 
    let 
      xs = 
        Cons(100, Cons(101, Nil())) 
      in 
        match xs { 
          | Cons(y, ys) => 
              match ys { 
                | Cons(z, zs) => zs 
                | Nil() => Nil() 
              } 
          | Nil() => Nil() 
        }


def 
  main
    ( n : int 
    ) : List int = 
      let 
        xs = 
          Cons
            ( 100
            , Cons
              ( 101
              , Nil()
              )
            ) 
        in 
          match xs { 
            | Cons(y, ys) => 
                match ys { 
                  | Cons(z, zs) => zs 
                  | Nil() => Nil() 
                } 
            | Nil() => Nil() 
          }

def 
  fact(n : int) : int = 
    if 
      n == 0 
        then 
          1 
        else 
          n * fact(n - 1) 

def main(a : int) : int = 
  fact(5)


extern
  printInt : int -> unit

type List a
  = Nil
  | Cons a (List a)

const 
  speedLimit : int = 4

def 
  main(z : int) : int =
   let
     h =
       z + 1
     in
       let
         g =
           lam(x) => 
             x
         in
           let
             f =
               lam(y) =>
                 y + h
             in
               (g(f))(g(5)) + f(1)
         

let
  r =
    { name = "Bob", id = 1 }
  in
    field 
      { name = n | q } = 
        r 
      in 
        n
    
-------------------
-------------------
-------------------
-------------------
    



main(z) =
  let 
    f =
      lam[x, y] => x + y
    in
      let 
        q : int -> int =
          (if z > 5 then f else g)(5)
        in
          q(3)

--

main(z) =
  let 
    f =
      lam[x, y] => x + y
    in
      let 
        q : int -> int =
          (lam[v0, v1] => if z > 5 then f(v0, v1) else g(v0, v1))(5)
        in
          q(3)

--

f0 = lam[v0, v1] => if z > 5 then f(v0, v1) else g(v0, v1)

main(z) =
  let 
    f =
      lam[x, y] => x + y
    in
      let 
        q : int -> int =
          f0(5)
        in
          q(3)

--

f0[v0, v1] = if z > 5 then f(v0, v1) else g(v0, v1)

main(z) =
  let 
    f =
      lam[x, y] => x + y
    in
      let 
        q : int -> int =
          f0(5)
        in
          q(3)

--

f0[v0, v1] = if z > 5 then f(v0, v1) else g(v0, v1)

main(z) =
  let 
    f =
      lam[x, y] => x + y
    in
      let 
        q : int -> int =
          lam(v0) => f0(5, v0)
        in
          q(3)

--

f0[v0, v1] = if z > 5 then f(v0, v1) else g(v0, v1)
f1[v0] = f0(5, v0)

main(z) =
  let 
    f =
      lam[x, y] => x + y
    in
      let 
        q =
          f1
        in
          q(3)




-------------------
-------------------
-------------------
-------------------


main(z) =
  let 
    g = 
      lam(x) => 
        x
    in
      let 
        f =
          lam(y) => y + 1
      in
        (g(f))(g(5))



