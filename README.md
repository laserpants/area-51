# pong

[-] gen. types
[-] adts
[-] codegen
[-] ?

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





-- ///////////////////


let
  id =
    lam(x) => 
      x
  in
    let
      g =
        id
      in
        (g(id))(f(5))


--


def
  id(x : a) : a =
    x

let
  f =
    lam(g) =>
      g(5)
  in
    f(id)


--


let
  id =
    lam(x) =>
      x
  in
    let
      f =
        lam(g) =>
          g(5)
      in
        f(id)


let id = lam(x) => x in let f = lam(g) => g(5) in f(id)


t2 -> t2

$lam1(x : t2) : t2 = 
  x : t2                      ( template )

(int -> t6) -> t6

$lam2(g : int -> t6) : t6 =
  [g : int -> t6](5)          ( template )


--

let f = lam(g) => g(1) in let const5 = lam(x) => 5 in f(const5)

$lam1(g : int -> t3) : t3 =
  [g : int -> t3](1)

$lam2(x : t6) : int =
  5



let 
  f : (int -> t1) -> t1 = 
    lam(g) => 
      g(1) 
  in 
    let 
      const5 : t2 -> int = 
        lam(x) => 
          5 
      in 
        <f : (int -> int) -> int>(const5 : int -> int)


--


def
  const(x : a, y : b) : a =
    x

let
  f =
    const(5)
  in
    f(3)


--


let
  const(x : a, y : b) : a =
    x
  in
    let
      f =
        const(5)
      in
        f(3)





===

let 
  f : (int -> t1) -> t1 = 
    lam(g) => 
      g(1) 
  in 
    let 
      const5 : t2 -> int = 
        lam(x) => 
          5 
      in 
        <f : (int -> int) -> int>(const5 : int -> int)


let 
  f : (int -> t1) -> t1 = 
    lam(g) => 
      g(1) 
  in 
    let
      f_0 : (int -> int) -> int =
        lam(g) => 
          g(1) 
      in
        let 
          const5 : t2 -> int = 
            lam(x) => 
              5 
          in 
            let 
              const5_0 : int -> int = 
                lam(x) => 
                  5 
              in
                <f_0 : (int -> int) -> int>(const5_0 : int -> int)



let 
  id : t1 -> t1 = 
    lam(x) => 
      x 
    in 
      let 
        f : (int -> t2) -> t2 = 
          lam(g) => 
            g(5) 
          in 
            <f : (int -> int) -> int>(id : int -> int)


let 
  id : t1 -> t1 = 
    lam(x) => 
      x 
    in 
      let
        id_0 : int -> int = 
          lam(x) => 
            x 
        in
          let 
            f : (int -> t2) -> t2 = 
              lam(g) => 
                g(5) 
              in 
                let
                  f_0 : (int -> int) -> int =
                    lam(g) => 
                      g(5) 
                  in
                    <f : (int -> int) -> int>(id : int -> int)




