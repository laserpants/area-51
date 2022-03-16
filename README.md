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
    select r { name = n | q } => n
    
    
