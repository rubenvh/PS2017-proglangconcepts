namespace proglangconcepts

module Chapter01 =
    // let env = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)];;
    let emptyenv = []

    let rec lookup env x =
         match env with
            | []           -> failwith (x + "not found")
            | (y, v)::r    -> if (x = y) then v else lookup r x
     
    type expr =
        | CstI of int
        | Var of string
        | Prim of string * expr * expr
 
    let rec eval (e: expr) (env: (string * int) list) : int =
         match e with
             | CstI i               -> i
             | Var x                -> lookup env x
             | Prim("+", e1, e2)    -> eval e1 env + eval e2 env
             | Prim("-", e1, e2)    -> eval e1 env - eval e2 env
             | Prim("*", e1, e2)    -> eval e1 env * eval e2 env
             | Prim _               -> failwith "unknown primitive";;
     

     