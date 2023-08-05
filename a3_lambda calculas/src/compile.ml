(* Take the De Bruijn representation of a lambda, convert it to C code.*)
open Debruijn
open Format

let counter = ref 0

let next () =
    let c = !counter in
    let _ = (counter := c+1) in
    c

let rec doit t =
    match t with
    | Var i -> printf "nth(env, %i)" i
    | Abs t -> (printf "void *f_%i(int *env, int x){\n" (next ());
            printf("\tint *env = extend(x, env);\n");
            printf("\tvoid *r = ");
            doit t;
            printf ("\n\treturn r;\n");
        printf "}\n")
    | App (t1, t2) -> ()

let compile t =
    let _ = printf "struct list{
    int data;
    struct *next;
    };\n\n" in
    let _ = printf "int *extend(int x, struct list *env){
    // leave to you
    }\n\n" in
    let _ = printf "int nth(struct list *env, int index){
        // leave to you
        }\n\n" in
    let _ = doit t in
    let _ = printf "int main(int argc, char **argv){
            f_0;
            return 0;
            }\n\n" in
        ()

let unit_test () =
    let _ = printf "\x1B[32m -------------- Compile : unit test--------------\x1B[0m\n" in
    let t = Abs (Var 0) in
        compile t




