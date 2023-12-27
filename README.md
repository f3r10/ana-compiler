# ana-compiler
Compiler written on haskell based on the course Compiler Construction (UCSD CSE 131) 
https://ucsd-cse131-f19.github.io/ 
https://github.com/ucsd-cse131-f19/ucsd-cse131-f19.github.io

## 🌅 Getting Started
### Dependencies
**Required**
- cabal-install version 3.10.2.1
- clang version 16.0.6
- nasm version 2.16.01

**Nix**
Much easier, it is possible to run `nix develop` to use the flake
## 🚀 Usage
The programs folder contains some examples like `binary_search_tree.ana`
```lisp
(type Node (Dict ((data Num) (leftChild Node) (rightChild Node))))

(def makeNode(d : Num): Node
  (Dict (data d) (leftChild (nil Node)) (rightChild (nil Node))))

(def insert(root: Node x: Num): Node
    (if (isNull root)
        (makeNode x)
        (if (> x (get root data))
          (set root rightChild (insert (get root rightChild) x))
          (set root leftChild (insert (get root leftChild) x)))))

(def search(root: Node x: Num): Node
  (if (isNull root)
    root
    (if (== x (get root data))
      root
      (if (> x (get root data))
        (search (get root rightChild) x)
        (search (get root leftChild) x)))))

(let ((b 20) (a (makeNode b)) (c (makeNode 10))) 
  (insert a 5) 
  (insert a 1) 
  (insert a 15) 
  (insert a 30) 
  (print (insert a 16)) 
  (search a 15))
```
Once a program is created, it is possible to compile it with
```make
make compiled/binary_search_tree.run
```
Once the program is compiled, it is possible to execute it with
```bash
compiled/binary_search_tree.run
```
Compiled programs can receive parameters like `programs/fib.ana` which has to be executed like `compiled/fib.run 5`

### Testing
```bash
cabal v2-test
```
