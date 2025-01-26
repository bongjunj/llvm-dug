`make` sure that you have `dune` and `make` installed!

```
make
./llvm-dug test/commuted.ll
```

Now you get `<funcname>.dot` file of def-use graph of the functions in the input llvm IR.
Go to https://dreampuf.github.io/GraphvizOnline/ to see the result.

![graphviz](https://github.com/user-attachments/assets/af8e85e4-4f95-4644-b479-96e21e2a7933)
