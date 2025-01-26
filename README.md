```
make
./llvm-dug test/commuted.ll
```

Now you get `<funcname>.dot` file of def-use graph of the functions in the input llvm IR.
Go to https://dreampuf.github.io/GraphvizOnline/ to see the result.