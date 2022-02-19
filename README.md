to build
```bash
dune build
```
to execute
```bash
dune exec ./main.exe -- --show-ast --show-type --show-ir --show-bc tests/test.ml
```

to execute with the vm
```bash
cd vm
cmake .
make
./vm ../tests/test.bc
```

