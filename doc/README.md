# Cool Compiler (aarch64)

## Basic setup

## Lexer (Lexer)
```
make -f Makefile.Mac lexer ARCH=aarch64
```

## Parser
```
make -f Makefile.Mac parser ARCH=aarch64
```
## Semant
```
make -f Makefile.Mac semant ARCH=aarch64
```
## Code generator
```
make -f Makefile.Mac cgen ARCH=aarch64
```


## Compile cool into aarch64 assembly
./coolc [your_file.cl] --arch=aarch64 \
e.g. `./coolc ./examples/cells.cl --arch=aarch64` // No GC

note: `-g` is `gc` mode. \
e.g. `./coolc ./examples/cells.cl -g --arch=aarch64`

## Linked into executable
as [your_aarch64.s] -o [your_aarch64.o]
ld as [your_aarch64.s] lib/trap_handler_aarch64.s -o [your_aarch64_exe] -lc

## Execute 
> under qemu
setarch `uname -m` -R ./[your_aarch64_exe] // temporarily disable ASLR for a particular program


## Known issues
1. How to work under ASLR ? (fPIC?)
2. `stdin`/`stdout` is hardcoded. (Type: `R_AARCH64_COPY`)
3. `fflush(stdin)` before `fgets` ([problem](https://stackoverflow.com/a/63369562/5321961))
4. Under GC mode, `Program received signal SIGSEGV, Segmentation fault. _GenGC_MajorC_stackloop () at trap_handler_aarch64.s`
