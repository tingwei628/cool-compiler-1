# Cool Compiler (aarch64)


## Lexer
```
make -f assignments/PA2/Makefile.Mac lexer ARCH=aarch64
mv lexer lexer_aarch64
```

## Parser
```
make -f assignments/PA3/Makefile.MacMake parser ARCH=aarch64
mv parser parser_aarch64
```
## Semant
```
make -f assignments/PA4/Makefile.Mac semant ARCH=aarch64
mv semant semant_aarch64
```
## Code generator
```
make -f assignments/PA5/Makefile.Mac cgen ARCH=aarch64
mv cgen cgen_aarch64
```

## Compile .cl(cool) into aarch64 assembly
`./coolc [your_file.cl] --arch=aarch64` \
e.g. `./coolc ./examples/cells.cl --arch=aarch64` // No GC

note: `-g` is `gc` mode. \
e.g. `./coolc ./examples/cells.cl -g --arch=aarch64`

## Linked into executable
`as [your_aarch64.s] -o [your_aarch64.o]`
`ld as [your_aarch64.s] lib/trap_handler_aarch64.s -o [your_aarch64_exe] -lc`

## Execute 
> under qemu
`setarch `uname -m` -R ./[your_aarch64_exe]` // temporarily disable ASLR for a particular program


## Known issues
1. How to work under ASLR ? (fPIC?)
2. `stdin`/`stdout` is hardcoded. (Type: `R_AARCH64_COPY`)
3. `fflush(stdin)` before `fgets` ([problem](https://stackoverflow.com/a/63369562/5321961))
4. Under GC mode, `Program received signal SIGSEGV, Segmentation fault. _GenGC_MajorC_stackloop () at trap_handler_aarch64.s`
