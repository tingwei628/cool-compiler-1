
# [COOL](https://en.wikipedia.org/wiki/Cool_(programming_language))(Classroom Object Oriented Language) Compiler under AArch64

## Generate COOL `Lexer/Parser/Semant/Cgen(Code generation)` under AArch64
> via [dockcross: cross compile toolchain](https://github.com/dockcross/dockcross)

Lexer
```
cd assignments/PA2
make -f Makefile.Mac lexer ARCH=aarch64
mv lexer lexer_aarch64
```
Parser
```
cd assignments/PA3
make -f Makefile.MacMake parser ARCH=aarch64
mv parser parser_aarch64
```
Semant
```
cd assignments/PA4
make -f Makefile.Mac semant ARCH=aarch64
mv semant semant_aarch64
```
Code generation
```
cd assignments/PA5
make -f Makefile.Mac cgen ARCH=aarch64
mv cgen cgen_aarch64
```

## Compile .cl(COOL) into AArch64 assembly
> still under dockcross

`./coolc [your_file.cl] --arch=aarch64` \
e.g. `./coolc ./examples/cells.cl --arch=aarch64` // No GC

note: `-g` is `gc` mode. \
e.g. `./coolc ./examples/cells.cl -g --arch=aarch64`

## Linked into executable

`as [your_aarch64.s] -o [your_aarch64.o]` // assemble \
`ld as [your_aarch64.s] lib/trap_handler_aarch64.s -o [your_aarch64_exe] -lc` // link


## Execute 
> under qemu-system-aarch64
```
setarch `uname -m` -R ./[your_aarch64_exe] // temporarily disable ASLR for a particular program
```

## Changes for AArch64
[emit.h](https://github.com/tingwei628/cool-compiler-1/blob/tingwei628-aarch64/assignments/PA5/emit.h) \
[cgen.cc](https://github.com/tingwei628/cool-compiler-1/blob/tingwei628-aarch64/assignments/PA5/cgen.cc) \
[trap_handler_aarch64.s](https://github.com/tingwei628/cool-compiler-1/blob/tingwei628-aarch64/lib/trap_handler_aarch64.s) (which is COOL runtime including GC)

## Known issues
1. How to work under ASLR ? (fPIC?)
2. `stdin`/`stdout` is hardcoded. (Type: `R_AARCH64_COPY`)
3. `fflush(stdin)` before `fgets` ([problem](https://stackoverflow.com/a/63369562/5321961))
4. Under GC mode, `Program received signal SIGSEGV, Segmentation fault. _GenGC_MajorC_stackloop () at trap_handler_aarch64.s`

## Reference
[MIPS_Instruction_Set](https://www.dsi.unive.it/~gasparetto/materials/MIPS_Instruction_Set.pdf) \
[ARMv8 A64 Quick Reference](https://courses.cs.washington.edu/courses/cse469/19wi/arm64.pdf) \
[arm64 assembly crash course](https://github.com/Siguza/ios-resources/blob/master/bits/arm64.md) \
[The Cool Runtime System](https://web.stanford.edu/class/cs143/materials/cool-runtime.pdf)
