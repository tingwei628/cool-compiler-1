///////////////////////////////////////////////////////////////////////
//
//  Assembly Code Naming Conventions:
//
//     Dispatch table            <classname>_dispTab
//     Method entry point        <classname>.<method>
//     Class init code           <classname>_init
//     Abort method entry        <classname>.<method>.Abort
//     Prototype object          <classname>_protObj
//     Integer constant          int_const<Symbol>
//     String constant           str_const<Symbol>
//
///////////////////////////////////////////////////////////////////////

#include "stringtab.h"
#define MAXINT  100000000

// Global names
#define CLASSNAMETAB         "class_nameTab"
#define CLASSOBJTAB          "class_objTab"
#define CLASSPARENTTAB       "class_parentTab"
#define INTTAG               "_int_tag"
#define BOOLTAG              "_bool_tag"
#define STRINGTAG            "_string_tag"
#define HEAP_START           "heap_start"

// Naming conventions
#define DISPTAB_SUFFIX       "_dispTab"
#define METHOD_SEP           "."
#define CLASSINIT_SUFFIX     "_init"
#define PROTOBJ_SUFFIX       "_protObj"
#define OBJECTPROTOBJ        "Object"PROTOBJ_SUFFIX
#define INTCONST_PREFIX      "int_const"
#define STRCONST_PREFIX      "str_const"
#define BOOLCONST_PREFIX     "bool_const"


#define EMPTYSLOT            0
#define LABEL                ":\n"

#define STRINGNAME (char *) "String"
#define INTNAME    (char *) "Int"
#define BOOLNAME   (char *) "Bool"
#define MAINNAME   (char *) "Main"

#if defined(AARCH64)

//
// information about object headers
//

#define WORD_SIZE    4
#define LOG_WORD_SIZE 2     // for logical shifts

#define DEFAULT_OBJFIELDS 3
#define TAG_OFFSET 0
#define SIZE_OFFSET 1
#define DISPTABLE_OFFSET 2

#define INVALID_CLASSTAG -1

#define STRING_SLOTS      1
#define INT_SLOTS         1
#define BOOL_SLOTS        1

#define GLOBAL        "\t.globl\t"
#define ALIGN         "\t.align\t2\n"
#define WORD          "\t.word\t"

//
// register names
//
#define ZERO  "xzr"		// Zero register (64-bit)
#define WZERO "wzr"		// Zero register (32-bit)
#define ACC   "x0"		// Accumulator (64-bit)
#define WACC  "w0"		// Accumulator (32-bit)
#define A1    "x1"		// For arguments to prim funcs (64-bit)
#define WA1   "w1"		// For arguments to prim funcs (32-bit)
#define SELF  "x19"		// Ptr to self (callee saves) (64-bit)
#define WSELF "w19"		// Ptr to self (callee saves) (32-bit)
#define T1    "x9"		// Temporary 1 (64-bit)
#define WT1   "w9"       // Temporary 1 (32-bit)
#define T2    "x10"		// Temporary 2 (64-bit)
#define WT2   "w10"		// Temporary 2 (32-bit)
#define T3    "x11"		// Temporary 3 (64-bit)
#define WT3   "w11"		// Temporary 3 (32-bit)
#define SP    "sp"		// Stack pointer (64-bit)
#define WSP   "wsp"      // Stack pointer (32-bit)
#define FP    "x29"		// Frame pointer (64-bit)
#define WFP   "w29"		// Frame pointer (32-bit)
#define RA    "x30"		// Return address (64-bit)
#define WRA   "w30"      // Return address (32-bit)

//
// Opcodes
//
#define JALR  "\tblr\t"
#define JAL   "\tbl\t"
#define RET   "\tret\t"

#define SW    "\tstr\t"
#define LW    "\tldr\t"
#define LI    "\tmov\t"
#define LA    "\tadr\t"

#define MOVE  "\tmov\t"
#define NEG   "\tneg\t"
#define ADD   "\tadd\t"
#define ADDI  "\tadd\t"
#define ADDU  "\tadd\t"
#define ADDIU "\tadd\t"
#define DIV   "\tsdiv\t"
#define MUL   "\tmul\t"
#define SUB   "\tsub\t"
#define SLL   "\tlsl\t"
#define BEQZ  "\tcbz\t"
#define BRANCH   "\tb\t"
#define CMP   "\tcmp\t" 
#define BEQ      "\tb.eq\t"
#define BNE      "\tb.ne\t"
#define BLEQ     "\tb.le\t" // less than or equal to
#define BLT      "\tb.lt\t" // less than
#define BGT      "\tb.gt\t" // greater than

#else

//
// information about object headers
//

#define WORD_SIZE    4
#define LOG_WORD_SIZE 2     // for logical shifts

#define DEFAULT_OBJFIELDS 3
#define TAG_OFFSET 0
#define SIZE_OFFSET 1
#define DISPTABLE_OFFSET 2

#define INVALID_CLASSTAG -1

#define STRING_SLOTS      1
#define INT_SLOTS         1
#define BOOL_SLOTS        1

#define GLOBAL        "\t.globl\t"
#define ALIGN         "\t.align\t2\n"
#define WORD          "\t.word\t"

//
// register names
//
#define ZERO "$zero"		// Zero register
#define ACC  "$a0"		// Accumulator
#define A1   "$a1"		// For arguments to prim funcs
#define SELF "$s0"		// Ptr to self (callee saves)
#define T1   "$t1"		// Temporary 1
#define T2   "$t2"		// Temporary 2
#define T3   "$t3"		// Temporary 3
#define SP   "$sp"		// Stack pointer
#define FP   "$fp"		// Frame pointer
#define RA   "$ra"		// Return address

//
// Opcodes
//
#define JALR  "\tjalr\t"
#define JAL   "\tjal\t"
#define RET   "\tjr\t"RA"\t"

#define SW    "\tsw\t"
#define LW    "\tlw\t"
#define LI    "\tli\t"
#define LA    "\tla\t"

#define MOVE  "\tmove\t"
#define NEG   "\tneg\t"
#define ADD   "\tadd\t"
#define ADDI  "\taddi\t"
#define ADDU  "\taddu\t"
#define ADDIU "\taddiu\t"
#define DIV   "\tdiv\t"
#define MUL   "\tmul\t"
#define SUB   "\tsub\t"
#define SLL   "\tsll\t"
#define BEQZ  "\tbeqz\t"
#define BRANCH   "\tb\t"
#define BEQ      "\tbeq\t"
#define BNE      "\tbne\t"
#define BLEQ     "\tble\t"
#define BLT      "\tblt\t"
#define BGT      "\tbgt\t"

#endif