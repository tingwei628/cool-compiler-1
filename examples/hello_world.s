# start of generated code
	.data
	.align	2
	.globl	class_nameTab
	.globl	Main_protObj
	.globl	Int_protObj
	.globl	String_protObj
	.globl	bool_const0
	.globl	bool_const1
	.globl	_int_tag
	.globl	_bool_tag
	.globl	_string_tag
_int_tag:
	.word	2
_bool_tag:
	.word	3
_string_tag:
	.word	4
	.globl	_MemMgr_INITIALIZER
_MemMgr_INITIALIZER:
	.word	_NoGC_Init
	.globl	_MemMgr_COLLECTOR
_MemMgr_COLLECTOR:
	.word	_NoGC_Collect
	.globl	_MemMgr_TEST
_MemMgr_TEST:
	.word	0
	.word	-1
str_const12:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const0
	.byte	0	
	.align	2
	.word	-1
str_const11:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const1
	.ascii	"Main"
	.byte	0	
	.align	2
	.word	-1
str_const10:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const2
	.ascii	"String"
	.byte	0	
	.align	2
	.word	-1
str_const9:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const1
	.ascii	"Bool"
	.byte	0	
	.align	2
	.word	-1
str_const8:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const3
	.ascii	"Int"
	.byte	0	
	.align	2
	.word	-1
str_const7:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const4
	.ascii	"IO"
	.byte	0	
	.align	2
	.word	-1
str_const6:
	.word	4
	.word	6
	.word	String_dispTab
	.word	int_const2
	.ascii	"Object"
	.byte	0	
	.align	2
	.word	-1
str_const5:
	.word	4
	.word	7
	.word	String_dispTab
	.word	int_const5
	.ascii	"_prim_slot"
	.byte	0	
	.align	2
	.word	-1
str_const4:
	.word	4
	.word	7
	.word	String_dispTab
	.word	int_const6
	.ascii	"SELF_TYPE"
	.byte	0	
	.align	2
	.word	-1
str_const3:
	.word	4
	.word	7
	.word	String_dispTab
	.word	int_const6
	.ascii	"_no_class"
	.byte	0	
	.align	2
	.word	-1
str_const2:
	.word	4
	.word	8
	.word	String_dispTab
	.word	int_const7
	.ascii	"<basic class>"
	.byte	0	
	.align	2
	.word	-1
str_const1:
	.word	4
	.word	8
	.word	String_dispTab
	.word	int_const8
	.ascii	"Hello, World.\n"
	.byte	0	
	.align	2
	.word	-1
str_const0:
	.word	4
	.word	11
	.word	String_dispTab
	.word	int_const9
	.ascii	"./examples/hello_world.cl"
	.byte	0	
	.align	2
	.word	-1
int_const9:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	25
	.word	-1
int_const8:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	14
	.word	-1
int_const7:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	13
	.word	-1
int_const6:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	9
	.word	-1
int_const5:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	10
	.word	-1
int_const4:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	2
	.word	-1
int_const3:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	3
	.word	-1
int_const2:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	6
	.word	-1
int_const1:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	4
	.word	-1
int_const0:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	0
	.word	-1
bool_const0:
	.word	3
	.word	4
	.word	Bool_dispTab
	.word	0
	.word	-1
bool_const1:
	.word	3
	.word	4
	.word	Bool_dispTab
	.word	1
class_nameTab:
	.word	str_const6
	.word	str_const7
	.word	str_const8
	.word	str_const9
	.word	str_const10
	.word	str_const11
class_parentTab:
	.word	-1
	.word	0
	.word	0
	.word	0
	.word	0
	.word	1
class_objTab:
	.word	Object_protObj
	.word	Object_init
	.word	IO_protObj
	.word	IO_init
	.word	Int_protObj
	.word	Int_init
	.word	Bool_protObj
	.word	Bool_init
	.word	String_protObj
	.word	String_init
	.word	Main_protObj
	.word	Main_init
Object_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
IO_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	IO.out_string
	.word	IO.out_int
	.word	IO.in_string
	.word	IO.in_int
Int_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
Bool_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
String_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	String.length
	.word	String.concat
	.word	String.substr
Main_dispTab:
	.word	Object.abort
	.word	Object.type_name
	.word	Object.copy
	.word	IO.out_string
	.word	IO.out_int
	.word	IO.in_string
	.word	IO.in_int
	.word	Main.main
	.word	-1
Object_protObj:
	.word	0
	.word	3
	.word	Object_dispTab
	.word	-1
IO_protObj:
	.word	1
	.word	3
	.word	IO_dispTab
	.word	-1
Int_protObj:
	.word	2
	.word	4
	.word	Int_dispTab
	.word	0
	.word	-1
Bool_protObj:
	.word	3
	.word	4
	.word	Bool_dispTab
	.word	0
	.word	-1
String_protObj:
	.word	4
	.word	5
	.word	String_dispTab
	.word	int_const0
	.word	0
	.word	-1
Main_protObj:
	.word	5
	.word	3
	.word	Main_dispTab
	.globl	heap_start
heap_start:
	.word	0
	.text
	.globl	Main_init
	.globl	Int_init
	.globl	String_init
	.globl	Bool_init
	.globl	Main.main
Object_init:
	add	sp, sp, #-12
	str	w29, [sp, #12]
	str	w19, [sp, #8]
	str	w30, [sp, #4]
	add	w29, wsp, #8
	mov	w19, w0
	mov	w0, w19
	ldr	w29, [sp, #12]
	ldr	w19, [sp, #8]
	ldr	w30, [sp, #4]
	add	sp, sp, #12
	ret	
IO_init:
	add	sp, sp, #-12
	str	w29, [sp, #12]
	str	w19, [sp, #8]
	str	w30, [sp, #4]
	add	w29, wsp, #4
	mov	w19, w0
	bl Object_init
	mov	w0, w19
	ldr	w29, [sp, #12]
	ldr	w19, [sp, #8]
	ldr	w30, [sp, #4]
	add	sp, sp, #12
	ret	
Int_init:
	add	sp, sp, #-12
	str	w29, [sp, #12]
	str	w19, [sp, #8]
	str	w30, [sp, #4]
	add	w29, wsp, #4
	mov	w19, w0
	bl Object_init
	mov	w0, w19
	ldr	w29, [sp, #12]
	ldr	w19, [sp, #8]
	ldr	w30, [sp, #4]
	add	sp, sp, #12
	ret	
Bool_init:
	add	sp, sp, #-12
	str	w29, [sp, #12]
	str	w19, [sp, #8]
	str	w30, [sp, #4]
	add	w29, wsp, #4
	mov	w19, w0
	bl Object_init
	mov	w0, w19
	ldr	w29, [sp, #12]
	ldr	w19, [sp, #8]
	ldr	w30, [sp, #4]
	add	sp, sp, #12
	ret	
String_init:
	add	sp, sp, #-12
	str	w29, [sp, #12]
	str	w19, [sp, #8]
	str	w30, [sp, #4]
	add	w29, wsp, #4
	mov	w19, w0
	bl Object_init
	mov	w0, w19
	ldr	w29, [sp, #12]
	ldr	w19, [sp, #8]
	ldr	w30, [sp, #4]
	add	sp, sp, #12
	ret	
Main_init:
	add	sp, sp, #-12
	str	w29, [sp, #12]
	str	w19, [sp, #8]
	str	w30, [sp, #4]
	add	w29, wsp, #4
	mov	w19, w0
	bl IO_init
	mov	w0, w19
	ldr	w29, [sp, #12]
	ldr	w19, [sp, #8]
	ldr	w30, [sp, #4]
	add	sp, sp, #12
	ret	
Main.main:
	add	sp, sp, #-12
	str	w29, [sp, #12]
	str	w19, [sp, #8]
	str	w30, [sp, #4]
	add	w29, wsp, #4
	mov	w19, w0
	adr	x0, str_const1
	str	w0, [sp, #0]
	add	sp, sp, #-4
	mov	w0, w19
	cmp	w0, wzr
	b.ne	 label0
	adr	x0, str_const0
	mov	w9, #3
	bl	_dispatch_abort
label0:
	ldr	w9, [x0, #8]
	ldr	w9, [x9, #12]
	blr		x9
	ldr	w29, [sp, #12]
	ldr	w19, [sp, #8]
	ldr	w30, [sp, #4]
	add	sp, sp, #12
	ret	

# end of generated code
