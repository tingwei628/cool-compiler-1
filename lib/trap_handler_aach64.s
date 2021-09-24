
	.data
// system call number
.equ SYS_READ, 0x3f
.equ SYS_WRITE, 0x40
.equ SYS_BRK, 0xd6
.equ SYS_EXIT, 0x5d
// Define some constants
.equ obj_eyecatch, -4 // Unique id to verify any object
.equ obj_tag, 0
.equ obj_size, 4
.equ obj_disp, 8
.equ obj_attr, 12
.equ int_slot, 12
.equ bool_slot, 12
.equ str_size, 12	// This is a pointer to an Int object!!!
.equ str_field, 16	// The beginning of the ascii sequence
.equ str_maxsize, 1026	// the maximum string length

.equ GenGC_HDRSIZE, 44				// size of GenGC header
.equ GenGC_HDRL0, 0					// pointers to GenGC areas
.equ GenGC_HDRL1, 4
.equ GenGC_HDRL2, 8
.equ GenGC_HDRL3, 12
.equ GenGC_HDRL4, 16
.equ GenGC_HDRMAJOR0, 20				// history of major collections
.equ GenGC_HDRMAJOR1, 24
.equ GenGC_HDRMINOR0, 28				// history of minor collections
.equ GenGC_HDRMINOR1, 32
.equ GenGC_HDRSTK, 36					// start of stack
.equ GenGC_HDRREG, 40					// current REG mask
.equ GenGC_HEAPEXPGRAN, 14				// 2^14=16K
.equ GenGC_OLDRATIO, 2				// 1/(2^2)=.25=25%

.equ MemMgr_REG_MASK, 0x007F0000

/*
callee register in AArch64
$s0 -> x19
$s1 -> x20
$s2 -> x21
$s3 -> x22
$s4 -> x23
$s5 -> x24
$s6 -> x25
$s7 -> x26
$gp -> x27

caller register in AArch64
$t1 -> x9
$t2 -> x10
$t3 -> x11
$t0 -> x12 (this is NOT typo)
$t4 -> x13
$t8 -> x14 
$t9 -> x15

arguments register in AArch64
$a0 -> x0
$a1 -> x1
$a2 -> x2

$v0 -> x6
$v1 -> x7

// $16 ~ $22 $24 $25 $30 $31
// $s0 ~ $s6 $t8 $t9 $fp $ra
// x19 ~ x25 x14 x15 x29 x30
*/
//.string directive is an alias for .asciz
_abort_msg:	.asciz "Abort called from class "
_colon_msg:	.asciz ":"
_dispatch_msg:  .asciz ": Dispatch to void.\n"
_cabort_msg:	.asciz "No match in case statement for Class "
_cabort_msg2:   .asciz "Match on void in case statement.\n"
_nl:		.asciz "\n"
_term_msg:	.asciz "COOL program successfully executed\n"
_sabort_msg1:	.asciz	"Index to substr is negative\n"
_sabort_msg2:	.asciz	"Index to substr is too big\n"
_sabort_msg3:	.asciz	"Length to substr too long\n"
_sabort_msg4:	.asciz	"Length to substr is negative\n"
_sabort_msg:	.asciz "Execution aborted.\n"
_objcopy_msg:	.asciz "Object.copy: Invalid object size.\n"
_gc_abort_msg:	.asciz "GC bug!\n"

// Exception Handler Message:
_uncaught_msg1: .asciz "\nUncaught Exception of Class "
_uncaught_msg2: .asciz "\nthrown. COOL program aborted.\n"
// Messages for the GenGC garbage collector
_GenGC_INITERROR:	.asciz "GenGC: Unable to initialize the garbage collector.\n"
_GenGC_COLLECT:		.asciz "Garbage collecting ...\n"
_GenGC_Major:		.asciz "Major ...\n"
_GenGC_Minor:		.asciz "Minor ...\n"
_GenGC_MINORERROR:	.asciz "GenGC: Error during minor garbage collection.\n"
_GenGC_MAJORERROR:	.asciz "GenGC: Error during major garbage collection.\n"
_GenGC_Init_test_msg:   .asciz "GenGC initialized in test mode.\n"
_GenGC_Init_msg:        .asciz "GenGC initialized.\n"

// Stack overflow handler message:
// _stack_overflow_msg: .asciiz " Stack overflow detected, COOL program aborted\n"

// Messages for the NoGC garabge collector
_NoGC_COLLECT:		.asciz "Increasing heap...\n"

	//.align 2
	.balign 8

	.text
	.globl __sig_handler2
__sig_handler2:
    stp x29, x30, [sp, #-32]!
    str x0, [sp, #28]
    mov x0, 31
    bl raise
    ldp x29, x30, [sp], #32
    ret

	.globl __sig_handler
__sig_handler:
	stp x29, x30, [sp, #-32]!
	mov x19, x0 // save the expression object
	str x0, [sp, #28]
	ldr x0, =_uncaught_msg1
    bl  puts
	adr x9, class_nameTab
	ldr	x6, [x19, #obj_tag] // Get object tag
    lsl	x6, x6, #2	// *4
	add	x9, x9, x6
	ldr	x9, [x9, #0]	// Load class name string obj.
	add	x0, x9, #str_field // Adjust to beginning of str
	bl  puts
    ldr x0, =_uncaught_msg2
    bl  puts
	ldp x29, x30, [sp], #32   
    mov x0, #1
    bl  exit // exit(1)

/*
	.globl _stack_overflow_abort
// Stack Overflow Message
_stack_overflow_abort:
	la	$a0 _stack_overflow_msg
	li	$v0 4
	syscall			# print message
	li	$v0 10
	syscall			# Exit
*/

# START
	.globl _start
_start:
    // register signal handler
	//signal(SIGINT,sig_handler2);
	mov x0, #2
    ldr x1, =__sig_handler2
    bl signal
	// signal(SIGILL, sig_handler);
	mov x0, #4
    ldr x1, =__sig_handler
    bl signal
	// signal(SIGABRT, sig_handler);
	mov x0, #6
    ldr x1, =__sig_handler
    bl signal
	// signal(SIGBUS, sig_handler);
	mov x0, #7
    ldr x1, =__sig_handler
    bl signal
	// signal(SIGFPE, sig_handler);
	mov x0, #8
    ldr x1, =__sig_handler
    bl signal
	// signal(SIGSEGV, sig_handler);
	mov x0, #11
    ldr x1, =__sig_handler
    bl signal
	// signal(SIGSYS, sig_handler);
    mov x0, #31
    ldr x1, =__sig_handler
    bl signal
 
	mov x0, #0 // mov x0, xzr
	bl sbrk //sbrk
	mov x2, x0 // sbrk return address save to x2
	mov	x1, #MemMgr_REG_MASK
	mov x0, sp // initialize the garbage collector	
	bl	_MemMgr_Init		// sets $gp and $s7 (limit)
	
	adr x0, Main_protObj // create the Main object
	bl Object.copy // Call copy
	addiu	$sp $sp -4
	sw	$a0 4($sp)		# save the Main object on the stack
	move	$s0 $a0			# set $s0 to point to self
	jal	Main_init		# initialize the Main object
	jal	Main.main		# Invoke main method
	.globl __main_return
__main_return: # where we return after the call to Main.main
	addiu	$sp $sp 4		# restore the stack
	la	$a0 _term_msg		# show terminal message
	li	$v0 4
	syscall
	li $v0 10
	syscall				# syscall 10 (exit)

#
#  Polymorphic equality testing function:
#  Two objects are equal if they are
#    - identical (pointer equality, inlined in code)
#    - have same tag and are of type BOOL,STRING,INT and contain the
#      same data
#
#  INPUT: The two objects are passed in $t1 and $t2
#  OUTPUT: Initial value of $a0, if the objects are equal
#          Initial value of $a1, otherwise
#
#  The tags for Int,Bool,String are found in the global locations
#  _int_tag, _bool_tag, _string_tag, which are initialized by the
#  data part of the generated code. This removes a consistency problem
#  between this file and the generated code.
#

	.globl	equality_test
equality_test:			# ops in $t1 $t2
				# true in A0, false in A1
				# assume $t1, $t2 are not equal
	beq	$t1 $zero _eq_false # $t2 can't also be void   
	beq     $t2 $zero _eq_false # $t1 can't also be void   
	lw	$v0 obj_tag($t1)	# get tags
	lw	$v1 obj_tag($t2)
	bne	$v1 $v0 _eq_false	# compare tags
	lw	$a2 _int_tag	# load int tag
	beq	$v1 $a2 _eq_int	# Integers
	lw	$a2 _bool_tag	# load bool tag
	beq	$v1 $a2 _eq_int	# Booleans
	lw	$a2 _string_tag # load string tag
	bne	$v1 $a2 _eq_false  # Not a primitive type
_eq_str: # handle strings
	lw	$v0, str_size($t1)	# get string size objs
	lw	$v1, str_size($t2)
	lw	$v0, int_slot($v0)	# get string sizes
	lw	$v1, int_slot($v1)
	bne	$v1 $v0 _eq_false
	beqz	$v1 _eq_true		# 0 length strings are equal
	add	$t1 str_field		# Point to start of string
	add	$t2 str_field
	move	$t0 $v0		# Keep string length as counter
_eq_l1:
	lbu	$v0,0($t1)	# get char
	add	$t1 1
	lbu	$v1,0($t2)
	add	$t2 1
	bne	$v1 $v0 _eq_false
	addiu	$t0 $t0 -1	# Decrement counter
	bnez	$t0 _eq_l1
	b	_eq_true		# end of strings
		
_eq_int:	# handles booleans and ints
	lw	$v0,int_slot($t1)	# load values
	lw	$v1,int_slot($t2)
	bne	$v1 $v0 _eq_false
_eq_true:
	jr	$ra		# return true
_eq_false:
	move	$a0 $a1		# move false into accumulator
	jr	$ra

#
#  _dispatch_abort
#
#      filename in $a0
#      line number in $t1
#  
#  Prints error message and exits.
#  Called on dispatch to void.
#
	.globl	_dispatch_abort
_dispatch_abort:		 
        sw      $t1 0($sp)       # save line number
        addiu   $sp $sp -4
	addiu   $a0 $a0 str_field # adjust to beginning of string
	li      $v0 4
	syscall                  # print file name
	la      $a0 _colon_msg
	li	$v0 4
	syscall                  # print ":"
	lw      $a0 4($sp)       # 
	li	$v0 1
	syscall			 # print line number
	li 	$v0 4
	la	$a0 _dispatch_msg
	syscall			 # print dispatch-to-void message
	li   	$v0 10
        syscall			 # exit


#
#  _case_abort2
#
#      filename in $a0
#      line number in $t1
#  
#  Prints error message and exits.
#  Called on case on void.
#
	.globl	_case_abort2
_case_abort2:		 
        sw      $t1 0($sp)       # save line number
        addiu   $sp $sp -4
	addiu   $a0 $a0 str_field # adjust to beginning of string
	li      $v0 4
	syscall                  # print file name
	la      $a0 _colon_msg
	li	$v0 4
	syscall                  # print ":"
	lw      $a0 4($sp)       # 
	li	$v0 1
	syscall			 # print line number
	li 	$v0 4
	la	$a0 _cabort_msg2
	syscall			 # print case-on-void message
	li   	$v0 10
        syscall			 # exit
	
#
#
#  _case_abort
#		Is called when a case statement has no match
#
#   INPUT:	$a0 contains the object on which the case was
#		performed
#
#   Does not return!
#
	.globl	_case_abort
_case_abort:			# $a0 contains case expression obj.
	move	$s0 $a0		# save the expression object
	la	$a0 _cabort_msg
	li	$v0 4
	syscall			# print_str
	la	$t1 class_nameTab
	lw	$v0 obj_tag($s0)	# Get object tag
	sll	$v0 $v0 2	# *4
	addu	$t1 $t1 $v0
	lw	$t1 0($t1)	# Load class name string obj.
	addiu	$a0 $t1 str_field # Adjust to beginning of str
	li	$v0 4		# print_str
	syscall
	la	$a0 _nl
	li	$v0 4		# print_str
	syscall
	li	$v0 10
	syscall			# Exit
	

#
# Copy method
#
#   Copies an object and returns a pointer to a new object in
#   the heap.  Note that to increase performance, the stack frame
#   is not set up unless it is absolutely needed.  As a result,
#   the frame is setup just before the call to "_MemMgr_Alloc" and
#   is destroyed just after it.  The increase in performance
#   occurs becuase the calls to "_MemMgr_Alloc" happen very
#   infrequently when the heap needs to be garbage collected.
#
#   INPUT:	$a0: object to be copied to free space in heap
#
#   OUTPUT:	$a0: points to the newly created copy.
#
#   Registers modified:
#	$t0, $t1, $t2, $t3, $t4, $v0, $v1, $a0, $a1, $a2, $gp, $s7
#

	.globl	Object.copy
Object.copy:
	add sp, sp, #-8 //create stack frame
	str x30, [sp, #8]
	str x0, [sp, #4]
	
	bl _MemMgr_Test // test GC area

	lw	$a0 4($sp)			# get object size
	lw	$a0 obj_size($a0)
	blez	$a0 _objcopy_error		# check for invalid size
	sll	$a0 $a0 2			# convert words to bytes
	addiu	$a0 $a0 4			# account for eyecatcher
	jal	_MemMgr_Alloc			# allocate storage
	addiu	$a1 $a0 4			# pointer to new object

	lw	$a0 4($sp)			# the self object
	lw	$ra 8($sp)			# restore return address
	addiu	$sp $sp 8			# remove frame
	lw	$t0 obj_size($a0)		# get size of object
	sll	$t0 $t0 2			# convert words to bytes
	b	_objcopy_allocated		# get on with the copy

# A faster version of Object.copy, for internal use (does not call
# _MemMgr_Test, and if possible not _MemMgr_Alloc)

_quick_copy:
	lw	$t0 obj_size($a0)		# get size of object to copy
	blez	$t0 _objcopy_error		# check for invalid size
	sll	$t0 $t0 2			# convert words to bytes
	addiu	$t1 $t0 4			# account for eyecatcher
	add	$gp $gp $t1			# allocate memory
	sub	$a1 $gp $t0			# pointer to new object
	blt	$gp $s7 _objcopy_allocated	# check allocation
_objcopy_allocate:
	sub	$gp $a1 4			# restore the original $gp
	addiu	$sp $sp -8			# frame size
	sw	$ra 8($sp)			# save return address
	sw	$a0 4($sp)			# save self
	move	$a0 $t1				# put bytes to allocate in $a0
	jal	_MemMgr_Alloc			# allocate storage
	addiu	$a1 $a0 4			# pointer to new object
	lw	$a0 4($sp)			# the self object
	lw	$ra 8($sp)			# restore return address
	addiu	$sp $sp 8			# remove frame
	lw	$t0 obj_size($a0)		# get size of object
	sll	$t0 $t0 2			# convert words to bytes
_objcopy_allocated:
	addiu	$t1 $0 -1
	sw	$t1 obj_eyecatch($a1)		# store eyecatcher
	add	$t0 $t0 $a0			# find limit of copy
	move	$t1 $a1				# save source
#  _objcopy_loop:
# 	lw	$v0 0($a0)			# copy word
# 	sw	$v0 0($t1)
# 	addiu	$a0 $a0 4			# update source
# 	addiu	$t1 $t1 4			# update destination
# 	bne	$a0 $t0 _objcopy_loop		# loop
# _objcopy_end:
# 	move	$a0 $a1				# put new object in $a0
# 	jr	$ra				# return
_objcopy_error:
	la	$a0 _objcopy_msg		# show error message
	li	$v0 4
	syscall
	li	$v0 10				# exit
	syscall

#
#
# Object.abort
#
#	The abort method for the object class (usually inherited by
#	all other classes)
#
#   INPUT:	$a0 contains the object on which abort() was dispatched.
#

	.globl	Object.abort
Object.abort:
	move	$s0 $a0		# save self
	li	$v0 4
	la	$a0 _abort_msg
	syscall			# print_str
	la	$t1 class_nameTab
	lw	$v0 obj_tag($s0)	# Get object tag
	sll	$v0 $v0 2	# *4
	addu	$t1 $t1 $v0
	lw	$t1 0($t1)	# Load class name string obj.
	addiu	$a0 $t1 str_field	# Adjust to beginning of str
	li	$v0 4		# print_str
	syscall
	la	$a0 _nl
	li	$v0 4
	syscall			# print new line
	li	$v0 10
	syscall			# Exit

#
#
# Object.type_name	
#
#   	INPUT:	$a0 object who's class name is desired
#	OUTPUT:	$a0 reference to class name string object
#

	.globl	Object.type_name
Object.type_name:
	la	$t1 class_nameTab
	lw	$v0 obj_tag($a0)	# Get object tag
	sll	$v0 $v0 2	# *4
	addu	$t1 $t1 $v0	# index table
	lw	$a0 0($t1)	# Load class name string obj.
	jr	$ra

#
#
# IO.out_string
#
#	Prints out the contents of a string object argument
#	which is on top of the stack.
#
#	$a0 is preserved!
#

	.globl	IO.out_string
IO.out_string:
	addiu	$sp $sp -4
	sw	$a0 4($sp)	# save self
	lw	$a0 8($sp)	# get arg
	addiu	$a0 $a0 str_field	# Adjust to beginning of str
	li	$v0 4		# print_str
	syscall
	lw	$a0 4($sp)	# return self
	addiu	$sp $sp 8	# pop argument
	jr	$ra

#
#
# IO.out_int
#
#	Prints out the contents of an integer object on top of the
#	stack.
#
#	$a0 is preserved!
#

	.globl	IO.out_int
IO.out_int:
	addiu	$sp $sp -4
	sw	$a0 4($sp)	# save self
	lw	$a0 8($sp)	# get arg
	lw	$a0 int_slot($a0)	# Fetch int
	li	$v0 1		# print_int
	syscall	
	lw	$a0 4($sp)	# return self
	addiu	$sp $sp 8	# pop argument
	jr	$ra

#
#
# IO.in_int
#
#	Returns an integer object read from the terminal in $a0
#

	.globl	IO.in_int
IO.in_int:
	addiu	$sp $sp -4
	sw	$ra 4($sp)	# save return address

        la      $a0 Int_protObj
        jal     _quick_copy	# Call copy
        jal     Int_init

	addiu	$sp $sp -4
	sw	$a0 4($sp)	# save new object

	li	$v0, 5		# read int
	syscall

	lw	$a0 4($sp)
	addiu	$sp $sp 4


	sw	$v0 int_slot($a0)	# store int read into obj
	lw	$ra 4($sp)
	addiu	$sp $sp 4
	jr	$ra

#
#
# IO.in_string
#
#	Returns a string object read from the terminal, removing the
#	'\n'
#
#	OUTPUT:	$a0 the read string object
#

	.globl	IO.in_string
IO.in_string:
	addiu	$sp $sp -8
	sw	$ra 8($sp)			# save return address
	sw	$0 4($sp)			# init GC area

	jal	_MemMgr_Test			# test GC area

	la	$a0 Int_protObj			# Int object for string size
	jal	_quick_copy
	jal	Int_init
	sw	$a0 4($sp)			# save it

	li	$a0 str_field			# size of string obj. header
	addiu	$a0 $a0 str_maxsize		# max size of string data
	jal	_MemMgr_QAlloc			# make sure enough room

	la	$a0 String_protObj		# make string object
	jal	_quick_copy
	jal	String_init
	lw	$t0 4($sp)			# get size object
	sw	$t0 str_size($a0)		# store size object in string
	sw	$a0 4($sp)			# save string object

	addiu	$gp $gp -4			# overwrite last word

_instr_ok:
 	li	$a1 str_maxsize			# largest string to read
 	move	$a0 $gp	
 	li	$v0, 8				# read string
 	syscall

 	move	$t0 $gp				# t0 to beginning of string
_instr_find_end:
 	lb	$v0 0($gp)
 	addiu	$gp $gp 1
 	bnez	$v0 _instr_find_end

 	# $gp points just after the null byte
 	lb	$v0 0($t0)			# is first byte '\0'?
 	bnez	$v0 _instr_noteof

 	# we read nothing. Return '\n' (we don't have '\0'!!!)
 	add	$v0 $zero 10			# load '\n' into $v0
 	sb	$v0 -1($gp)
 	sb	$zero 0($gp)			# terminate
 	addiu	$gp $gp 1
 	b	_instr_nonl

_instr_noteof:
 	# Check if there really is a '\n'
 	lb	$v0 -2($gp)
 	bne	$v0 10 _instr_nonl

 	# Write '\0' over '\n'
 	sb	$zero -2($gp)			# Set end of string where '\n' was
 	addiu	$gp $gp -1			# adjust for '\n'

_instr_nonl:
 	lw	$a0 4($sp)			# get pointer to new str obj
 	lw	$t1 str_size($a0)		# get pointer to int obj

 	sub	$t0 $gp $a0
 	subu	$t0 str_field			# calc actual str size
 	addiu	$t0  -1				# adjust for '\0'
 	sw	$t0 int_slot($t1)		# store string size in int obj
 	addi	$gp $gp 3			# was already 1 past '\0'
 	la	$t0 0xfffffffc
 	and	$gp $gp $t0			# word align $gp

 	sub	$t0 $gp $a0			# calc length
 	srl	$t0 $t0 2			# divide by 4
 	sw	$t0 obj_size($a0)		# set size field of obj

 	lw	$ra 8($sp)			# restore return address
 	addiu	$sp $sp 8
 	jr	$ra				# return

#
#
# String.length
#		Returns Int Obj with string length of self
#
#	INPUT:	$a0 the string object
#	OUTPUT:	$a0 the int object which is the size of the string
#

	.globl	String.length
String.length:
	lw	$a0 str_size($a0)	# fetch attr
	jr	$ra	# Return

#
# String.concat
#
#   Concatenates arg1 onto the end of self and returns a pointer
#   to the new object.
#
#	INPUT:	$a0: the first string object (self)
#		Top of stack: the second string object (arg1)
#
#	OUTPUT:	$a0 the new string object
#

	.globl	String.concat
String.concat:

	addiu	$sp $sp -16
	sw	$ra 16($sp)			# save return address
	sw	$a0 12($sp)			# save self arg.
	sw	$0 8($sp)			# init GC area
	sw	$0 4($sp)			# init GC area

	jal	_MemMgr_Test			# test GC area

	lw	$a0 12($sp)
	lw	$a0 str_size($a0)
	jal     _quick_copy			# Call copy
	sw	$a0 8($sp)			# save new size object

	lw	$t1 20($sp)			# load arg object
	lw	$t1 str_size($t1)		# get size object
	lw	$t1 int_slot($t1)		# arg string size
	blez	$t1 _strcat_argempty		# nothing to add
	lw	$t0 12($sp)			# load self object
	lw	$t0 str_size($t0)		# get size object
	lw	$t0 int_slot($t0)		# self string size
	addu	$t0 $t0 $t1			# new size
	sw	$t0 int_slot($a0)		# store new size

	addiu	$a0 $t0 str_field		# size to allocate
	addiu	$a0 $a0 4			# include '\0', +3 to align
	la	$t2 0xfffffffc		# 0xfffffffc (= 1111 1111 1111 1111 1111 1111 1111 1100)
	and	$a0 $a0 $t2			# align on word boundary (& 0xfffffffc which can be divided by 4)
	addiu   $a0 $a0 1                       # make size odd for GC <-|
	sw	$a0 4($sp)			# save size in bytes     |
	addiu	$a0 $a0 3			# include eyecatcher(4) -1
	jal	_MemMgr_QAlloc			# check memory

	lw	$a0 12($sp)			# copy self
	jal	_quick_copy			# Call copy
	lw	$t0 8($sp)			# get the Int object
	sw	$t0 str_size($a0)		# store it in the str obj.

	sub	$t1 $gp $a0			# bytes allocated by _quick_copy
	lw	$t0 4($sp)			# get size in bytes (no eyecatcher)
	sub     $t0 $t0 1                       # Remove extra 1 (was for GC)
	sub	$t1 $t0 $t1			# more memory needed
	addu	$gp $gp $t1			# allocate rest
	srl	$t0 $t0 2			# convert to words
	sw	$t0 obj_size($a0)		# save new object size

	lw	$t0 12($sp)			# get original self object
	lw	$t0 str_size($t0)		# get size object
	lw	$t0 int_slot($t0)		# self string size
	addiu	$t1 $a0 str_field		# points to start of string data
	addu	$t1 $t1 $t0			# points to end: '\0'
	lw	$t0 20($sp)			# load arg object
	addiu	$t2 $t0 str_field		# points to start of arg data
	lw	$t0 str_size($t0)		# get arg size
	lw	$t0 int_slot($t0)
	addu	$t0 $t0 $t2			# find limit of copy

_strcat_copy:
 	lb	$v0 0($t2)			# load from source
 	sb	$v0 0($t1)			# save in destination
 	addiu	$t2 $t2 1			# advance each index
 	addiu	$t1 $t1 1
 	bne	$t2 $t0 _strcat_copy		# check limit
 	sb	$0 0($t1)			# add '\0'

 	lw	$ra 16($sp)			# restore return address
 	addiu	$sp $sp 20			# pop argument
 	jr	$ra				# return

_strcat_argempty:
	lw	$a0 12($sp)			# load original self
	lw	$ra 16($sp)			# restore return address
	addiu	$sp $sp 20			# pop argument
	jr	$ra				# return

#
#
# String.substr(i,l)
#		Returns the sub string of self from i with length l
#		Offset starts at 0.
#
#	INPUT:	$a0 the string
#		length int object on top of stack (-4)
#		index int object below length on stack (-8)
#	OUTPUT:	The substring object in $a0
#

	.globl	String.substr
String.substr:
	addiu	$sp $sp -12		# frame
	sw	$ra 4($sp)		# save return
	sw	$a0 12($sp)		# save self
	sw	$0 8($sp)		# init GC area

	jal	_MemMgr_Test		# test GC area

	lw	$a0 12($sp)
	lw	$v0 obj_size($a0)
        la      $a0 Int_protObj		# ask if enough room to allocate
	lw	$a0 obj_size($a0)	#   a string object, an int object,
	add	$a0 $a0 $v0		#   and the string data
	addiu	$a0 $a0 2		# include 2 eyecatchers
	sll	$a0 $a0 2
	addi	$a0 $a0 str_maxsize
	jal	_MemMgr_QAlloc

_ss_ok:
	la	$a0 Int_protObj
	jal	_quick_copy
	jal	Int_init
	sw	$a0 8($sp)	# save new length obj
	la	$a0 String_protObj
	jal	_quick_copy
	jal	String_init	# new obj ptr in $a0
	move	$a2 $a0		# use a2 to make copy
	addiu	$gp $gp -4	# backup alloc ptr
	lw	$a1 12($sp)	# load orig
	lw	$t1 20($sp)	# index obj
	lw	$t2 16($sp)	# length obj
	lw	$t0 str_size($a1)
	lw	$v1 int_slot($t1) # index
	lw	$v0 int_slot($t0) # size of orig
	bltz	$v1 _ss_abort1	# index is smaller than 0
	bgt	$v1 $v0 _ss_abort2	# index > orig
	lw	$t3 int_slot($t2) # sub length
	add	$v1 $v1 $t3	# index+sublength
	bgt	$v1 $v0 _ss_abort3
	bltz	$t3 _ss_abort4
	lw	$t4 8($sp)	# load new length obj
	sw	$t3 int_slot($t4) # save new size
	sw	$t4 str_size($a0) # store size in string
	lw	$v1 int_slot($t1) # index
	addiu	$a1 $a1 str_field # advance src to str
	add	$a1 $a1 $v1	  # advance to indexed char
	addiu	$a2 $a2 str_field # advance dst to str
	beqz	$t3 _ss_end	  # empty length
_ss_loop:
	lb	$v0 0($a1)
	addiu	$a1 $a1 1	# inc src
	sb	$v0 0($a2)
	addiu	$a2 $a2 1	# inc dst
	addiu	$t3 $t3 -1	# dec ctr
	bnez	$t3 _ss_loop
_ss_end:
	sb	$zero 0($a2)	# null terminate (sb rt address ;store byte in rt to address)
	move	$gp $a2
	addiu	$gp $gp 4	# realign the heap ptr
	la	$t0 0xfffffffc
	and	$gp $gp $t0	# word align $gp

	sub	$t0 $gp $a0	# calc object size
	srl	$t0 $t0 2	# div by 4 (srl - shift right logical)
	sw	$t0 obj_size($a0)

	lw	$ra 4($sp)
	addiu	$sp $sp 20	# pop arguments
	jr	$ra

_ss_abort1:
	la	$a0 _sabort_msg1
	b	_ss_abort
_ss_abort2:
	la	$a0 _sabort_msg2
	b	_ss_abort
_ss_abort3:
	la	$a0 _sabort_msg3
	b	_ss_abort
_ss_abort4:
	la	$a0 _sabort_msg4
_ss_abort:
	li	$v0 4
	syscall
	la	$a0 _sabort_msg
	li	$v0 4
	syscall
	li	$v0 10		# exit
	syscall

//
// MemMgr Memory Manager
//
//   The MemMgr functions give a consistent view of the garbage collectors.
//   This allows multiple collectors to exist in one file and the easy
//   selection of different collectors.  It includes functions to initialize
//   the collector and to reserve memory and query its status.
//
//   The following assumptions are made:
//
//     1) The allocation of memory involves incrementing the $gp pointer.
//        The $s7 pointer serves as a limit.  The collector function is
//        called before $s7 is exceeded by $gp.
//
//     2) The initialization functions all take the same arguments as
//        defined in "_MemMgr_Init".
//
//     3) The garbage collector functions all take the arguments.  "$a0"
//        contains the end of the stack to check for pointers.  "$a1"
//        contains the size in bytes needed by the program and must be
//        preserved across the function call.
//

/* 
 Initialize the Memory Manager

   Call the initialization routine for the garbage collector.

   INPUT:
	$a0: start of stack
	$a1: initial Register mask
	$a2: end of heap
	heap_start: start of the heap

   OUTPUT:
	$gp: lower bound of the work area
	$s7: upper bound of the work area

   Registers modified:
	$t0, initializer function
*/

	.globl _MemMgr_Init
_MemMgr_Init:
	str x30, [sp, #-8]! // save return address
	adr x12, _MemMgr_INITIALIZER // pointer to initialization
	ldr x12,  [x12, #0]
	blr x12 // initialize
	ldr x30, [sp], #8 // restore return address
	ret // return

#
# Memory Allocation
#
#   Allocates the requested amount of memory and returns a pointer
#   to the start of the block.
#
#   INPUT:
#	$a0: size of allocation in bytes
#	$s7: limit pointer of the work area
#	$gp: current allocation pointer
#	heap_start: start of heap
#
#   OUTPUT:
#	$a0: pointer to new memory block
#
#   Registers modified:
#	$t0, $a1, collector function
#

	.globl _MemMgr_Alloc
_MemMgr_Alloc:
    add x27, x27, x0 // attempt to allocate storage
	cmp x27, x26
	b.lt _MemMgr_Alloc_end // check allocation
	sub x27, x27, x0 // restore $gp
	add sp, sp, #-8
	str x30, [sp, #4] // save return address
	mov x1, x0 // size
	add x0, sp, #4 // end of stack to collect
	adr x12, _MemMgr_COLLECTOR // pointer to collector function
	ldr x12, [x12 #0]
	blr x12 // garbage collect
    ldr x30, [sp, #4]
	add sp, sp, #8
	mov x0, x1 // put size into $a0
	add x27, x27, x0 // allocate storage
_MemMgr_Alloc_end:
	sub x0, x27, x0
	ret // return

#
# Query Memory Allocation
#
#   Verifies that the requested amount of memory can be allocated
#   within the work area.
#
#   INPUT:
#	$a0: size of allocation in bytes
#	$s7: limit pointer of the work area
#	$gp: current allocation pointer
#	heap_start: start of heap
#
#   OUTPUT:
#	$a0: size of allocation in bytes (unchanged)
#
#   Registers modified:
#	$t0, $a1, collector function
#

	.globl _MemMgr_QAlloc
_MemMgr_QAlloc:
	add	$t0 $gp $a0			# attempt to allocate storage
	blt	$t0 $s7 _MemMgr_QAlloc_end	# check allocation
	addiu	$sp $sp -4
	sw	$ra 4($sp)			# save return address
	move	$a1 $a0				# size
	addiu	$a0 $sp 4			# end of stack to collect
	la	$t0 _MemMgr_COLLECTOR		# pointer to collector function
	lw	$t0 0($t0)
	jalr	$t0				# garbage collect
	lw	$ra 4($sp)			# restore return address
	addiu	$sp $sp 4
	move	$a0 $a1				# put size into $a0
_MemMgr_QAlloc_end:
	jr	$ra				# return

#
# Test heap consistency
#
#   Runs the garbage collector in the hope that this will help detect
#   garbage collection bugs earlier.
#
#   INPUT: (the usual GC stuff)
#	$s7: limit pointer of the work area
#	$gp: current allocation pointer
#	heap_start: start of heap
#
#   OUTPUT:
#	none
#
#   Registers modified:
#	$t0, $a1, collector function

	.globl	_MemMgr_Test
_MemMgr_Test:
	adr x12, _MemMgr_TEST // Check if testing enabled
	ldr x12, [x12, #0]
	cmp x12, xzr
	b.eq _MemMgr_Test_end

# Allocate 0 bytes
	add sp, sp, #-8 // Save return address
	str x30, [sp, #4]
	mov x1, #0 // size = 0
	add x0, sp, #4 // end of stack to collect
	adr x12, _MemMgr_COLLECTOR // pointer to collector function
	ldr x12, [x12, #0]
	blr x12 // garbage collect
	ldr x30, [sp, #4] // restore return address
	add sp, sp, #8

_MemMgr_Test_end:
	ret

#
# GenGC Generational Garbage Collector
#
#   This is an implementation of a generational garbage collector
#   as described in "Simple Generational Garbage Collection and Fast
#   Allocation" by Andrew W. Appel [Princeton University, March 1988].
#   This is a two generation scheme which uses an assignment table
#   to handle root pointers located in the older generation objects.
#
#   When the work area is filled, a minor garbage collection takes place
#   which moves all live objects into the reserve area.  These objects
#   are then incorporated into the old area.  New reserve and work areas
#   are setup and allocation can continue in the work area.  If a break-
#   point is reached in the size of the old area just after a minor
#   collection, a major collection then takes place.  All live objects in
#   the old area are then copied into the new area, expanding the heap if
#   necessary.  The X and new areas are then block copied back L1-L0
#   bytes to form the next old area.
#
#   The assignment table is implemented as a stack growing towards the
#   allocation pointer ($gp) in the work area.  If they cross, a minor
#   collection is then carried out.  This allows the garbage collector to
#   to have to keep a fixed table of assignments.  As a result, programs
#   with many assignments will tend not to be bogged down with extra
#   garbage collections.
#
#   The unused area was implemented to help keep the garbage collector
#   from continually expanding the heap.  This buffer zone allows major
#   garbage collections to happen earlier, reducing the risk of expansions
#   due to too many live objects in the old area.  The histories kept by
#   the garbage collector in MAJOR0, MAJOR1, MINOR0, and MINOR1 also help
#   to prevent unnecessary expansions of the heap.  If many live objects
#   were recently collected, the garbage collections will start to occur
#   sooner.
#
#   Note that during a minor collection, the work area is guaranteed to
#   fit within the reserve area.  However, during a major collection, the
#   old area will not necessarily fit in the new area.  If the latter occurs,
#   "_GenGC_OfsCopy" will detect this and expand the heap.
#
#   The heap is expanded on two different occasions:
#
#     1) After a major collection, the old area is set to be at most
#        1/(2^GenGC_OLDRATIO) of the usable heap (L0 to L3).  Note that
#        first L4 is checked to see if any of the unused memory between L3
#        and L4 is enough to satisfy this requirement.  If not, then the
#        heap will be expanded.  If it is, the appropriate amount will be
#        transfered from the unused area to the work/reserve area.
#
#     2) During a major collection, if the live objects in the old area
#        do not fit within the new area, the heap is expanded and $s7
#        is updated to reflact this.  This value later gets stored back
#        into L4.
#
#   During a normal allocation and minor collections, the heap has the
#   following form:
#
#      Header
#       |
#       |   Older generation objects
#       |    |
#       |    |             Minor garbage collection area
#       |    |              |
#       |    |              |                Allocation area
#       |    |              |                 |
#       |    |              |                 |           Assignment table
#       |    |              |                 |            |
#       |    |              |                 |            |   Unused
#       |    |              |                 |            |    |
#       v    v              v                 v            v    v
#     +----+--------------+-----------------+-------------+---+---------+
#     |XXXX| Old Area     | Reserve Area    | Work Area   |XXX| Unused  |
#     +----+--------------+-----------------+-------------+---+---------+
#      ^    ^              ^                 ^    ^        ^   ^         ^
#      |    |              |                 |    |-->  <--|   |         |
#      |    L0             L1                L2  $gp      $s7  L3        L4
#      |
#     heap_start
#
#     $gp (allocation pointer): points to the next free word in the work
#         area during normal allocation.  During a minor garbage collection,
#         it points to the next free work in the reserve area.
#
#     $s7 (limit pointer): points to the limit that $gp can traverse.  Between
#         it and L3 sits the assignment table which grows towards $gp.
#
#   During a Major collection, the heap has the following form:
#
#      Header
#       |
#       |   Older generation objects
#       |    |
#       |    |                 Objects surviving last minor garbage collection
#       |    |                  |
#       |    |                  |         Major garbage collection area
#       |    |                  |          |
#       v    v                  v          v
#     +----+------------------+----------+------------------------------+
#     |XXXX| Old Area         | X        | New Area                     |
#     +----+------------------+----------+------------------------------+
#      ^    ^                  ^      ^   ^      ^                       ^
#      |    |                  |      |   |      |-->                    |
#      |    L0                 L1     |   L2    $gp                   L4, $s7
#      |                              |
#     heap_start                     breakpoint
#
#     $gp (allocation pointer): During a major collection, this points
#         into the next free word in the new area.
#
#     $s7 (limit pointer): During a major collection, the points to the
#         limit of heap memory.  $gp is not allowed to pass this value.
#         If the objects in the live old area cannot fit in the new area,
#         more memory is allocated and $s7 is adjusted accordingly.
#
#     breakpoint: Point where a major collection will occur.  It is
#         calculated by the following formula:
#
#         breakpoint = MIN(L3-MAX(MAJOR0,MAJOR1)-MAX(MINOR0,MINOR1),
#                          L3-(L3-L0)/2)
#
#         where (variables stored in the header):
#           MAJOR0 = total size of objects in the new area after last major
#                    collection.
#           MAJOR1 = (MAJOR0+MAJOR1)/2
#           MINOR0 = total size of objects in the reserve area after last
#                    minor collection.
#           MINOR1 = (MINOR0+MINOR1)/2
#
#   The following assumptions are made in the garbage collection
#   process:
#
#     1) Pointers on the Stack:
#        Every word on the stack that ends in 0 (i.e., is even) and is
#	 a valid address in the heap is assumed to point to an object
#        in the heap.  Even heap addresses on the stack that are actually
#	 something else (e.g., raw integers) will probably cause an
#        garbage collection error.
#
#     2) Object Layout:
#        Besides the Int, String, and Bool objects (which are handled
#        separately), the garbage collector assumes that each attribute
#        in an object is a pointer to another object.  It, however,
#        still does as much as possible to verify this before actually
#        updating any fields.
#
#     3) Pointer tests:
#        In order to be verified as an object, a pointer must undergo
#        certain tests:
#
#          a) The pointer must point within the correct storage area.
#          b) The word before the pointer (obj_eyecatch) must be the
#             word 0xFFFF FFFF
#          c) The word at the pointer must not be 0xFFFF FFFF (i.e.
#             -1 cannot be a class tag)
#
#        These tests are performed whenever any data could be a pointer
#        to keep any non-pointers from being updated accidentally.  The
#        functions "_GenGC_ChkCopy" and "_GenGC_OfsCopy" are responsible
#        for these checks.
#
#     4) The size stored in the object does not include the word required
#        to store the eyecatcher for the object in the heap.  This allows
#        the prototype objects to not require its own eyecatcher.  Also,
#        a size of 0 is invalid because it is used as a flag by the garbage
#        collector to indicate a forwarding pointer in the "obj_disp" field.
#
#     5) Roots are contained in the following areas: the stack, registers
#        specified in the REG mask, and the assignment table.
#

#
# Constants
#

#
# GenGC header offsets from "heap_start"
#
/*
GenGC_HDRSIZE=44				# size of GenGC header
GenGC_HDRL0=0					# pointers to GenGC areas
GenGC_HDRL1=4
GenGC_HDRL2=8
GenGC_HDRL3=12
GenGC_HDRL4=16
GenGC_HDRMAJOR0=20				# history of major collections
GenGC_HDRMAJOR1=24
GenGC_HDRMINOR0=28				# history of minor collections
GenGC_HDRMINOR1=32
GenGC_HDRSTK=36					# start of stack
GenGC_HDRREG=40					# current REG mask
*/
#
# Granularity of heap expansion
#
#   The heap is always expanded in multiples of 2^k, where
#   k is the granularity.
#
/*
GenGC_HEAPEXPGRAN=14				# 2^14=16K
*/
#
# Old to usable heap size ratio
#
#   After a major collection, the ratio of size of old area to the usable
#   size of the heap is at most 1/(2^k) where k is the value provided.
#
/*
GenGC_OLDRATIO=2				# 1/(2^2)=.25=25%
*/
#
# Mask to speficy which registers can be automatically updated
# when a garbage collection occurs.  The Automatic Register Update
# (ARU) mask has a bit set for all possible registers the
# garbage collector is able to handle.  The Register (REG) mask
# determines which register(s) are actually updated.
#
# BITS----------------------------
# 3 2         1         0
# 10987654321098765432109876543210
# --------------------------------
#
# 11000011011111110000000000000000  <-  Auto Register Update (ARU) mask
# +--++--++--++--++--++--++--++--+      $s0-$s6, $t8-$t9, $s8, $ra
#    C   3   7   F   0   0   0   0     ($16-$22, $24-$25, $30, $31)
#

//GenGC_ARU_MASK=0xC37F0000

#
# Functions
#

#
# Initialization
#
#   Sets up the header information block for the garbage collector.
#   This block is located at the start of the heap ("heap_start")
#   and includes information needed by the garbage collector.  It
#   also calculates the barrier for the reserve and work areas and
#   sets the L2 pointer accordingly, rounding off in favor of the
#   reserve area.
#
#   INPUT:
#	$a0: start of stack
#	$a1: initial Register mask
#	$a2: end of heap
#	heap_start: start of the heap
#
#   OUTPUT:
#	$gp: lower bound of the work area
#	$s7: upper bound of the work area
#
#   Registers modified:
#	$t0, $t1, $v0, $a0
#

	.globl _GenGC_Init
_GenGC_Init:
	adr x12, heap_start
	add x9, x12, #GenGC_HDRSIZE
	str x9, [x12, #GenGC_HDRL0] // save start of old area
	str x9, [x12, #GenGC_HDRL1] // save start of reserve area
	sub x9, x2, x9 // find reserve/work area barrier
	lsr x9, x9, #1
	ldr x6, =0xfffffffc
	and x9, x9, x6
	cmp t1, xzr            // heap initially to small
	b.le _GenGC_Init_error //
	sub	x27, x2, x9
	str x27, [x12, #GenGC_HDRL2] // save start of work area
	str x2, [x12, #GenGC_HDRL3] // save end of work area
	mov x26, x2 // set limit pointer
	str xzr, [x12, #GenGC_HDRMAJOR0] // clear histories
	str xzr, [x12, #GenGC_HDRMAJOR1]
	str xzr, [x12, #GenGC_HDRMINOR0]
	str xzr, [x12, #GenGC_HDRMINOR1]
	str x0, [x12, #GenGC_HDRSTK] // save stack start
	str x1, [x12, #GenGC_HDRREG] // save register mask
	mov x0, #0 // get heap end
    bl sbrk
	mov x6, x0
	str x6, [x12, #GenGC_HDRL4] // save heap limit
    adr x12, _MemMgr_TEST // Check if testing enabled
	ldr x12, [x12, #0]
	cmp x12, xzr
	b.eq _MemMgr_Test_false
	ldr x0, =_GenGC_Init_test_msg // tell user GC is in test mode
	bl puts
	b _GenGC_Init_end
_MemMgr_Test_false:
	ldr x0, =_GenGC_Init_msg
	bl puts
_GenGC_Init_end:
	ret // return
_GenGC_Init_error:
	ldr x0, =_GenGC_INITERROR //show error message
	bl puts
	mov x0, #1 // exit
	bl exit

#
# Record Assignment
#
#   Records an assignment in the assignment table.  Note that because
#   $s7 is always greater than $gp, an assignment can always be
#   recorded.
#
#   INPUT:
#	$a1: pointer to the pointer being modified
#	$s7: limit pointer of the work area
#	$gp: current allocation pointer
#	heap_start: start of heap
#
#   Registers modified:
#	$t0, $t1, $t2, $v0, $v1, $a1, $a2, $gp, $s7
#
#   sm: $a0 is explicitly saved in the GC case so that in the normal
#   case the caller need not save/restore $a0
#
#   sm: Apparently _GenGC_Collect wants $a0 to be the last+1 word
#   of the stack, rather than the last word; I've therefore changed
#     addiu   $a0 $sp 4
#   to
#     addiu   $a0 $sp 0     (i.e. move $a0 $sp)
#   Just in case this isn't exactly right, I've also put 0 into that 
#   last spot so it will definitely be safely ignored.
#

	.globl _GenGC_Assign
_GenGC_Assign:
	addiu	$s7 $s7 -4
	sw	$a1 0($s7)			# save pointer to assignment
	bgt	$s7 $gp _GenGC_Assign_done
	addiu	$sp $sp -8
	sw	$ra 8($sp)			# save return address
	sw	$a0 4($sp)			# sm: save $a0
	move    $a1 $0				# size
	addiu	$a0 $sp 0			# end of stack to collect
	sw      $0 0($sp)                       # play it safe with off-by-1
	jal	_GenGC_Collect
	lw	$ra 8($sp)			# restore return address
	lw	$a0 4($sp)			# restore $a0
	addiu	$sp $sp 8
_GenGC_Assign_done:
	jr	$ra				# return

	.globl	_gc_check
_gc_check:
	beqz	$a1, _gc_ok			# void is ok
	lw	$a2 obj_eyecatch($a1)		# and check if it is valid
	addiu	$a2 $a2 1
	bnez	$a2 _gc_abort
_gc_ok:
	jr	$ra

_gc_abort:		 
	la      $a0 _gc_abort_msg
	li	$v0 4
	syscall                  # print gc message
	li   	$v0 10
        syscall			 # exit


#
# Generational Garbage Collection
#
#   This function implements the generational garbage collection.
#   It first calls the minor collector, "_GenGC_MinorC", and then
#   updates its history in the header.  The breakpoint is then
#   calculated.  If the breakpoint is reached or there is still not
#   enough room to allocate the requested size, a major garbage
#   collection then takes place by calling "_GenGC_MajorC".  After
#   the major collection, the size of the old area is analyzed.  If
#   it is greater than 1/(2^GenGC_OLDRATIO) of the total usable heap
#   size (L0 to L3), the heap is expanded.  Also, if there is still not
#   enough room to allocate the requested size, the heap is expanded
#   further to make sure that the specified amount of memory can be
#   allocated. If there is enough room in the unused area (L3 to L4),
#   this memory is used and the heap is not expanded.  The $s7 and $gp
#   pointers are then set as well as the L2 pointer.  If a major collection
#   is not done, the X area is incorporated into the old area
#   (i.e. the L2 pointer is moved into L1) and $s7, $gp, and L2 are
#   then set.
#
#   INPUT:
#	$a0: end of stack
#	$a1: size will need to allocate in bytes
#	$s7: limit pointer of thw work area
#	$gp: current allocation pointer
#	heap_start: start of heap
#
#   OUTPUT:
#	$a1: size will need to allocate in bytes (unchanged)
#
#   Registers modified:
#	$t0, $t1, $t2, $t3, $t4, $v0, $v1, $a0, $a2, $gp, $s7
#

	.globl _GenGC_Collect
_GenGC_Collect:
	add sp, sp, #24
	str x30, [sp, #12] // save return address
	str x0, [sp, #8] // save stack end
	str x1, [sp, #4] // save size
	ldr x0, =_GenGC_COLLECT // print collection message
	bl puts
	ldr x0, [sp, #8] // restore stack end
	bl _GenGC_MinorC // minor collection
	adr x1, heap_start
	ldr x9, [x1, #GenGC_HDRMINOR1]
	add x9, x9, x0
	lsr x9, x9, #1
	str x9, [x1, #GenGC_HDRMINOR1] // update histories
    str x0, [x1, #GenGC_HDRMINOR0]
	mov x12, x9 // set $t0 to max of minor
	cmp x9, x0
	b.gt _GenGC_Collect_maxmaj
	mov x12, x0
_GenGC_Collect_maxmaj:
	ldr x9, [x1, #GenGC_HDRMAJOR0] // set $t1 to max of major
	ldr x10, [x1, #GenGC_HDRMAJOR1]
	cmp x9, x10
	b.gt _GenGC_Collect_maxdef
	mov x9, x10
_GenGC_Collect_maxdef:
	ldr x10, [x1, #GenGC_HDRL3]
	sub x12, x10, x12 // set $t0 to L3-$t0-$t1
	sub x12, x12, x9
	ldr x9, [x1, #GenGC_HDRL0] // set $t1 to L3-(L3-L0)/2
	sub x9, x10, x9
	lsr x9, x9, #1
	sub x9, x10, x9
	cmp x12, x9
	b.lt _GenGC_Collect_breakpt // set $t0 to minimum of above
	mov x12, x9
_GenGC_Collect_breakpt:
	ldr x9, [x1, #GenGC_HDRL1] // get end of old area
	cmp x9, x12
	b.ge _GenGC_Collect_major
	ldr x12, [x1, #GenGC_HDRL2]
	ldr x9, [x1, #GenGC_HDRL3]
	ldr x10, [sp, #4] // load requested size into $t2
	sub x12, x9, x12 // find reserve/work area barrier
	lsr x12, x12, #1
	ldr x11, =0xfffffffc
	and x12, x12, x11
	sub x12, x9, x12 // reserve/work barrier
	add x10, x12, x10 // test allocation
	cmp x10, x9
	b.ge _GenGC_Collect_major // check if work area too small
_GenGC_Collect_nomajor:
 	ldr x9, [x1, #GenGC_HDRL2]
	str x9, [x1, #GenGC_HDRL1] // expand old area
	str x12, [x1, #GenGC_HDRL2] // set new reserve/work barrier
	mov x27, x12 // set $gp
	ldr x26, [x1, #GenGC_HDRL3] // load limit into $s7
	b _GenGC_Collect_done
_GenGC_Collect_major:
	ldr x0, =_GenGC_Major // print collection message
	bl puts
	ldr x0, [sp, #8] // restore stack end
	bl _GenGC_MajorC // major collection
	adr x1, heap_start
	ldr x9, [x1, #GenGC_HDRMAJOR1]
	add x9, x9, x0
	lsr x9, x9, #1
	str x9, [x1, #GenGC_HDRMAJOR1] // update histories
	str x0, [x1, #GenGC_HDRMAJOR0]
	ldr x9, [x1, #GenGC_HDRL3] // find ratio of the old area
	ldr x12, [x1, #GenGC_HDRL0]
	sub x9, x9, x12
	lsr x9, x9, #GenGC_OLDRATIO
	add x9, x12, x9
	ldr x12, [x1, #GenGC_HDRL1]
	sub x12, x12, x9
	lsl x12, x12, #GenGC_OLDRATIO // amount to expand in $t0
	ldr x9, [x1, #GenGC_HDRL3] // load L3
	ldr x10, [x1, #GenGC_HDRL1] // load L1
	sub x10, x9, x10
	lsr x10, x10, #1
	ldr x11, =0xfffffffc
	and x10, x10, x11
	sub x9, x9, x10 // reserve/work barrier
	ldr x10, [sp, #4] // restore size
	add x9, x9, x10
	ldr x10, [x1, #GenGC_HDRL3] // load L3
	sub x9, x9, x10 // test allocation
	add x9, x9, #4 // adjust for round off errors
	lsl x9, x9, #1 // need to allocate $t1 memory
	cmp x9, x12
	b.lt _GenGC_Collect_enough // put max of $t0, $t1 in $t0
	mov x12, x9
_GenGC_Collect_enough:
	cmp x12, xzr
	b.le _GenGC_Collect_setL2 // no need to expand
	add x9, xzr, #1 // put 1 in $t1
	lsl x9, x9, #GenGC_HEAPEXPGRAN // get granularity of expansion
	add x9, x9, #-1 // align to granularity
	add x12, x12, x9
	mvn x9, x9
	and x12, x12, x9 // total memory needed
	ldr x9, [x1, #GenGC_HDRL3] // load L3
	ldr x9, [x1, #GenGC_HDRL4] // load L4
	sub x9, x10, x9
	sub x10, x12, x9 // actual amount to allocate
	cmp x10, xzr
	b.gt _GenGC_Collect_getmem // check if really need to allocate
_GenGC_Collect_xfermem:
	ldr x26, [x1, #GenGC_HDRL3] // load L3
	add x26, x26, x12 // expand by $t0, set $s7
	str x26, [x1, #GenGC_HDRL3] // save L3
	b _GenGC_Collect_findL2
_GenGC_Collect_getmem:
	mov x0, x10 // set the size to expand the heap
	bl sbrk
	mov x0, #0 // get new end of heap in $v0
	bl sbrk
	str x0, [x1, #GenGC_HDRL4] // save L4
	str x0, [x1, #GenGC_HDRL3] // save L3
	mov x26, x0 // set $s7
	b _GenGC_Collect_findL2
_GenGC_Collect_setL2:
	ldr x26, [x1, #GenGC_HDRL3] // load L3
_GenGC_Collect_findL2:
	ldr x9, [x1, #GenGC_HDRL1] // load L1
	sub x9, x26, x9
	lsr x9, x9, #1
	ldr x12, =0xfffffffc
	and x9, x9, x12
	sub x27, x26, x9 // reserve/work barrier
	str x27, [x1, #GenGC_HDRL2] // save L2
_GenGC_Collect_done:
// Clear new generation to catch missing pointers
	mov x12, x27
_GenGC_Clear_loop:
	str xzr, [x12, #0]
 	add x12, x12, #4
	cmp x12, x26
	b.lt _GenGC_Clear_loop
	
	str x1, [sp, #4] // restore size
	str x30, [sp, #12] // restore return address
 	add sp, sp, #24
	ret // return

#
# Check and Copy an Object
#
#   Checks that the input pointer points to an object is a heap
#   object.  If so, it then checks for a forwarding pointer by
#   checking for an object size of 0.  If found, the forwarding
#   pointer is returned.  If not found, the object is copied to $gp
#   and a pointer to it is returned.  The following tests are done to
#   determine if the object is a heap object:
#
#     1) The pointer is within the specified limits
#     2) The pointer is even
#     3) The word before the pointer is the eye catcher 0xFFFF FFFF
#     4) The word at the pointer is a valid tag (i.e. not equal to
#        0xFFFF FFFF)
#
#   INPUT:
#	$a0: pointer to check and copy
#	$a1: lower bound object should be within.
#	$a2: upper bound object should be within.
#	$gp: current allocation pointer
#
#   OUTPUT:
#	$a0: if input points to a heap object then it is set to the
#            new location of object.  If not, it is unchanged.
#	$a1: lower bound object should be within. (unchanged)
#	$a2: upper bound object should be within. (unchanged)
#
#   Registers modified:
#	$t0, $t1, $t2, $v0, $a0, $gp
#

	.globl _GenGC_ChkCopy
_GenGC_ChkCopy:
 	cmp x0, x1
	b.lt _GenGC_ChkCopy_done // check bounds
	cmp x0, x2
	b.ge _GenGC_ChkCopy_done
	and x10, x0, #1 // check if odd
	cmp x10, xzr
	b.ne _GenGC_ChkCopy_done
	add x10, xzr, #-1
	ldr x9, [x0, #obj_eyecatch] // check eyecatcher
	cmp x10, x9
	b.ne _gc_abort
	ldr x9, [x0, #obj_tag] // check object tag
	cmp x10, x9
	b.eq _GenGC_ChkCopy_done
	ldr x9, [x0, #obj_size] // get size of object
	cmp  x9, xzr
	b.eq _GenGC_ChkCopy_forward // if size = 0, get forwarding pointer
	mov x12, x0 // save pointer to old object in $t0
	add x27, x27, #4 // allocate memory for eyecatcher
	mov x0, x27 // get address of new object
	str x10, [x0, #obj_eyecatch] // save eye catcher
	lsl x9, x9, #2 // convert words to bytes
	add x9, x12, x9 // set $t1 to limit of copy
	mov x10, x12 // set $t2 to old object
_GenGC_ChkCopy_loop:
	ldr x6, [x12, #0] // copy
	str x6, [x27, #0]
	add x12, x12, #4 // update each index
	add x27, x27, #4
 	cmp x12, x9
	b.ne _GenGC_ChkCopy_loop // check for limit of copy
 	str xzr, [x10, #obj_size] // set size to 0
	str x0, [x10, #obj_disp] // save forwarding pointer
_GenGC_ChkCopy_done:
	ret // return
_GenGC_ChkCopy_forward:
	ldr x0, [x0, #obj_disp] // get forwarding pointer
 	ret // return


#
# Minor Garbage Collection
#
#   This garbage collector is run when ever the space in the work
#   area is used up by objects and the assignment table.  The live
#   objects are found and copied to the reserve area.  The L2 pointer
#   is then set to the end of the live objects.  The collector consists
#   of six phases:
#
#     1) Set $gp into the reserve area and set the inputs for ChkCopy
#
#     2) Scan the stack for root pointers into the heap.  The beginning
#        of the stack is in the header and the end is an input to this
#        function.  Look for the appropriate stack flags and act
#        accordingly.  Use "_GenGC_ChkCopy" to validate the pointer and
#        get the new pointer, and then update the stack entry.
#
#     3) Check the registers specified in the Register (REG) mask to
#        automatically update.  This mask is stored in the header.  If
#        bit #n in the mask is set, register #n will be passed to
#        "_GenGC_ChkCopy" and updated with its result.  "_GenGC_SetRegMask"
#        can be used to update this mask.
#
#     4) The assignemnt table is now checked.  $s7 is moved from its
#        current position until it hits the L3 pointer.  Each entry is a
#        pointer to the pointer that must be checked.  Again,
#        "_GenGC_ChkCopy" is used and the pointer updated.
#
#     5) At this point, all root objects are in the reserve area.  This
#        area is now traversed object by object (from L1 to $gp).  It
#        results in a breadth first search of the live objects collected.
#        All attributes of objects are treated as pointers except the
#        "Int", "Bool", and "String" objects.  The first two are skipped
#        completely, and the first attribute of the string object is
#        analyzed (should be a pointer to an "Int" object).
#
#     6) At this point, L2 is set to the end of the live objects in the
#        reserve area.  This is in preparation for a major collection.
#        The size of all the live objects collected is then computed and
#        returned.
#
#   INPUT:
#	$a0: end of stack
#	$s7: limit pointer of this area of storage
#	$gp: current allocation pointer
#	heap_start: start of heap
#
#   OUTPUT:
#	$a0: size of all live objects collected
#
#   Registers modified:
#	$t0, $t1, $t2, $t3, $t4, $v0, $v1, $a0, $a1, $a2, $gp, $s7
#
// $16 ~ $22 $24 $25 $30 $31
// $s0 ~ $s6 $t8 $t9 $fp $ra
 	.globl _GenGC_MinorC
_GenGC_MinorC:
	add sp, sp, #-40
 	str x30, [sp, #20] // save return address
	adr x12, heap_start
	ldr x1, [x12, #GenGC_HDRL2] // set lower bound to work area
	mov x2, x26 // set upper bound for ChkCopy
	ldr x27, [x12, #GenGC_HDRL1] // set $gp into reserve area
	str x0, [sp, #16] // save stack end
	ldr x12, [x12, #GenGC_HDRSTK] // set $t0 to stack start
	mov x9, x0 // set $t1 to stack end
	cmp x12, x9
	b.le _GenGC_MinorC_stackend // check for empty stack
_GenGC_MinorC_stackloop: // $t1 stack end, $t0 index
 	add x12, x12, #-4 // update index
	str x12, [sp, #12] // save stack index
	ldr x0, [x12, #4] // get stack item
	bl _GenGC_ChkCopy // check and copy
	ldr x12, [sp, #12] // load stack index
	str x0, [x12, #4]
	ldr x9, [sp, #16] // restore stack end
	cmp x12, x9 // loop
	b.gt _GenGC_MinorC_stackloop
_GenGC_MinorC_stackend:
 	adr x12, heap_start
	ldr x12, [x12, #GenGC_HDRREG] // get Register mask
	str x12, [sp, #16] // save Register mask
_GenGC_MinorC_reg16:
 	lsr x12, x12, #16 // shift to proper bit
	add x9, x12, #1
	cmp x9, xzr
	b.eq _GenGC_MinorC_reg17 // check if set
	mov x0, x19 // set test pointer
	bl _GenGC_ChkCopy
	mov x19, x0 // update register
_GenGC_MinorC_reg17:
	ldr x12, [sp, #16] // restore mask
 	lsr x12, x12, #17 // shift to proper bit
	add x9, x12, #1
	cmp x9, xzr
	b.eq _GenGC_MinorC_reg18 // check if set
	mov x0, x20 // set test pointer
	bl _GenGC_ChkCopy // check and copy
	mov x20, x0 // update register
	//lw	$t0 16($sp)			# restore mask
 	//srl	$t0 $t0 17			# shift to proper bit
 	//andi	$t1 $t0 1
 	//beq	$t1 $0 _GenGC_MinorC_reg18	# check if set
 	//move	$a0 $17				# set test pointer
 	//jal	_GenGC_ChkCopy			# check and copy
 	//move	$17 $a0				# update register
_GenGC_MinorC_reg18:
 	ldr x12, [sp, #16] // restore mask
 	lsr x12, x12, #18 // shift to proper bit
	add x9, x12, #1
	cmp x9, xzr
	b.eq _GenGC_MinorC_reg19 // check if set
	mov x0, x21 // set test pointer
	bl _GenGC_ChkCopy // check and copy
	mov x21, x0 // update register
	//lw	$t0 16($sp)			# restore mask
 	//srl	$t0 $t0 18			# shift to proper bit
 	//andi	$t1 $t0 1
 	//beq	$t1 $0 _GenGC_MinorC_reg19	# check if set
 	//move	$a0 $18				# set test pointer
 	//jal	_GenGC_ChkCopy			# check and copy
 	//move	$18 $a0				# update register
_GenGC_MinorC_reg19:
 	ldr x12, [sp, #16] // restore mask
 	lsr x12, x12, #19 // shift to proper bit
	add x9, x12, #1
	cmp x9, xzr
	b.eq _GenGC_MinorC_reg20 // check if set
	mov x0, x22 // set test pointer
	bl _GenGC_ChkCopy // check and copy
	mov x22, x0 // update register
	//lw	$t0 16($sp)			# restore mask
 	//srl	$t0 $t0 19			# shift to proper bit
 	//andi	$t1 $t0 1
 	//beq	$t1 $0 _GenGC_MinorC_reg20	# check if set
 	//move	$a0 $19				# set test pointer
 	//jal	_GenGC_ChkCopy			# check and copy
 	//move	$19 $a0				# update register
_GenGC_MinorC_reg20:
 	ldr x12, [sp, #16] // restore mask
 	lsr x12, x12, #20 // shift to proper bit
	add x9, x12, #1
	cmp x9, xzr
	b.eq _GenGC_MinorC_reg21 // check if set
	mov x0, x23 // set test pointer
	bl _GenGC_ChkCopy // check and copy
	mov x23, x0 // update register
	//lw	$t0 16($sp)			# restore mask
 	//srl	$t0 $t0 20			# shift to proper bit
 	//andi	$t1 $t0 1
 	//beq	$t1 $0 _GenGC_MinorC_reg21	# check if set
 	//move	$a0 $20				# set test pointer
 	//jal	_GenGC_ChkCopy			# check and copy
 	//move	$20 $a0				# update register
_GenGC_MinorC_reg21:
 	ldr x12, [sp, #16] // restore mask
 	lsr x12, x12, #21 // shift to proper bit
	add x9, x12, #1
	cmp x9, xzr
	b.eq _GenGC_MinorC_reg22 // check if set
	mov x0, x24 // set test pointer
	bl _GenGC_ChkCopy // check and copy
	mov x24, x0 // update register
	//lw	$t0 16($sp)			# restore mask
 	//srl	$t0 $t0 21			# shift to proper bit
 	//andi	$t1 $t0 1
 	//beq	$t1 $0 _GenGC_MinorC_reg22	# check if set
 	//move	$a0 $21				# set test pointer
 	//jal	_GenGC_ChkCopy			# check and copy
 	//move	$21 $a0				# update register
_GenGC_MinorC_reg22:
 	ldr x12, [sp, #16] // restore mask
 	lsr x12, x12, #22 // shift to proper bit
	add x9, x12, #1
	cmp x9, xzr
	b.eq _GenGC_MinorC_reg24 // check if set
	mov x0, x25 // set test pointer
	bl _GenGC_ChkCopy // check and copy
	mov x25, x0 // update register
	//lw	$t0 16($sp)			# restore mask
 	//srl	$t0 $t0 22			# shift to proper bit
 	//andi	$t1 $t0 1
 	//beq	$t1 $0 _GenGC_MinorC_reg24	# check if set
 	//move	$a0 $22				# set test pointer
 	//jal	_GenGC_ChkCopy			# check and copy
 	//move	$22 $a0				# update register
_GenGC_MinorC_reg24:
    ldr x12, [sp, #16] // restore mask
 	lsr x12, x12, #24 // shift to proper bit
	add x9, x12, #1
	cmp x9, xzr
	b.eq _GenGC_MinorC_reg25 // check if set
	mov x0, x14 // set test pointer
	bl _GenGC_ChkCopy // check and copy
	mov x14, x0 // update register
 	//lw	$t0 16($sp)			# restore mask
 	//srl	$t0 $t0 24			# shift to proper bit
 	//andi	$t1 $t0 1
 	//beq	$t1 $0 _GenGC_MinorC_reg25	# check if set
 	//move	$a0 $24				# set test pointer
 	//jal	_GenGC_ChkCopy			# check and copy
 	//move	$24 $a0				# update register
_GenGC_MinorC_reg25:
    ldr x12, [sp, #16] // restore mask
 	lsr x12, x12, #25 // shift to proper bit
	add x9, x12, #1
	cmp x9, xzr
	b.eq _GenGC_MinorC_reg30 // check if set
	mov x0, x15 // set test pointer
	bl _GenGC_ChkCopy // check and copy
	mov x15, x0 // update register
 	//lw	$t0 16($sp)			# restore mask
 	//srl	$t0 $t0 25			# shift to proper bit
 	//andi	$t1 $t0 1
 	//beq	$t1 $0 _GenGC_MinorC_reg30	# check if set
 	//move	$a0 $25				# set test pointer
 	//jal	_GenGC_ChkCopy			# check and copy
 	//move	$25 $a0				# update register
_GenGC_MinorC_reg30:
 	ldr x12, [sp, #16] // restore mask
 	lsr x12, x12, #30 // shift to proper bit
	add x9, x12, #1
	cmp x9, xzr
	b.eq _GenGC_MinorC_reg31 // check if set
	mov x0, x29 // set test pointer
	bl _GenGC_ChkCopy // check and copy
	mov x29, x0 // update register
	//lw	$t0 16($sp)			# restore mask
 	//srl	$t0 $t0 30			# shift to proper bit
 	//andi	$t1 $t0 1
 	//beq	$t1 $0 _GenGC_MinorC_reg31	# check if set
 	//move	$a0 $30				# set test pointer
 	//jal	_GenGC_ChkCopy			# check and copy
 	//move	$30 $a0				# update register
_GenGC_MinorC_reg31:
 	ldr x12, [sp, #16] // restore mask
 	lsr x12, x12, #31 // shift to proper bit
	add x9, x12, #1
	cmp x9, xzr
	b.eq _GenGC_MinorC_regend // check if set
	mov x0, x30 // set test pointer
	bl _GenGC_ChkCopy // check and copy
	mov x30, x0 // update register
	//lw	$t0 16($sp)			# restore mask
 	//srl	$t0 $t0 31			# shift to proper bit
 	//andi	$t1 $t0 1
 	//beq	$t1 $0 _GenGC_MinorC_regend	# check if set
 	//move	$a0 $31				# set test pointer
 	//jal	_GenGC_ChkCopy			# check and copy
 	//move	$31 $a0				# update register
_GenGC_MinorC_regend:
	adr x12, heap_start
	ldr x11, [x12, #GenGC_HDRL0] // lower limit of old area
 	ldr x13, [x12, #GenGC_HDRL1] // upper limit of old area
	ldr x12, [x12, #GenGC_HDRL3] // get L3
	str x12, [sp, #16] // save index limit
	cmp x26, x12
	b.ge _GenGC_MinorC_assnend // check for no assignments
	//la	$t0 heap_start
 	//lw	$t3 GenGC_HDRL0($t0)		# lower limit of old area
 	//lw	$t4 GenGC_HDRL1($t0)		# upper limit of old area
 	//lw	$t0 GenGC_HDRL3($t0)		# get L3
 	//sw	$t0 16($sp)			# save index limit
 	//bge	$s7 $t0 _GenGC_MinorC_assnend	# check for no assignments
_GenGC_MinorC_assnloop:				# $s7 index, $t0 limit
 	ldr x0, [x26, #0] // get table entry
	cmp x0, x11
	b.lt _GenGC_MinorC_assnnext // must point into old area
	cmp x0, x13
	b.ge _GenGC_MinorC_assnnext
	ldr x0, [x0, #0] // get pointer to check
	bl _GenGC_ChkCopy // check and copy
	ldr x12, [x26, #0]
	str x0, [x12, #0] // update pointer
	ldr x12, [sp, #16] // restore index limit
	//lw	$a0 0($s7)			# get table entry
 	//blt	$a0 $t3 _GenGC_MinorC_assnnext	# must point into old area
 	//bge	$a0 $t4 _GenGC_MinorC_assnnext
 	//lw	$a0 0($a0)			# get pointer to check
 	//jal	_GenGC_ChkCopy			# check and copy
 	//lw	$t0 0($s7)
 	//sw	$a0 0($t0)			# update pointer
 	//lw	$t0 16($sp)			# restore index limit
_GenGC_MinorC_assnnext:
 	add x26, x26, #4 // update index
	cmp x26, x12
	b.lt _GenGC_MinorC_assnloop // loop
	//addiu	$s7 $s7 4			# update index
 	//blt	$s7 $t0 _GenGC_MinorC_assnloop	# loop
_GenGC_MinorC_assnend:
 	adr x12, heap_start
	ldr x12, [x12, #GenGC_HDRL1] // start of reserve area
	cmp x12, x27
	b.ge _GenGC_MinorC_heapend // check for no objects
	//la	$t0 heap_start
 	//lw	$t0 GenGC_HDRL1($t0)		# start of reserve area
 	//bge	$t0 $gp _GenGC_MinorC_heapend	# check for no objects
_GenGC_MinorC_heaploop:				# $t0: index, $gp: limit
 	add x12, x12, #4 // skip over eyecatcher
	add x9, xzr, #-1 // check for eyecatcher
	ldr x10, [x12, #obj_eyecatch]
	cmp x9, x10
	b.ne _GenGC_MinorC_error // eyecatcher not found
	ldr x0, [x12, #obj_size] // get object size
	lsl x0, x0, #2 // words to bytes
	ldr x9, [x12, #obj_tag] // get the object's tag
	ldr x10, =_int_tag
	ldr x10, [x10] // test for int object
	cmp x9, x10
	b.eq _GenGC_MinorC_int
	ldr x10, =_bool_tag
	ldr x10, [x10] // test for bool object 
	cmp x9, x10
	b.eq _GenGC_MinorC_bool
	ldr x10, =_string_tag
	ldr x10, [x10] // test for string object
	cmp x9, x10
	b.eq _GenGC_MinorC_string
	//addiu	$t0 $t0 4			# skip over eyecatcher
 	//addiu	$t1 $0 -1			# check for eyecatcher
 	//lw	$t2 obj_eyecatch($t0)
 	//bne	$t1 $t2 _GenGC_MinorC_error	# eyecatcher not found
 	//lw	$a0 obj_size($t0)		# get object size
 	//sll	$a0 $a0 2			# words to bytes
 	//lw	$t1 obj_tag($t0)		# get the object's tag
 	//lw	$t2 _int_tag			# test for int object
 	//beq	$t1 $t2 _GenGC_MinorC_int
 	//lw	$t2 _bool_tag			# test for bool object
 	//beq	$t1 $t2 _GenGC_MinorC_bool
 	//lw	$t2 _string_tag			# test for string object
 	//beq	$t1 $t2 _GenGC_MinorC_string
_GenGC_MinorC_other:
 	add x9, x12, #obj_attr // start at first attribute
	add x10, x12, x0 // limit of attributes
	cmp x9, x10
	b.ge _GenGC_MinorC_nextobj  //check for no attributes
	str x12, [sp, #16] // save pointer to object
	str x0, [sp, #12] // save object size
	str x10, [sp, #4] // save limit
	//addi	$t1 $t0 obj_attr		# start at first attribute
 	//add	$t2 $t0 $a0			# limit of attributes
 	//bge	$t1 $t2 _GenGC_MinorC_nextobj	# check for no attributes
 	//sw	$t0 16($sp)			# save pointer to object
 	//sw	$a0 12($sp)			# save object size
 	//sw	$t2 4($sp)			# save limit
_GenGC_MinorC_objloop:				# $t1: index, $t2: limit
 	str x9, [sp, #8] // save index
	ldr x9, [x9, #0] // set pointer to check
	bl _GenGC_ChkCopy // check and copy
	ldr x9, [sp, #8] // restore index
	str x0, [x9, #0] // update object pointer
	ldr x10, [sp, #4] // restore limit
	add x9, x9, #4
	cmp x9, x10
	b.lt _GenGC_MinorC_objloop // loop
	//sw	$t1 8($sp)			# save index
 	//lw	$a0 0($t1)			# set pointer to check
 	//jal	_GenGC_ChkCopy			# check and copy
 	//lw	$t1 8($sp)			# restore index
 	//sw	$a0 0($t1)			# update object pointer
 	//lw	$t2 4($sp)			# restore limit
 	//addiu	$t1 $t1 4
 	//blt	$t1 $t2 _GenGC_MinorC_objloop	# loop
_GenGC_MinorC_objend:
 	ldr x12, [sp, #16] // restore pointer to object
	ldr x0, [sp, #12] // restore object size
	b _GenGC_MinorC_nextobj // next object
	//lw	$t0 16($sp)			# restore pointer to object
 	//lw	$a0 12($sp)			# restore object size
 	//b	_GenGC_MinorC_nextobj		# next object
_GenGC_MinorC_string:
 	str x12, [sp, #16] // save pointer to object
	str x0, [sp, #12] // save object size
	ldr x0, [x12, #str_size]
	bl _GenGC_ChkCopy // check and copy
	ldr x12, [sp, #16] // restore pointer to object
	str x0, [x12, #str_size] // update size pointer
	ldr x0, [sp, #12] // restore object size
	//sw	$t0 16($sp)			# save pointer to object
 	//sw	$a0 12($sp)			# save object size
 	//lw	$a0 str_size($t0)		# set test pointer
 	//jal	_GenGC_ChkCopy			# check and copy
 	//lw	$t0 16($sp)			# restore pointer to object
 	//sw	$a0 str_size($t0)		# update size pointer
 	//lw	$a0 12($sp)			# restore object size
_GenGC_MinorC_int:
_GenGC_MinorC_bool:
_GenGC_MinorC_nextobj:
 	add x12, x12, x0 // find next object
	cmp x12, x27 // loop
	b.lt _GenGC_MinorC_heaploop
	//add	$t0 $t0 $a0			# find next object
 	//blt	$t0 $gp _GenGC_MinorC_heaploop	# loop
_GenGC_MinorC_heapend:
 	adr x12, heap_start
	str x27, [x12, #GenGC_HDRL2] // set L2 to $gp
	ldr x0, [x12, #GenGC_HDRL1]
	sub x0, x27, x0 // find size after collection
	ldr x30, [sp, #20] // restore return address
	add sp, sp, #40
	ret // return
	//la	$t0 heap_start
 	//sw	$gp GenGC_HDRL2($t0)		# set L2 to $gp
 	//lw	$a0 GenGC_HDRL1($t0)
 	//sub	$a0 $gp $a0			# find size after collection
 	//lw	$ra 20($sp)			# restore return address
 	//addiu	$sp $sp 20
 	//jr	$ra				# return
_GenGC_MinorC_error:
 	ldr x0, =_GenGC_MINORERROR // show error message
    bl puts
	mov x0, #1
	bl exit // exit(1)
	//la	$a0 _GenGC_MINORERROR		# show error message
 	//li	$v0 4
 	//syscall
 	//li	$v0 10				# exit
 	//syscall

#
# Check and Copy an Object with an Offset
#
#   Checks that the input pointer points to an object is a heap object.
#   If so, the pointer is checked to be in one of two areas.  If the
#   pointer is in the X area, L0-L1 is added to the pointer, and the
#   new pointer is returned.  If the pointer points within the old area,
#   it then checks for a forwarding pointer by checking for an object
#   size of 0.  If found, the forwarding pointer is returned.  If not
#   found, the heap is then analyzed to make sure the object can be
#   copied.  It then expands the heap if necessary (updating only $s7),
#   and the copies the object to the $gp pointer.  It takes the new
#   pointer, adds L0-L1 to it, then saves this modified new pointer in
#   the forwarding (obj_disp) field and sets the flag (obj_size to 0).
#   Finally, it returns this pointer.  Note that this pointer does not
#   actually point to the object at this time.  This entire area will
#   later be block copied.  After that, this pointer will be valid.
#   The same tests are done here as in "_GenGC_ChkCopy" to verify that
#   this is a heap object.
#
#   INPUT:
#	$a0: pointer to check and copy with an offset
#	$a1: L0 pointer
#	$a2: L1 pointer
#	$v1: L2 pointer
#	$gp: current allocation pointer
#	$s7: L4 pointer
#
#   OUTPUT:
#	$a0: if input points to a heap object then it is set to the
#            new location of object.  If not, it is unchanged.
#	$a1: L0 pointer (unchanged)
#	$a2: L1 pointer (unchanged)
#	$v1: L2 pointer (unchanged)
#
#   Registers modified:
#	$t0, $t1, $t2, $v0, $a0, $gp, $s7
#

 	.globl _GenGC_OfsCopy
_GenGC_OfsCopy:
 	blt	$a0 $a1 _GenGC_OfsCopy_done	# check lower bound
 	bge	$a0 $v1 _GenGC_OfsCopy_done	# check upper bound
 	andi	$t2 $a0 1			# check if odd
 	bnez	$t2 _GenGC_OfsCopy_done
 	addiu	$t2 $0 -1
 	lw	$t1 obj_eyecatch($a0)		# check eyecatcher
 	bne	$t2 $t1 _gc_abort
 	lw	$t1 obj_tag($a0)		# check object tag
 	beq	$t2 $t1 _GenGC_OfsCopy_done
 	blt	$a0 $a2 _GenGC_OfsCopy_old	# check if old, X object
 	sub	$v0 $a1 $a2			# compute offset
 	add	$a0 $a0 $v0			# apply pointer offset
 	jr	$ra				# return
_GenGC_OfsCopy_old:
 	lw	$t1 obj_size($a0)		# get size of object
 	sll	$t1 $t1 2			# convert words to bytes
 	beqz	$t1 _GenGC_OfsCopy_forward	# if size = 0, get forwarding pointer
 	move	$t0 $a0				# save pointer to old object in $t0
 	addu	$v0 $gp $t1			# test allocation
 	addiu	$v0 $v0 4
 	blt	$v0 $s7 _GenGC_OfsCopy_memok	# check if enoguh room for object
 	sub	$a0 $v0 $s7			# amount to expand minus 1
 	addiu	$v0 $0 1
 	sll	$v0 $v0 GenGC_HEAPEXPGRAN
 	add	$a0 $a0 $v0
 	addiu	$v0 $v0 -1
 	nor	$v0 $v0 $v0			# get grain mask
 	and	$a0 $a0 $v0			# align to grain size
 	li	$v0 9
 	syscall					# expand heap
 	li	$v0 9
 	move	$a0 $0
 	syscall					# get end of heap in $v0
 	move	$s7 $v0				# save heap end in $s7
 	move	$a0 $t0				# restore pointer to old object in $a0
_GenGC_OfsCopy_memok:
 	addiu	$gp $gp 4			# allocate memory for eyecatcher
 	move	$a0 $gp				# get address of new object
 	sw	$t2 obj_eyecatch($a0)		# save eye catcher
 	addu	$t1 $t0 $t1			# set $t1 to limit of copy
 	move	$t2 $t0				# set $t2 to old object
_GenGC_OfsCopy_loop:
 	lw	$v0 0($t0)			# copy
 	sw	$v0 0($gp)
 	addiu	$t0 $t0 4			# update each index
 	addiu	$gp $gp 4
 	bne	$t0 $t1 _GenGC_OfsCopy_loop	# check for limit of copy
 	sw	$0 obj_size($t2)		# set size to 0
 	sub	$v0 $a1 $a2			# compute offset
 	add	$a0 $a0 $v0			# apply pointer offset
 	sw	$a0 obj_disp($t2)		# save forwarding pointer
_GenGC_OfsCopy_done:
 	jr	$ra				# return
_GenGC_OfsCopy_forward:
 	lw	$a0 obj_disp($a0)		# get forwarding pointer
 	jr	$ra				# return

#
# Major Garbage Collection
#
#   This collection occurs when ever the old area grows beyond a specified
#   point.  The minor collector sets up the Old, X, and New areas for
#   this collector.  It then collects all the live objects in the old
#   area (L0 to L1) into the new area (L2 to L3).  This collection consists
#   of five phases:
#
#     1) Set $gp into the new area (L2), and $s7 to L4.  Also set the
#        inputs for "_GenGC_OfsCopy".
#
#     2) Traverse the stack (see the minor collector) using "_GenGC_OfsCopy".
#
#     3) Check the registers (see the minor collector) using "_GenGC_OfsCopy".
#
#     4) Traverse the heap from L1 to $gp using "_GenGC_OfsCopy".  Note
#        that this includes the X area.  (see the minor collector)
#
#     5) Block copy the region L1 to $gp back L1-L0 bytes to create the
#        next old area.  Save the end in L1.  Calculate the size of the
#        live objects collected from the old area and return this value.
#
#   Note that the pointers returned by "_GenGC_OfsCopy" are not valid
#   until the block copy is done.
#
#   INPUT:
#	$a0: end of stack
#	heap_start: start of heap
#
#   OUTPUT:
#	$a0: size of all live objects collected
#
#   Registers modified:
#	$t0, $t1, $t2, $v0, $v1, $a0, $a1, $a2, $gp, $s7
#

 	.globl _GenGC_MajorC
_GenGC_MajorC:
 	addiu	$sp $sp -20
 	sw	$ra 20($sp)			# save return address
 	la	$t0 heap_start
 	lw	$s7 GenGC_HDRL4($t0)		# limit pointer for collection
 	lw	$gp GenGC_HDRL2($t0)		# allocation pointer for collection
 	lw	$a1 GenGC_HDRL0($t0)		# set inputs for OfsCopy
 	lw	$a2 GenGC_HDRL1($t0)
 	lw	$v1 GenGC_HDRL2($t0)
 	sw	$a0 16($sp)			# save stack end
 	lw	$t0 GenGC_HDRSTK($t0)		# set $t0 to stack start
 	move	$t1 $a0				# set $t1 to stack end
 	ble	$t0 $t1 _GenGC_MajorC_stackend	# check for empty stack
_GenGC_MajorC_stackloop: 			# $t1 stack end, $t0 index
 	addiu	$t0 $t0 -4			# update index
 	sw	$t0 12($sp)			# save stack index
 	lw	$a0 4($t0)			# get stack item
 	jal	_GenGC_OfsCopy			# check and copy
 	lw	$t0 12($sp)			# load stack index
 	sw	$a0 4($t0)
 	lw	$t1 16($sp)			# restore stack end
 	bgt	$t0 $t1 _GenGC_MajorC_stackloop	# loop
_GenGC_MajorC_stackend:
 	la	$t0 heap_start
 	lw	$t0 GenGC_HDRREG($t0)		# get Register mask
 	sw	$t0 16($sp)			# save Register mask
_GenGC_MajorC_reg16:
	srl	$t0 $t0 16			# shift to proper bit
 	andi	$t1 $t0 1
 	beq	$t1 $0 _GenGC_MajorC_reg17	# check if set
 	move	$a0 $16				# set test pointer
 	jal	_GenGC_OfsCopy			# check and copy
 	move	$16 $a0				# update register
_GenGC_MajorC_reg17:
 	lw	$t0 16($sp)			# restore mask
 	srl	$t0 $t0 17			# shift to proper bit
 	andi	$t1 $t0 1
 	beq	$t1 $0 _GenGC_MajorC_reg18	# check if set
 	move	$a0 $17				# set test pointer
 	jal	_GenGC_OfsCopy			# check and copy
 	move	$17 $a0				# update register
_GenGC_MajorC_reg18:
 	lw	$t0 16($sp)			# restore mask
 	srl	$t0 $t0 18			# shift to proper bit
 	andi	$t1 $t0 1
 	beq	$t1 $0 _GenGC_MajorC_reg19	# check if set
 	move	$a0 $18				# set test pointer
 	jal	_GenGC_OfsCopy			# check and copy
 	move	$18 $a0				# update register
_GenGC_MajorC_reg19:
 	lw	$t0 16($sp)			# restore mask
 	srl	$t0 $t0 19			# shift to proper bit
 	andi	$t1 $t0 1
 	beq	$t1 $0 _GenGC_MajorC_reg20	# check if set
 	move	$a0 $19				# set test pointer
 	jal	_GenGC_OfsCopy			# check and copy
 	move	$19 $a0				# update register
_GenGC_MajorC_reg20:
 	lw	$t0 16($sp)			# restore mask
 	srl	$t0 $t0 20			# shift to proper bit
 	andi	$t1 $t0 1
 	beq	$t1 $0 _GenGC_MajorC_reg21	# check if set
 	move	$a0 $20				# set test pointer
 	jal	_GenGC_OfsCopy			# check and copy
 	move	$20 $a0				# update register
_GenGC_MajorC_reg21:
 	lw	$t0 16($sp)			# restore mask
 	srl	$t0 $t0 21			# shift to proper bit
 	andi	$t1 $t0 1
 	beq	$t1 $0 _GenGC_MajorC_reg22	# check if set
 	move	$a0 $21				# set test pointer
 	jal	_GenGC_OfsCopy			# check and copy
 	move	$21 $a0				# update register
_GenGC_MajorC_reg22:
 	lw	$t0 16($sp)			# restore mask
 	srl	$t0 $t0 22			# shift to proper bit
 	andi	$t1 $t0 1
 	beq	$t1 $0 _GenGC_MajorC_reg24	# check if set
 	move	$a0 $22				# set test pointer
 	jal	_GenGC_OfsCopy			# check and copy
 	move	$22 $a0				# update register
_GenGC_MajorC_reg24:
 	lw	$t0 16($sp)			# restore mask
 	srl	$t0 $t0 24			# shift to proper bit
 	andi	$t1 $t0 1
 	beq	$t1 $0 _GenGC_MajorC_reg25	# check if set
 	move	$a0 $24				# set test pointer
 	jal	_GenGC_OfsCopy			# check and copy
 	move	$24 $a0				# update register
_GenGC_MajorC_reg25:
 	lw	$t0 16($sp)			# restore mask
 	srl	$t0 $t0 25			# shift to proper bit
 	andi	$t1 $t0 1
 	beq	$t1 $0 _GenGC_MajorC_reg30	# check if set
 	move	$a0 $25				# set test pointer
 	jal	_GenGC_OfsCopy			# check and copy
 	move	$25 $a0				# update register
_GenGC_MajorC_reg30:
 	lw	$t0 16($sp)			# restore mask
 	srl	$t0 $t0 30			# shift to proper bit
 	andi	$t1 $t0 1
 	beq	$t1 $0 _GenGC_MajorC_reg31	# check if set
 	move	$a0 $30				# set test pointer
 	jal	_GenGC_OfsCopy			# check and copy
 	move	$30 $a0				# update register
_GenGC_MajorC_reg31:
 	lw	$t0 16($sp)			# restore mask
 	srl	$t0 $t0 31			# shift to proper bit
 	andi	$t1 $t0 1
 	beq	$t1 $0 _GenGC_MajorC_regend	# check if set
 	move	$a0 $31				# set test pointer
 	jal	_GenGC_OfsCopy			# check and copy
 	move	$31 $a0				# update register
_GenGC_MajorC_regend:
 	la	$t0 heap_start
 	lw	$t0 GenGC_HDRL1($t0)		# start of X area
 	bge	$t0 $gp _GenGC_MajorC_heapend	# check for no objects
_GenGC_MajorC_heaploop:				# $t0: index, $gp: limit
 	addiu	$t0 $t0 4			# skip over eyecatcher
 	addiu	$t1 $0 -1			# check for eyecatcher
 	lw	$t2 obj_eyecatch($t0)
 	bne	$t1 $t2 _GenGC_MajorC_error	# eyecatcher not found
 	lw	$a0 obj_size($t0)		# get object size
 	sll	$a0 $a0 2			# words to bytes
 	lw	$t1 obj_tag($t0)		# get the object's tag
 	lw	$t2 _int_tag			# test for int object
 	beq	$t1 $t2 _GenGC_MajorC_int
 	lw	$t2 _bool_tag			# test for bool object
 	beq	$t1 $t2 _GenGC_MajorC_bool
 	lw	$t2 _string_tag			# test for string object
 	beq	$t1 $t2 _GenGC_MajorC_string
_GenGC_MajorC_other:
 	addi	$t1 $t0 obj_attr		# start at first attribute
 	add	$t2 $t0 $a0			# limit of attributes
 	bge	$t1 $t2 _GenGC_MajorC_nextobj	# check for no attributes
 	sw	$t0 16($sp)			# save pointer to object
 	sw	$a0 12($sp)			# save object size
 	sw	$t2 4($sp)			# save limit
_GenGC_MajorC_objloop:				# $t1: index, $t2: limit
 	sw	$t1 8($sp)			# save index
 	lw	$a0 0($t1)			# set pointer to check
 	jal	_GenGC_OfsCopy			# check and copy
 	lw	$t1 8($sp)			# restore index
 	sw	$a0 0($t1)			# update object pointer
 	lw	$t2 4($sp)			# restore limit
 	addiu	$t1 $t1 4
 	blt	$t1 $t2 _GenGC_MajorC_objloop	# loop
_GenGC_MajorC_objend:
 	lw	$t0 16($sp)			# restore pointer to object
 	lw	$a0 12($sp)			# restore object size
 	b	_GenGC_MajorC_nextobj		# next object
_GenGC_MajorC_string:
 	sw	$t0 16($sp)			# save pointer to object
 	sw	$a0 12($sp)			# save object size
 	lw	$a0 str_size($t0)		# set test pointer
 	jal	_GenGC_OfsCopy			# check and copy
 	lw	$t0 16($sp)			# restore pointer to object
	sw	$a0 str_size($t0)		# update size pointer
 	lw	$a0 12($sp)			# restore object size
_GenGC_MajorC_int:
_GenGC_MajorC_bool:
_GenGC_MajorC_nextobj:
 	add	$t0 $t0 $a0			# find next object
 	blt	$t0 $gp _GenGC_MajorC_heaploop	# loop
_GenGC_MajorC_heapend:
 	la	$t0 heap_start
 	lw	$a0 GenGC_HDRL2($t0)		# get end of collection
 	sub	$a0 $gp $a0			# get length after collection
 	lw	$t1 GenGC_HDRL0($t0)		# get L0
 	lw	$t2 GenGC_HDRL1($t0)		# get L1
 	bge	$t2 $gp _GenGC_MajorC_bcpyend	# test for empty copy
_GenGC_MajorC_bcpyloop:				# $t2 index, $gp limit, $t1 dest
 	lw	$v0 0($t2)			# copy
 	sw	$v0 0($t1)
 	addiu	$t2 $t2 4			# update each index
 	addiu	$t1 $t1 4
 	bne	$t2 $gp _GenGC_MajorC_bcpyloop	# loop
_GenGC_MajorC_bcpyend:
 	sw	$s7 GenGC_HDRL4($t0)		# save end of heap
 	lw	$t1 GenGC_HDRL0($t0)		# get L0
 	lw	$t2 GenGC_HDRL1($t0)		# get L1
 	sub	$t1 $t2 $t1			# find offset of block copy
 	sub	$gp $gp $t1			# find end of old area
 	sw	$gp GenGC_HDRL1($t0)		# save end of old area
 	lw	$ra 20($sp)			# restore return address
 	addiu	$sp $sp 20
 	jr	$ra				# return
_GenGC_MajorC_error:
 	la	$a0 _GenGC_MAJORERROR		# show error message
 	li	$v0 4
 	syscall
 	li	$v0 10				# exit
 	syscall

#
# Set the Register (REG) mask
#
#   If bit #n is set in the Register mask, register #n will be
#   automatically updated by the garbage collector.  Note that
#   this mask is masked (ANDed) with the ARU mask.  Only those
#   registers in the ARU mask can be updated automatically.
#
#   INPUT:
#	$a0: new Register (REG) mask
#	heap_start: start of the heap
#
#   Registers modified:
#	$t0
#

# 	.globl	_GenGC_SetRegMask
# _GenGC_SetRegMask:
# 	li	$t0 GenGC_ARU_MASK		# apply Automatic Register Mask (ARU)
# 	and	$a0 $a0 $t0
# 	la	$t0 heap_start			# set $t0 to the start of the heap
# 	sw	$a0 GenGC_HDRREG($t0)		# save the Register mask
# 	jr	$ra				# return

#
# Query the Register (REG) mask
#
#   INPUT:
#	heap_start: start of the heap
#
#   OUTPUT:
#	$a0: current Register (REG) mask
#
#   Registers modified:
#	none
#

# 	.globl	_GenGC_QRegMask
# _GenGC_QRegMask:
# 	la	$a0 heap_start			# set $a0 to the start of the heap
# 	lw	$a0 GenGC_HDRREG($a0)		# get the Register mask
# 	jr	$ra				# return


#
# NoGC Garbage Collector
#
#   NoGC does not attempt to do any garbage collection.
#   It simply expands the heap if more memory is needed.
#

#
# Some constants
#

//NoGC_EXPANDSIZE=0x10000				# size to expand heap

#
# Initialization
#
#   INPUT:
#	none
#
#   OUTPUT:
#	$gp: lower bound of the work area
#	$s7: upper bound of the work area
#
#   Registers modified:
#	$a0, $v0
#
	.globl _NoGC_Init
_NoGC_Init:
	adr x27, heap_start // set $gp to the start of the heap
	mov x0, #0 // get heap end
    bl sbrk  // sbrk
	mov x26, x0 // set limit pointer
	ret

#
# Collection
#
#   Expand the heap as necessary.
#
#   INPUT:
#	$a1: size will need to allocate in bytes
#	$s7: limit pointer of thw work area
#	$gp: current allocation pointer
#
#   OUTPUT:
#	$a1: size will need to allocate in bytes (unchanged)
#
#   Registers modified:
#	$t0, $a0, $v0, $gp, $s7
#

	.globl _NoGC_Collect
_NoGC_Collect:
	la	$a0 _NoGC_COLLECT		# show collection message
	li	$v0 4
	syscall

_NoGC_Collect_loop:
	add	$t0 $gp $a1			# test allocation
	blt	$t0 $s7 _NoGC_Collect_ok	# stop if enough
	li	$v0 9				# expand heap
	li	$a0 NoGC_EXPANDSIZE		# set the size to expand the heap
	syscall					# sbrk
	li	$v0 9				# get heap end
	move	$a0 $zero
	syscall					# sbrk
	move	$s7 $v0				# set limit pointer
	b	_NoGC_Collect_loop		# loop
_NoGC_Collect_ok:
	jr	$ra				# return


