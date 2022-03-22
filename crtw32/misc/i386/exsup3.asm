;***
;exsup3.asm
;
;	Copyright (c) 1994-2001, Microsoft Corporation. All rights reserved.
;
;Purpose:
;	Exception handling for i386.  This is just the C9.0 version of
;	the language-specific exception handler.  The C8.0 version is
;	found in exsup2.asm, and the routines common to both C8 and C9
;	are found in exsup.asm.
;
;Notes:
;
;Revision History:
;	01-10-94  PML	Create VC/C++ 2.0 (C9.0) version from C8.0 original
;	01-11-95  SKS	Remove MASM 5.X support
;	04-18-95  JWM	Added NLG support
;	06-07-95  JWM	__SetNLGCode() used, for multithread safety.
;	06-20-95  JWM	__SetNLGCode() removed, code passed on stack (11803).
;	03-09-01  PML   Add FPO directives for proper callstacks (vs7#221754)
;
;*******************************************************************************

;hnt = -D_WIN32 -Dsmall32 -Dflat32 -Mx $this;

;Define small32 and flat32 since these are not defined in the NT build process
small32 equ 1
flat32  equ 1

.xlist
include pversion.inc
?DFDATA =	1
?NODATA =	1
include cmacros.inc
include exsup.inc
.list

;REVIEW: can we get rid of _global_unwind2, and just use
; the C runtimes version, _global_unwind?

extrn __global_unwind2:near
extrn __local_unwind2:near
extrn __NLG_Notify:near

;typedef struct _EXCEPTION_REGISTRATION PEXCEPTION_REGISTRATION;
;struct _EXCEPTION_REGISTRATION{
;/* _esp, xpointers at negative offset */
;     int _esp;
;     PEXCEPTION_POINTERS xpointers;
;     struct _EXCEPTION_REGISTRATION *prev;
;     void (*handler)(PEXCEPTION_RECORD, PEXCEPTION_REGISTRATION, PCONTEXT, PEXCEPTION_RECORD);
;     struct scopetable_entry *scopetable;
;     int trylevel;
;};
;private (at negative offsets from node ptr)
    _esp		=	-8
    xpointers		=	-4
_C9_EXCEPTION_REGISTRATION struc	; C9.0 version
;public:
                        dd      ?	; prev (common)
                        dd      ?	; handler (common)
;private:
    scopetable          dd      ?
    trylevel            dd      ?
_C9_EXCEPTION_REGISTRATION ends
FRAME_EBP_OFFSET equ 16

;#define EXCEPTION_MAXIMUM_PARAMETERS 4
;typedef struct _EXCEPTION_RECORD EXCEPTION_RECORD;
;typedef EXCEPTION_RECORD *PEXCEPTION_RECORD;
;struct _EXCEPTION_RECORD{
;    NTSTATUS ExceptionCode;
;    ULONG ExceptionFlags;
;    struct _EXCEPTION_RECORD *ExceptionRecord;
;    PVOID ExceptionAddress;
;    ULONG NumberParameters;
;    ULONG ExceptionInformation[EXCEPTION_MAXIMUM_PARAMETERS];
;};
_EXCEPTION_RECORD struc
    exception_number    dd      ?
    exception_flags     dd      ?
    exception_record    dd      ?
    exception_address   dd      ?
    number_parameters   dd      ?
    exception_information dd 4 dup(?)
_EXCEPTION_RECORD ends
SIZEOF_EXCEPTION_RECORD equ 36

;/* following is the structure returned by the _exception_info() intrinsic. */
;typedef struct _EXCEPTION_POINTERS EXCEPTION_POINTERS;
;typedef struct EXCEPTION_POINTERS *PEXCEPTION_POINTERS;
;struct _EXCEPTION_POINTERS{
;    PEXCEPTION_RECORD ExceptionRecord;
;    PCONTEXT Context;
;};
_EXCEPTION_POINTERS struc
    ep_xrecord          dd      ?
    ep_context          dd      ?
_EXCEPTION_POINTERS ends
SIZEOF_EXCEPTION_POINTERS equ 8

extern __NLG_Destination:_NLG_INFO

assumes DS,DATA
assumes FS,DATA

__except_list equ 0

;struct _SCOPETABLE_ENTRY{
;     int enclosing_level;              /* lexical level of enclosing scope */
;     int (*filter)(PEXCEPTION_RECORD); /* NULL for a termination handler */
;     void (*specific_handler)(void);   /* xcpt or termination handler */
;};
;struct _SCOPETABLE_ENTRY Scopetable[NUMTRYS];
_SCOPETABLE_ENTRY struc
    enclosing_level     dd      ?
    filter              dd      ?
    specific_handler    dd      ?
_SCOPETABLE_ENTRY ends

BeginCODE

;EXTERN C __SetNLGCode:near

;/* _EXCEPT_HANDLER3 - Try to find an exception handler listed in the scope
; * table associated with the given registration record, that wants to accept
; * the current exception. If we find one, run it (and never return).
; * RETURNS: (*if* it returns)
; *  DISPOSITION_DISMISS - dismiss the exception.
; *  DISPOSITION_CONTINUE_SEARCH - pass the exception up to enclosing handlers
; */
;int _except_handler3(
;       PEXCEPTION_RECORD exception_record,
;       PEXCEPTION_REGISTRATION registration,
;       PCONTEXT context,
;       PEXCEPTION_REGISTRATION dispatcher)
;{
;    int ix, filter_result;
;
;    for(ix=registration->trylevel; ix!=-1; ix=registration->xscope[ix].enclosing_level){
;       /* if filter==NULL, then this is an entry for a termination handler */
;       if(registration->xscope[ix].filter){
;           /* NB: call to the filter may trash the callee save
;              registers. (this is *not* a standard cdecl function) */
;           filter_result=(*registration->xscope[ix].filter)(xinfo);
;           if(filter_result==FILTER_DISMISS)
;               return(-1); /* dismiss */
;           if(filter_result==FILTER_ACCEPT){
;               _global_unwind2(registration);
;               _local_unwind2(registration, ix);
;               (*registration->xscope[ix].specific_handler)(void);
;               assert(UNREACHED); /*should never return from handler*/
;           }
;           assert(filter_result==FILTER_CONTINUE_SEARCH);
;       }
;    }
;    return(0); /* didnt find one */
;}
	db	'VC20'	;; VC/C++ 2.0/32-bit (C9.0) version
	db	'XC00'	;; so debugger can recognize this proc (cuda:3936)

xrecord      equ [ebp+8]
registration equ [ebp+12]
context      equ [ebp+16]
dispatcher   equ [ebp+20]

xp           equ [ebp-8]


;_except_handlex是专门给x86的程序使用的
;由于线程起始都是在try-except之中，所以每个线程都会有RtlUnhandledExceptionFilter，而且他肯定是在SEH的最后
;一般来说用户的try-except的处理例程是_except_handlex，用户写下了try-except的结构之后，编译器会自动构建一个
;registration结构，根据这个结构就可以还原出try-except-finally。然后这个结构是运行到这片try代码的时候挂入异常
;链表，然后执行完后从链表中摘掉。
;在进入try的时候，除了执行我们的代码，一开始编译器还会为注册的registration.trylevel赋值，表示这片try出异常先找哪个
;scopetable，之后一层层的执行except就靠scopetable中的enclosing_level
;
;struct _EH4_SCOPETABLE {
;        DWORD GSCookieOffset;
;        DWORD GSCookieXOROffset;
;        DWORD EHCookieOffset;
;        DWORD EHCookieXOROffset;
;        _EH4_SCOPETABLE_RECORD ScopeRecord[1];
;};

;struct _EH4_SCOPETABLE_RECORD {
;        DWORD EnclosingLevel;
;        long (*FilterFunc)();
;            union {
;            void (*HandlerAddress)();
;            void (*FinallyFunc)(); 
;    };
;};
;
;TRYLEVEL_NONE           equ     -1
;TRYLEVEL_INVALID        equ     -2
;
;
;
_except_handler3 proc C ; RtlpExecuteHandlerForException? 
	; FPO = 0 dwords locals allocated in prolog
	;       3 dword parameters
	;       10 bytes in prolog
	;       7 registers saved (includes locals to work around debugger bug)
	;       1 EBP is used
	;       0 frame type = FPO
	.FPO    (0,3,10,7,1,0)
	push	ebp
	mov	ebp, esp
	sub	esp, 8
	push	ebx
	push	esi
	push	edi
	push	ebp

	;4*4b for callee saves + 4b return address + 4b param = 24

	;DF in indeterminate state at time of exception, so clear it
	cld

        mov     ebx, registration               ;ebx= PEXCEPTION_REGISTRATION
        mov     eax, xrecord

        test    [eax.exception_flags], EXCEPTION_UNWIND_CONTEXT
        jnz     _lh_unwinding

        ;build the EXCEPTION_POINTERS locally store its address in the
        ; registration record. this is the pointer that is returned by
        ; the _eception_info intrinsic.
        mov     xp.ep_xrecord, eax
        mov     eax, context
        mov     xp.ep_context, eax
        lea     eax, xp
        mov     [ebx.xpointers], eax

        mov     esi, [ebx.trylevel]             ;esi= try level
        mov     edi, [ebx.scopetable]           ;edi= scope table base
_lh_top:
        cmp     esi, TRYLEVEL_NONE
        je      short _lh_bagit
        lea     ecx, [esi+esi*2]                ;ecx= trylevel*3 因为_EH4_SCOPETABLE_RECORD含有三个4字节成员
        cmp     dword ptr [(edi+ecx*4).filter], 0 ; finally块filter为null
        je      short _lh_continue              ;从下标enclosing_level的scopetable继续找

        ;filter may trash *all* registers, so save ebp and scopetable offset
	;++也就是说windows会保证filter执行前后重要寄存器保持不变
        push    esi
        push    ebp

	lea	ebp, FRAME_EBP_OFFSET[ebx]
        call    [(edi+ecx*4).filter]            ;call the filter

        pop     ebp
        pop     esi
        ;--
        ;ebx may have been trashed by the filter, so we must reload
        mov     ebx, registration

	; Accept <0, 0, >0 instead of just -1, 0, +1
	or	eax, eax
	jz	short _lh_continue;filter返回FILTER_CONTINUE_SEARCH，遍历下一个scopetable
	js	short _lh_dismiss ;filter返回FILTER_DISMISS.说明异常被filter处理了，继续原来的地方执行
        ;assert(eax==FILTER_ACCEPT)

	;filter返回FILTER_ACCEPT，也就是让except块执行
        ;reload xscope base, cuz it was trashed by the filter call
        mov     edi, [ebx.scopetable]
        ;load handler address before we loose components of address mode
        push    ebx                             ;registration*
        call    __global_unwind2                ;run term handlers 全局展开
        add     esp, 4

        ;setup ebp for the local unwinder and the specific handler
	lea	ebp, FRAME_EBP_OFFSET[ebx]

        ;the stop try level == accepting except level
        push    esi                             ;stop try level
        push    ebx                             ;registration*
        call    __local_unwind2
        add     esp, 8
        lea     ecx, [esi+esi*2]                ;ecx=trylevel*3

        push    01h
        mov     eax, [(edi+ecx*4).specific_handler];
        call    __NLG_Notify

; set the current trylevel to our enclosing level immediately
; before giving control to the handler. it is the enclosing
; level, if any, that guards the handler.
        mov     eax, [(edi+ecx*4).enclosing_level]
        mov     [ebx.trylevel], eax
        call    [(edi+ecx*4).specific_handler]  ;call the except handler 也就是except块
        ;assert(0)                              ;(NB! should not return)

_lh_continue:
        ;reload the scope table base, possibly trashed by call to filter
        mov     edi, [ebx.scopetable]
        lea     ecx, [esi+esi*2]
        mov     esi, [edi+ecx*4+0]              ;load the enclosing trylevel
        jmp     short _lh_top

_lh_dismiss:
        mov     eax, DISPOSITION_DISMISS        ;dismiss the exception
        jmp     short _lh_return

_lh_bagit:
        mov     eax, DISPOSITION_CONTINUE_SEARCH
        jmp     short _lh_return

_lh_unwinding:
        push    ebp
	lea	ebp, FRAME_EBP_OFFSET[ebx]
        push    -1
        push    ebx
        call    __local_unwind2
        add     esp, 8
        pop     ebp
        ;the return value is not really relevent in an unwind context
        mov     eax, DISPOSITION_CONTINUE_SEARCH

_lh_return:
	pop	ebp
	pop	edi
	pop	esi
	pop	ebx
	mov	esp,ebp
	pop	ebp
	ret
_except_handler3 endp

public __seh_longjmp_unwind@4
__seh_longjmp_unwind@4 proc near
	push	ebp
	mov	ecx, 8[esp]
	mov	ebp, [ecx.saved_ebp]
	mov	eax, [ecx.saved_trylevel]
	push	eax
	mov	eax, [ecx.saved_xregistration]
	push	eax
	call	__local_unwind2
	add	esp, 8
	pop	ebp
	ret	4
__seh_longjmp_unwind@4 endp

EndCODE
END
