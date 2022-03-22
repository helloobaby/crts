; Listing generated by Microsoft (R) Optimizing Compiler Version 13.00.8982 

	TITLE	fpctrl.c

PUBLIC	_get_fpsr
_TEXT	SEGMENT
_status$ = 0
_get_fpsr PROC NEAR					; COMDAT

    sub     rsp, 8
	stmxcsr	DWORD PTR _status$[rsp]
	mov	    eax, DWORD PTR _status$[rsp]
    add     rsp, 8
	ret	0

_get_fpsr ENDP
_TEXT	ENDS


PUBLIC	_set_fpsr
_TEXT	SEGMENT
_status$ = 8
_set_fpsr	PROC NEAR					; COMDAT

    mov     DWORD PTR _status$[rsp], ecx
    ldmxcsr DWORD PTR _status$[rsp]
	ret	0

_set_fpsr	ENDP
_TEXT	ENDS

PUBLIC  _fclrf
_TEXT   SEGMENT
_fclrf PROC    NEAR
	stmxcsr	DWORD PTR _status$[rsp]
    mov     ecx, 0ffffffc0h
    and     DWORD PTR _status$[rsp], ecx
    ldmxcsr DWORD PTR _status$[rsp]
_fclrf ENDP
_TEXT   ENDS


PUBLIC	_frnd
_TEXT	SEGMENT
_frnd PROC NEAR
    CVTPD2DQ    xmm(1), xmm(0)
    CVTDQ2PD    xmm(0), xmm(1)
	ret	0
_frnd ENDP
_TEXT	ENDS
END
