	page	,132
	title	strrev - reverse a string in place
;***
;strrev.asm - reverse a string in place
;
;	Copyright (c) 1985-2001, Microsoft Corporation. All rights reserved.
;
;Purpose:
;	defines _strrev() - reverse a string in place (not including
;	'\0' character)
;
;Revision History:
;	10-26-83  RN	initial version
;	07-16-87  JCR	Added check for empty string (fixes large model bug)
;	05-18-88  SJM	Add model-independent (large model) ifdef
;	08-04-88  SJM	convert to cruntime/ add 32-bit support
;	08-23-88  JCR	386 cleanup, minor alterations
;	10-26-88  JCR	General cleanup for 386-only code
;	10-26-88  JCR	Re-arrange regs to avoid push/pop ebx
;	03-26-90  GJF	Changed to _stdcall. Also, fixed the copyright.
;	01-18-91  GJF	ANSI naming.
;	05-10-91  GJF	Back to _cdecl, sigh...
;
;*******************************************************************************

	.xlist
	include cruntime.inc
	.list

page
;***
;char *_strrev(string) - reverse a string in place
;
;Purpose:
;	Reverses the order of characters in the string.  The terminating
;	null character remains in place.
;
;	Algorithm:
;	char *
;	_strrev (string)
;	      char *string;
;	      {
;	      char *start = string;
;	      char *left = string;
;	      char ch;
;
;	      while (*string++)
;		      ;
;	      string -= 2;
;	      while (left < string)
;		      {
;		      ch = *left;
;		      *left++ = *string;
;		      *string-- = ch;
;		      }
;	      return(start);
;	      }
;
;	NOTE: There is a check for an empty string in the following code.
;	Normally, this would fall out of the "cmp si,di" instruction in the
;	loop portion of the routine.  However, if the offset of the empty
;	string is 0 (as it could be in large model), then the cmp does not
;	catch the empty string and the routine essentially hangs (i.e., loops
;	moving bytes one at a time FFFFh times).  An explicit empty string
;	check corrects this.
;
;Entry:
;	char *string - string to reverse
;
;Exit:
;	returns string - now with reversed characters
;
;Uses:
;
;Exceptions:
;
;*******************************************************************************

	CODESEG

	public	_strrev
_strrev proc \
	uses edi esi, \
	string:ptr byte

	mov	edi,[string]	; di = string
	mov	edx,edi 	; dx=pointer to string; save return value

	mov	esi,edi 	; si=pointer to string
	xor	eax,eax 	; search value (null)
	or	ecx,-1		; cx = -1
repne	scasb			; find null
	cmp	ecx,-2		; is string empty? (if offset value is 0, the
	je	short done	; cmp below will not catch it and we'll hang).

	dec	edi		; string is not empty, move di pointer back
	dec	edi		; di points to last non-null byte

lupe:
	cmp	esi,edi 	; see if pointers have crossed yet
	jae	short done	; exit when pointers meet (or cross)

	mov	ah,[esi]	; get front byte...
	mov	al,[edi]	;   and end byte
	mov	[esi],al	; put end byte in front...
	mov	[edi],ah	;   and front byte at end
	inc	esi		; front moves up...
	dec	edi		;   and end moves down
	jmp	short lupe	; keep switching bytes

done:
	mov	eax,edx 	; return value: string addr

ifdef	_STDCALL_
	ret	DPSIZE		; _stdcall return
else
	ret			; _cdecl return
endif

_strrev endp
	end
