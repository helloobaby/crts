/***
*ehdata.h -
*
*	Copyright (c) 1993-1995, Microsoft Corporation. All rights reserved.
*
*Purpose:
*	Declare misc. types, macros, etc. for implementation
*	of C++ Exception Handling for the run-time and the compiler.
*	Hardware independent, assumes Windows NT.
*
* Portions of this header file can be disabled by defining the following
* macros:
*	_EHDATA_NOHEADERS - suppresses inclusion of standard header files
*		If this is specified, then appropriate typedefs or macros must
*		be provided by some other means.
*	_EHDATA_NOTHROW - suppresses definitions used only to describe a throw
*	_EHDATA_NOFUNCINFO - suppresses definitions for the frame descriptor
*	_EHDATA_NONT - suppresses definitions of our version of NT's stuff
*
* Other conditional compilation macros:
*    CC_EXPLICITFRAME - if true, representation of registration node includes
*	the value of the frame-pointer for that frame, making the location
*	of the registration node on the frame flexible.  This is intended
*	primarily for early testing.
*
*       [Internal]
*
*Revision History:
*       05-20-93  BS	Module Created.
*	03-01-94  CFW	Remove CONTEXT def for x86 for TiborL.
*	03-03-94  TL	Mips (_M_MRX000 >= 4000) changes
*	09-02-94  SKS	This header file added.
*	09-12-94  GJF	Merged in changes from/for DEC (Al Doser, dated 6/20,
*			and Bill Baxter, dated 6/28).
*	11-06-94  GJF	Changed pack pragma to 8 byte alignment.
*       02-14-95  CFW   Clean up Mac merge.
*       03-22-95  PML   Add const for read-only structs
*       03-29-95  CFW   Add error message to internal headers.
*	04-14-95  JWM	Added EH_ABORT_FRAME_UNWIND_PART for EH/SEH exception handling.
*	04-20-95  TGL	Added iFrameNestLevel field to MIPS FuncInfo
*	04-27-95  JWM	EH_ABORT_FRAME_UNWIND_PART now #ifdef ALLOW_UNWIND_ABORT.
*	06-08-95  JWM	Merged CRT version of ehdata.h into langapi source.
*	01-25-00  GB    Declared _CxxThrowException __declspec(noreturn)
*	02-08-00  GB    Added HT_ISCOMPLUSEH
*	03-27-00  PML   Remove CC_P7_SOFT25, which is now on permanently.
*       09-20-00  GB    Add __cdecl to URT C++ EH support routines (vs7#89576)
*
****/

#ifndef _INC_EHDATA
#define _INC_EHDATA

#ifdef ONLY_VALUES
#define _EHDATA_NOHEADERS
#endif

#ifndef _CRTBLD
#ifndef _VC_VER_INC
#ifdef _M_ALPHA
#include "vcver.h"
#else
#include "..\include\vcver.h"
#endif
#endif
#endif /* _CRTBLD */


#if defined(_M_IX86) && _M_IX86 >= 300 /*IFSTRIP=IGN*/
# ifndef CC_EXPLICITFRAME
#  define CC_EXPLICITFRAME	0	// If non-zero, we're using a hack version of the
								// registration node.
# endif
#endif

#ifndef _EHDATA_NOHEADERS
#include <stddef.h>
#include <excpt.h>
#if defined(_WIN32)
#include <windows.h>
#else
#include <nowin.h>
#endif
#endif /* _EHDATA_NOHEADERS */

#pragma pack(push, ehdata, 4)

#define EH_EXCEPTION_NUMBER	('msc' | 0xE0000000)	// The NT Exception # that we use
#define EH_MAGIC_NUMBER1	0x19930520		// The magic # identifying this version
							// As magic numbers increase, we have to keep track of
							// the versions that we are backwards compatible with.
#if defined(_M_IA64) || defined(_M_AMD64)
#define EH_EXCEPTION_PARAMETERS 4			// Number of parameters in exception record
#else
#define EH_EXCEPTION_PARAMETERS 3			// Number of parameters in exception record
#endif

#ifdef ALLOW_UNWIND_ABORT
#define EH_ABORT_FRAME_UNWIND_PART EH_EXCEPTION_NUMBER+1
#endif

#define EH_EMPTY_STATE	-1

#ifndef ONLY_VALUES

//
// PMD - Pointer to Member Data: generalized pointer-to-member descriptor
//

typedef struct PMD
{
#if defined(_WIN64)	 /*IFSTRIP=IGN*/
	__int32		mdisp;		// Offset of intended data within base
	__int32		pdisp;		// Displacement to virtual base pointer
	__int32		vdisp;		// Index within vbTable to offset of base
#else
	ptrdiff_t	mdisp;		// Offset of intended data within base
	ptrdiff_t	pdisp;		// Displacement to virtual base pointer
	ptrdiff_t	vdisp;		// Index within vbTable to offset of base
#endif
	} PMD;

//
// PMFN - Pointer to Member Function
//			M00REVIEW: we may need something more than this, but this will do for now.
//

#ifndef WANT_NO_TYPES
#if defined(_WIN64)	 /*IFSTRIP=IGN*/
#if (defined(_M_IA64) || defined(_M_AMD64)) && !defined(VERSP_P7)	/*IFSTRIP=IGN*/
typedef	__int32	PMFN;					// Image relative offset of Member Function
#else
typedef void (* __ptr64 PMFN)(void* __ptr64);
#endif
#else
typedef void (*PMFN)(void);
#endif
#endif // WANT_NO_TYPES

//
// TypeDescriptor - per-type record which uniquely identifies the type.
//
// Each type has a decorated name which uniquely identifies it, and a hash
// value which is computed by the compiler.  The hash function used is not
// important; the only thing which is essential is that it be the same for
// all time.
//
// The special type '...' (ellipsis) is represented by a null name.
//
#pragma warning(disable:4200)		// get rid of obnoxious nonstandard extension warning

#if defined(_M_ALPHA64) || defined(_M_IA64) || defined(VERSP_P7) || defined(_M_AMD64)
#pragma pack(push, TypeDescriptor, 8)
#endif

#ifndef WANT_NO_TYPES
typedef struct TypeDescriptor
{
#if defined(_WIN64) /*IFSTRIP=IGN*/
	const void * __ptr64	pVFTable;	// Field overloaded by RTTI
	void * __ptr64			spare;		// reserved, possible for RTTI
#else	// _WIN64
#if defined(_RTTI)
	const void *	pVFTable;	// Field overloaded by RTTI
#else
	DWORD	hash;			// Hash value computed from type's decorated name
#endif
	void *	spare;			// reserved, possible for RTTI
#endif	// _WIN64
	char	name[];			// The decorated name of the type; 0 terminated.
	} TypeDescriptor;
#endif // WANT_NO_TYPES

#if defined(_M_ALPHA64) || defined(_M_IA64) || defined(VERSP_P7) || defined(_M_AMD64)
#pragma pack(pop, TypeDescriptor)
#endif
#pragma warning(default:4200)

#define TD_HASH(td)		((td).hash)
#define TD_NAME(td)		((td).name)

#define TD_IS_TYPE_ELLIPSIS(td) ((td == NULL) || (TD_NAME(*td)[0] == '\0'))


#ifndef _EHDATA_NOTHROW

/////////////////////////////////////////////////////////////////////////////
//
// Description of the thrown object.  (M00REVIEW: not final)
//
// This information is broken down into three levels, to allow for maximum
// comdat folding (at the cost of some extra pointers).
//
// ThrowInfo is the head of the description, and contains information about
// 				the particular variant thrown.
// CatchableTypeArray is an array of pointers to type descriptors.  It will
//				be shared between objects thrown by reference but with varying
//				qualifiers.
// CatchableType is the description of an individual type, and how to effect
//				the conversion from a given type.
//
//---------------------------------------------------------------------------


//
// CatchableType - description of a type that can be caught.
//
// Note:  although isSimpleType can be part of ThrowInfo, it is more
//		  convenient for the run-time to have it here.
//

#ifndef WANT_NO_TYPES
typedef const struct _s_CatchableType {
	unsigned int	properties;				// Catchable Type properties (Bit field)
#if defined(_WIN64) /*IFSTRIP=IGN*/
#if (defined(_M_IA64) || defined(_M_AMD64)) && !defined(VERSP_P7)	/*IFSTRIP=IGN*/
	__int32			pType;					// Image relative offset of TypeDescriptor
#else
	TypeDescriptor * __ptr64 pType;			// Pointer to the type descriptor for this type
#endif
#else
	TypeDescriptor *pType;					// Pointer to the type descriptor for this type
#endif
	PMD 			thisDisplacement;		// Pointer to instance of catch type within
											//		thrown object.
	int				sizeOrOffset;			// Size of simple-type object or offset into
											//  buffer of 'this' pointer for catch object
	PMFN			copyFunction;			// Copy constructor or CC-closure
} CatchableType;
#endif // WANT_NO_TYPES

#define CT_IsSimpleType			0x00000001		// type is a simple type
#define CT_ByReferenceOnly		0x00000002		// type must be caught by reference
#define CT_HasVirtualBase		0x00000004		// type is a class with virtual bases

#define CT_PROPERTIES(ct)	((ct).properties)
#if defined(_M_IA64) || defined(_M_AMD64)
#define CT_PTD_IB(ct,ib)		((TypeDescriptor *)((ib) + (ct).pType))
#define CT_COPYFUNC_IB(ct,ib)	((void (* __ptr64)(void* __ptr64))((ib) + (ct).copyFunction))
#else
#define CT_PTD(ct)			((ct).pType)
#define CT_COPYFUNC(ct)		((ct).copyFunction)
#endif
#define CT_THISDISP(ct)		((ct).thisDisplacement)
#define CT_SIZE(ct)			((ct).sizeOrOffset)
#define CT_OFFSET(ct)		((ct).sizeOrOffset)
#define CT_HASH(ct)			(TD_HASH(*CT_PTD(ct)))
#define CT_NAME(ct)			(TD_NAME(*CT_PTD(ct)))

#define SET_CT_ISSIMPLETYPE(ct)		(CT_PROPERTIES(ct) |= CT_IsSimpleType)
#define SET_CT_BYREFONLY(ct)		(CT_PROPERTIES(ct) |= CT_ByReferenceOnly)
#define SET_CT_HASVB(ct)			(CT_PROPERTIES(ct) |= CT_HasVirtualBase)

#define CT_ISSIMPLETYPE(ct)			(CT_PROPERTIES(ct) & CT_IsSimpleType)		// Is it a simple type?
#define CT_BYREFONLY(ct)			(CT_PROPERTIES(ct) & CT_ByReferenceOnly)	// Must it be caught by reference?
#define CT_HASVB(ct)				(CT_PROPERTIES(ct) & CT_HasVirtualBase)		// Is this type a class with virtual bases?

//
// CatchableTypeArray - array of pointers to catchable types, with length
//
#pragma warning(disable:4200)		// get rid of obnoxious nonstandard extension warning
#ifndef WANT_NO_TYPES
typedef const struct _s_CatchableTypeArray {
	int	nCatchableTypes;
#if defined(_WIN64) /*IFSTRIP=IGN*/
#if (defined(_M_IA64) || defined(_M_AMD64)) && !defined(VERSP_P7)	/*IFSTRIP=IGN*/
	__int32			arrayOfCatchableTypes[];	// Image relative offset of Catchable Types
#else
	CatchableType	* __ptr64 arrayOfCatchableTypes[];
#endif
#else
	CatchableType	*arrayOfCatchableTypes[];
#endif
	} CatchableTypeArray;
#endif // WANT_NO_TYPES
#pragma warning(default:4200)

//
// ThrowInfo - information describing the thrown object, staticly built
// at the throw site.
//
// pExceptionObject (the dynamic part of the throw; see below) is always a
// reference, whether or not it is logically one.  If 'isSimpleType' is true,
// it is a reference to the simple type, which is 'size' bytes long.  If
// 'isReference' and 'isSimpleType' are both false, then it's a UDT or
// a pointer to any type (ie pExceptionObject points to a pointer).  If it's
// a pointer, copyFunction is NULL, otherwise it is a pointer to a copy
// constructor or copy constructor closure.
//
// The pForwardCompat function pointer is intended to be filled in by future
// versions, so that if say a DLL built with a newer version (say C10) throws,
// and a C9 frame attempts a catch, the frame handler attempting the catch (C9)
// can let the version that knows all the latest stuff do the work.
//

#ifndef WANT_NO_TYPES
typedef const struct _s_ThrowInfo {
	unsigned int	attributes;			// Throw Info attributes (Bit field)
	PMFN			pmfnUnwind;			// Destructor to call when exception
										// has been handled or aborted.

#if defined(_WIN64) /*IFSTRIP=IGN*/
#if (defined(_M_IA64) || defined(_M_AMD64)) && !defined(VERSP_P7)	/*IFSTRIP=IGN*/
	__int32			pForwardCompat;		// Image relative offset of Forward compatibility frame handler
	__int32			pCatchableTypeArray;// Image relative offset of CatchableTypeArray
#else
	int	(__cdecl* __ptr64 pForwardCompat)(...);	// Forward compatibility frame handler
	CatchableTypeArray	* __ptr64 pCatchableTypeArray;	// Pointer to list of pointers to types.
#endif
#else
	int	(__cdecl*pForwardCompat)(...);	// Forward compatibility frame handler
	CatchableTypeArray	*pCatchableTypeArray;	// Pointer to list of pointers to types.
#endif
} ThrowInfo;
#endif // WANT_NO_TYPES

#define TI_IsConst			0x00000001		// thrown object has const qualifier
#define TI_IsVolatile		0x00000002		// thrown object has volatile qualifier
#define TI_IsUnaligned		0x00000004		// thrown object has unaligned qualifier

#define THROW_ATTRS(t)			((t).attributes)
#if defined(_M_IA64) || defined(_M_AMD64)
#define THROW_UNWINDFUNC_IB(t,ib)		((void (* __ptr64)(void* __ptr64))((ib) + (t).pmfnUnwind))
#define THROW_FORWARDCOMPAT_IB(t,ib)	((int(__cdecl * __ptr64)(...))((ib) + (t).pForwardCompat))
#define THROW_CTARRAY_IB(t,ib)			((CatchableTypeArray*)((ib) + (t).pCatchableTypeArray))
#define THROW_COUNT_IB(t,ib)			(THROW_CTARRAY_IB(t,ib)->nCatchableTypes)
#define THROW_CTLIST_IB(t,ib)			(THROW_CTARRAY_IB(t,ib)->arrayOfCatchableTypes)
#else
#define THROW_FORWARDCOMPAT(t)	((t).pForwardCompat)
#define THROW_COUNT(t)			((t).pCatchableTypeArray->nCatchableTypes)
#define THROW_CTLIST(t)			((t).pCatchableTypeArray->arrayOfCatchableTypes)
#endif
#define THROW_UNWINDFUNC(t)		((t).pmfnUnwind)
#define THROW_PCTLIST(t)		(&THROW_CTLIST(t))
#define THROW_CT(t, n)			(*THROW_CTLIST(t)[n])
#define THROW_PCT(t, n)			(THROW_CTLIST(t)[n])

#define SET_TI_ISCONST(t)		(THROW_ATTRS(t) |= TI_IsConst)		// Is the object thrown 'const' qualified
#define SET_TI_ISVOLATILE(t)	(THROW_ATTRS(t) |= TI_IsVolatile)	// Is the object thrown 'volatile' qualified
#define SET_TI_ISUNALIGNED(t)	(THROW_ATTRS(t) |= TI_IsUnaligned)	// Is the object thrown 'unaligned' qualified

#define THROW_ISCONST(t)		(THROW_ATTRS(t) & TI_IsConst)
#define THROW_ISVOLATILE(t)		(THROW_ATTRS(t) & TI_IsVolatile)
#define THROW_ISUNALIGNED(t)	(THROW_ATTRS(t) & TI_IsUnaligned)

//
// Here's how to throw:
// M00HACK: _ThrowInfo is the name of the type that is 'pre-injected' into the
// compiler; since this prototype is known to the FE along with the pre-injected
// types, it has to match exactly.
//
#if _MSC_VER >= 900 /*IFSTRIP=IGN*/
__declspec (noreturn) extern "C" void __stdcall _CxxThrowException(void* pExceptionObject, _ThrowInfo* pThrowInfo);
#else
// If we're not self-building, we need to use the name that we defined above.
__declspec (noreturn) extern "C" void __stdcall _CxxThrowException(void* pExceptionObject, ThrowInfo* pThrowInfo);
#endif

#ifndef WANT_NO_TYPES
extern "C" int __cdecl __CxxExceptionFilter(void*, void*, int, void *);

// Returns true if the object is really a C++ exception
// If it is, stores the previous exception in *storage, and saves the current one
// This is needed to keep track of the current exception object (used for rethrow & destruction)
extern "C" int __cdecl __CxxRegisterExceptionObject(void *exception, void *storage);

// Returns true if exception is a C++ rethrown exception
// This is needed, so Unregister can know whether or not to destroy the object
extern "C" int __cdecl __CxxDetectRethrow(void *exception);

// Returns the byte count of stack space required to store the exception info
extern "C" int __cdecl __CxxQueryExceptionSize(void);

// Pops the current exception, restoring the previous one from *storage
// This detects whether or not the exception object needs to be destroyed
extern "C" void __cdecl __CxxUnregisterExceptionObject(void *storage, int rethrow);

#endif // WANT_NO_TYPES

#endif /* _EHDATA_NOTHROW */


#ifndef _EHDATA_NOFUNCINFO

/////////////////////////////////////////////////////////////////////////////
//
// Describing 'try/catch' blocks:
//
//---------------------------------------------------------------------------

//
// Current state of a function.
// -1 is the 'blank' state, ie there is nothing to unwind, no try blocks active.
//

typedef int __ehstate_t;		// The type of a state index


//
// HandlerType - description of a single 'catch'
//

#ifndef WANT_NO_TYPES
typedef const struct _s_HandlerType {
	unsigned int	adjectives;			// Handler Type adjectives (bitfield)
#if defined(_M_IA64) || defined(_M_AMD64)	 /*IFSTRIP=IGN*/
	__int32			dispType;			// Image relative offset of the corresponding type descriptor
	__int32			dispCatchObj;		// Displacement of catch object from base
	__int32			dispOfHandler;		// Image relative offset of 'catch' code
#if defined(_M_AMD64)
    __int32         dispFrame;          // displacement of address of function frame wrt establisher frame
#endif
#else
	TypeDescriptor	*pType;				// Pointer to the corresponding type descriptor
#if defined(_M_ALPHA64)
	__int32			dispCatchObj;		// Displacement of catch object from base
#else
	ptrdiff_t		dispCatchObj;		// Displacement of catch object from base
#endif
#if _M_MRX000 >= 4000	 /*IFSTRIP=IGN*/
	ULONG			frameNestLevel;		// The static nesting level of parent function
#endif
	void *			addressOfHandler;	// Address of 'catch' code
#endif
} HandlerType;
#endif

#define HT_IsConst			0x00000001		// type referenced is 'const' qualified
#define HT_IsVolatile		0x00000002		// type referenced is 'volatile' qualified
#define HT_IsUnaligned		0x00000004		// type referenced is 'unaligned' qualified
#define HT_IsReference		0x00000008		// catch type is by reference
#define HT_IsResumable		0x00000010		// the catch may choose to resume (Reserved)
#define HT_IsComplusEh      0x80000000      // Is handling within complus eh.

#define HT_ADJECTIVES(ht)		((ht).adjectives)
#if defined(_M_IA64) || defined(_M_AMD64)	 /*IFSTRIP=IGN*/
#define HT_PTD_IB(ht,ib)		((TypeDescriptor*)((ib) + (ht).dispType))
#define HT_HANDLER_IB(ht,ib)	((void* __ptr64)((ib) + (ht).dispOfHandler))
#else
#define HT_PTD(ht)				((ht).pType)
#define HT_HANDLER(ht)			((ht).addressOfHandler)
#endif
#define HT_DISPCATCH(ht)		((ht).dispCatchObj)
#if _M_MRX000 >= 4000	 /*IFSTRIP=IGN*/
#define HT_FRAMENEST(ht)		((ht).frameNestLevel)
#endif
#define HT_NAME(ht)				(TD_NAME(*HT_PTD(ht)))
#define HT_HASH(ht)				(TD_HASH(*HT_PTD(ht)))
#define HT_IS_TYPE_ELLIPSIS(ht)	TD_IS_TYPE_ELLIPSIS(HT_PTD(ht))

#define SET_HT_ISCONST(ht)		(HT_ADJECTIVES(ht) |= HT_IsConst)
#define SET_HT_ISVOLATILE(ht)	(HT_ADJECTIVES(ht) |= HT_IsVolatile)
#define SET_HT_ISUNALIGNED(ht)	(HT_ADJECTIVES(ht) |= HT_IsUnaligned)
#define SET_HT_ISREFERENCE(ht)	(HT_ADJECTIVES(ht) |= HT_IsReference)
#define SET_HT_ISRESUMABLE(ht)	(HT_ADJECTIVES(ht) |= HT_IsResumable)
#define SET_HT_ISCOMPLUSEH(ht)  (HT_ADJECTIVES(ht) |= HT_IsComplusEh)

#define HT_ISCONST(ht)			(HT_ADJECTIVES(ht) & HT_IsConst)		// Is the type referenced 'const' qualified
#define HT_ISVOLATILE(ht)		(HT_ADJECTIVES(ht) & HT_IsVolatile)		// Is the type referenced 'volatile' qualified
#define HT_ISUNALIGNED(ht)		(HT_ADJECTIVES(ht) & HT_IsUnaligned)	// Is the type referenced 'unaligned' qualified
#define HT_ISREFERENCE(ht)		(HT_ADJECTIVES(ht) & HT_IsReference)	// Is the catch type by reference
#define HT_ISRESUMABLE(ht)		(HT_ADJECTIVES(ht) & HT_IsResumable)	// Might the catch choose to resume (Reserved)
#define HT_ISCOMPLUSEH(ht)      (HT_ADJECTIVES(ht) & HT_IsComplusEh)

//
// HandlerMapEntry - associates a handler list (sequence of catches) with a
//	range of eh-states.
//

#ifndef WANT_NO_TYPES
typedef const struct _s_TryBlockMapEntry {
	__ehstate_t	tryLow;				// Lowest state index of try
	__ehstate_t	tryHigh;			// Highest state index of try
#if !defined(_M_ALPHA)
	__ehstate_t	catchHigh;			// Highest state index of any associated catch
#endif
	int			nCatches;			// Number of entries in array
#if defined(_M_IA64) || defined(_M_AMD64)	/*IFSTRIP=IGN*/
	__int32		dispHandlerArray;	// Image relative offset of list of handlers for this try
#else
	HandlerType *pHandlerArray;		// List of handlers for this try
#endif
} TryBlockMapEntry;
#endif // WANT_NO_TYPES

#define TBME_LOW(hm)		((hm).tryLow)
#define TBME_HIGH(hm)		((hm).tryHigh)
#define TBME_CATCHHIGH(hm)	((hm).catchHigh)
#define TBME_NCATCHES(hm)	((hm).nCatches)
#if defined(_M_IA64) || defined(_M_AMD64)	/*IFSTRIP=IGN*/
#define TBME_PLIST(hm,ib)	((HandlerType*)((ib) + (hm).dispHandlerArray))
#define TBME_CATCH(hm,n,ib)	(TBME_PLIST(hm,ib)[n])
#define TBME_PCATCH(hm,n,ib)(&(TBME_PLIST(hm,ib)[n]))
#else
#define TBME_PLIST(hm)		((hm).pHandlerArray)
#define TBME_CATCH(hm, n)	(TBME_PLIST(hm)[n])
#define TBME_PCATCH(hm, n)	(&(TBME_PLIST(hm)[n]))
#endif


/////////////////////////////////////////////////////////////////////////////
//
// Description of the function:
//
//---------------------------------------------------------------------------

//
// UnwindMapEntry - Description of each state transition for unwinding
//	the stack (ie destructing objects).
//
// The unwind map is an array, indexed by current state.  Each entry specifies
// the state to go to during unwind, and the action required to get there.
// Note that states are represented by a signed integer, and that the 'blank'
// state is -1 so that the array remains 0-based (because by definition there
// is never any unwind action to be performed from state -1).  It is also
// assumed that state indices will be dense, ie that there will be no gaps of
// unused state indices in a function.
//

typedef const struct _s_UnwindMapEntry {
	__ehstate_t		toState;			// State this action takes us to
#if defined(_M_IA64) || defined(_M_AMD64)	/*IFSTRIP=IGN*/
	__int32			action;				// Image relative offset of funclet
#else
	void			(*action)(void);	// Funclet to call to effect state change
#endif
} UnwindMapEntry;

#define UWE_TOSTATE(uwe)	((uwe).toState)
#if defined(_M_IA64) || defined(_M_AMD64)
#define UWE_ACTION_IB(uwe,ib)	((void (*__ptr64)(void))((ib) + (uwe).action))
#else
#define UWE_ACTION(uwe)			((uwe).action)
#endif

#if _M_MRX000 >= 4000 || defined(_M_MPPC) || defined(_M_PPC) || defined(_M_IA64) || defined(_M_AMD64)	 /*IFSTRIP=IGN*/
typedef struct IptoStateMapEntry {
#if defined(_M_IA64) || defined(_M_AMD64)	/*IFSTRIP=IGN*/
	__int32		Ip;		// Image relative offset of IP
#else
	ULONG		Ip;
#endif
	__ehstate_t	State;
} IptoStateMapEntry;
#endif

//
// FuncInfo - all the information that describes a function with exception
//	handling information.
//

// bbtFlags values
#define BBT_UNIQUE_FUNCINFO 1

#ifndef WANT_NO_TYPES
typedef const struct _s_FuncInfo
{
    unsigned int		magicNumber:29;		// Identifies version of compiler
    unsigned int        bbtFlags:3;         // flags that may be set by BBT processing
	__ehstate_t			maxState;			// Highest state number plus one (thus
											// number of entries in unwind map)
#if defined(_M_IA64) || defined (_M_AMD64)	/*IFSTRIP=IGN*/
	__int32				dispUnwindMap;		// Image relative offset of the unwind map
	unsigned int		nTryBlocks;			// Number of 'try' blocks in this function
	__int32				dispTryBlockMap;	// Image relative offset of the handler map
	unsigned int		nIPMapEntries;		// # entries in the IP-to-state map. NYI (reserved)
	__int32				dispIPtoStateMap;	// Image relative offset of the IP to state map
	__int32				dispUwindHelp;		// Displacement of unwind helpers from base
#else
	UnwindMapEntry		*pUnwindMap;		// Where the unwind map is
	unsigned int		nTryBlocks;			// Number of 'try' blocks in this function
	TryBlockMapEntry	*pTryBlockMap;		// Where the handler map is
#if defined(_M_ALPHA)
    signed int          EHContextDelta;     // Frame offset of EHContext record
#endif
	unsigned int		nIPMapEntries;		// # entries in the IP-to-state map. NYI (reserved)
#if _M_MRX000 >= 4000	 /*IFSTRIP=IGN*/
	IptoStateMapEntry	*pIPtoStateMap;     // An IP to state map..
	ptrdiff_t			dispUnwindHelp;		// Displacement of unwind helpers from base
	int					iTryBlockIndex;		// Used by catch functions only
	int					iFrameNestLevel;	// The static nesting level of parent function
#elif defined(_M_MPPC) || defined(_M_PPC)
	IptoStateMapEntry	*pIPtoStateMap;		// An IP to state map..
#else
	void				*pIPtoStateMap;		// An IP to state map.  NYI (reserved).
#endif
#endif
} FuncInfo;
#endif // WANT_NO_TYPES

#define FUNC_MAGICNUM(fi)			((fi).magicNumber)
#define FUNC_MAXSTATE(fi)		((fi).maxState)
#define FUNC_NTRYBLOCKS(fi)		((fi).nTryBlocks)
#define FUNC_NIPMAPENT(fi)		((fi).nIPMapEntries)
#if defined(_M_IA64) || defined (_M_AMD64)
#define FUNC_PUNWINDMAP(fi,ib)	((UnwindMapEntry*)((ib) + (fi).dispUnwindMap))
#define FUNC_PHANDLERMAP(fi,ib)	((TryBlockMapEntry*)((ib) + (fi).dispTryBlockMap))
#define FUNC_IPMAP(fi,ib)		((IptoStateMapEntry*)((ib) + (fi).dispIPtoStateMap))
#define FUNC_UNWIND(fi,st,ib)	(FUNC_PUNWINDMAP(fi,ib)[st])
#define FUNC_PUNWIND(fi,st,ib)	(&FUNC_UNWIND(fi,st,ib))
#define FUNC_TRYBLOCK(fi,n,ib)	(FUNC_PHANDLERMAP(fi,ib)[n])
#define FUNC_PTRYBLOCK(fi,n,ib)	(&FUNC_TRYBLOCK(fi,n,ib))
#else
#define FUNC_PUNWINDMAP(fi)		((fi).pUnwindMap)
#define FUNC_PHANDLERMAP(fi)	((fi).pTryBlockMap)
#define FUNC_IPMAP(fi)			((fi).pIPtoStateMap)
#define FUNC_UNWIND(fi, st)		((fi).pUnwindMap[st])
#define FUNC_PUNWIND(fi, st)	(&FUNC_UNWIND(fi, st))
#define FUNC_TRYBLOCK(fi,n)		((fi).pTryBlockMap[n])
#define FUNC_PTRYBLOCK(fi,n)	(&FUNC_TRYBLOCK(fi, n))
#endif
#if defined(_M_ALPHA)
#define FUNC_EHCONTEXTDELTA(fi) ((fi).EHContextDelta)
#endif
#if _M_MRX000 >= 4000		 /*IFSTRIP=IGN*/
#define FUNC_IPTOSTATE(fi,n)	((fi).pIPtoStateMap[n])
#define FUNC_PIPTOSTATE(fi,n)	(&FUNC_IPTOSTATE(fi,n))
#define FUNC_DISPUNWINDHELP(fi)	((fi).dispUnwindHelp)
#define FUNC_TRYBLOCKINDEX(fi)	((fi).iTryBlockIndex)
#define FUNC_FRAMENEST(fi)		((fi).iFrameNestLevel)
#elif defined(_M_MPPC) || defined(_M_PPC)
#define FUNC_IPTOSTATE(fi,n)	((fi).pIPtoStateMap[n])
#define FUNC_PIPTOSTATE(fi,n)	(&FUNC_IPTOSTATE(fi,n))
#elif defined(_M_IA64) || defined (_M_AMD64)
#define FUNC_IPTOSTATE(fi,n,ib)	(FUNC_IPMAP(fi,ib)[n])
#define FUNC_PIPTOSTATE(fi,n,ib)(&FUNC_IPTOSTATE(fi,n,ib))
#define FUNC_DISPUNWINDHELP(fi)	((fi).dispUwindHelp)
#else
#define FUNC_IPTOSTATE(fi,n) 	__ERROR_NYI__
#endif

#endif /* _EHDATA_NOFUNCINFO */

#ifndef _EHDATA_NONT

/////////////////////////////////////////////////////////////////////////////
//
// Data types that are variants of data used by NT (and Chicago) to manage
// exception handling.
//
//---------------------------------------------------------------------------

/////////////////////////////////////////////////////////////////////////////
//
// A stack registration node (i386 only)
//

#if defined(_M_IX86) && _M_IX86 >= 300 /*IFSTRIP=IGN*/
struct EHRegistrationNode {
	/* void *			stackPtr */		// Stack ptr at entry to try (below address point)
	EHRegistrationNode	*pNext;			// Next node in the chain
	void *				frameHandler;	// The handler function for this frame
	__ehstate_t			state;			// The current state of this function
#if CC_EXPLICITFRAME
	void *				frame;			// Value of ebp for this frame
#endif
};

#if !CC_EXPLICITFRAME
				// Cannonical offset
# define FRAME_OFFSET	sizeof(EHRegistrationNode)
#endif

#define PRN_NEXT(prn)		((prn)->pNext)
#define PRN_HANDLER(prn)	((prn)->frameHandler)
#define PRN_STATE(prn)		((prn)->state)
#define PRN_STACK(prn)		(((void**)(prn))[-1])
#if CC_EXPLICITFRAME
# define PRN_FRAME(prn)		((prn)->frame)
#else
# define PRN_FRAME(prn)		((void*)(((char*)prn) + FRAME_OFFSET))
#endif

typedef void DispatcherContext;		// Meaningless on Intel

#elif _M_MRX000 >= 4000 /*IFSTRIP=IGN*/
//
// On MIPS we don't have a registration node, just a pointer to the stack frame base
//
typedef ULONG EHRegistrationNode;

#define PRN_NEXT(prn)		__ERROR__
#define PRN_HANDLER(prn)	__ERROR__
#define PRN_STATE(prn)		__ERROR__
#define PRN_STACK(prn)		__ERROR__
#define PRN_FRAME(prn)		__ERROR__

#define FRAME_OFFSET		0
#if !defined(_NTSUBSET_)
typedef struct _RUNTIME_FUNCTION {
    ULONG BeginAddress;
    ULONG EndAddress;
    EXCEPTION_DISPOSITION (*ExceptionHandler)();
    PVOID HandlerData;
    ULONG PrologEndAddress;
} RUNTIME_FUNCTION, *PRUNTIME_FUNCTION;
#endif

typedef struct _xDISPATCHER_CONTEXT {
    ULONG ControlPc;
    PRUNTIME_FUNCTION FunctionEntry;
    ULONG EstablisherFrame;
    PCONTEXT ContextRecord;
} DispatcherContext;					// changed the case of the name to conform to EH conventions

#elif defined(_M_IA64) /*IFSTRIP=IGN*/

#define PRN_NEXT(prn)		__ERROR__
#define PRN_HANDLER(prn)	__ERROR__
#define PRN_STATE(prn)		__ERROR__
#define PRN_STACK(prn)		__ERROR__
#define PRN_FRAME(prn)		__ERROR__

#define FRAME_OFFSET		0

#if !defined(_NTSUBSET_)
typedef struct _FRAME_POINTERS {
    __int64 MemoryStackFp;                     // memory stack frame pointer
    __int64 BackingStoreFp;                    // backing store frame pointer
} FRAME_POINTERS, *PFRAME_POINTERS;

typedef struct _UNWIND_INFO {
    unsigned __int16 Version;                  // Version Number
    unsigned __int16 Flags;                    // Flags
    unsigned __int32 DataLength;               // Length of Descriptor Data
} UNWIND_INFO, *PUNWIND_INFO;

//这是现代msvc的实现 
typedef struct _RUNTIME_FUNCTION {             
    unsigned __int32 BeginAddress;             // image relative offset to start of function
    unsigned __int32 EndAddress;               // image relative offset to end of function
    unsigned __int32 UnwindInfoAddress;        // image relative offset to unwind info block
} RUNTIME_FUNCTION, *PRUNTIME_FUNCTION;
#endif

typedef struct _xDISPATCHER_CONTEXT {
    FRAME_POINTERS EstablisherFrame;
    __int64 ControlPc;
    __int64 ImageBase;
    PRUNTIME_FUNCTION FunctionEntry;
    PCONTEXT ContextRecord;
} DispatcherContext;					// changed the case of the name to conform to EH conventions


//
// On P7 we don't have a registration node, just a pointer to the stack frame base
//
typedef FRAME_POINTERS EHRegistrationNode;

#elif defined(_M_AMD64)/*IFSTRIP=IGN*/

#define PRN_NEXT(prn)		__ERROR__
#define PRN_HANDLER(prn)	__ERROR__
#define PRN_STATE(prn)		__ERROR__
#define PRN_STACK(prn)		__ERROR__
#define PRN_FRAME(prn)		__ERROR__

#define FRAME_OFFSET		0

/*
typedef enum _UNWIND_OP_CODES {

	UWOP_PUSH_NONVOL = 0,

	UWOP_ALLOC_LARGE,		// 1

	UWOP_ALLOC_SMALL,		// 2

	UWOP_SET_FPREG, 		// 3

	UWOP_SAVE_NONVOL,		// 4

	UWOP_SAVE_NONVOL_FAR,	// 5

	UWOP_SPARE_CODE1,		// 6

	UWOP_SPARE_CODE2,		// 7

	UWOP_SAVE_XMM128,		// 8

	UWOP_SAVE_XMM128_FAR,	// 9

	UWOP_PUSH_MACHFRAME 	// 10

} UNWIND_OP_CODES, *PUNWIND_OP_CODES;

*/

#if !defined(_NTSUBSET_)
//https://docs.microsoft.com/en-us/previous-versions/ck9asaa9(v=vs.140)
/*
.rdata:0000000140002840 stru_140002840  UNWIND_INFO <9, 4, 1, 0>
.rdata:0000000140002840                                         ; DATA XREF: .pdata:0000000140005024↓o
.rdata:0000000140002844                 UNWIND_CODE <4, 42h>    ; UWOP_ALLOC_SMALL

//此函数的开头
.text:0000000140001130             ; __unwind { // __C_specific_handler_0
.text:0000000140001130 48 83 EC 28                 sub     rsp, 28h

举个例子，
CodeOffset是4
UnwindOp是2，OpInfo是4
2是UWOP_ALLOC_SMALL，IDA可以分析出来，28h是4*8+8(MSDN有介绍公式)

*/
typedef union _UNWIND_CODE {//占2个字节
    struct {
        unsigned char CodeOffset;//epilog指令的下一条
        unsigned char UnwindOp : 4;//每个Op对应着一个UpInfo结构，详情看上面MSDN
        unsigned char OpInfo : 4;
    };
    unsigned short FrameOffset;
} UNWIND_CODE, *PUNWIND_CODE;

//https://docs.microsoft.com/en-us/previous-versions/ddssxxy8(v=vs.140)
//RUNTIME_FUNCTION里的UnwindInfoAddress所指结构体
/*
Flags:
UNW_FLAG_NHANDLER 0
UNW_FLAG_EHANDLER 1 
UNW_FLAG_UHANDLER 2	
UNW_FLAG_CHAININFO 4 
*/
/*
举个魔兽dump的例子
.rdata:000000014274E090 stru_14274E090  UNWIND_INFO <21h, 5, 2, 0>
.rdata:000000014274E090                                         ; DATA XREF: .pdata:00000001430F6414↓o
.rdata:000000014274E094                 UNWIND_CODE <5, 0E4h>   ; UWOP_SAVE_NONVOL
.rdata:000000014274E096                 dw 0Fh
.rdata:000000014274E098                 RUNTIME_FUNCTION <rva loc_1400B037F, rva loc_1400B0526, \
.rdata:000000014274E098                                   rva stru_14274E068>

Version为1，Flags为UNW_FLAG_CHAININFO，SizeOfProlog为5,CountOfCodes为2，FrameRegister和FrameOffset为0

*/
/*
typedef struct _SCOPE_TABLE {

	ULONG Count;

	struct

	{

		ULONG BeginAddress;

		ULONG EndAddress;

		ULONG HandlerAddress;

		ULONG JumpTarget;

	} ScopeRecord[1];

} SCOPE_TABLE, *PSCOPE_TABLE;

.rdata:0000000140002808 stru_140002808  UNWIND_INFO_HDR <9, 4, 1, 0>
.rdata:0000000140002808                                         ; DATA XREF: .pdata:0000000140005018↓o
.rdata:000000014000280C                 UNWIND_CODE <4, 42h>    ; UWOP_ALLOC_SMALL
.rdata:000000014000280E                 align 4 ;这里占2个字节
.rdata:0000000140002810                 dd rva __C_specific_handler_0
.rdata:0000000140002814                 dd 2     ;ScopeTable Count
.rdata:0000000140002818                 dd 10E4h ScopeTable[0].BeginAddress IDA就根据这些信息来帮我们自动分析异常的try-except-finally
.rdata:000000014000281C                 dd 10F8h ScopeTable[0].EndAddress
.rdata:0000000140002820                 dd 1EB0h ScopeTable[0].HandlerAddress
.rdata:0000000140002824                 dd 10F8h ScopeTable[0].JumpTarget
.rdata:0000000140002828                 dd 10E4h x[1].x
.rdata:000000014000282C                 dd 110Fh 
.rdata:0000000140002830                 dd 1EBDh
.rdata:0000000140002834                 dd 110Fh

*/
typedef struct _UNWIND_INFO {
    unsigned char Version : 3;                 // Version Number 一般都是1			 |第一个字节，根据实验结果
    unsigned char Flags   : 5;                 // Flags 				  		 |有try-except是9h(00001.001)，有try-except-finally是19h(00011.001)，try-finally是11h(00010.001)
    unsigned char SizeOfProlog;				   //								 |没有用异常处理就是1h，21h不常见
    unsigned char CountOfCodes;//后面几个unwind_code								 |
    unsigned FrameRegister : 4;				   //								 |
    unsigned FrameOffset   : 4;		    	   //							     |
    UNWIND_CODE UnwindCode[1];				   //								 |如果第一个字节是9或者19h，那么在UnwindCode后面还有一些信息
/*  UNWIND_CODE MoreUnwindCode[((CountOfCodes+1)&~1)-1];						 |
 *  union {
 *      OPTIONAL ULONG ExceptionHandler;
 *      OPTIONAL ULONG FunctionEntry;
 *  };
 *  OPTIONAL ULONG ExceptionData[];
 */
} UNWIND_INFO, *PUNWIND_INFO;

//几乎每个函数都有这个结构，在PE文件的EXCEPTION_DIRECTORY
//Begin、End代表这个函数的起始地址和结束地址
typedef struct _RUNTIME_FUNCTION {             
    unsigned __int32 BeginAddress;             // image relative offset to start of function
    unsigned __int32 EndAddress;               // image relative offset to end of function
    unsigned __int32 UnwindInfoAddress;        // image relative offset to unwind info block
} RUNTIME_FUNCTION, *PRUNTIME_FUNCTION;
#endif


typedef struct _xDISPATCHER_CONTEXT {
    __int64 ControlPc;
    __int64 ImageBase;
    PRUNTIME_FUNCTION FunctionEntry;
    ULONG_PTR EstablisherFrame;
    ULONG64 TargetIp;
    PCONTEXT ContextRecord;
    PVOID LanguageHandler;
    PVOID HandlerData;
    PVOID HistoryTable;
} DispatcherContext;					// changed the case of the name to conform to EH conventions

//
// On P7 we don't have a registration node, just a pointer to the stack frame base
//
typedef ULONG_PTR EHRegistrationNode;

#elif defined(_M_ALPHA)
//
// On Alpha we don't have a registration node,
//     just a pointer to the stack frame base
//
typedef ULONG_PTR EHRegistrationNode;

#define PRN_NEXT(prn)           __ERROR__
#define PRN_HANDLER(prn)        __ERROR__
#define PRN_STATE(prn)          __ERROR__
#define PRN_STACK(prn)          __ERROR__
#define PRN_FRAME(prn)          __ERROR__

#if defined(_M_ALPHA64)	 /*IFSTRIP=IGN*/
#pragma pack(push, EHContext, 8)
#endif

#define FRAME_OFFSET            0
#if !defined(_NTSUBSET_)
typedef struct _RUNTIME_FUNCTION {
    ULONG_PTR BeginAddress;
    ULONG_PTR EndAddress;
    EXCEPTION_DISPOSITION (*ExceptionHandler)();
    PVOID HandlerData;    // ptr to FuncInfo record
    ULONG_PTR PrologEndAddress;
} RUNTIME_FUNCTION, *PRUNTIME_FUNCTION;
#endif

typedef struct _xDISPATCHER_CONTEXT {
    ULONG_PTR ControlPc;
    PRUNTIME_FUNCTION FunctionEntry;
    ULONG_PTR EstablisherFrame;  // Virtual Frame Pointer
    PCONTEXT ContextRecord;
} DispatcherContext;            // changed the case of the name to conform to EH conventions

//
// _EHCONTEXT is a struct built in the frame by the compiler.
// On entry to a function, compiler generated code stores the
// address of the base of the fixed frame area (the so-called
// Real Frame Pointer) into the Rfp. On every state transition,
// compiler generated code stores the current state index into
// the State field.
//
// The FuncInfo record for the function contains the offset of
// the _EHCONTEXT record from the Virtual Frame Pointer - a
// pointer to the highest address of the frame so this offset
// is negative (frames grow down in the address space).
//
typedef struct _EHCONTEXT {
    ULONG State;
    PVOID Rfp;
} EHContext;
#if defined(_M_ALPHA64)	 /*IFSTRIP=IGN*/
#pragma pack(pop, EHContext)
#endif

#define VIRTUAL_FP(pDC) (pDC->EstablisherFrame)

#define REAL_FP(VirtualFP, pFuncInfo)           \
    (((EHContext *)((char *)VirtualFP           \
     + pFuncInfo->EHContextDelta)) -> Rfp)

#define EH_STATE(VirtualFP, pFuncInfo)          \
    (((EHContext *)((char *)VirtualFP           \
     + pFuncInfo->EHContextDelta)) -> State)

#elif defined(_M_M68K)
struct EHRegistrationNode {
/*	void * 				_sp;			// The stack pointer for the entry of try/catch	*/
	void *				frameHandler;	// The handler function for this frame
	__ehstate_t			state;			// The current state of this function
};

#define PRN_HANDLER(prn)	((prn)->frameHandler)
#define PRN_STATE(prn)		((prn)->state)

typedef void DispatcherContext;		// Meaningless on Mac


#elif defined(_M_PPC) || defined(_M_MPPC)
//
// On PowerPC we don't have a registration node, just a pointer to the stack
// frame base
//
typedef ULONG EHRegistrationNode;

#define PRN_NEXT(prn)		__ERROR__
#define PRN_HANDLER(prn)	__ERROR__
#define PRN_STATE(prn)		__ERROR__
#define PRN_STACK(prn)		__ERROR__
#define PRN_FRAME(prn)		__ERROR__

#define FRAME_OFFSET		0

#if !defined(_NTSUBSET_)
typedef struct _RUNTIME_FUNCTION {
    ULONG BeginAddress;
    ULONG EndAddress;
    EXCEPTION_DISPOSITION (*ExceptionHandler)(...);
    PVOID HandlerData;
    ULONG PrologEndAddress;
} RUNTIME_FUNCTION, *PRUNTIME_FUNCTION;
#endif

typedef struct _xDISPATCHER_CONTEXT {
    ULONG ControlPc;
    PRUNTIME_FUNCTION FunctionEntry;
    ULONG EstablisherFrame;
    PCONTEXT ContextRecord;
} DispatcherContext;
    // changed the case of the name to conform to EH conventions

#if defined(_M_MPPC)
typedef struct _ftinfo {
    ULONG dwMagicNumber;                // magic number
    void *pFrameInfo;			// pointer to runtime frame info
    PRUNTIME_FUNCTION rgFuncTable;	// function table
    ULONG cFuncTable;			// number of function entry
    ULONG dwEntryCF;			// address of starting of the code fragment
    ULONG dwSizeCF;			// size of the code fragment
} FTINFO, *PFTINFO;

#define offsFTINFO              64
#endif

#else
#error "Machine not supported"
#endif

/////////////////////////////////////////////////////////////////////////////
//
// The NT Exception record that we use to pass information from the throw to
// the possible catches.
//
// The constants in the comments are the values we expect.
// This is based on the definition of EXCEPTION_RECORD in winnt.h.
//
#if defined(_M_IA64) || defined(_M_ALPHA64)	|| defined(_M_AMD64) /*IFSTRIP=IGN*/
#pragma pack(push, ExceptionRecord, 8)
#endif
#ifndef WANT_NO_TYPES
typedef struct EHExceptionRecord {
	DWORD		ExceptionCode;			// The code of this exception. (= EH_EXCEPTION_NUMBER)
	DWORD		ExceptionFlags;			// Flags determined by NT
    struct _EXCEPTION_RECORD *ExceptionRecord;	// An extra exception record (not used)
    void * 		ExceptionAddress;		// Address at which exception occurred
    DWORD 		NumberParameters;		// Number of extended parameters. (= EH_EXCEPTION_PARAMETERS)
	struct EHParameters {
		DWORD		magicNumber;		// = EH_MAGIC_NUMBER1
		void *		pExceptionObject;	// Pointer to the actual object thrown
		ThrowInfo	*pThrowInfo;		// Description of thrown object
#if defined(_M_IA64) || defined(_M_AMD64)
		void		*pThrowImageBase;	// Image base of thrown object
#endif
		} params;
} EHExceptionRecord;
#endif // WANT_NO_TYPES
#if defined(_M_IA64) || defined(_M_ALPHA64) || defined(_M_AMD64)	 /*IFSTRIP=IGN*/
#pragma pack(pop, ExceptionRecord)
#endif

#define PER_CODE(per)		((per)->ExceptionCode)
#define PER_FLAGS(per)		((per)->ExceptionFlags)
#define PER_NEXT(per)		((per)->ExceptionRecord)
#define PER_ADDRESS(per)	((per)->ExceptionAddress)
#define PER_NPARAMS(per)	((per)->NumberParameters)
#define PER_MAGICNUM(per)	((per)->params.magicNumber)
#define PER_PEXCEPTOBJ(per)	((per)->params.pExceptionObject)
#define PER_PTHROW(per)		((per)->params.pThrowInfo)
#if defined(_M_IA64) || defined(_M_AMD64)
#define PER_PTHROWIB(per)	((per)->params.pThrowImageBase)
#endif
#define PER_THROW(per)		(*PER_PTHROW(per))

#define PER_ISSIMPLETYPE(t)	(PER_THROW(t).isSimpleType)
#define PER_ISREFERENCE(t)	(PER_THROW(t).isReference)
#define PER_ISCONST(t)		(PER_THROW(t).isConst)
#define PER_ISVOLATILE(t)	(PER_THROW(t).isVolatile)
#define PER_ISUNALIGNED(t)	(PER_THROW(t).isUnaligned)
#define PER_UNWINDFUNC(t)	(PER_THROW(t).pmfnUnwind)
#define PER_PCTLIST(t)		(PER_THROW(t).pCatchable)
#define PER_CTLIST(t)		(*PER_PCTLIST(t))

#define PER_IS_MSVC_EH(per)	((PER_CODE(per) == EH_EXCEPTION_NUMBER) && 			\
		 					 (PER_NPARAMS(per) == EH_EXCEPTION_PARAMETERS) &&	\
		 					 (PER_MAGICNUM(per) == EH_MAGIC_NUMBER1))

/////////////////////////////////////////////////////////////////////////////
//
// NT kernel routines and definitions required to implement exception handling:
//
// (from ntxcapi.h, which is not a public header file)
//
//---------------------------------------------------------------------------

#ifndef _NTXCAPI_

// begin_ntddk
//
// Exception flag definitions.
//

// begin_winnt
#define EXCEPTION_NONCONTINUABLE 0x1    // Noncontinuable exception
// end_winnt

// end_ntddk
#define EXCEPTION_UNWINDING 0x2         // Unwind is in progress
#define EXCEPTION_EXIT_UNWIND 0x4       // Exit unwind is in progress
#define EXCEPTION_STACK_INVALID 0x8     // Stack out of limits or unaligned
#define EXCEPTION_NESTED_CALL 0x10      // Nested exception handler call
#define EXCEPTION_TARGET_UNWIND 0x20    // Target unwind in progress
#define EXCEPTION_COLLIDED_UNWIND 0x40  // Collided exception handler call

#define EXCEPTION_UNWIND (EXCEPTION_UNWINDING | EXCEPTION_EXIT_UNWIND | \
                          EXCEPTION_TARGET_UNWIND | EXCEPTION_COLLIDED_UNWIND)

#define IS_UNWINDING(Flag) ((Flag & EXCEPTION_UNWIND) != 0)
#define IS_DISPATCHING(Flag) ((Flag & EXCEPTION_UNWIND) == 0)
#define IS_TARGET_UNWIND(Flag) (Flag & EXCEPTION_TARGET_UNWIND)
#define IS_EXIT_UNWIND(Flag) (Flag & EXCEPTION_EXIT_UNWIND)

#if !defined(_M_M68K)
#ifdef __cplusplus
extern "C" {
#endif

void WINAPI
RtlUnwind (
    IN void * TargetFrame OPTIONAL,
    IN void * TargetIp OPTIONAL,
    IN PEXCEPTION_RECORD ExceptionRecord OPTIONAL,
    IN void * ReturnValue
    );

#if defined(_M_IA64) /*IFSTRIP=IGN*/
#define STATUS_LONGJUMP 0x80000026
RtlUnwind2 (
    IN FRAME_POINTERS TargetFrame OPTIONAL,
    IN void * TargetIp OPTIONAL,
    IN PEXCEPTION_RECORD ExceptionRecord OPTIONAL,
    IN void * ReturnValue,
    IN PCONTEXT ContextRecord
    );

PRUNTIME_FUNCTION
RtlLookupFunctionEntry (
     unsigned __int64 ControlPc,
     unsigned __int64 *ImageBase,
     unsigned __int64 *TargetGp
    );

#elif defined(_M_AMD64) /*IFSTRIP=IGN*/
#define STATUS_LONGJUMP 0x80000026
RtlUnwindEx (
    IN void * TargetFrame OPTIONAL,
    IN void * TargetIp OPTIONAL,
    IN PEXCEPTION_RECORD ExceptionRecord OPTIONAL,
    IN void * ReturnValue,
    IN PCONTEXT ContextRecord,
    IN void *HistoryTable
    );

PRUNTIME_FUNCTION
RtlLookupFunctionEntry (
     unsigned __int64 ControlPc,
     unsigned __int64 *ImageBase,
     void             *HistoryTable
    );

VOID
RtlRaiseException (
    IN PEXCEPTION_RECORD ExceptionRecord
    );

#endif

#if defined(_M_ALPHA)
#define STATUS_UNWIND 0xc0000027

void WINAPI
RtlUnwindRfp (
    IN void * TargetRealFrame OPTIONAL,
    IN void * TargetIp OPTIONAL,
    IN PEXCEPTION_RECORD ExceptionRecord OPTIONAL,
    IN void * ReturnValue
    );
#endif

#if defined(_M_PPC)
ULONG WINAPI
RtlVirtualUnwind (
    IN ULONG ControlPc,
    IN PRUNTIME_FUNCTION FunctionEntry,
    IN OUT PCONTEXT ContextRecord,
    OUT PBOOLEAN InFunction,
    OUT PULONG EstablisherFrame,
    IN OUT PVOID ContextPointers OPTIONAL,
    IN ULONG LowStackLimit,
    IN ULONG HighStackLimit
    );

PRUNTIME_FUNCTION
RtlLookupFunctionEntry (
    IN ULONG ControlPc
    );
#endif

#if defined(_M_MPPC)
ULONG WINAPI
RtlVirtualUnwind (
    IN ULONG ControlPc,
    IN PRUNTIME_FUNCTION FunctionEntry,
    IN OUT PCONTEXT ContextRecord,
    OUT PBOOLEAN InFunction,
    OUT PULONG EstablisherFrame,
    IN OUT PVOID ContextPointers OPTIONAL,
    IN ULONG LowStackLimit,
    IN ULONG HighStackLimit
    );

PRUNTIME_FUNCTION
RtlLookupFunctionEntry (
    IN PRUNTIME_FUNCTION RuntimeFunction,
    IN ULONG ControlPc,
    IN ULONG Rtoc
    );

VOID
RtlRaiseException (
    IN PEXCEPTION_RECORD ExceptionRecord
    );
#endif

#ifdef __cplusplus
}
#endif
#endif

#endif /* _NTXCAPI_ */

#endif /* _EHDATA_NONT */

#endif /* ONLY_VALUES */

#pragma pack(pop, ehdata)

#endif /* _INC_EHDATA */