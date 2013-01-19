	SUBROUTINE HTXSY  ( itype, isym, ijust, ixoff, iyoff, rotn, 
     +			    x, y, cchar, lens, iret )
C************************************************************************
C* HTXSY - VG								*
C*									*
C* This subroutine draws hardware text and symbol on the device.	*
C*									*
C* HTXSY  ( ITYPE, ISYM, IJUST, IXOFF, IYOFF, ROTN, X, Y,		*
C*						CCHAR, LENS, IRET )	*
C*									*
C* Input parameters:							*
C*	ITYPE 		INTEGER		Symbol type   			*
C*	ISYM		INTEGER		Symbol number 			*
C*	IJUST		INTEGER		Text justification (-1, 0, 1)	*
C*	IXOFF		INTEGER		X offset			*
C*	IYOFF		INTEGER		Y offset			*
C*	ROTN		REAL		rotation			*
C*	X 		REAL		X coordinate 			*
C*	Y 		REAL		Y coordinate			*
C*	CCHAR		CHAR*		Text				*
C*	LENS		INTEGER		Length of text			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* E. Safford/GSC	 6/97		Initial coding (copied gn/htxsy)*
C* E. Safford/GSC	 7/97		Added ijust, offsets and rotn   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*)	cchar
C*
	CHARACTER       txtout*160
C------------------------------------------------------------------------
	iret = NORMAL
C*
        CALL ST_NULL ( cchar, txtout, lens, ier )
	CALL VTXSY   ( itype, isym, ijust, ixoff, iyoff, rotn, x, y, 
     +		       txtout, lens, iret )
C*
        RETURN
        END
