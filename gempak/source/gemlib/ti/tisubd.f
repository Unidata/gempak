	SUBROUTINE TI_SUBD  ( idtarr, jdtarr, iret )
C************************************************************************
C* TI_SUBD								*
C*									*
C* This subroutine subtracts a day from the time in an integer array.	*
C* The input and output arrays may be the same array.			*
C*									*
C* TI_SUBD  ( IDTARR, JDTARR, IRET )					*
C*									*
C* Input parameters:							*
C*	IDTARR (5)	INTEGER		Time array (YYYY,MM,DD,HH,MM)	*
C*									*
C* Output parameters:							*
C*	JDTARR (5)	INTEGER		Time array (YYYY,MM,DD,HH,MM)	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid year		*
C*					 -8 = invalid month		*
C*					 -9 = invalid day		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* K. Tyle/GSC		11/95	Re-implemented using TI_SUBM		*
C* I. Durham/GSC	 9/98   Removed excess comment line		*
C************************************************************************
	INTEGER		idtarr (*), jdtarr (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Subtract 1440 mintues (1 day) from time.
C
	CALL TI_SUBM ( idtarr, 1440, jdtarr, iret )
C*
	RETURN
	END
