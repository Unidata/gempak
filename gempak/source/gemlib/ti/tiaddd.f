	SUBROUTINE TI_ADDD  ( idtarr, jdtarr, iret )
C************************************************************************
C* TI_ADDD								*
C*									*
C* This subroutine adds a day to the time in an integer array.  The	*
C* input and output arrays may be the same array.			*
C*									*
C* TI_ADDD  ( IDTARR, JDTARR, IRET )					*
C*									*
C* Input parameters:							*
C*	IDTARR (5)	INTEGER		Time array (YYYY,MM,DD,HH,MM)	*
C*									*
C* Output parameters:							*
C*	JDTARR (5)	INTEGER		Time array (YYYY,MM,DD,HH,MM)	*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = invalid year		*
C*					 -8 = invalid month		*
C*					 -9 = invalid day		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	 9/88	Fixed leap year				*
C* K. Tyle/GSC		11/95	Re-implemented using TI_ADDM		*
C* I. Durham/GSC	 9/98   Deleted excess comment line		*
C************************************************************************
	INTEGER		idtarr (*), jdtarr (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Add 1440 minutes (1 day) to time.
C
	CALL TI_ADDM (idtarr, 1440, jdtarr, iret)
C*
	RETURN
	END
