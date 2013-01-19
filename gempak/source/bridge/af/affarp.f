	SUBROUTINE AF_FARP  ( bullx, lenbx, ibxptr, report, lenr, iret )
C************************************************************************
C* AF_FARP                                                              *
C*									*
C* This subroutine locates and returns the next report from within an	*
C* UAXX10 EGRR aircraft bulletin.  These bulletins do not have an '='   *
C* to delineate the end of each report. Upon entry, IBXPTR points to    *
C* the character in the bulletin with which to begin the search for the *
C* next report.								*
C*									*
C* AF_FARP ( BULLX, LENBX, IBXPTR, REPORT, LENR, IRET )		        *
C*									*
C* Input parameters:							*
C*	BULLX		CHAR*		Text portion of bulletin 	*
C*	LENBX		INTEGER		Length of BULLX 		*
C*									*
C* Input and output parameters:						*
C*	IBXPTR		INTEGER		Pointer within BULLX 		*
C*									*
C* Output parameters:							*
C*	REPORT		CHAR*		Report 				*
C*	LENR		INTEGER		Length of report 		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*					 -1 = no more reports in	*
C*					      bulletin			*
C**									*
C* Log:									*
C* A. Hardy/GSC		 3/99	Based on AF_GRPT		        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report, bullx
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for the end of the bulletin.
C
        IF  ( ibxptr .gt. lenbx )  THEN
            iret = -1
            RETURN
        END IF
C
C*	Set the current bulletin pointer as the start of the next
C*	report.
C
        istart = ibxptr
C
C*      Look for the end of the next report.
C
        ipt1 = INDEX ( bullx ( istart : lenbx ), 'KT' )
        IF  ( ipt1 .eq. 0 )  THEN
            iret = -1
            RETURN
        END IF
	iend = istart + ipt1 
        ibxptr = iend + 1
C
C*	Set the output values.
C
	report = bullx ( istart : iend )
	lenr = iend + istart 
C*
	RETURN
	END
