	SUBROUTINE MT_NEXT ( iret )
C************************************************************************
C* MT_NEXT                                                              *
C*                                                                      *
C* This subroutine will increment the pointer in a METAR report and	*
C* check for end of report.  Except for IRET, all variables reside in 	*
C* the MTCMN common block /postpr/.					*
C* 								        *
C* MT_NEXT  ( IRET )			 			        *
C*								        *
C* Input parameters:	 					        *
C*	NUM		INTEGER	   Number of fields to be decoded       *
C*								        *
C* Input and output parameters:					        *
C*	IPTR		INTEGER	   Pointer to string array element      *
C*								        *
C* Output parameters:	 					        *
C*	DONLIN		LOGICAL	   Flag for completed processing        *
C*	IMHERE		INTEGER	   Pointer for next decode rtn to use   *
C*	IRET		INTEGER	   Return code                          *
C*				     0 = normal return	        	*
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	 6/95				                *
C* K. Tyle/GSC		 1/97	Reorganized header and comments		*
C* D. Kidwell/NCEP	 4/98   Revised prologue	                *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C------------------------------------------------------------------------
	iret = 0
	iptr = iptr + 1
	IF ( iptr .gt. num ) THEN
	    donlin = .true.
	    imhere = 0
	END IF
C*
	RETURN
	END
