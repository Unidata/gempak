	SUBROUTINE MT_MMT6 ( stmmt6, iret )
C************************************************************************
C* MT_MMT6							        *
C*								        *
C* This subroutine decodes the 6-hourly maximum and minimum 		*
C* temperatures.  The values are stored in common.			*
C*								        *
C* MT_MMT6 ( STMMT6, IRET )				       	        *
C*								        *
C* Input parameters:						        *
C*	STMMT6		CHAR*		Possible temperature string     *
C*									*
C* Output parameters:							*
C*      RIVALS(IRT6XC)	REAL		6 hour maximum temperature      *
C*      RIVALS(IRT6NC)	REAL		6 hour minimum temperature      *
C*	IRET		INTEGER		Return code		        *
C*					   0 = normal return 	        *
C*					  21 = decode error 	        *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	11/95	Original author		 	        *
C* K. Tyle/GSC		 1/97	Reorganized header and comments		*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* D. Kidwell/NCEP       4/98   New interface                           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	stmmt6
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	CALL MT_TPRM ( stmmt6 ( 2:5 ), tmm, iret )
	IF ( iret .eq. 0 ) THEN
	    IF ( stmmt6 ( 1:1 ) .eq. '1' ) THEN
		IF ( ERMISS ( rivals ( irt6xc ) ) ) 
     +	          rivals ( irt6xc ) = tmm
	      ELSE
		IF ( ERMISS ( rivals ( irt6nc ) ) )
     +	          rivals ( irt6nc ) = tmm
	    END IF
	  ELSE
	    iret = 21
	END IF
C*
	RETURN
	END 
