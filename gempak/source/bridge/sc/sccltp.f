	SUBROUTINE SC_CLTP  ( stcltp, cltp, iret )
C************************************************************************
C* SC_CLTP							        *
C*								        *
C* This subroutine decodes low, middle, and/or high cloud types from  	*
C* the remarks section of an SCD report.				*
C*								        *
C* SC_CLTP  ( STCLTP, CLTP, IRET )			       	        *
C*								        *
C* Input parameters:						        *
C*	STCLTP		CHAR*		Possible cloud string		*
C*								        *
C* Output parameters:						        *
C*      CLTP (*)	REAL 		FMH-7 cloud type array          *
C*	IRET		INTEGER		Return code		        *
C*					  0 = normal return 	        *
C*					 14 = decode error 	        *
C**								        *
C* Log:							       	  	*
C* D. Kidwell/NCEP	 3/97	Based on MT_CLTP                        *
C* A. Hardy/GSC         12/97   Added sccmn.cmn for interface           *
C* D. Kidwell/NCEP	 6/98	Cleaned up prologue                     *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE		'sccmn.cmn'
C*
	CHARACTER*(*) 	stcltp
	REAL		cltp (*)
C*
C------------------------------------------------------------------------
	iret = 0
C
	DO i = 1, 3
	    CALL ST_CRNM ( stcltp ( i:i ), cltp ( i ), jret )
	    IF ( jret .eq. -2 ) THEN
	        IF ( stcltp ( i:i ) .eq. '/' ) THEN
		    cltp ( i ) = RMISSD
	          ELSE
		    iret = 14
	        END IF
	    END IF
	END DO
C*
	RETURN
	END
