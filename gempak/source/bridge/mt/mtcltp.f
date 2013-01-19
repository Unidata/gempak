	SUBROUTINE MT_CLTP ( stcltp, iret )
C************************************************************************
C* MT_CLTP							        *
C*								        *
C* This subroutine will decode low, middle, and/or high cloud types	*
C* from the remarks section of a METAR report.				*
C* The cloud type values are stored in common.		        	*
C*								        *
C* MT_CLTP ( STCLTP, IRET )				       	        *
C*								        *
C* Input parameters:						        *
C*	STCLTP		CHAR*		Possible cloud string		*
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRCTYL)	REAL		Low-level cloud type WMO 0513   *
C*	RIVALS(IRCTYM)	REAL		Mid-level cloud type WMO 0515   *
C*	RIVALS(IRCTYH)	REAL		High-level cloud type WMO 0509  *
C*	IRET		INTEGER		Return code		        *
C*					  0 = normal return 	        *
C*					 14 = decode error 	        *
C**								        *
C* Log:							       	  	*
C* D. Kidwell/NCEP	11/95	Original author			        *
C* K. Tyle/GSC		 1/97	Reorganized header and comments		*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* D. Kidwell/NCEP       6/97   Replaced ST_CRNM iwth ST_INTG           *
C* D. Kidwell/NCEP       4/98   New interface                           *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	stcltp
C-----------------------------------------------------------------------
	iret = 0
	DO i = 3, 5
	    IF ( stcltp ( i:i ) .ne. '/' ) THEN
	        CALL ST_INTG ( stcltp ( i:i ), icltp, jret )
		IF ( jret .eq. 0 ) THEN
		    cltyp = FLOAT ( icltp )
		    IF ( i .eq. 3 ) THEN
			rivals ( irctyl ) = cltyp
		      ELSE IF ( i .eq. 4 ) THEN
			rivals ( irctym ) = cltyp
		      ELSE
			rivals ( irctyh ) = cltyp
		    END IF
	          ELSE
		    iret = 14
	        END IF
	    END IF
	END DO
C*
	RETURN
	END
