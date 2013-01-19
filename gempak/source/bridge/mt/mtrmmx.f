	SUBROUTINE MT_RMMX ( strrmk, numrmk, iret )
C************************************************************************
C* MT_RMMX			 				        *
C*								        *
C* This subroutine determines whether the mean sea level pressure has   *
C* been reported in the remarks fields of a Mexican METAR report.  The  *
C* format for Mexican remarks differs from that for the US.             *
C*								        *
C* MT_RMMX ( STRRMK, NUMRMK, IRET )					*
C*								        *
C* Input parameters:						        *
C*	STRRMK(*)	CHAR*		Array of remarks	        *
C*	NUMRMK		INTEGER		Number of entries in strrmk     *
C*								        *
C* Output parameters:						        *
C*	IRET		INTEGER		Return code		        *
C*					   0 = normal return 	        *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	 3/00	                                        *
C* D. Kidwell/NCEP	 4/00	Allowed alternate forms with SLP        *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	strrmk ( * )
C*
	CHARACTER 	remark*40, slp*3
C*
        INCLUDE         'ERMISS.FNC'
C-----------------------------------------------------------------------
 	iret = 0
	irmk = 1
	slp  = ' '
C
	DO WHILE ( irmk .le. numrmk )
	    remark = strrmk ( irmk )
	    lenrmk = INDEX ( remark, ' ' ) - 1
	    IF ( lenrmk .eq. 6 ) THEN
		IF ( ( remark ( :3 ) .eq. 'SLP' ) .or.
     +		     ( remark ( :3 ) .eq. 'SPL' ) ) THEN
		    slp = remark ( 4:6 )
		END IF
	      ELSE IF ( lenrmk .eq. 3 ) THEN
		IF ( ( remark ( :3 ) .eq. 'SLP' ) .or.
     +		     ( remark ( :3 ) .eq. 'SPL' ) ) THEN
		  ELSE
		    slp = remark ( :3 )
		END IF
	    END IF
	    IF ( slp ( :1 ) .ne. ' ' ) THEN
C
C*		Try to decode this field as a mean sea level pressure.
C
		IF ( slp .eq. '///' ) RETURN
	        IF ( ERMISS ( rivals ( irpmsl ) ) ) THEN
		    CALL MT_SLP ( slp, ier ) 
		    IF ( ier .eq. 0 ) THEN
		        RETURN
		      ELSE
			slp = ' '
		    END IF
	        END IF
	    END IF
C
	    irmk = irmk + 1
	END DO
C*
	RETURN
	END
