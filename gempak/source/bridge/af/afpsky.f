	SUBROUTINE AF_PSKY  ( report, issky, iesky, iret )
C************************************************************************
C* AF_PSKY								*
C*									*
C* This subroutine decodes and stores the sky cover data from within	*
C* a PIREP report.							*
C*									*
C* AF_PSKY  ( REPORT, ISSKY, IESKY, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PIREP report 			*
C*	ISSKY		INTEGER		Pointer to start of sky cover 	*
C*					data within REPORT 		*
C*	IESKY		INTEGER		Pointer to end of sky cover    	*
C*					data within REPORT 		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRNCLD)	REAL		Number of sky cover levels	*
C*	RIVALS (IRCLAM)	REAL		Cloud amount          		*
C*	RIVALS (IRHCBF)	REAL		Base of cloud in feet  	        *
C*	RIVALS (IRHCTF)	REAL		Top of cloud in feet  	        *
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 6/99	                                        *
C* D. Kidwell/NCEP	 7/99	Changed meters to feet in prologue      *
C* D. Kidwell/NCEP	 8/99	(irhoc.) -> (irhc.f); correct clam val; *
C*				added check for haze                    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report
C*
	INTEGER		islyr ( MXNLYR ), ielyr ( MXNLYR )
	REAL		clam (6)
C*
	DATA		clam / 0., 11., 12., 8., -1., 13. /
C-----------------------------------------------------------------------
	iret = 0
C
C*	Break up the input string into groups of "like-type" in order
C*	to facilitate decoding.
C
	CALL AF_BKGP  ( report ( issky : iesky ), ierbgp )
	IF  ( ierbgp .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	There may be multiple layers of sky cover data reported;
C*	if so, then each layer is separated from the others by a
C*	"like-type" group containing a "/" character.
C*	Separate these layers.
C
	CALL AF_PLYR  ( '/SK', islyr, ielyr, nlyr, ierlyr )
C
C*	Decode and store the sky cover data.
C
	ncld = 0
	DO ii = 1, nlyr
	    CALL AF_PSLR  ( islyr (ii), ielyr (ii),
     +			    iclam, hocb, hoct, ierslr )
	    IF  ( ierslr .eq. 0 )  THEN
		ncld = ncld + 1
		IF ( ( ( iclam .ge. 1 ) .and. ( iclam .le. 4 ) ) .or.
     +		     ( iclam .eq. 6 ) ) THEN
C
C*		    Map the cloud numbers to interface values using
C*		    WMO bufr table 020011 (cloud amount).
C
      		    rivals ( irclam ( ncld ) ) = clam ( iclam )
		  ELSE
      		    rivals ( irclam ( ncld ) ) = RMISSD
		END IF
		rivals ( irhcbf ( ncld ) ) = hocb
		rivals ( irhctf ( ncld ) ) = hoct
	    END IF
	END DO
	rivals ( irncld ) = ncld
C
C*	Look for haze reported as sky cover.
C
	CALL AF_PHAZ ( ier )
C*
	RETURN
	END
