	SUBROUTINE AF_PTMP  ( report, istmp, ietmp, iret )
C************************************************************************
C* AF_PTMP								*
C*									*
C* This subroutine decodes and stores the temperature data from within	*
C* a PIREP report.							*
C*									*
C* AF_PTMP  ( REPORT, ISTMP, IETMP, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PIREP report 			*
C*	ISTMP		INTEGER		Pointer to start of temperature	*
C*					data within REPORT 		*
C*	IETMP		INTEGER		Pointer to end of temperature	*
C*					data within REPORT 		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRTMPC) REAL		Temperature in Celsius          *
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* D. Kidwell/NCEP	 7/99	Added check for Fahrenheit              *
C* J. Ator/NCEP		09/99	Allow 1-digit temperatures		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report
C*
	INCLUDE		'affnc.fnc'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Break up the input string into groups of "like-type" in order
C*	to facilitate decoding.
C
	CALL AF_BKGP  ( report ( istmp : ietmp ), ierbgp )
	IF  ( ierbgp .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	Locate, decode, and store the temperature data from within
C*	the first 3 "like-type" groups.  It is identifiable as a
C*	numeric "like-type" group of at least 1 digit, possibly
C*	preceded by a "like-type" group containing the sign of
C*	the temperature.
C
	DO ii = 1, ( IEDX ( 1, 3, nflds ) )
	    IF  ( ( itypsf ( ii ) .eq. NMR ) .and.
     +		    ( lensf ( ii ) .ge. 1 ) )   THEN
		IF  ( ii .eq. 1 )  THEN
C
C*		    This is the first "like-type" group, so assume
C*		    that the sign of the temperature is positive.
C
		    CALL AF_TMPC
     +			( '+', fields ( ii ) ( 1 : lensf ( ii ) ),
     +			  iertmp )
		ELSE
C
C*		    Assume that the previous "like-type" group
C*		    contains the sign of the temperature.
C
		    CALL AF_TMPC
     +			( fields ( ii - 1 ) ( 1 : lensf ( ii - 1 ) ),
     +			  fields ( ii ) ( 1 : lensf ( ii ) ),
     +			  iertmp )
		END IF
C
C*		Check if temperature was reported in Fahrenheit.
C
		IF ( ( iertmp .eq. 0 ) .and. ( ii .lt. nflds ) ) THEN
		    IF ( ( lensf ( ii + 1 ) .eq. 1 ) .and.
     +			 ( fields ( ii + 1 ) ( 1:1 ) .eq. 'F' ) ) THEN
			rivals ( irtmpc ) = PR_TMFC ( rivals ( irtmpc) )
		    END IF
		END IF
		RETURN
	    END IF
	END DO
C*
	RETURN
	END
