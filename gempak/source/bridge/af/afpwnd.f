	SUBROUTINE AF_PWND  ( report, iswnd, iewnd, iret )
C************************************************************************
C* AF_PWND								*
C*									*
C* This subroutine decodes and stores the wind data from within a	*
C* PIREP report.							*
C*									*
C* AF_PWND  ( REPORT, ISWND, IEWND, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PIREP report 			*
C*	ISWND		INTEGER		Pointer to start of wind data	*
C*					within REPORT 			*
C*	IEWND		INTEGER		Pointer to end of wind data	*
C*					within REPORT 			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
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
	CALL AF_BKGP  ( report ( iswnd : iewnd ), ierbgp )
	IF  ( ierbgp .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	Locate, decode, and store the wind data from within the first 3
C*	"like-type" groups.  It is identifiable as a numeric "like-type"
C*	group of 5 or 6 digits.
C
	DO ii = 1, ( IEDX ( 1, 3, nflds ) )
	    IF  (  ( itypsf ( ii ) .eq. NMR ) .and.
     +		    ( lensf ( ii ) .ge. 5 ) .and.
     +		    ( lensf ( ii ) .le. 6 )  )   THEN
		CALL AF_WIND  ( fields ( ii ) ( 1 : 3 ),
     +				fields ( ii ) ( 4 : lensf ( ii ) ),
     +				ierwnd )
		RETURN
	    END IF
	END DO
C*
	RETURN
	END
