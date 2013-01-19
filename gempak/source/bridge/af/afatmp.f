	SUBROUTINE AF_ATMP  ( isdx, maxsc, istmp, ietmp, iret )
C************************************************************************
C* AF_ATMP								*
C*									*
C* This subroutine locates, decodes, and stores the temperature data	*
C* from	within an AIREP report.  The report must have already been	*
C* broken up into "like-type" groups using subroutine AF_BKGP.		*
C* Output includes the indices of the "like-type" groups which contain	*
C* the start and the end of the temperature data; these values are set	*
C* to IMISSD if	the search for the data was unsuccessful.		*
C*									*
C* AF_ATMP  ( ISDX, MAXSC, ISTMP, IETMP, IRET )				*
C*									*
C* Input parameters:							*
C*	ISDX		INTEGER		Index of "like-type" group with	*
C*					which to begin search for data	*
C*	MAXSC		INTEGER		Maximum number of "like-type"	*
C*					groups to search following ISDX	*
C*									*
C* Output parameters:							*
C*	ISTMP		INTEGER		Index of "like-type" group which*
C*					contains start of data		*
C*	IETMP		INTEGER		Index of "like-type" group which*
C*					contains end of data		*
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
	INCLUDE		'affnc.fnc'
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = 0
	istmp = IMISSD
	ietmp = IMISSD
C
C*	Locate the temperature data.
C
	ii = isdx
	maxii = IEDX ( ii, maxsc, nflds )
C
	DO WHILE  (  ( ( ii + 1 ) .le. maxii ) .and.
     +			( istmp .eq. IMISSD )  )
	    IF  ( itypsf ( ii + 1 ) .eq. NMR )  THEN
		IF  (  ( itypsf ( ii ) .eq. ALPHA )  .and.
     +			( ( fields ( ii ) (1:1) .eq. 'M' ) .or.
     +			  ( fields ( ii ) (1:1) .eq. 'P' ) )  )  THEN
		    IF  (  ( lensf ( ii ) .eq. 1 )  .or.
     +			   ( ( lensf ( ii ) .eq. 2 ) .and.
     +			     ( fields ( ii ) (2:2) .eq. 'S' ) )  )  THEN
C
C*			The temperature data has been found as a
C*			"like-type" group consisting of "PS", "P",
C*			"MS", or "M" followed by a numeric "like-type"
C*			group.
C
			istmp = ii
		    END IF
		END IF
		IF  ( istmp .eq. IMISSD )  THEN
		    IF  (  ( itypsf ( ii ) .eq. NALNMR ) .and.
     +			    ( lensf ( ii ) .eq. 1 ) .and.
     +			   ( ( fields ( ii ) (1:1) .eq. '+' ) .or.
     +			     ( fields ( ii ) (1:1) .eq. '-' ) )  )  THEN
C
C*			The temperature data has been found as a
C*			"like-type" group consisting of "+" or "-"
C*			followed by a numeric "like-type" group.
C
			istmp = ii
		    END IF
		END IF
	    END IF
	    ii = ii + 1
	END DO
C
	IF  ( istmp .ne. IMISSD )  THEN
	    ietmp = istmp + 1
C
C*	    Decode and store the temperature data.
C
	    CALL AF_TMPC
     +		( fields ( istmp ) ( 1 : lensf ( istmp ) ),
     +		  fields ( ietmp ) ( 1 : lensf ( ietmp ) ),
     +		  iertmp )
	END IF
C*
	RETURN
	END
