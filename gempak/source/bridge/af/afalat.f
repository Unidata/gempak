	SUBROUTINE AF_ALAT  ( isdx, maxsc, islat, ielat, iret )
C************************************************************************
C* AF_ALAT								*
C*									*
C* This subroutine locates, decodes, and stores the latitude data	*
C* from	within an AIREP report.  The report must have already been	*
C* broken up into "like-type" groups using subroutine AF_BKGP.		*
C* Output includes the indices of the "like-type" groups which contain	*
C* the start and the end of the latitude data; these values are set	*
C* to IMISSD if	the search for the data was unsuccessful.		*
C*									*
C* AF_ALAT  ( ISDX, MAXSC, ISLAT, IELAT, IRET )				*
C*									*
C* Input parameters:							*
C*	ISDX		INTEGER		Index of "like-type" group with	*
C*					which to begin search for data	*
C*	MAXSC		INTEGER		Maximum number of "like-type"	*
C*					groups to search following ISDX	*
C*									*
C* Output parameters:							*
C*	ISLAT		INTEGER		Index of "like-type" group which*
C*					contains start of data		*
C*	IELAT		INTEGER		Index of "like-type" group which*
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
	islat = IMISSD
	ielat = IMISSD
C
C*	Locate the latitude data.  It is identifiable as a numeric
C*	"like-type" group followed by a "like-type" group consisting
C*	solely of an "N" or an "S".
C
	ii = isdx
	maxii = IEDX ( ii, maxsc, nflds )
C
	DO WHILE  (  ( ( ii + 1 ) .le. maxii ) .and.
     +			( islat .eq. IMISSD )  )
	    IF  (  ( itypsf ( ii ) .eq. NMR ) .and.
     +		   ( lensf ( ii + 1 ) .eq. 1 )  )  THEN
		IF  ( ( fields ( ii + 1 ) (1:1) .eq. 'N' ) .or.
     +		      ( fields ( ii + 1 ) (1:1) .eq. 'S' ) )  THEN
C
C*		    The latitude data has been found.
C
		    islat = ii
		END IF
	    END IF
	    ii = ii + 1
	END DO
C
	IF  ( islat .ne. IMISSD )  THEN
	    ielat = islat + 1
C
C*	    Decode and store the latitude data.
C
	    CALL AF_SLAT  (  fields ( islat ) ( 1 : lensf ( islat ) ),
     +			     fields ( ielat ) (1:1),
     +			     ierslt  )
	END IF
C*
	RETURN
	END
