	SUBROUTINE AF_AWND  ( isdx, maxsc, iswnd, iewnd, iret )
C************************************************************************
C* AF_AWND								*
C*									*
C* This subroutine locates, decodes, and stores the wind data		*
C* from	within an AIREP report.  The report must have already been	*
C* broken up into "like-type" groups using subroutine AF_BKGP.		*
C* Output includes the indices of the "like-type" groups which contain	*
C* the start and the end of the wind data; these values are set		*
C* to IMISSD if	the search for the data was unsuccessful.		*
C*									*
C* AF_AWND  ( ISDX, MAXSC, ISWND, IEWND, IRET )				*
C*									*
C* Input parameters:							*
C*	ISDX		INTEGER		Index of "like-type" group with	*
C*					which to begin search for data	*
C*	MAXSC		INTEGER		Maximum number of "like-type"	*
C*					groups to search following ISDX	*
C*									*
C* Output parameters:							*
C*	ISWND		INTEGER		Index of "like-type" group which*
C*					contains start of data		*
C*	IEWND		INTEGER		Index of "like-type" group which*
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
	iswnd = IMISSD
	iewnd = IMISSD
C
	ii = isdx
	maxii = IEDX ( ii, maxsc, nflds )
C
	DO WHILE  ( ( ii .le. maxii ) .and. ( iswnd .eq. IMISSD ) )
C
C*	    Look for the wind data as a six-digit "like-type" group
C*	    that was not concatenated with any other "like-type" groups
C*	    within the original report.
C
	    IF  (  ( itypsf ( ii ) .eq. NMR ) .and.
     +		   ( lensf ( ii ) .eq. 6 ) .and.
     +		   ( irfnsf ( ii - 1 ) .ne. irfnsf ( ii ) )  )  THEN
		IF  ( ( ii .eq. nflds ) .or.
     +		     ( irfnsf ( ii + 1 ) .ne. irfnsf ( ii ) ) )  THEN
C
C*		    The wind data has been found.
C
		    iswnd = ii
		    iewnd = ii
C
C*		    Decode and store the wind data.
C
		    CALL AF_WIND  ( fields ( iswnd ) (1:3),
     +				    fields ( iswnd ) (4:6),
     +				    ierwnd )
		END IF
	    END IF
	    IF  ( ( iswnd .eq. IMISSD ) .and.
     +	    	     ( ( ii + 2 ) .le. maxii ) )  THEN
C
C*		Look for the wind data as two numeric "like-type"
C*		groups separated by a "like-type" group consisting
C*		solely of a "/".
C
		IF  (  ( itypsf ( ii ) .eq. NMR ) .and.
     +		       ( itypsf ( ii + 2 ) .eq. NMR ) .and.
     +			( lensf ( ii + 1 ) .eq. 1 ) .and.
     +			( fields ( ii + 1 ) (1:1) .eq. '/' )  )  THEN
C
C*		    The wind data has been found.
C
		    iswnd = ii
		    iewnd = ii + 2
C
C*		    Decode and store the wind data.
C
		    CALL AF_WIND
     +			( fields ( iswnd ) ( 1 : lensf ( iswnd ) ),
     +			  fields ( iewnd ) ( 1 : lensf ( iewnd ) ),
     +			  ierwnd )
		END IF
	    END IF
	    ii = ii + 1
	END DO
C*
	RETURN
	END
