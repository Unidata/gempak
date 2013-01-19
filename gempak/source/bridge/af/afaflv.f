	SUBROUTINE AF_AFLV  ( isdx, maxsc, isflv, ieflv, iret )
C************************************************************************
C* AF_AFLV								*
C*									*
C* This subroutine locates, decodes, and stores the flight level data	*
C* from	within an AIREP report.  The report must have already been	*
C* broken up into "like-type" groups using subroutine AF_BKGP.		*
C* Output includes the indices of the "like-type" groups which contain	*
C* the start and the end of the flight level data; these values are set	*
C* to IMISSD if	the search for the data was unsuccessful.		*
C*									*
C* AF_AFLV  ( ISDX, MAXSC, ISFLV, IEFLV, IRET )				*
C*									*
C* Input parameters:							*
C*	ISDX		INTEGER		Index of "like-type" group with	*
C*					which to begin search for data	*
C*	MAXSC		INTEGER		Maximum number of "like-type"	*
C*					groups to search following ISDX	*
C*									*
C* Output parameters:							*
C*	ISFLV		INTEGER		Index of "like-type" group which*
C*					contains start of data		*
C*	IEFLV		INTEGER		Index of "like-type" group which*
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
	isflv = IMISSD
	ieflv = IMISSD
C
C*	Locate the flight level data.
C
	ii = isdx
	maxii = IEDX ( ii, maxsc, nflds )
C
	DO WHILE  (  ( ( ii + 1 ) .le. maxii ) .and.
     +			( isflv .eq. IMISSD )  )
	    IF  (  ( lensf ( ii ) .eq. 1 ) .and.
     +		    ( fields ( ii ) (1:1) .eq. 'F' ) .and.
     +		    ( itypsf ( ii + 1 ) .eq. NMR )  )  THEN
C
C*		The flight level data has been found as a "like-type"
C*		group consisting of "F" followed by a numeric
C*		"like-type" group.
C
		isflv = ii
	    END IF
	    ii = ii + 1
	END DO
C
	IF  ( isflv .ne. IMISSD )  THEN
	    ieflv = isflv + 1
C
C*	    Decode and store the flight level data.
C
	    CALL AF_FLVL
     +		( fields ( ieflv ) ( 1 : lensf ( ieflv ) ),
     +		  ierflv )
	END IF
C*
	RETURN
	END
