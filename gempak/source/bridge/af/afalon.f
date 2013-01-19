	SUBROUTINE AF_ALON  ( isdx, maxsc, islon, ielon, iret )
C************************************************************************
C* AF_ALON								*
C*									*
C* This subroutine locates, decodes, and stores the longitude data	*
C* from	within an AIREP report.  The report must have already been	*
C* broken up into "like-type" groups using subroutine AF_BKGP.		*
C* Output includes the indices of the "like-type" groups which contain	*
C* the start and the end of the longitude data; these values are set	*
C* to IMISSD if	the search for the data was unsuccessful.		*
C*									*
C* AF_ALON  ( ISDX, MAXSC, ISLON, IELON, IRET )				*
C*									*
C* Input parameters:							*
C*	ISDX		INTEGER		Index of "like-type" group with	*
C*					which to begin search for data	*
C*	MAXSC		INTEGER		Maximum number of "like-type"	*
C*					groups to search following ISDX	*
C*									*
C* Output parameters:							*
C*	ISLON		INTEGER		Index of "like-type" group which*
C*					contains start of data		*
C*	IELON		INTEGER		Index of "like-type" group which*
C*					contains end of data		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* A. Hardy/GSC         05/98   Modified to store 180 degrees long.     *
C* A. Hardy/GSC         05/98   Modified not to read from memory for 180*
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
	islon = IMISSD
	ielon = IMISSD
C
C*	Locate the longitude data.  It is identifiable as a numeric
C*	"like-type" group followed by a "like-type" group consisting
C*	solely of an "E" or an "W".
C
	ii = isdx
	maxii = IEDX ( ii, maxsc, nflds )
C
	DO WHILE  (  ( ( ii + 1 ) .le. maxii ) .and.
     +			( islon .eq. IMISSD )  )
	    IF  (  ( itypsf ( ii ) .eq. NMR ) .and.
     +		   ( lensf ( ii + 1 ) .eq. 1 )  )  THEN
		IF  ( ( fields ( ii + 1 ) (1:1) .eq. 'E' ) .or.
     +		      ( fields ( ii + 1 ) (1:1) .eq. 'W' ) )  THEN
C
C*		    The longitude data has been found.
C
		    islon = ii
		END IF
	    END IF
	    ii = ii + 1
	END DO
C
C*      If the reported longitude is the dateline, most reports do not
C*      have an 'E' or 'W'.  This will set the longitude in the common.
C
	IF ( ( ( fields ( isdx + 2 ) (1:5) .eq. '18000') .or. 
     +       ( fields ( isdx + 2 ) (1:3) .eq. '180') ) .and.
     +       ( maxii .ge. 3 ) ) rivals ( irslon ) = 180
C
	IF  ( islon .ne. IMISSD )  THEN
	    ielon = islon + 1
C
C*	    Decode and store the longitude data.
C
	    CALL AF_SLON  (  fields ( islon ) ( 1 : lensf ( islon ) ),
     +			     fields ( ielon ) (1:1),
     +			     iersln  )
	END IF
C*
	RETURN
	END
