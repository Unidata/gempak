	SUBROUTINE AF_ATRB  ( isdx, maxsc, istrb, ietrb, iret )
C************************************************************************
C* AF_ATRB								*
C*									*
C* This subroutine locates, decodes, and stores the turbulence data	*
C* from	within an AIREP report.  The report must have already been	*
C* broken up into "like-type" groups using subroutine AF_BKGP.		*
C* Output includes the indices of the "like-type" groups which contain	*
C* the start and the end of the turbulence data; these values are set	*
C* to IMISSD if	the search for the data was unsuccessful.		*
C*									*
C* AF_ATRB  ( ISDX, MAXSC, ISTRB, IETRB, IRET )				*
C*									*
C* Input parameters:							*
C*	ISDX		INTEGER		Index of "like-type" group with	*
C*					which to begin search for data	*
C*	MAXSC		INTEGER		Maximum number of "like-type"	*
C*					groups to search following ISDX	*
C*									*
C* Output parameters:							*
C*	RIVALS (IRNTRB)	REAL		Number of turbulence levels	*
C*	RIVALS (IRDGOT)	REAL		Degree of turbulence		*
C*	RIVALS (IRFQOT)	REAL		Frequency of turbulence		*
C*	RIVALS (IRTPOT)	REAL		Type of turbulence		*
C*	ISTRB		INTEGER		Index of "like-type" group which*
C*					contains start of data		*
C*	IETRB		INTEGER		Index of "like-type" group which*
C*					contains end of data		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* D. Kidwell/NCEP       6/99   Changed turbulence code values          *
C* J. Ator/NCEP		08/99	Initialize dgot to RMISSD		*
C* D. Kidwell/NCEP       2/00   Added call to AF_ATBX                   *
C* D. Kidwell/NCEP       5/00   Added special codes for NWA,JAL,ANA,JAZ *
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *  
C*                              DATA statement                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER	airlin*3, tbcode (9)*5
	LOGICAL		code, tbtabl
	INCLUDE		'ERMISS.FNC'
	INCLUDE		'affnc.fnc'
C*
	DATA		tbcode / 'ZERO', 'ONE', 'TWO', 'THREE', 'FOUR',
     +			         'FIVE', 'SIX', 'SEVEN', 'O' /
C*
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = 0
	istrb = IMISSD
	ietrb = IMISSD
	dgot = RMISSD
	fqot = RMISSD
	tpot = RMISSD
C
C*	Locate the turbulence data.
C
	ii = isdx
	maxii = IEDX ( ii, maxsc, nflds )
	code  = .false.
C
	DO WHILE  (  ( ii .le. maxii ) .and. ( istrb .eq. IMISSD )  )
	    IF  (  ( ( lensf ( ii ) .eq. 2 ) .and.
     +		     ( fields ( ii ) (1:2) .eq. 'TB' ) )
     +			.or.
     +		   ( ( lensf ( ii ) .ge. 4 ) .and.
     +		     ( ( fields ( ii ) (1:4) .eq. 'TURB' ) .or.
     +		       ( fields ( ii ) (1:4) .eq. 'TRBC' ) ) )  ) THEN 
C
C*		The start of the turbulence data has been found as
C*		a "like-type" group consisting of "TB", "TURB", or
C*		"TRBC".
C
		istrb = ii
C
C*		There may or may not be an additional "like-type"
C*		group consisting of "CODE" directly before the
C*		"like-type" group containing the alphabetic or
C*		numeric turbulence indicator.
C
		IF  (  ( ii + 1 ) .le. maxii )  THEN
		    IF  (  ( lensf ( ii + 1 ) .eq. 4 ) .and.
     +			( fields ( ii + 1 ) (1:4) .eq. 'CODE' )  )  THEN
		        ietrb = ii + 2
		        code  = .true.
		      ELSE
		        ietrb = ii + 1
		    END IF
		  ELSE
		    ietrb = ii
		END IF
	    ELSE IF  (  ( lensf ( ii ) .eq. 4 ) .and.
     +			( fields ( ii ) (1:4) .eq. 'CODE' )  )  THEN
C
C*		The turbulence data has been found as a "like-type"
C*		group consisting of "CODE" followed by a "like-type"
C*		group containing the alphabetic or numeric turbulence
C*		indicator.
C
		istrb = ii
		ietrb = ii + 1
		code  = .true.
	    END IF
	    ii = ii + 1
	END DO
C
C*	Was any turbulence data found??
C
	IF  ( ( istrb .eq. IMISSD ) .or. ( ietrb .eq. IMISSD ) )  THEN
	    RETURN
	END IF
	IF  ( ietrb .gt. maxii )  ietrb = maxii
C
C*	Decode the turbulence data.
C
	airlin = civals ( icacid ) ( :3 )
	tbtabl = .false.
C
C*	First check to see if this is an AIREP from an airline which
C*	has its own turbulence code table.
C
	IF ( ( airlin .eq. 'NWA' ) .or. ( airlin .eq. 'JAL' ) .or.
     +	     ( airlin .eq. 'ANA' ) .or. ( airlin .eq. 'JAZ' ) ) THEN
	    IF ( itypsf ( ietrb ) .eq. NMR ) THEN
		CALL ST_INTG ( fields ( ietrb ) ( :lensf ( ietrb ) ),
     +			       idgot, ier )
		IF ( ( ier .eq. 0 ) .and. ( idgot .ge. 0 ) .and.
     +		     ( idgot .le. 7 ) ) THEN
		    tbtabl = .true.
		END IF
	      ELSE IF ( itypsf ( ietrb ) .eq. ALPHA ) THEN
		CALL ST_FIND ( fields ( ietrb ) ( :lensf ( ietrb ) ),
     +			       tbcode, 9, ipos, ier ) 
		IF ( ipos .gt. 0 ) THEN
		    idgot = ipos - 1
		    IF ( idgot .eq. 8 ) idgot = 0
		    tbtabl = .true.
		END IF
	    END IF
	END IF
C
C*	Degree of turbulence values are stored in the interface format
C*	using the following code figures -
C*		0 = NONE
C*		2 = LIGHT
C*		3 = LIGHT TO MODERATE
C*		4 = MODERATE
C*		5 = MODERATE TO SEVERE
C*		6 = SEVERE
C*		8 = EXTREME
C
	IF ( tbtabl ) THEN
C
C*	    Convert the airline turbulence code values to their 
C*	    interface values.
C
	    IF ( idgot .eq. 1 ) THEN
	        dgot = 2.
	        fqot = 1.
	      ELSE IF ( idgot .eq. 7 ) THEN
	        dgot = 8.
	      ELSE
	        dgot = FLOAT ( idgot ) 
	    END IF
	  ELSE
C
C*	    Decode the turbulence either as plain English or, if
C*	    numeric, use WMO code table 0302.
C
	    IF  ( itypsf ( ietrb ) .eq. ALPHA )  THEN
	        CALL AF_TBID ( fields ( ietrb ) ( 1 : lensf ( ietrb ) ),
     +			       dgot, iertbd )
	      ELSE IF  ( itypsf ( ietrb ) .eq. NMR )  THEN
	        CALL ST_INTG ( fields ( ietrb ) ( 1 : lensf ( ietrb ) ),
     +			       idgot, ier )
	        IF  (  ( ier .eq. 0 ) .and. ( idgot .ge. 0 ) .and.
     +		       ( idgot .le. 3 )  )  THEN
		    dgot = FLOAT ( idgot * 2 )
		    code = .true.
	        END IF
	    END IF
C
	    IF ( .not. code ) THEN
C
C*	        There may be more turbulence data to decode.  Check the
C*	        surrounding syntax.
C
	        dgotin = dgot
	        CALL AF_ATBX ( ietrb, 6, dgotin, dgot, fqot, tpot, 
     +			       ier1 )
	        IF ( ERMISS ( dgot ) ) THEN
		    IF ( ier1 .eq. 0 ) THEN
		        fqotsv = fqot
		        tpotsv = tpot
		    END IF
C
C*	            No degree of turbulence data was found after the
C*	            "TB", "TURB" or "TRBC" keyword.  Look at the groups
C*		    preceding the keyword.
C
		    CALL AF_ATBX ( ietrb - 5, 4, dgotin, dgot, fqot,
     +			           tpot, ier2 )
		    IF ( ( ier2 .lt. 0 ) .and. ( ier1 .eq. 0 ) ) THEN
		        fqot = fqotsv
		        tpot = tpotsv
		    END IF
	        END IF
	    END IF
	END IF
C
	IF  ( ERMISS ( dgot ) .and. ERMISS ( fqot ) .and.
     +	      ERMISS ( tpot ) ) THEN
	  ELSE
C
C*	    Store the turbulence data.
C
	    rivals ( irntrb ) = 1
	    rivals ( irdgot (1) ) = dgot
	    rivals ( irfqot (1) ) = fqot
	    rivals ( irtpot (1) ) = tpot
	END IF
C*
	RETURN
	END
