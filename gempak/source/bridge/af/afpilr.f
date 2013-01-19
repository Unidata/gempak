	SUBROUTINE AF_PILR  ( isilyr, ieilyr, afic, hboi, htoi, tpoi,
     +			      iret )
C************************************************************************
C* AF_PILR								*
C*									*
C* This subroutine decodes a layer of icing data from within a PIREP	*
C* report.								*
C*									*
C* AF_PILR  ( ISILYR, IEILYR, AFIC, HBOI, HTOI, TPOI, IRET )	        *
C*									*
C* Input parameters:							*
C*	ISILYR		INTEGER		Index of "like-type" group which*
C*					contains start of layer 	*
C*	IEILYR		INTEGER		Index of "like-type" group which*
C*					contains end of layer	 	*
C*									*
C* Output parameters:							*
C*	AFIC		REAL		Airframe icing 			*
C*	HBOI		REAL		Height of base of icing 	*
C*	HTOI		REAL		Height of top of icing 		*
C*	TPOI		REAL		Type of icing 			*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = could not decode layer 	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		10/96						*
C* J. Ator/NP12		01/97	Don't assume that "RIME" -> "in cloud" 	*
C*				nor that "CLR" -> "in precipitation" 	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		12/98	Initialize tids, aids via DATA stmts	*
C* D. Kidwell/NCEP	 6/99	Added type, icing intensity range       *
C* D. Kidwell/NCEP	 7/99	Removed obsolete (commented) code       *
C* J. Wu/GSC            07/00   Moved INCLUDE 'ERMISS.FNC' before the   *
C*                              DATA statement                          *
C* D. Kidwell/NCEP	 8/05	Allowed blank to separate intens range  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	LOGICAL		gotalt
C*
	PARAMETER	( NTIDS = 6 )
	CHARACTER	tids ( NTIDS )*5
C*
	PARAMETER	( NAIDS = 2 )
	CHARACTER	aids ( NAIDS )*3
C*
	INCLUDE		'ERMISS.FNC'
	INCLUDE		'affnc.fnc'
C*
	DATA		tids
     +			/ 'RIME', 'CLR ', 'CLEAR', 'MXD ', 'MX  ',
     +			  'MIXED' /
C*
	DATA		aids
     +			/ 'ABV', 'BLO' /
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = -1
	ifptr = isilyr
	afic = RMISSD
	hboi = RMISSD
	htoi = RMISSD
	tpoi = RMISSD
C
C*	Scan no further than the first 3 "like-type" groups of this
C*	layer in order to locate an intensity indicator.
C
	ii = ifptr
	maxii = IEDX ( ii, 3, ieilyr )
C
	DO WHILE  (  ( ii .le. maxii ) .and. ( ERMISS ( afic ) )  )
	    CALL AF_ICID  ( fields ( ii ) ( 1 : lensf ( ii ) ),
     +			    afic, iericd )
	    ii = ii + 1
	END DO
C
	IF  ( ERMISS ( afic ) )  THEN
	    RETURN
	END IF
	ifptr = ii
C
	IF  ( ifptr .le. ieilyr )  THEN
	    id = 0
C
C*	    Check whether the next group is an intensity indicator.
C	    
	    CALL AF_ICID ( fields ( ifptr ) ( 1 : lensf ( ifptr ) ), 
     +			   afic2, iericd )
	    IF ( ERMISS ( afic2 ) ) THEN
C
C*	        Does the next "like-type" group consist of a hyphen?
C
	        IF  (  ( lensf ( ifptr ) .eq. 1 ) .and.
     +		       ( fields ( ifptr ) (1:1) .eq. '-' )  )  THEN
C
C*		    The intensity was reported as a range.  The range
C*		    indicator values are
C*			    TRACE-LIGHT     = 2
C*			    LIGHT-MODERATE  = 4
C*			    MODERATE-SEVERE = 7
C
		    CALL AF_ICID  
     +			 ( fields (ifptr + 1) ( 1:lensf (ifptr + 1) ), 
     +			   afic2, iericd )
		    id = 2
		END IF
	      ELSE
		id = 1
	    END IF
	    IF  ( ( .not. ERMISS ( afic  ) ) .and.
     +		  ( .not. ERMISS ( afic2 ) ) ) THEN
	        afic = NINT ( afic + afic2 + 1. ) / 2
	      ELSE IF ( .not. ERMISS ( afic2 ) ) THEN
		afic = afic2
	    END IF
	    ifptr = ifptr + id
	END IF
C
C*	Scan no further than the next 2 "like-type" groups of this
C*	layer in order to locate a type indicator.  This indicator
C*	is optional.
C
	jj = 0
	ii = ifptr
	maxii = IEDX ( ii, 2, ieilyr )
C
	DO WHILE  ( ( ii .le. maxii ) .and. ( jj .eq. 0 ) )
	    CALL ST_FIND  ( fields (ii), tids, NTIDS, jj, ier )
	    ii = ii + 1
	END DO
C
	IF  ( jj .ne. 0 )  THEN
	    ifptr = ii
	    IF ( jj .eq. 1 ) THEN
		tpoi = 1.
	      ELSE IF ( jj .le. 3 ) THEN
		tpoi = 2.
	      ELSE
		tpoi = 3.
	    END IF
	END IF
C
C*	Scan no further than the next 3 "like-type" groups of this
C*	layer in order to locate an altitude indicator.  This indicator
C*	is optional.
C
	gotalt = .false.
	ii = ifptr
	maxii = IEDX ( ii, 3, ieilyr )
C
	DO WHILE  ( ( ii .le. maxii ) .and. ( .not. gotalt ) )
	    CALL ST_FIND  ( fields (ii), aids, NAIDS, jj, ier )
	    IF  ( jj .ne. 0 )  THEN
C
C*		The altitude was reported as having an undefined
C*		boundary below (i.e. "BLO" ) or above (i.e. "ABV" )
C*		a defined level.
C
		gotalt = .true.
		IF  (  ( ( ii + 1 ) .le. ieilyr ) .and.
     +			( itypsf ( ii + 1 ) .eq. NMR )  )  THEN
		    CALL AF_HHFM
     +			( fields ( ii + 1 ) ( 1 : lensf ( ii + 1 ) ),
     +			  rhm, ierhfm )
		    IF  ( .not. ERMISS ( rhm ) )  THEN
			IF  ( jj .eq. 1 )  THEN
			  hboi = rhm 
			ELSE
			  htoi = rhm
			END IF
		    END IF
		END IF
	    ELSE IF  (  ( ( ii + 2 ) .le. ieilyr ) .and.
     +		       ( itypsf ( ii ) .eq. NMR ) .and.
     +		     ( itypsf ( ii + 2 ) .eq. NMR ) .and.
     +		      ( lensf ( ii + 1 ) .eq. 1 )  .and.
     +		     ( fields ( ii + 1 ) (1:1) .eq. '-' )  )  THEN
C
C*		The altitude was reported as being bounded between
C*		two defined levels.
C 
		gotalt = .true.
		CALL AF_HHFM
     +		    ( fields ( ii ) ( 1 : lensf ( ii ) ),
     +		      rhm1, ierhfm )
		CALL AF_HHFM
     +		    ( fields ( ii + 2 ) ( 1 : lensf ( ii + 2 ) ),
     +		      rhm2, ierhfm )
		IF  (  ( .not. ( ERMISS ( rhm1 ) ) ) .and.
     +		       ( .not. ( ERMISS ( rhm2 ) ) )  )  THEN
		    hboi = AMIN1 ( rhm1, rhm2 )
		    htoi = AMAX1 ( rhm1, rhm2 )
		END IF
	    END IF
	    ii = ii + 1
	END DO
C
	iret = 0
C*
	RETURN
	END
