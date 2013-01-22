	SUBROUTINE AF_PTLR  ( istlyr, ietlyr, dgot, hbot, htot, fqot,
     +			      tpot, iret )
C************************************************************************
C* AF_PTLR								*
C*									*
C* This subroutine decodes a layer of turbulence data from within a	*
C* PIREP report.							*
C*									*
C* AF_PTLR ( ISTLYR, IETLYR, DGOT, HBOT, HTOT, FQOT, TPOT, IRET )       *
C*									*
C* Input parameters:							*
C*	ISTLYR		INTEGER		Index of "like-type" group which*
C*					contains start of layer		*
C*	IETLYR		INTEGER		Index of "like-type" group which*
C*					contains end of layer		*
C*									*
C* Output parameters:							*
C*	DGOT		REAL		Degree of turbulence 		*
C*	HBOT		REAL		Height of base of turbulence 	*
C*	HTOT		REAL		Height of top of turbulence 	*
C*	FQOT		REAL		Frequency of turbulence		*
C*	TPOT		REAL		Type of turbulence 		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = could not decode layer 	*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		12/98	Initialize tids, aids via DATA stmts	*
C* D. Kidwell/NCEP	 6/99	Added frequency, type, intensity range  *
C* D. Kidwell/NCEP	 7/99	Added check on dgot value of 7          *
C* D. Kidwell/NCEP	 8/99	Do not set fqot when dgot = 0           *
C* D. Kidwell/NCEP	 4/00	Allow ' TO ' to define a range          *
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *
C*                              DATA statement                          *
C* D. Kidwell/NCEP	 4/03	Allow ' OCNL ' to define a range        *
C* A. Hardy/NCEP	 8/03	Added check for '-OCNL'			*
C* D. Kidwell/NCEP	 8/05	Allowed blank to separate intens range  *
C* S. Jacobs/NCEP	 9/12	Check for MTN and reset the intensity	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	LOGICAL		gotalt, mtnflg
C*
	PARAMETER	( NTIDS = 3 )
	CHARACTER	tids ( NTIDS )*4
C*
	PARAMETER	( NAIDS = 2 )
	CHARACTER	aids ( NAIDS )*3
C*
	PARAMETER	( NFIDS = 6 )
	CHARACTER	fids ( NFIDS )*3
C*
	INCLUDE		'ERMISS.FNC'
	INCLUDE		'affnc.fnc'
C*
	DATA		tids
     +			/ 'CAT ', 'CHOP', 'LLWS' /
C*
	DATA		aids
     +			/ 'ABV', 'BLO' /
C*
	DATA		fids
     +			/ 'CON', 'STE', 'INT', 'OCN', 'OCA', 'ISO' /
C-----------------------------------------------------------------------
C
C*	Initialize variables.
C
	iret = -1
	ifptr = istlyr
	dgot = RMISSD
	hbot = RMISSD
	htot = RMISSD
	fqot = RMISSD
	tpot = RMISSD
	mtnflg = .false.
C
C*	Scan no further than the first "like-type" group of this
C*	layer in order to locate a frequency indicator.
C
	CALL ST_FIND ( fields ( ifptr ) ( 1:3 ), fids, NFIDS, jj, ier )
	IF ( jj .gt. 0 ) THEN
	    ifptr = ifptr + 1
	    IF ( jj .le. 2 ) THEN
		fqot = 3.
	      ELSE IF ( jj .le. 3 ) THEN
		fqot = 2.
	      ELSE
		fqot = 1.
	    END IF
	END IF
C
C*	Scan no further than the first 3 "like-type" groups of this
C*	layer in order to locate an intensity indicator.
C
	ii = ifptr
	maxii = IEDX ( ii, 3, ietlyr )
	DO WHILE  (  ( ii .le. maxii ) .and. ( ERMISS ( dgot ) )  )
	    CALL AF_TBID  ( fields ( ii ) ( 1 : lensf ( ii ) ),
     +			    dgot, iertbd )
	    ii = ii + 1
	END DO
	IF  ( ERMISS ( dgot ) )  THEN
C
C*	    Check for low level wind shear (LLWS) before returning.
C
	    IF ( ifptr .le. ietlyr ) THEN
		IF ( fields ( ifptr ) ( 1:4 ) .eq. tids ( NTIDS ) ) THEN
                    tpot = NTIDS
		    iret = 0
		END IF
	    END IF
	    RETURN
	END IF
	ifptr = ii
C
	IF  ( ifptr .le. ietlyr )  THEN
	    id = -1
C
C*	    Check whether the next group is an intensity indicator.
C
	    CALL AF_TBID ( fields ( ifptr ) ( 1 : lensf ( ifptr ) ),
     +		           dgot2, iertbd )
	    IF  ( fields(ifptr)(1:lensf(ifptr)) .eq. 'MTN' )  THEN
		mtnflg = .true.
	    END IF
	    IF ( ERMISS ( dgot2 ) ) THEN
C
C*	        Does the next "like-type" group consist of a hyphen or
C*	        TO or OCNL?
C
	        IF  ( ( ( lensf ( ifptr ) .eq. 1 ) .and.
     +		        ( fields ( ifptr ) (1:1) .eq. '-' ) ) .or.
     +                ( ( lensf ( ifptr ) .eq. 2 ) .and.
     +		        ( fields ( ifptr ) (1:2) .eq. 'TO' ) ) .or.
     +                ( ( lensf ( ifptr ) .eq. 4 ) .and.
     +		        ( fields ( ifptr ) (1:4) .eq. 'OCNL' ) ) )  THEN
C
C*		    The intensity was reported as a range.
C*		    The range indicator values are
C*			    LIGHT-MODERATE	= 3
C*			    MODERATE-SEVERE	= 5
C
C*	            Check for '-OCNL' as part of the turbulence values.
C
		    id = 1
                    IF ( fields ( ifptr ) (1:1) .eq. '-' ) THEN
		        IF ( ( fields ( ifptr + 1 ) .eq. 'OCNL' ) .and.
     +			     ( lensf ( ifptr + 1 ) .eq. 4 ) ) THEN
		            id = 2
                        END IF
                    END IF
C
		    CALL AF_TBID
     +		         ( fields ( ifptr+id ) ( 1:lensf ( ifptr+id ) ),
     +		           dgot2, iertbd )
		END IF
	      ELSE
		id = 0
	    END IF
	    IF  ( ( .not. ERMISS ( dgot  ) ) .and.
     +		  ( .not. ERMISS ( dgot2 ) ) ) THEN
		dgot = NINT ( dgot + dgot2 ) / 2
		IF ( NINT ( dgot ) .eq. 7 ) THEN
		    dgot = 8.
		  ELSE IF ( NINT ( dgot ) .eq. 1 ) THEN
		    dgot = 2.
		END IF
	      ELSE IF ( .not. ERMISS ( dgot2 ) ) THEN
		dgot = dgot2
	    END IF
	    ifptr = ifptr + id + 1
	END IF
C
C*	Scan no further than the next 2 "like-type" groups of this
C*	layer in order to locate a type indicator.  This indicator
C*	is optional.
C
	jj = 0
	ii = ifptr
	maxii = IEDX ( ii, 2, ietlyr )
	DO WHILE  ( ( ii .le. maxii ) .and. ( jj .eq. 0 ) )
	    CALL ST_FIND  ( fields (ii), tids, NTIDS, jj, ier )
	    ii = ii + 1
	END DO
C
	IF  ( jj .ne. 0 )  THEN
	    tpot  = FLOAT ( jj )
	    ifptr = ii
	END IF
C
C*	Scan no further than the next 3 "like-type" groups of this
C*	layer in order to locate an altitude indicator.  This indicator
C*	is optional.
C
	gotalt = .false.
	ii = ifptr
	maxii = IEDX ( ii, 3, ietlyr )
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
		IF  (  ( ( ii + 1 ) .le. ietlyr ) .and.
     +			( itypsf ( ii + 1 ) .eq. NMR )  )  THEN
		    CALL AF_HHFM
     +			( fields ( ii + 1 ) ( 1 : lensf ( ii + 1 ) ),
     +			  rhm, ierhfm )
		    IF  ( .not. ERMISS ( rhm ) )  THEN
			IF  ( jj .eq. 1 )  THEN
			  hbot = rhm 
			ELSE
			  htot = rhm
			END IF
		    END IF
		END IF
	    ELSE IF  (  ( ( ii + 2 ) .le. ietlyr ) .and.
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
		    hbot = AMIN1 ( rhm1, rhm2 )
		    htot = AMAX1 ( rhm1, rhm2 )
		END IF
	    END IF
	    ii = ii + 1
	END DO
C
	IF  ( mtnflg )  THEN
	    dgot = RMISSD
	    hbot = RMISSD
	    htot = RMISSD
	END IF
C
	iret = 0
C*
	RETURN
	END
