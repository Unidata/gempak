	SUBROUTINE G2T_OUTLOOK ( ktype, lunb, nt, iret )
C************************************************************************
C* G2T_OUTLOOK								*
C*									*
C* This subroutine creates OUTLOOK part of the OFF text.  The text is	*
C* determined by the "worst" scenario of the covered period with no 	*
C* trending.								*
C*									*
C* G2T_OUTLOOK ( KTYPE, LUNB, NT, IRET )				*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Data type			*
C*					 1 = Wave			*
C*					 2 = Wind			*
C*	LUNB		INTEGER		LUN for table file		*
C*	NT		INTEGER		Nth time step			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/07	Created					*
C* T. Lee/SAIC		11/07	Added NT for combining periods		*
C* T. Lee/SAIC		11/07	Called G2T_GAP				*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER	ooo*256, end*7
	CHARACTER	wd_r(2)*2, wd_e(2)*2
	LOGICAL		dirflg, E1, E2
	INCLUDE		'EQUAL.FNC'
C------------------------------------------------------------------------
	iret = 0
	dirflg = .false.
C
	ooo = ' '	
	end = ' '
	nt1 = nt + 1
	nt2 = nt + 2
C
	itrnd = 1
	CALL G2T_ABC ( ktype, nt, itrnd, ier )
C
	itrnd = 2
	CALL G2T_ABC ( ktype, nt1, itrnd, ier )
C
	min_d1  = mxn_d  ( ktype, 1, 1 )
	max_d1  = mxn_d  ( ktype, 1, 2 )
	min_de1 = mxn_de ( ktype, 1, 1 )
	max_de1 = mxn_de ( ktype, 1, 2 )
	E1 = eflag_d ( ktype, 1 )
C
	min_d2  = mxn_d  ( ktype, 2, 1 )
	max_d2  = mxn_d  ( ktype, 2, 2 )
	min_de2 = mxn_de ( ktype, 2, 1 )
	max_de2 = mxn_de ( ktype, 2, 2 )
	E2 = eflag_d ( ktype, 2 )
C
	IF ( ktype .eq. 2 .and. LT ( max_d1, max_d2 ) )  THEN
	    wdir_d ( 1, 1 ) = wdir_d ( 1, 2 )
	    wdir_d ( 2, 1 ) = wdir_d ( 2, 2 )
	END IF
	min_d1 = AMIN0 ( min_d1, min_d2 )
	max_d1 = AMAX0 ( max_d1, max_d2 )
C
	IF ( E1 .and. E2 )  THEN
	    IF ( LT ( max_de1, max_de2 ) )  THEN
		id_d ( ktype, 1 ) = id_d ( ktype, 2 )
		wdir_de ( 1, 1 ) = wdir_de ( 1, 2 )
		wdir_de ( 2, 1 ) = wdir_de ( 2, 2 )
	    END IF
	    min_de1 = AMIN0 ( min_de1, min_de2 )
	    max_de1 = AMAX0 ( max_de1, max_de2 )
	  ELSE IF ( E2 )  THEN
	    IF ( max_de2 .gt. max_d1 )  THEN
		min_de1 = min_de2
		max_de1 = max_de2
		id_d ( ktype, 1 ) = id_d ( ktype, 2 )
		E1 = .true.
		IF ( ktype .eq. 2 )  THEN
		    wdir_de ( 1, 1 ) = wdir_de ( 1, 2 )
		    wdir_de ( 2, 1 ) = wdir_de ( 2, 2 )
		END IF
	    END IF
	  ELSE IF ( E1 )  THEN
	    IF ( max_de1 .le. max_d1 )  THEN
		wd_r ( 1 ) =  wdir_d ( 1, 1 )
		wd_r ( 2 ) =  wdir_d ( 2, 1 )
		wd_e ( 1 ) =  wdir_de ( 1, 1 )
		wd_e ( 2 ) =  wdir_de ( 2, 1 )
 		dirflg =  ( max_d1 .le. LGHTS ) .or.
     +                ( CNE ( wd_r ( 1 ), wd_e ( 1 ) ) .and.
     +                  CNE ( wd_r ( 1 ), wd_e ( 2 ) ) ) .or.
     +                ( CNE ( wd_r ( 2 ), wd_e ( 1 ) ) .and.
     +                  CNE ( wd_r ( 2 ), wd_e ( 2 ) ) )
		IF ( .not. dirflg .or. ( max_de1 .lt. max_d1 ) )  THEN
		    min_de1  = IMISSD
		    max_de1  = IMISSD
		    E1 = .false.
		    id_d ( ktype, 1 ) = ' '
		    IF ( ktype .eq. 2 )  THEN
		        wdir_de ( 1, 1 ) = ' '
		        wdir_de ( 2, 1 ) = ' '
		    END IF
		END IF
	    END IF
	END IF
C
	itrnd = 2
	CALL G2T_ABC ( ktype, nt2, itrnd, ier )
C
	min_d2  = mxn_d  ( ktype, 2, 1 )
	max_d2  = mxn_d  ( ktype, 2, 2 )
	min_de2 = mxn_de ( ktype, 2, 1 )
	max_de2 = mxn_de ( ktype, 2, 2 )
	E2 =  eflag_d ( ktype, 2 )
C
	IF ( ktype .eq. 2 .and. LT ( max_d1, max_d2 ) )  THEN
	    wdir_d ( 1, 1 ) = wdir_d ( 1, 2 )
	    wdir_d ( 2, 1 ) = wdir_d ( 2, 2 )
	END IF
C
	min_d1 = AMIN0 ( min_d1, min_d2 )
	max_d1 = AMAX0 ( max_d1, max_d2 )
C
	IF ( E1 .and. E2 )  THEN
	    IF ( LT ( max_de1, max_de2 ) )  THEN
		id_d ( ktype, 1 ) = id_d ( ktype, 2 )
		wdir_de ( 1, 1 ) = wdir_de ( 1, 2 )
		wdir_de ( 2, 1 ) = wdir_de ( 2, 2 )
	    END IF
	    min_de1 = AMIN0 ( min_de1, min_de2 )
	    max_de1 = AMAX0 ( max_de1, max_de2 )
	  ELSE IF ( E2 )  THEN
	    IF ( max_de2 .gt. max_d1 )  THEN
		min_de1 = min_de2
		max_de1 = max_de2
		id_d ( ktype, 1 ) = id_d ( ktype, 2 )
		E1 = .true.
		IF ( ktype .eq. 2 )  THEN
		    wdir_de ( 1, 1 ) = wdir_de ( 1, 2 )
		    wdir_de ( 2, 1 ) = wdir_de ( 2, 2 )
		END IF
	    END IF
	  ELSE IF ( E1 )  THEN
	    IF ( max_de1 .le. max_d1 )  THEN
		wd_r ( 1 ) =  wdir_d ( 1, 1 )
		wd_r ( 2 ) =  wdir_d ( 2, 1 )
		wd_e ( 1 ) =  wdir_de ( 1, 1 )
		wd_e ( 2 ) =  wdir_de ( 2, 1 )
 		dirflg =  ( max_d1 .le. LGHTS ) .or.
     +                ( CNE ( wd_r ( 1 ), wd_e ( 1 ) ) .and.
     +                  CNE ( wd_r ( 1 ), wd_e ( 2 ) ) ) .or.
     +                ( CNE ( wd_r ( 2 ), wd_e ( 1 ) ) .and.
     +                  CNE ( wd_r ( 2 ), wd_e ( 2 ) ) )
		IF ( .not. dirflg .or.  ( max_de1 .lt. max_d1 ) ) THEN
		    min_de1 = IMISSD
		    max_de1 = IMISSD
		    E1 = .false.
		    id_d ( ktype, 1 ) = ' '
		    IF ( ktype .eq. 2 )  THEN
		        wdir_de ( 1, 1 ) = ' '
		        wdir_de ( 2, 1 ) = ' '
		    END IF
		END IF
	    END IF
	END IF
	mxn_d  ( ktype, 1, 1 ) = min_d1
	mxn_d  ( ktype, 1, 2 ) = max_d1
	eflag_d ( ktype, 1 ) = E1
	IF ( E1 )  THEN
	    mxn_de ( ktype, 1, 1 ) = min_de1
	    mxn_de ( ktype, 1, 2 ) = max_de1
	END IF
C
C*	Further simplify the text
C
	itrnd = 1
	min = mxn_d ( ktype, itrnd, 1 )
	max = mxn_d ( ktype, itrnd, 2 )
	CALL G2T_RANGE ( ktype, max, irange, ier )
	IF ( ABS ( max - irange ) .gt. min )
     +	     mxn_d ( ktype, itrnd, 1 ) = ABS ( max - irange ) 
C
	mine = mxn_de ( ktype, itrnd, 1 )
	maxe = mxn_de ( ktype, itrnd, 2 )
	CALL G2T_RANGE ( ktype, maxe, irange, ier )
	IF ( ABS ( maxe - irange ) .gt. mine )
     +	     mxn_de ( ktype, itrnd, 1 ) = ABS ( maxe - irange ) 
	CALL G2T_TRIM ( ktype, itrnd, iret )
	IF ( .not. eflag_d ( ktype, 1 ) ) id_d ( ktype, 1 ) = ' '
	IF ( E1 )  CALL G2T_GAP ( ktype, ier )
C
	CALL G2T_RANGX ( ktype, lunb, nt, itrnd, ier )
	CALL G2T_APPEX ( ktype, lunb, nt, itrnd, ' ', ier )
C*
	RETURN
	END 
