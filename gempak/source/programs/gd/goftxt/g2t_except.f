	SUBROUTINE G2T_EXCEPT ( ktype, nt, nz, iret )
C************************************************************************
C* G2T_EXCEPT								*
C*									*
C* This subroutine stores the EXCEPT part to common.			*
C*									*
C* G2T_EXCEPT ( KTYPE, NT, IRET )					*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Grid parameter type		*
C*					 1 = wave			*
C*					 2 = wind			*
C*	NT		INTEGER		Nth time step			*
C*	NZ		INTEGER		Nth zone area			*
C*									*
C* Output Parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		03/07						*
C* T. Lee/SAIC		11/07	Added lesser values to EXCEPT		*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	LOGICAL		proces
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Store "Except Portion" information for winds.
C
	proces = .true.
	kk = 2
	mxw_1 = mxval ( ktype, 1, nt )
	mxw_s = mxval_s ( ktype, 1, nt )
C
C*	Store "Except Portion" information.
C
	DO WHILE ( proces ) 
	    IF ( mxw_1 .lt. mxw_s ) THEN
		IF ( mxval_s ( ktype, kk, nt ) .eq.  mxw_s )  THEN 
	    	    CALL G2T_RANGE ( ktype, mxw_s, jrange, ier )
	    	    mnws = mnval_s ( ktype, kk, nt )
		    IF ( ABS ( mxw_s - mnws ) .gt. jrange ) 
     +			mnval ( ktype, kk, nt ) = mxw_s - jrange
		    nbnd  ( ktype, nt )  = kk 
		    fsuba ( ktype, nt ) = .true.
		    subid ( ktype, nt ) = squad ( nz, kk )
		    proces = .false.
		END IF
	      ELSE IF ( mxw_1 .eq. mxw_s .and. mnzflg ( ktype ) )  THEN
		jtemp = mxw_1
		DO ii = 2, nsubzn ( nz )
		    IF ( .not. ERMISS ( FLOAT (mxval (ktype,ii,nt) ) ) 
     +			 .and.
     +			 mxval ( ktype, ii, nt ) .lt. jtemp )  THEN
			kk = ii
			jtemp = mxval ( ktype, ii, nt )
		    END IF
		END DO
		mtemp = mxval ( ktype, kk, nt )
		ntemp = mnval ( ktype, kk, nt )
		CALL G2T_RANGE ( ktype, mtemp, jrange, ier )
		ktemp = mtemp - jrange
		mnval ( ktype, kk, nt ) = AMAX0 ( ntemp, ktemp )
C
		nbnd  ( ktype, nt ) = kk
		fsuba ( ktype, nt ) = .true.
		subid ( ktype, nt ) = squad ( nz, kk )
		proces = .false.
	    END IF
	    kk = kk + 1
	    IF  ( kk .gt. nsubzn ( nz ) ) proces = .false.
	END DO
C*
	RETURN
	END
