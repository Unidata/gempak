	SUBROUTINE G2T_TRIM2 ( ktype, iret )
C************************************************************************
C* G2T_TRIM2								*
C*									*
C* This subroutine simplifies the RANGE and EXCEPTION in the two 	*
C* trending blocks if |R1-R2| and/or |E1/E2| <= threshold value.	*
C*									*
C* G2T_TRIM2 ( KTYPE, IRET )						*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Data type			*
C*					 1 = Wave			*
C*					 2 = Wind			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/07	Created					*
C* T. Lee/SAIC		07/07	Text simplification for wind		*
C* T. Lee/SAIC		09/07	Added R1E1 -> R1, R1 -> R1E1 cases	*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER	id1*(MAXIDS), id2*(MAXIDS)
	CHARACTER*2	wd_c(2), wd_ce(2)
	LOGICAL		E1, E2, loc_ok, kflag, greatr
C------------------------------------------------------------------------
	iret = 0
	loc_ok = .false.
	greatr = .false.
C
C*	Simplify data from two trending blocks respectively.
C
	itrnd = 1
	CALL G2T_TRIM ( ktype, itrnd, ier ) 
	IF ( .not. eflag_d ( ktype, itrnd ) )id_d ( ktype, itrnd ) = ' '
C
	itrnd = 2
	CALL G2T_TRIM ( ktype, itrnd, ier ) 
	IF ( .not. eflag_d ( ktype, itrnd ) )id_d ( ktype, itrnd ) = ' '
C
C*	Simplify the text between the blocks.
C
	maxw1 = mxn_d  ( ktype, 1, 2 )
	minw1 = mxn_d  ( ktype, 1, 1 )
	mxwe1 = mxn_de ( ktype, 1, 2 )
	mnwe1 = mxn_de ( ktype, 1, 1 )
	id1   = id_d   ( ktype, 1 )
	E1 = eflag_d ( ktype, 1 )
C
	maxw2 = mxn_d  ( ktype, 2, 2 )
	minw2 = mxn_d  ( ktype, 2, 1 )
	mxwe2 = mxn_de ( ktype, 2, 2 )
	mnwe2 = mxn_de ( ktype, 2, 1 )
	id2   = id_d   ( ktype, 2 )
	E2 = eflag_d ( ktype, 2 )
C
	loc_ok = id1 .eq. id2
C
	idif = ABS ( maxw1 - maxw2 )
	IF  ( ktype .eq. 1 )  THEN
	    IF ( E1 .and. E2 ) THEN
		jdif = ABS ( mxwe1 - mxwe2 )
		IF  ( idif .le. MXWDIF .and. jdif .le. MXWDIF .and.
     +		      loc_ok )  THEN
		    mxr = AMAX0 ( mxwe1, mxwe2 )
		    mnr = AMIN0 ( mnwe1, mnwe2 )
C
		    CALL G2T_RANGE ( ktype, mxr, irange, ier ) 
		    IF ( ABS ( mxr - mnr ) .gt. irange ) 
     +			 mnr = mxr - irange
		    itrnd = 1
		    mxn_de ( ktype, itrnd, 2 ) = mxr
		    mxn_de ( ktype, itrnd, 1 ) = mnr
C
		    mxr = AMAX0 ( maxw1, maxw2 )
		    mnr = AMIN0 ( minw1, minw2 )
		    CALL G2T_RANGE ( ktype, mxr, irange, ier ) 
		    IF ( ABS ( mxr - mnr ) .gt. irange )
     +			 mnr = mxr - irange
		    mxn_d  ( ktype, itrnd, 1 ) = mnr
		    mxn_d  ( ktype, itrnd, 2 ) = mxr
		    CALL G2T_TRIM ( ktype, itrnd, ier )
C
		    itrnd = 2
		    DO kk = 1, 2
		        mxn_de ( ktype, itrnd, kk ) = IMISSD
		        mxn_d  ( ktype, itrnd, kk ) = IMISSD
		    END DO
		    eflag_d ( ktype, itrnd ) = .false.
		END IF
C
C*	      Only the 1st trending block has the EXCEPT part.
C*	      Simplification occurs when the EXCEPT part is greater
C*	      than range.
C
	      ELSE IF ( E1 .and. .not. E2 )  THEN
		kdif = ABS ( minw2 - minw1 )
		greatr = mxwe1 .gt. maxw1
		IF  ( idif .le. MXWDIF .and. kdif .le. MNWDIF .and.
     +		      greatr )  THEN
		    mxr = AMAX0 ( maxw1, maxw2 )
		    mnr = AMIN0 ( minw1, minw2 )
		    CALL G2T_RANGE ( ktype, mxr, irange, ier ) 
		    IF ( ABS ( mxr - mnr ) .gt. irange )
     +			 mnr = mxr - irange
		    itrnd = 1
		    mxn_d  ( ktype, itrnd, 1 ) = mnr
		    mxn_d  ( ktype, itrnd, 2 ) = mxr
		    CALL G2T_TRIM ( ktype, itrnd, ier )
C
		    itrnd = 2
		    DO kk = 1, 2
			mxn_d  ( ktype, itrnd, kk ) = IMISSD
		    END DO
		END IF
C
C*	      Only the 2nd trending block has the EXCEPT part.
C
	      ELSE IF ( .not. E1 .and. E2 )  THEN
		kdif  = ABS ( minw2 - minw1 )
		greatr = mxwe2 .gt. maxw2
		IF  ( idif .le. MXWDIF .and. kdif .le. MNWDIF 
     +			    .and. greatr )  THEN
		    mxr = AMAX0 ( maxw1, maxw2 )
		    mnr = AMIN0 ( minw1, minw2 )
		    CALL G2T_RANGE ( ktype, mxr, irange, ier ) 
		    IF ( ABS ( mxr - mnr ) .gt. irange )
     +			 mnr = mxr - irange
		    itrnd = 1
		    mxn_d  ( ktype, itrnd, 1 ) = mnr
		    mxn_d  ( ktype, itrnd, 2 ) = mxr
		    mxn_de ( ktype, itrnd, 1 ) = mnwe2
		    mxn_de ( ktype, itrnd, 2 ) = mxwe2
		    id_d ( ktype, itrnd ) = id_d ( ktype, 2 )
		    eflag_d ( ktype, itrnd ) = .true.
		    CALL G2T_TRIM ( ktype, itrnd, ier )
C
		    itrnd = 2
		    DO kk = 1, 2
			mxn_de ( ktype, itrnd, kk ) = IMISSD
			mxn_d  ( ktype, itrnd, kk ) = IMISSD
		    END DO
		    id_d ( ktype, itrnd ) = ' '
		    eflag_d ( ktype, itrnd ) = .false.
		END IF
C
C*	      Neither trending block has the EXCEPT part.
C
	      ELSE IF ( .not. E1 .and. .not. E2 )  THEN
		kdif = ABS ( minw2 - minw1 )
		idifr = maxw1 - maxw2
		kdifr = minw1 - minw2
		IF ( idifr .gt. 0 .and. kdifr .le. 0 .or.
     +		     idifr .le. 0 .and. kdifr .ge. 0 )  THEN
		    mxr = AMAX0 ( maxw1, maxw2 )
		    mnr = AMIN0 ( minw1, minw2 )
		    itrnd = 1
		    mxn_d  ( ktype, itrnd, 1 ) = mnr
		    mxn_d  ( ktype, itrnd, 2 ) = mxr
C
		    itrnd = 2
		    DO kk = 1, 2
		        mxn_d ( ktype, itrnd, kk ) = IMISSD
		    END DO
		  ELSE IF  ( idif .le. MXWDIF .and. kdif .le. MNWDIF ) 
     +		    THEN
		    mxr = AMAX0 ( maxw1, maxw2 )
		    mnr = AMIN0 ( minw1, minw2 )
		    CALL G2T_RANGE ( ktype, mxr, irange, ier ) 
		    IF ( ABS ( mxr - mnr ) .gt. irange )  
     +			 mnr = mxr - irange
		    itrnd = 1
		    mxn_d  ( ktype, itrnd, 1 ) = mnr
		    mxn_d  ( ktype, itrnd, 2 ) = mxr
C
		    itrnd = 2
		    DO kk = 1, 2
		        mxn_d ( ktype, itrnd, kk ) = IMISSD
		    END DO
		END IF
	    END IF
	  ELSE IF ( ktype .eq. 2 )  THEN
C
C*	    Both trending blocks have the EXCEPT parts.
C
	    CALL G2T_WDIR2 ( kflag, wd_c, wd_ce, ier )
	    IF ( E1 .and. E2 ) THEN
		jdif = ABS ( mxwe1 - mxwe2 )
		IF  ( idif .le. MXSDIF .and. jdif .le. MXSDIF .and.
     +		      loc_ok .and. kflag )  THEN
		    mxr = AMAX0 ( mxwe1, mxwe2 )
		    mnr = AMIN0 ( mnwe1, mnwe2 )
		    CALL G2T_RANGE ( ktype, mxr, irange, ier ) 
		    IF ( ABS ( mxr - mnr ) .gt. irange ) 
     +			 mnr = mxr - irange
		    itrnd = 1
		    mxn_de ( ktype, itrnd, 1 ) = mnr
		    mxn_de ( ktype, itrnd, 2 ) = mxr
C
		    mxr = AMAX0 ( maxw1, maxw2 )
		    mnr = AMIN0 ( minw1, minw2 )
		    CALL G2T_RANGE ( ktype, mxr, irange, ier ) 
		    IF ( ABS ( mxr - mnr ) .gt. irange )
     +			 mnr = mxr - irange
		    mxn_d  ( ktype, itrnd, 1 ) = mnr
		    mxn_d  ( ktype, itrnd, 2 ) = mxr
C
		    DO ii = 1, 2
			wdir_d  ( ii, itrnd ) = wd_c ( ii )
			wdir_de ( ii, itrnd ) = wd_ce ( ii )
		    END DO
		    CALL G2T_TRIM ( ktype, itrnd, ier )
c
		    itrnd = 2
		    DO ii = 1, 2
			wdir_d  ( ii, itrnd ) = ' '
			wdir_de ( ii, itrnd ) = ' '
		    	mxn_de ( ktype, itrnd, ii ) = IMISSD
		    	mxn_d  ( ktype, itrnd, ii ) = IMISSD
		    END DO
		    eflag_d ( ktype, itrnd ) = .false.
		END IF
C
C*	      Only the 1st trending block has the EXCEPT part.
C
	      ELSE IF ( E1 .and. .not. E2 )  THEN
		kdif = ABS ( minw2 - minw1 )
		greatr = mxwe1 .gt. maxw1
		IF  ( idif .le. 0 .and. kdif .le. 0 .and. 
     +		      kflag .and. greatr )  THEN
C
		    itrnd = 1
		    DO ii = 1, 2
			wdir_d  ( ii, itrnd ) = wd_c ( ii )
		    END DO
C
		    itrnd = 2
		    DO kk = 1, 2
			wdir_d  ( kk, itrnd ) = ' '
			mxn_d  ( ktype, itrnd, kk ) = IMISSD
		    END DO
		END IF
C
C*	      Only the 2nd trending block has the EXCEPT part.
C
	      ELSE IF ( .not. E1 .and. E2 )  THEN
		kdif = ABS ( minw2 - minw1 )
		greatr = mxwe2 .gt. maxw2
		IF  ( idif .le. 0 .and. kdif .le. 0 .and.
     +		      kflag .and. greatr )  THEN
		    itrnd = 1
		    mxn_de ( ktype, itrnd, 1 ) = mnwe2
		    mxn_de ( ktype, itrnd, 2 ) = mxwe2
		    eflag_d ( ktype, itrnd ) = .true.
		    id_d ( ktype, itrnd ) = id_d ( ktype, 2 )
C
		    DO ii = 1, 2
			wdir_d  ( ii, itrnd ) = wd_c ( ii )
		    END DO
		    CALL G2T_TRIM ( ktype, itrnd, ier )
C
		    itrnd = 2
		    DO ik = 1, 2
			wdir_de ( ik, 1 ) = wdir_de ( ik, itrnd )
			wdir_d  ( ik, itrnd ) = ' '
			wdir_de ( ik, itrnd ) = ' '
			id_d ( ktype, itrnd ) =  ' '
			mxn_de ( ktype, itrnd, ik ) = IMISSD
			mxn_d  ( ktype, itrnd, ik ) = IMISSD
		    END DO
		    eflag_d ( ktype, itrnd ) = .false.
		END IF
C
C*	      Neither trending block has the EXCEPT part.
C
	      ELSE IF ( .not. E1 .and. .not. E2 )  THEN
		kdif = ABS ( minw2 - minw1 )
		idifr = maxw1 - maxw2
		kdifr = minw1 - minw2
		IF ( (  idifr .gt. 0 .and. kdifr .le. 0 .or.
     +		        idifr .le. 0 .and. kdifr .ge. 0 ) .and. kflag )
     +			THEN
		    mxr = AMAX0 ( maxw1, maxw2 )
		    mnr = AMIN0 ( minw1, minw2 )
		    itrnd = 1
		    mxn_d  ( ktype, itrnd, 1 ) = mnr
		    mxn_d  ( ktype, itrnd, 2 ) = mxr
		    itrnd = 2
		    DO jj = 1, 2
			wdir_d  ( jj, 1 ) = wd_c ( jj )
			wdir_d  ( jj, itrnd ) = ' '
		    	mxn_d ( ktype, itrnd, jj ) = IMISSD
		    END DO
C
		  ELSE IF  ( idif .le. MXSDIF .and. kdif .le. MNSDIF 
     +		      .and. loc_ok .and. kflag )  THEN
		    mxr = AMAX0 ( maxw1, maxw2 )
		    mnr = AMIN0 ( minw1, minw2 )
		    CALL G2T_RANGE ( ktype, mxr, irange, ier ) 
		    IF ( ABS ( mxr - mnr ) .gt. irange )  
     +			 mnr = mxr - irange
		    itrnd = 1
		    mxn_d  ( ktype, itrnd, 2 ) = mxr
		    mxn_d  ( ktype, itrnd, 1 ) = mnr
C
		    itrnd = 2
		    DO ii = 1, 2
			wdir_d  ( ii, 1 ) = wd_c ( ii )
			wdir_d  ( ii, itrnd ) = ' '
		    	mxn_d ( ktype, itrnd, ii ) = IMISSD
		    END DO
		END IF
	    END IF
	END IF
C
	CALL G2T_GAP ( ktype, ier )
C*
	RETURN
	END 
