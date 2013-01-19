	SUBROUTINE G2T_TRIM ( ktype, itrnd, iret )
C************************************************************************
C* G2T_TRIM								*
C*									*
C* This subroutine simplifies the RANGE and EXCEPT parts in a single 	*
C* trending block.  The rules of simplification are using following	*
C* order:								*
C*	1. RANGE is a subset of EXCEPT or vise versa;			*
C*	2. The differences between the max/min of RANGE and EXCEPT	*
C*	   are less or equal to user specified threshold values, which	*
C*	   are defined in G2T_PARM.TBL, then the RANGE and EXCEPT parts	*
C*	   will be combined;						*
C*	3. The total span of RANGE and EXCEPT is within the allowable	*
C*	   range set forth by OPC and TPC.  The EXCEPT part will be	*
C*	   eliminated while the range is expanded.			*
C*									*
C* G2T_TRIM ( KTYPE, ITRND, IRET )					*
C*									*
C* Input parameters:							*
C*	KTYPE		INTEGER		Data type:			*
C*					 1 = Wave			*
C*					 2 = Wind			*
C*	ITRND		INTEGER		Index for trending block	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		06/07	Created					*
C* T. Lee/SAIC		07/07	Implemented amended rules		*
C* T. Lee/SAIC		11/07	Re-sequenced simplification order	*
C************************************************************************
	INCLUDE		'goftxt.cmn'
	LOGICAL		eflag, dirflg, kflag, wok, sok
	CHARACTER	wd_r(2)*2, wd_e(2)*2, wd_c(2)*2
	INTEGER		kwind(2), kwave(2)
	INCLUDE		'EQUAL.FNC'
C------------------------------------------------------------------------
	iret = 0
	dirflg = .false.
C
	mnr = mxn_d  ( ktype, itrnd, 1 )
	mxr = mxn_d  ( ktype, itrnd, 2 )
	eflag = eflag_d ( ktype , itrnd )
C
	IF ( .not. eflag )  THEN 
	    CALL G2T_RANGE ( ktype, mxr, jrange, iret )
	    IF ( ABS ( mxr - mnr ) .gt. jrange )  mnr = mxr - jrange
	    mne = IMISSD
	    mxe = IMISSD
	  ELSE
	    mne = mxn_de ( ktype, itrnd, 1 )
	    mxe = mxn_de ( ktype, itrnd, 2 )
C
	    IF ( ktype .eq. 2 )  THEN
		DO ii = 1, 2
		   wd_r ( ii ) =  wdir_d ( ii, itrnd )
		   wd_e ( ii ) =  wdir_de (ii, itrnd )
		END DO
C
		dirflg =  ( mxr .le. LGHTS ) .or.
     +			  ( CNE ( wd_r ( 1 ), wd_e ( 1 ) ) .and.
     +			    CNE ( wd_r ( 1 ), wd_e ( 2 ) ) ) .or.
     +			  ( CNE ( wd_r ( 2 ), wd_e ( 1 ) ) .and.
     +			    CNE ( wd_r ( 2 ), wd_e ( 2 ) ) )
	    END IF
	    mdifr = mxe - mxr
	    ndifr = mne - mnr
	    mdif = ABS ( mdifr )
	    ndif = ABS ( ndifr )
C
C*	    Range is a subset of EXCEPT or vise versa.
C
	    IF ( ktype .eq. 1 )  THEN
		IF ( mdifr .ge. 0 .and. ndifr .le. 0 .or.
     +		     mdifr .le. 0 .and. ndifr .ge. 0 )  THEN
		    mxr = AMAX0 ( mxe, mxr )
		    mnr = AMIN0 ( mne, mnr )
	
	            eflag = .false.
		    id_d ( ktype, itrnd ) = ' '
	            mxe = IMISSD
	            mne = IMISSD
C
C*		  If mdif/ndif is less than threshold values. 
C
		  ELSE IF (mdif .le. MXWDIF .and. ndif .le. MNWDIF) THEN
		    mxr = AMAX0 ( mxe, mxr )
		    mnr = AMIN0 ( mnr, mne )
	            CALL G2T_RANGE ( ktype, mxr, irange, iret )
	            IF ( ABS ( mxr - mnr ) .gt. irange )
     +			 mnr = mxr - irange
	            eflag = .false.
		    id_d ( ktype, itrnd ) = ' '
	            mxe = IMISSD
	            mne = IMISSD
		  ELSE
C
C*		    Expand to allowable range.
C
		    kwave ( 1 ) = AMIN0 ( mnr, mne )
		    kwave ( 2 ) = AMAX0 ( mxe, mxr )
		    CALL G2T_CHECK( ktype, kwave, kwave, wok, sok, ier )
		    IF ( wok )  THEN
			mnr = kwave ( 1 )
			mxr = kwave ( 2 )
			mne = IMISSD
			mxe = IMISSD
			eflag = .false.
			id_d ( ktype, itrnd ) = ' '
		    END IF
		END IF
	      ELSE IF ( ktype .eq. 2 )  THEN
C
C*		Wind speed in RANGE is subset of the EXCEPT, or the span
C*		of the wind speed is in the allowable range, RANGE and
C*		EXCEPT may be combined providing that the wind directions
C*		are within the spread. 
C
		CALL G2T_WDIR1 ( itrnd, kflag, wd_c, ier )
		IF ( kflag )  THEN
C
C*		     Range is a subset of EXCEPT or vise versa.
C
		    IF ( mdifr .ge. 0 .and. ndifr .le. 0 .or.
     +			 mdifr .le. 0 .and. ndifr .ge. 0 )  THEN
			IF ( mdifr .ge. 0 )  THEN
			    DO ii = 1, 2
				wdir_d ( ii, itrnd ) = wd_c ( ii )
				wdir_de (ii, itrnd ) =  ' '
			    END DO
			    mxr = AMAX0 ( mxe, mxr )
			    mnr = AMIN0 ( mne, mnr )
			END IF
	  		eflag = .false.
			id_d ( ktype, itrnd ) = ' '
			mxe = IMISSD
			mne = IMISSD
C
C*		      MDIF/NDIF is less than threshold values.
C
		      ELSE IF (  mdif .le. MXSDIF .and. 
     +			         ndif .le. MNSDIF )  THEN
			mxr = AMAX0 ( mxe, mxr )
			mnr = AMIN0 ( mnr, mne )
			CALL G2T_RANGE ( ktype, mxr, irange, iret )
			IF ( ABS ( mxr - mnr ) .gt. irange )
     +				mnr = mxr - irange
			DO ii = 1, 2
			    wdir_d ( ii, itrnd ) = wd_c ( ii )
			    wdir_de (ii, itrnd ) =  ' '
			END DO
			id_d ( ktype, itrnd ) = ' '
			eflag = .false.
			mxe = IMISSD
			mne = IMISSD
		      ELSE
C
C*			Expand to allowable range.
C
			kwind ( 1 ) = AMIN0 ( mne, mnr )
			kwind ( 2 ) = AMAX0 ( mxe, mxr )
			CALL G2T_CHECK ( ktype,  kwave, kwind, wok, sok,
     +					 ier )
			IF ( sok )  THEN
			    DO ii = 1, 2
				wdir_d ( ii, itrnd ) = wd_c ( ii )
				wdir_de (ii, itrnd ) =  ' '
			    END DO
			    mnr = kwind ( 1 )
			    mxr = kwind ( 2 )
			    mne = IMISSD
			    mxe = IMISSD
			    eflag = .false.
			    id_d ( ktype, itrnd ) = ' '
			END IF
		    END IF
		END IF	
	    END IF
	END IF 
C
	mxn_d  ( ktype, itrnd, 1 ) = mnr
	mxn_d  ( ktype, itrnd, 2 ) = mxr
	mxn_de ( ktype, itrnd, 1 ) = mne
	mxn_de ( ktype, itrnd, 2 ) = mxe
	eflag_d ( ktype , itrnd ) = eflag
C*
	RETURN
	END 
