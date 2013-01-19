	SUBROUTINE RA_TMPX  ( ipnaft, ighr, asoflg, ipxaft, 
     +			      t06x, t12x, t24x, tmdx,
     +			      t06n, t12n, t24n, tmdn, iret )
C************************************************************************
C* RA_TMPX								*
C*									*
C* This subroutine decodes the maximum, minimum and/or extreme 		*
C* temperature, depending on the time of day, in an airways or		*
C* ASOS report.								*
C*									*
C* FORMAT ThhX/N:  T   = Temperature in Fahrenheit			*
C*		   hh  = ( 06, 12, 24 ) time period for min/max		*
C*			 MD = previous day, midnight to midnight 	*
C*		   X/N = Maximum or Minimum				*
C*									*
C* Source (group)	 0Z 	6Z	12Z	18Z	once/day	*
C*									*
C* SAO (TxTx) 		T12X	T24X					*
C* SAO (TnTn) 				T12N	T24N			*
C* ASOS (1sTxTxTx) 	T06X	T06X	T06X	T06X			*
C* ASOS (2sTnTnTn)	T06N	T06N	T06N	T06N			*
C*									*
C* ASOS (4STTTSTTT) or SAO (4TxTxTnTn) 			TMDX 		*
C* ASOS (4STxTxTxSTnTnTn) 	       			TMDN		*
C*									*
C*									*
C* RA_TMPX ( IPNAFT, IGHR, ASOFLG, IPXAFT, 				*
C*           T06X, T12X, T24X, TMDX, T06N, T12N, T24N, TMDN, IRET )	*
C*									*
C* Input parameters:							*
C*	IPNAFT		INTEGER		First field after snow data	*
C*	IGHR		INTEGER		Report hour (UTC)		*
C*	ASOFLG		LOGICAL		ASOS Report flag		*
C*									*
C* Output parameters:							*
C*	IPXAFT		INTEGER		First field after max/min temp	*
C*	T06X		REAL		Maximum 06 hour temp in F	*
C*	T12X		REAL		Maximum 12 hour temp in F	*
C*	T24X		REAL		Maximum 24 hour temp in F	*
C*	TMDX		REAL		Max daily temp (F) (12M-12M LST)*
C*	T06N		REAL		Minimum 06 hour temp in F	*
C*	T12N		REAL		Minimum 12 hour temp in F	*
C*	T24N		REAL		Minimum 24 hour temp in F	*
C*	TMDN		REAL		Min daily temp (F) (12M-12M LST)*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1  = end of report		*
C*					  -2  = not found		*
C**									*
C* Log:									*
C* K. MILES/FSU		 6/90						*
C* J. Walker/FSU	12/90		Rewrote				*
C* P. Bruehl/Unidata	 2/94		Removed references to clouds,   *
C*					added TMAX,TMIN,TMPD		*
C* P. Bruehl/Unidata	 3/94		Added ASOS, redid variables	*
C************************************************************************
	LOGICAL		done, asoflg, match
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'racmn.cmn'
	
	CHARACTER	sss*3
C-----------------------------------------------------------------------
	iret   = 0
	ipr24  = 0
	t06x   = RMISSD
	t12x   = RMISSD
	t24x   = RMISSD
	tmdx   = RMISSD
	t06n   = RMISSD
	t12n   = RMISSD
	t24n   = RMISSD
	tmdn   = RMISSD
	ipxaft = ipnaft
	done   = .false.
	inc    = ipxaft
C
C*	Look for two digit number, or a 5 digit number starting with 4, 
C*	or if ASOS flag, 5 digit numbers starting with 1 and 2 and a
C*	9 digit number stargint with 4 in next few fields.
C
	DO WHILE (.not. done )
	    IF  ( inc .gt. nfield )  THEN
		done = .true.
		ipxaft = inc
		iret = -1
	    ELSE IF  ( asoflg )  THEN
C
C*		ASOS Flag
C
		match = .false.
		IF  ( ( ifsize (inc) .eq. 5 ) .and. 
     +		      ( iftype (inc) .eq. 2 ) ) THEN

		    IF  ( ( ifintg (inc) / 10000 ) .eq. 7 )  THEN
C
C*			24 hour Precip group 7RRRR, mark this point
C*			for later use.
C
			ipr24 = inc
			inc = inc + 1
			match = .true.
		    END IF
		    IF  ( ( ifintg (inc) / 10000 ) .eq. 1 )  THEN
C
C*			6 hour Maximum Temperature group 1STxTxTx
C
			isign = 1
			IF  ( ( ifintg (inc) / 1000 ) .ge. 11 )
     +			    isign = -1 
C
			t06x = isign * MOD ( ifintg (inc), 1000 )
			inc = inc + 1
			match = .true.
		    END IF
		    IF  ( ( ifintg (inc) / 10000 ) .eq. 2 )  THEN	
C
C*			6 hour Minimum Temperature group 2STnTnTn
C
			isign = 1
			IF  ( ( ifintg (inc) / 1000 ) .ge. 21 ) 
     +			    isign = -1 
C
			t06n = isign * MOD ( ifintg (inc), 1000 )
			inc = inc + 1
			match = .true.
		    END IF
		END IF
		IF  ( ( ifsize (inc) .eq. 9 ) .and. 
     +		      ( iftype (inc) .eq. 2 ) .and.
     +		      ( ( ifintg (inc) / 100000000 ) .eq. 4 ) ) THEN
C
C*		    24 hour max & min temp midnight-midnight
C*		    4STxTxTxSTnTnTn
C
		    isgnx = 1
		    IF  ( ( ifintg (inc) / 10000000 ) .ge. 41 )
     +			isgnx = -1 
C
		    sss = cfield (inc) (3:5)
		    READ ( sss, 1001, IOSTAT=ios ) tmdx
1001 		    FORMAT ( BN, F3.0 )
		    IF  ( ios .ne. 0 )  RETURN
		    tmdx = isgnx * tmdx
C
		    isgnn = 1
		    sss = cfield (inc) (6:6)
		    READ ( sss, 1002, IOSTAT=ios ) itmp
1002		    FORMAT ( BN, I1 )
		    IF  ( ios .ne. 0 )  RETURN
		    IF  ( itmp .eq. 1 )  isgnn = -1
		    tmdn = isgnn * MOD ( ifintg (inc), 1000 )
		    inc = inc + 1
		    match = .true.
		END IF
		IF  ( .not. match )  inc = inc + 1
	    ELSE IF  ( ( ifsize (inc) .eq. 2 ) .and.
     +		       ( iftype (inc) .eq. 2 ) .and.
     +		       ( ( iftype (inc+1) .ne. 3 ) .or. 
     +			 ( inc+1 .gt. nfield ) ) ) THEN
C
C*		Tn/xTn/x Group
C
		IF  ( ighr .eq.  0 )  t12x = ifintg (inc)
		IF  ( ighr .eq.  6 )  t24x = ifintg (inc)
		IF  ( ighr .eq. 12 )  t12n = ifintg (inc)
		IF  ( ighr .eq. 18 )  t24n = ifintg (inc)
		done = .true.
		ipxaft = inc + 1
	    ELSE IF  ( ( ifsize (inc) .eq. 5 ) .and.
     +		       ( iftype (inc) .eq. 2 ) .and.
     +		       ( ( ifintg (inc) / 10000 ) .eq. 4 ) )  THEN
C
C*		4TxTxTnTn Group with both numbers present
C
		t12n = MOD ( ifintg (inc), 100 )
		sss = cfield (inc) (2:3)
		READ ( sss, 1000, IOSTAT=ios ) tmdx
1000		FORMAT ( BN, F2.0 )
		IF ( ios .ne. 0 ) RETURN
		done = .true.
		ipxaft = inc + 1
	    ELSE IF  ( ( iftype (inc) .eq. 2 ) .and.
     +		       ( iftype (inc+2) .eq. 3 ) ) THEN
		IF  ( ( ifsize (inc) .eq. 3 ) .and.
     +		      ( ( ifintg (inc) / 100 ) .eq. 4 ) ) THEN
C
C*		    4TxTxTnTn Group with only TxTx present (4TxTx//)
C
		    tmdx = MOD ( ifintg (inc), 100 )
		    done = .true.
		    ipxaft = inc + 1
		ELSE IF ( ( ifsize (inc) .eq. 1 ) .and.
     +			  ( ifintg (inc) .eq. 4 ) .and.
     +			  ( iftype (inc+3) .eq. 2 ) ) THEN
C
C*		    4TxTxTnTn Group with only TnTn present (4//TnTn)
C
		    t12n = MOD ( ifintg (inc+3), 100 )
		    done = .true.
		    ipxaft = inc + 1
		ELSE
		    inc = inc + 1
		ENDIF
	    ELSE IF  ( ( ifsize (inc) .eq. 5 ) .and.
     +		       ( iftype (inc) .eq. 2 ) .and.
     +		       ( ( ifintg (inc) / 10000 ) .eq. 2 ) )  THEN
C
C* 		24 hour Precip group 2RRRR, mark this point for
C*		later use.
C
		ipr24 = inc
		inc = inc + 1
	    ELSE 
C
C*		Not Tn/xTn/x or 4TxTxTnTn group
C
		inc=inc+1
	    END IF
	END DO 
	IF  ( ipr24 .ne. 0 )  ipxaft = ipr24
C*
 	RETURN
	END
