	SUBROUTINE RA_CSYB ( ipnaft, ipoaft, csyl, csym, csyh, iret )
C************************************************************************
C* RA_CSYB								*
C*									*
C* This subroutine decodes the cloud code group (cloud graphics symbol)	*
C* from the WMO cloud genera.						*
C*									*
C* RA_CSYB ( IPNAFT, IPOAFT, CSYL, CSYM, CSYH, IRET )			*
C*									*
C* Input parameters:							*
C*	IPNAFT		INTEGER		First field after apprr data	*
C*									*
C* Output parameters:							*
C*	IPOAFT		INTEGER		First field after cloud code grp*
C*	CSYL		REAL		Low-level cloud genera code	*
C*	CSYM		REAL		Mid-level cloud genera code	*
C*	CSYH		REAL		High-level cloud genera code	*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = beyond end of report	*
C*					  -2 = not a cloud report	*
C**									*
C* Log:									*
C* P.Bruehl/Unidata	2/94						*
C* P.Bruehl/Unidata	11/94	Corrected 0'ing between reports		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'racmn.cmn'
C*	
	CHARACTER	sss*1
C-----------------------------------------------------------------------
	iret   = 0
	isyl   = IMISSD
	isym   = IMISSD
	isyh   = IMISSD
	ipoaft = ipnaft
	inc    = ipoaft
C
C*	Look for up to four digit number starting with "1" in next field.
C
	IF ( inc .gt. nfield ) THEN
	    ipoaft = inc
	    iret = -1
	    RETURN
	ELSE IF ( ( ifsize (inc) .eq. 4 ) .and.
     +		  ( iftype (inc) .eq. 2 ) )  THEN
C
C*	    Check to see if this is the clouds in the form 1NNN
C
	    indcat = ifintg (inc) / 1000
	    IF  ( indcat .eq. 1 )  THEN
		sss = cfield (inc) (2:2)
		READ ( sss, 1000, IOSTAT = ios) isyl
		IF  ( ios .ne. 0 )  RETURN
		sss = cfield (inc) (3:3)
		READ ( sss, 1000, IOSTAT = ios) isym
 1000		FORMAT ( BN, I1 )
		IF ( ios .ne. 0 ) RETURN
		isym = isym + 10
		isyh = MOD ( ifintg (inc), 10 ) + 20
		ipoaft = inc+1
	    ELSE
C
C*		Not a cloud group, RETURN
C
		iret = -2
		RETURN
	    ENDIF			
	ELSE IF ( ( ifsize (inc) .eq. 3 ) .and.
     +		  ( iftype (inc) .eq. 2 ) )  THEN
C
C*	    Check to see if this is the clouds in the form 1NN/
C
	    indcat = ifintg (inc) / 100
	    IF ( ( (inc + 1) .le. nfield ) .and.
     +		 ( iftype (inc+1) .eq. 3 ) .and. 
     +		 ( indcat .eq. 1 ) ) THEN
		sss = cfield (inc) (2:2)
		READ ( sss, 1000, IOSTAT = ios ) isyl
		IF  ( ios .ne. 0 )  RETURN
		isym = MOD ( ifintg (inc), 10 ) + 10
		isyh = IMISSD
		ipoaft = inc+2
	    ELSE
C
C*		Not a cloud group, RETURN
C
		iret = -2
		RETURN
	    ENDIF			
	ELSE IF ( ( ifsize (inc) .eq. 2 ) .and.
     +		  ( iftype (inc) .eq. 2 ) )  THEN
C
C*	    Check to see if this is the clouds in the form 1N//
C
	    indcat = ifintg (inc) / 10
	    IF ( ( (inc + 2) .le. nfield ) .and. 
     +		 ( iftype (inc+2) .eq. 3) .and. 
     +           ( indcat .eq. 1 ) ) THEN
		isyl = MOD ( ifintg (inc), 10 )
		isym = IMISSD
		isyh = IMISSD
		ipoaft = inc+3
	    ELSE
C
C*		Not a cloud group, RETURN
C
		iret = -2
		RETURN
	    ENDIF			
	END IF
C
C*	Assign the cloud values.
C
	csyl = FLOAT ( isyl )
	csym = FLOAT ( isym )
	csyh = FLOAT ( isyh )
C*
 	RETURN
	END
