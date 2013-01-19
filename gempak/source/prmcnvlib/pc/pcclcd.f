	SUBROUTINE PC_CLCD  ( noutpm, rdata, cdata, iret )
C************************************************************************
C* PC_CLCD								*
C*									*
C* This subroutine checks conditions for level parameters.  If a	*
C* computation calls for addition, multiplication, subtraction or	*
C* division, it is performed here.  If <, >, or = has been used to	*
C* set conditions, the data are checked.  If the conditions are not	*
C* met, the return code is set to a negative number.			*
C*									*
C* PC_CLCD  ( NOUTPM, RDATA, CDATA, IRET )				*
C*									*
C* Input parameters:							*
C*	NOUTPM		INTEGER		Number of output parameters	*
C*									*
C* Input and output parameters:						*
C*	RDATA (NOUTPM)	REAL		Real valued output		*
C*	CDATA (NOUTPM)	CHAR*		Character output		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-14 = conditions not met	*
C**									*
C* M. desJardins/GSFC	11/89						*
C* M. desJardins/GSFC	 4/90	Fixed bug eliminating light (-) cond	*
C* S. Jacobs/NCEP	 3/99	Changed cc from 8 char to 12 char	*
C* D. Kidwell/NCEP	 4/04	Added check on msgcnd                   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'pccmn.cmn'
C*
	CHARACTER*(*)	cdata (*)
	REAL		rdata (*)
C*
	LOGICAL		good, chtype, mm
	CHARACTER	ss*1, cc*12
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Check the conditions for each output parameter until the end
C*	is reached or until some condition is not met.
C
	good = .true.
	iprm = 1
	DO WHILE  ( good .and. ( iprm .le. noutpm ) )
	    rr = rdata (iprm)
	    mm = ERMISS ( rr )
	    cc = cdata (iprm)
	    chtype = qchr ( iprm )
C
C*	    Check each condition.
C
	    kcond = 1
	    DO WHILE  ( good .and. ( kcond .le. nlvcnd (iprm) ) )
C
C*		Check type of condition.
C
		ss = symlev ( kcond, iprm )
		IF  ( ss .eq. '<' )  THEN
		    IF  ( mm .or. ( rr .gt. rlvcnd ( kcond, iprm ) ) )
     +			good = .false.
		  ELSE IF  ( ss .eq. '>' )  THEN
		    IF  ( mm .or. ( rr .lt. rlvcnd ( kcond, iprm ) ) )
     +			good = .false.
		  ELSE IF  ( chtype .and. ( ss .eq. '=' ) )  THEN
		    good = .false.
		    j    = 1
		    DO WHILE  ( .not. good .and. 
     +				( j .le. nclvcc ( kcond, iprm ) ) )
			ipos = INDEX  ( cc, clvcnd ( j, kcond, iprm ) 
     +					(: ilncnd ( j, kcond, iprm ) ) )
			IF  ( ipos .ne. 0 )  THEN
			    CALL ST_LSTR (clvcnd (j,kcond,iprm), is, it)
			    ipos = ipos + is
			    good = .true.
			    IF  ( ( ipos .lt. 8 ) .and. 
     +				  ( cc (ipos:ipos) .eq. '-' ) ) 
     +				good = .false.
			END IF
			j = j + 1
		    END DO
		  ELSE IF  ( ss .eq. '=' )  THEN
		    IF  ( ( mm .and. .not. msgcnd ( kcond, iprm ) ) .or. 
     +			  ( rr .ne. rlvcnd ( kcond, iprm ) ) )
     +			good = .false.
		  ELSE IF  ( ss .eq. '*' )  THEN
		    IF  ( .not. mm )  THEN
			rr = rr * rlvcnd ( kcond, iprm )
			rdata ( iprm ) = rr
		    END IF
		  ELSE IF  ( ss .eq. '/' )  THEN
		    IF  ( .not. mm )  THEN
			rr = rr / rlvcnd ( kcond, iprm )
			rdata ( iprm ) = rr
		    END IF
		  ELSE IF  ( ss .eq. '+' )  THEN
		    IF  ( .not. mm )  THEN
			rr = rr + rlvcnd ( kcond, iprm )
			rdata ( iprm ) = rr
		    END IF
		  ELSE IF  ( ss .eq. '-' )  THEN
		    IF  ( .not. mm )  THEN
			rr = rr - rlvcnd ( kcond, iprm )
			rdata ( iprm ) = rr
		    END IF
		END IF
C
C*		Increment condition counter.
C
		kcond = kcond + 1
	    END DO
C
C*	    If last parameter was not good, return code is negative.
C
	    IF  ( .not. good )  THEN
		iret = -14
	    END IF
C
C*	    Increment parameter counter.
C
	    iprm = iprm + 1
	END DO
C*
	RETURN
	END
