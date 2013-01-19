	SUBROUTINE PC_CSCD  ( noutpm, rdata, iret )
C************************************************************************
C* PC_CSCD								*
C*									*
C* This subroutine checks the conditions for station parameters.  If	*
C* a computation calls for addition, multiplication, subtraction or	*
C* division, it is performed here.  If <, >, or = has been used to	*
C* set conditions, the data are checked.  If the conditions are not	*
C* met, all data are set to missing values.				*
C*									*
C* PC_CSCD  ( NOUTPM, RDATA, IRET )					*
C*									*
C* Input parameters:							*
C*	NOUTPM		INTEGER		Number of output parameters	*
C*									*
C* Input and output parameters:						*
C*	RDATA (NOUTPM)	REAL		Real valued output		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-14 = conditions not met	*
C**									*
C* M. desJardins/GSFC	11/89						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'pccmn.cmn'
C*
	REAL		rdata (*)
C*
	LOGICAL		good, mm
	CHARACTER	ss*1
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
C
C*	    Check each condition.
C
	    kcond = 1
	    DO WHILE  ( good .and. ( kcond .le. nstcnd (iprm) ) )
C
C*		Check type of condition.
C
		ss = symstn ( kcond, iprm )
		IF  ( ss .eq. '<' )  THEN
		    IF  ( mm .or. ( rr .gt. rstcnd ( kcond, iprm ) ) )
     +			good = .false.
		  ELSE IF  ( ss .eq. '>' )  THEN
		    IF  ( mm .or. ( rr .lt. rstcnd ( kcond, iprm ) ) )
     +			good = .false.
		  ELSE IF  ( ss .eq. '=' )  THEN
		    IF  ( mm .or. ( rr .ne. rstcnd ( kcond, iprm ) ) )
     +			good = .false.
		  ELSE IF  ( ss .eq. '*' )  THEN
		    IF  ( .not. mm )  THEN
			rr = rr * rstcnd ( kcond, iprm )
			rdata ( iprm ) = rr
		    END IF
		  ELSE IF  ( ss .eq. '/' )  THEN
		    IF  ( .not. mm )  THEN
			rr = rr / rstcnd ( kcond, iprm )
			rdata ( iprm ) = rr
		    END IF
		  ELSE IF  ( ss .eq. '+' )  THEN
		    IF  ( .not. mm )  THEN
			rr = rr + rstcnd ( kcond, iprm )
			rdata ( iprm ) = rr
		    END IF
		  ELSE IF  ( ss .eq. '-' )  THEN
		    IF  ( .not. mm )  THEN
			rr = rr - rstcnd ( kcond, iprm )
			rdata ( iprm ) = rr
		    END IF
		END IF
C
C*		Increment condition counter.
C
		kcond = kcond + 1
	    END DO
C
C*	    If last parameter was not good, the return code is set to
C*	    a negative error number.
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
