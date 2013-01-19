	SUBROUTINE SNOCMP  ( nparms, rlevel, nlev, levtyp, lvert,
     +			     sndata, numlev, levout, rdata, iret )
C************************************************************************
C* SNOCMP								*
C*									*
C* This subroutine gets the requested sounding data and computes the	*
C* parameters at all the station levels.				*
C*									*
C* SNOCMP  ( NPARMS, RLEVEL, NLEV, LEVTYP, LVERT, SNDATA, NUMLEV,	*
C*           LEVOUT, RDATA, IRET )					*
C*									*
C* Input parameters:							*
C*	NPARMS		INTEGER		Number of parameters		*
C*	RLEVEL (NLEV)	REAL		Levels				*
C*	NLEV		INTEGER		Number of levels		*
C*	LEVTYP		INTEGER		Level type			*
C*	LVERT		INTEGER		Output vertical coordinate	*
C*	SNDATA (*)	REAL		Input sounding			*
C*	NUMLEV		INTEGER		Number of levels in SNDATA	*
C*									*
C* Output parameters:							*
C*	LEVOUT		INTEGER		Number of levels computed	*
C*	RDATA (*)	REAL		Output sounding			*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 9/87	GEMPAK4					*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		rlevel (*), rdata (*), sndata (*)
C*
	CHARACTER	cdata (LLMXDT)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret   = 0
	levout = 0
	knt    = 1
C
C*	Check for list of levels or range.
C
	IF  ( levtyp .eq. 1 )  THEN
C
C*	    Always compute surface data first.
C
	    CALL PC_CMVR  ( 0., lvert, sndata, rdata (knt), 
     +			    cdata (knt), ier )
	    IF  ( rlevel (1) .eq. 0. )  THEN
		istart = 2
	      ELSE
		istart = 1
	    END IF
C
C*	    Loop through the list of levels.
C
	    levout = 1
	    knt    = nparms + 1
	    DO  lev = istart, nlev
		CALL PC_CMVR  ( rlevel (lev), lvert, sndata, 
     +				rdata (knt), cdata (knt), ier )
		IF  ( .not. ERMISS (rdata (knt) ) )  THEN
		    levout = levout + 1
		    knt    = knt + nparms
		END IF
	    END DO
C
C*	    Check for surface as only data.
C
	    IF  ( ( levout .eq. 1 ) .and. ( rlevel (1) .ne. 0. ) )  THEN
		levout = 0
	    END IF
C
C*	    Check for range with increment.
C
	  ELSE IF  ( levtyp .eq. 2 )  THEN			
C
C*	    Find bottom level.
C
	    CALL PC_FLVL  ( rlevel (1), lvert, sndata, begin, lvnum1,
     +			    lvnum2, levloc, ier )
	    IF  ( levloc .eq. 1 )  THEN
		ilev = lvnum1
	      ELSE IF  ( levloc .eq. 2 )  THEN
		ilev = lvnum2
	      ELSE IF  ( levloc .eq. 3 )  THEN
		ilev = 1
	      ELSE
		ilev = numlev + 1
	    END IF
C
C*	    Find top level.
C
	    CALL PC_FLVL  ( rlevel (2), lvert, sndata, end, lvnum1,
     +			    lvnum2, levloc, ier )
	    IF  ( levloc .eq. 1 )  THEN
		jlev = lvnum1
	      ELSE IF  ( levloc .eq. 2 )  THEN
		jlev = lvnum1
	      ELSE IF  ( levloc .eq. 4 )  THEN
		jlev = numlev
	      ELSE
		jlev = 0
	    END IF
C
C*	    Always compute surface data first.
C
	    CALL PC_CMVR  ( 0., lvert, sndata, rdata (knt), 
     +			    cdata (knt), ier )
	    IF  ( ilev .eq. 1 )  THEN
		iilev = 2
	      ELSE
		iilev = ilev
	    END IF
C
C*	    Compute data at each level.
C
	    levout = 1
	    knt    = nparms + 1
	    DO  lev = iilev, jlev
		CALL PC_CMLV  ( lev, sndata, rdata (knt), cdata (knt), 
     +				ier )
		IF  ( .not. ERMISS  ( rdata (knt) ) )  THEN
		    levout = levout + 1
		    knt    = knt + nparms
		END IF
	    END DO
C
C*	    Check for surface as only data.
C
	    IF  ( ( levout .eq. 1 ) .and. ( ilev .ne. 1 ) )  THEN
		levout = 0
	    END IF
	END IF
C*
	RETURN
	END
