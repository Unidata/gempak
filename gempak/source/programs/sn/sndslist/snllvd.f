	SUBROUTINE SNLLVD  ( data, ndlev, nparms, vlevel, nlev, lvert,
     +			     levtyp, rdata, mlev, iret )
C************************************************************************
C* SNLLVD								*
C*									*
C* This subroutine gets the level data for SNLIST.			*
C*									*
C* SNLLVD  ( DATA, NDLEV, NPARMS, VLEVEL, NLEV, LVERT, LEVTYP, RDATA,	*
C*           MLEV, IRET )						*
C*									*
C* Input parameters:							*
C*	DATA (*)	REAL		Sounding data			*
C*	NDLEV		INTEGER		Number of levels		*
C*	NPARMS		INTEGER		Number of output parameters	*
C*	VLEVEL (NLEV)	REAL		Levels				*
C*	NLEV		INTEGER		Number of levels		*
C*	LVERT		INTEGER		Output vertical coordinate	*
C*	LEVTYP		INTEGER		Level type			*
C*					  1 = list			*
C*					  2 = range without increment	*
C*									*
C* Output parameters:							*
C*	RDATA 		REAL		Level data			*
C*	  (NPARMS,MLEV)							*
C*	MLEV		INTEGER		Output levels			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88	GEMPAK 4.1				*
C* J. Whistler/SSAI	02/91	Removed variable levloc and replaced    *
C*				with itype.				*
C* T. Lee/GSC		 2/97	Moved mlev out of the IF loop		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		data (*), vlevel (*), rdata ( NPARMS, * )
C*
	CHARACTER	cdata (MMPARM)*8
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	mlev = 0
C
C*	Loop thru the levels and compute the parameters.
C
	IF  ( levtyp .eq. 1 )  THEN			
C
C*	    A list of levels.
C
	    knt  = 1
	    DO  lev = 1, nlev
		CALL PC_CMVR  ( vlevel (lev), lvert, data, 
     +				rdata ( 1, knt ), cdata, ier )
		IF  ( .not. ERMISS ( rdata (1,knt) ) )  THEN
		    mlev = mlev + 1
		    knt  = knt  + 1
		END IF
	    END DO
C
C*	    Check for a range without increment.
C
	  ELSE IF  ( levtyp .eq. 2 )  THEN			
C
C*	    Find bottom and top levels.
C
	    CALL PC_FLVL  ( vlevel (1), lvert, data, rrr, lvnum1,
     +			    lvnum2, itype, ier )
	    IF  ( itype .eq. 3 )  THEN
		ilev = 1
	      ELSE IF  ( itype .eq. 4 )  THEN
		ilev = ndlev + 1
	      ELSE IF  ( itype .eq. 2 )  THEN
		ilev = lvnum2
	      ELSE
		ilev = lvnum1
	    END IF
C*
	    CALL PC_FLVL  ( vlevel (2), lvert, data, end, lvnum1,
     +			    lvnum2, itype, ier )
	    IF  ( itype .eq. 3 )  THEN
		jlev = 0
	      ELSE IF  ( itype .eq. 4 )  THEN
		jlev = ndlev
	      ELSE 
		jlev = lvnum1
	    END IF
C
C*	    Compute data at these levels.
C
	    knt  = 1
	    DO  lev = ilev, jlev
		CALL PC_CMLV  ( lev, data, rdata (1,knt), cdata, 
     +				ier )
		IF  ( .not. ERMISS ( rdata (1,knt) ) )  THEN
		    mlev = mlev + 1
		    knt  = knt  + 1
		END IF
	    END DO
	END IF
C*
	RETURN
	END
