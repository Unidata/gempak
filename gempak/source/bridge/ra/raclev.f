	SUBROUTINE RA_CLEV  ( cldtyp, cldhgt, ncld, chc1, chc2, chc3,
     +			      iret )
C************************************************************************
C* RA_CLEV								*
C*									*
C* This subroutine uses the cloud information decoded from an airways	*
C* report and returns it encoded in three combined cloud height 	*
C* and coverage reports.  If -X ( partially obscured ) is reported,	*
C* 1000 is added to the first report.  The combined value is the	*
C* height * 10 + coverage.						*
C*									*
C* RA_CLEV  ( CLDTYP, CLDHGT, NCLD, CHC1, CHC2, CHC3, IRET )		*
C*									*
C* Input parameters:							*
C*	CLDTYP (NCLD)	REAL		GEMPAK cloud types		*
C*	CLDHGT (NCLD)	REAL		Cloud height in hundreds of feet*
C*	NCLD		INTEGER		Number of cloud reports		*
C*									*
C* Output parameters:							*
C*	CHC1		REAL		Cloud report 1			*
C*	CHC2		REAL		Cloud report 2			*
C*	CHC3		REAL		Cloud report 3			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* B. Doty/RDS		11/87						*
C* I. Graffman/RDS	 4/88						*
C* M. desJardins/GSFC	 9/89	GEMPAK 5				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		cldtyp (*), cldhgt (*)
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Set values to missing.
C
	chc1 = RMISSD
	chc2 = RMISSD
	chc3 = RMISSD
C
C*	Return if there are no cloud reports.
C
	IF  ( ncld .eq. 0 )  RETURN
C
C*	Move reported clouds into low, mid and high clouds.
C
	knt = 0
	IF  ( ERMISS ( cldhgt (1) ) .and. ( cldtyp (1) .eq. 9. ) )  THEN
	    is = 2
	    IF  ( ncld .eq. 1 )  chc1 = 10000.
	  ELSE
	    is = 1
	END IF
	DO  i = is, ncld
	    IF  ( ( .not. ERMISS ( cldhgt (i) ) ) .or.
     +		  ( cldtyp (i) .eq. 1. ) )  THEN
		IF  ( ERMISS ( cldhgt (i) ) )  THEN
		    ccc = cldtyp (i)
		  ELSE
		    ccc = cldhgt (i) * 10. + cldtyp (i)
		END IF
		knt = knt + 1
		IF  ( knt .eq. 1 )  THEN
		    IF  ( is .eq. 1 )  THEN
			chc1 = ccc
		      ELSE
			chc1 = 10000. + ccc
		    END IF
		  ELSE IF  ( knt .eq. 2 )  THEN
		    chc2 = ccc
		  ELSE IF  ( knt .eq. 3 )  THEN
		    chc3 = ccc
		END IF
	    END IF
	END DO
C*
	RETURN
	END
