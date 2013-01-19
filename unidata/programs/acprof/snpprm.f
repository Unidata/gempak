	SUBROUTINE SNPPRM  ( vcoord, snparm, line, wind, pname1, pname2,
     +			     ip1arr, ip2arr, wintyp, iwncol, tunit,
     +			     iret )
C************************************************************************
C* SNPPRM								*
C*									*
C* This subroutine checks to see which parameters are to be plotted	*
C* and sets up the calculations.					*
C*									*
C* SNPPRM  ( VCOORD, SNPARM, LINE, WIND, PNAME1, PNAME2, IP1ARR,	*
C*           IP2ARR, WINTYP, IWNCOL, TUNIT, IRET )			*
C*									*
C* Input parameters:							*
C*	VCOORD		CHAR*		Vertical coordinate		*
C*	SNPARM		CHAR*		Parameter input			*
C*	LINE		CHAR*		Line characteristic input	*
C*	WIND		CHAR*		Wind input			*
C*									*
C* Output parameters:							*
C*	PNAME1		CHAR*		Parameter 1			*
C*	PNAME2		CHAR*		Parameter 2			*
C*	IP1ARR (3)	INTEGER		Color/type/width for parm 1	*
C*	IP2ARR (3)	INTEGER		Color/type/width for parm 2	*
C*	WINTYP		CHAR*		Wind type (B,A)			*
C*	IWNCOL		INTEGER		Wind color			*
C*	TUNIT		CHAR*1		Temperature units (K,C,F)	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = VCOORD cannot be computed	*
C*                                      -10 = SNPARM not specified	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/NMC         07/90   Added SNPARM, LINE			*
C* K. Brill/NMC		02/92	Added TUNIT				*
C* K. Brill/NMC		02/92	Set conditions on level parameters	*
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	snparm, line, wind, pname1, pname2, wintyp,
     +			vcoord, tunit
	INTEGER		ip1arr (*), ip2arr (*)
C*
	REAL            cval (2)
	INTEGER         icol (2), ityp (2), iwid (2), ilab(2)
	CHARACTER	parms (5)*4, winuni*1
	LOGICAL		cmpflg (5), chrflg (5), scflag
	CHARACTER	tmprm (MMPARM)*4, tmcnd (MMPARM)*12,
     +			prmcnd (MMPARM)*12
C----------------------------------------------------------------------
	iret = 0
	DO  i = 1, 5
	    parms (i) = ' '
	END DO
C
C*	Set the first parameter to the vertical coordinate.
C
	parms (1) = vcoord
C*
	IF ( snparm .eq. ' ' ) THEN
           iret = -10
	   CALL ER_WMSG ( 'SNPROF', iret, ' ', ier )
	   RETURN
	END IF
C
C*	Get parameter list from input and insert in to PARMS after 1.
C
	CALL IN_PRMC ( MMPARM, snparm, tmprm, tmcnd, num, ier )
	IF ( num .gt. 2 ) num = 2
	prmcnd (1) = ' '
	DO i = 1, num
	    parms (i+1) = tmprm (i)
	    prmcnd (i+1) = tmcnd (i)
	END DO
C
C*	Set the remaining 2 conditions.
C
	prmcnd (4) = ' '
	prmcnd (5) = ' '
C
C*	Decode color/ line type/ width.
C
	CALL IN_LINE ( line, cval, num, icol, ityp, iwid, ilab,
     +		       smth, fltr, scflag, ier )
C*
	ip1arr (1) = icol (1)
 	ip1arr (2) = ityp (1)
	ip1arr (3) = iwid (1)
	IF ( num .eq. 2 ) THEN
	  ip2arr (1) = icol (2)
	  ip2arr (2) = ityp (2)
	  ip2arr (3) = iwid (2)
	END IF
C
C*	Make sure that parameters have been capitalized.
C
	DO  i = 1, 3
	    CALL ST_LCUC  ( parms (i), parms (i), ier )
	END DO
C
C*	Set the unit of TEMPERATURE.
C
	jk = INDEX ( parms (2), 'K' )
	jf = INDEX ( parms (2), 'F' )
	IF ( jk .ne. 0 ) THEN
	    tunit = 'K'
	ELSE IF ( jf .ne. 0 ) THEN
	    tunit = 'F'
	ELSE
	    tunit = 'C'
	END IF
C
C*	Get wind input.
C
	CALL IN_WIND  ( wind, wintyp, winuni, iwncol, iret )
	IF  ( iwncol .ne. 0 )  THEN
	    parms (4) = 'DRCT'
	    IF  ( winuni .eq. 'K' )  THEN
		parms (5) = 'SKNT'
	      ELSE
		parms (5) = 'SPED'
	    END IF
	END IF
C
C*	Check that something is to be computed.
C
	IF  ( ( parms (2) .ne. ' ' ) .or. ( parms (3) .ne. ' ' ) .or.
     +	      ( parms (4) .ne. ' ' ) .or. ( parms (5) .ne. ' ' ) )  THEN
C
C*	    See which parameters can be computed.
C
	    CALL PC_DFLV  ( 5, parms, chrflg, cmpflg, n, ier )
C
C*	    Check that the vertical coordinate can be computed.
C
	    IF  ( .not. cmpflg (1) )  THEN
		iret = -7
		CALL ER_WMSG  ( 'SNPROF', iret, vcoord, ier )
		RETURN
	    END IF
C
C*	    Set conditions on parameters.
C
	    CALL PC_SLCD ( 5, prmcnd, ier )
C
C*	    Check parameter 1.
C
	    IF  ( parms (2) .ne. ' ' )  THEN
		IF  ( .not. cmpflg (2) )  THEN
		    CALL ER_WMSG  ( 'SNPROF', +1, parms (2), ier )
		    pname1 = ' '
		  ELSE IF  ( chrflg (2) )  THEN
		    CALL ER_WMSG  ( 'SNPROF', +2, parms (2), ier )
		    pname1 = ' '
		  ELSE
		    pname1 = parms (2)
		END IF
	      ELSE
		pname1 = ' '
	    END IF
C
C*	    Check parameter 2.
C
	    IF  ( parms (3) .ne. ' ' )  THEN
		IF  ( .not. cmpflg (3) )  THEN
		    CALL ER_WMSG  ( 'SNPROF', +1, parms (3), ier )
		    pname2 = ' '
		  ELSE IF  ( chrflg (3) )  THEN
		    CALL ER_WMSG  ( 'SNPROF', +2, parms (3), ier )
		    pname2 = ' '
		  ELSE
		    pname2 = parms (3)
		END IF
	      ELSE
		pname2 = ' '
	    END IF
C
C*	    Check winds.
C
	    IF  ( parms (4) .ne. ' ' )  THEN
		IF  ( ( .not. cmpflg (4) ) .or. ( .not. cmpflg (5) ) )
     +								THEN
		    CALL ER_WMSG  ( 'SNPROF', +3, ' ', ier )
		    iwncol = 0
		END IF
	    END IF
	END IF
C
C*	Check that computable parameters have a color.
C
	IF  ( ( pname1 .ne. ' ' ) .and. ( ip1arr (1) .eq. 0 ) )  THEN
	    CALL ER_WMSG  ( 'SNPROF', +4, pname1, ier )
	END IF
C*
	IF  ( ( pname2 .ne. ' ' ) .and. ( ip2arr (1) .eq. 0 ) )  THEN
	    CALL ER_WMSG  ( 'SNPROF', +4, pname2, ier )
	    pname2 = ' '
	END IF
C*
	RETURN
	END
