	SUBROUTINE SFXPRM  ( newfil, ntrace, trace, trcur, parms, 
     +			     ntparm, icolor, iptprm, prmtyp, range, 
     +			     witnes, nparms, iret )
C************************************************************************
C* SFXPRM								*
C*									*
C* This subroutine extracts and sets up parameters to process.		*
C*									*
C* SFXPRM  ( NEWFIL, NTRACE, TRACE, TRCUR, PARMS, NTPARM, ICOLOR,	*
C*           IPTPRM, PRMTYP, RANGE, WITNES, NPARMS, IRET )		*
C*									*
C* Input parameters:							*
C*	NEWFIL		LOGICAL		New file flag			*
C*	NTRACE		INTEGER		Number of traces		*
C*	TRACE (5)	CHAR*		Input for traces		*
C*									*
C* Input and output parameters:						*
C*	TRCUR (5)	CHAR*		Current trace values		*
C*									*
C* Output parameters:							*
C*	PARMS (4,2,5)	CHAR*		Parameters			*
C*	NTPARM (2,5)	INTEGER		Number of parameters		*
C*	ICOLOR (4,2,5)	INTEGER		Colors				*
C*	IPTPRM (4,2,5)	INTEGER		Pointers to parameters		*
C*	PRMTYP (4,2,5)	CHAR*		Parameter type			*
C*	RANGE  (2,5)	CHAR*		Input for range			*
C*	WITNES (2,5)	CHAR*		Input for witness lines		*
C*	NPARMS		INTEGER		Number of parameters		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-15 = no parameters		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/90	Rewrote					*
C* M. desJardins/GSFC	10/90	Eliminated newprm			*
C* S. Jacobs/SUNYA	 5/91	Insert choice for wind barbs		*
C*				(BRBM=m/s or BRBK=knts)			*
C* K. Brill/NMC		06/91	Correction to check for ARRW and DARR	*
C* S. Jacobs/SSAI	 9/91	Added sky cover and cloud type plotting	*
C* K. Brill/NMC		12/91	Added pressure tndncy & past wx symbols;*
C*				Changes for combined wind (sssddd);	*
C*				Added prmtyp 'P' for SKYK or SKYM	*
C* S. Jacobs/NCEP	 2/96	Changed ttt from 48 to 72 characters	*
C* D. Kidwell/NCEP	 5/98	Added check for DAWV; np->nparms on last*
C*				DO loop & set prmtyp = ' ' to fix bug   *
C* D. Kidwell/NCEP	 3/02	Added check for DASH                    *
C* D. Kidwell/NCEP	 9/02	Added check for BRGK, TWSY, TSKC        *
C* D. Kidwell/NCEP	 5/03	Added check for TPWS, AWSY, VWSY, WSKC  *
C* D. Kidwell/NCEP	10/04	Added check for TCSL                    *
C* D. Kidwell/NCEP	 4/05	Added check for BRPK                    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	trace (*), trcur (*), parms (4,2,*), 
     +			prmtyp (4,2,*), range (2,*), witnes (2,*)
	LOGICAL		newfil
	INTEGER		icolor (4,2,*), ntparm (2,*), iptprm (4,2,*)
C*
	LOGICAL		cmpflg (MMPARM), chrflg (MMPARM),
     +			levflg (MMPARM), newprm
	INTEGER		ksave (MMPARM), jsave (MMPARM), isave (MMPARM)
	CHARACTER	tpart (2)*72, tarr (4)*48, parr (4)*12,
     +			ppp (MMPARM)*4, p*12, ttt*72, wprm*4
C------------------------------------------------------------------------
	iret   = 0
C
C*	Check current values of trace.
C
	IF  ( newfil )  THEN
	    newprm = .true.
	  ELSE
	    newprm = .false.
	    DO  i = 1, ntrace
		IF  ( trace (i) .ne. trcur (i) )  THEN
		    newprm = .true.
		    trcur (i) = trace (i)
		END IF
	    END DO
	END IF
C
C*	Return if there is no change.
C
	IF  ( .not. newprm )  RETURN
C
C*	Initialize output arrays.
C
	DO  i = 1, 4
	    DO  j = 1, 2
		DO  k = 1, 5
		    parms (i,j,k)  = ' '
		    iptprm (i,j,k) = 0
		    prmtyp (i,j,k) = ' '
		END DO
	    END DO
	END DO
C
C*	Loop through the traces breaking the input into parts.
C
	nparms = 0
	DO  i = 1, ntrace
C
C*	    Break the list at the ! which separates the left and right
C*	    sides.
C
	    CALL ST_LCUC  ( trace (i), ttt, ier )
	    CALL ST_CLST  ( ttt, '!', ' ', 2, tpart, n, ier )
C
C*	    Process the left and then the right side.
C
	    DO  j = 1, 2
C
C*		Break string into PARMS / COLORS / RANGE / WITNES.
C
		CALL ST_CLST  ( tpart (j), '/', ' ', 4, tarr, n, ier )
C
C*		Get the parameter names.
C
		CALL ST_CLST ( tarr (1), ';', ' ', 4, parr, n, ier )
C
C*		Get parameters into array and check conditions.
C
		ntparm (j,i) = 0
		DO  k = 1, 4
C
C*		    Add parameter to the list for trace.  Also keep
C*		    running list of parameters and pointers showing
C*		    which trace and which parameter it is.
C
		    p = parr (k) ( 1:4 )
		    parms  ( k, j, i ) = parr (k)
C
C*		    Skip blank parameters and add others to the list.
C
		    IF  ( p .ne. ' ' )  THEN
			ntparm ( j, i ) = ntparm ( j, i ) + 1
			nparms = nparms + 1
			ppp ( nparms ) = parr (k) ( 1:4 )
			isave ( nparms ) = i
			jsave ( nparms ) = j
			ksave ( nparms ) = k
			iptprm ( k, j, i ) = nparms
C
C*			Save location of parameter in parms list.
C
			IF  ( ( p .eq. 'BARB' ) .or. 
     +			      ( p (1:3) .eq. 'BRB' ) .or.
     +			      ( p .eq. 'BRGK' ) .or.
     +			      ( p .eq. 'BRPK' ) .or.
     +			      ( p (1:3) .eq. 'ARR' ) .or.
     +			      ( p .eq. 'DARR' ) .or.
     +			      ( p .eq. 'DAWV' ) .or.
     +			      ( p .eq. 'DASH' ) )  THEN
			    IF  ( k .lt. 4 )  THEN
				wprm = p (1:4)
				prmtyp ( k, j, i ) = 'W' 
			      ELSE
				prmtyp ( k, j, i ) = ' '
				CALL ER_WMSG  ( 'SFGRAM', -5, p, ier )
				ntparm ( j, i ) = ntparm ( j, i ) - 1
			    END IF
			  ELSE IF  ( ( p .eq. 'WSYM' ) .or.
     +				     ( p .eq. 'TWSY' ) .or.
     +				     ( p .eq. 'TPWS' ) .or.
     +				     ( p .eq. 'AWSY' ) .or.
     +				     ( p .eq. 'VWSY' ) .or.
     +				     ( p .eq. 'SKYC' ) .or.
     +				     ( p .eq. 'TSKC' ) .or.
     +				     ( p .eq. 'WSKC' ) .or.
     +				     ( p .eq. 'PWTH' ) .or.
     +				     ( p .eq. 'PTND' ) .or.
     +                               ( p .eq. 'PTSY' ) .or.
     +				     ( p .eq. 'CSYL' ) .or.
     +				     ( p .eq. 'CSYM' ) .or.
     +				     ( p .eq. 'CSYH' ) .or.
     +				     ( p .eq. 'CSYT' ) .or.
     +				     ( p .eq. 'TCSL' ) )  THEN
			    IF  ( k .lt. 4 )  THEN
				prmtyp ( k, j, i ) = 'S' 
			      ELSE
				prmtyp ( k, j, i ) = ' '
				CALL ER_WMSG  ( 'SFGRAM', -5, p, ier )
				ntparm ( j, i ) = ntparm ( j, i ) - 1
			    END IF
			  ELSE IF  ( ( p .eq. 'GUST' ) .or.
     +				     ( p .eq. 'GUMS' ) )  THEN
			    prmtyp ( k, j, i ) = 'G'
			  ELSE IF  ( ( p .eq. 'SKYM' ) .or.
     +				     ( p .eq. 'SKYK' ) )  THEN
                            IF  ( k .lt. 4 )  THEN
                                prmtyp ( k, j, i ) = 'P' 
                              ELSE
                                prmtyp ( k, j, i ) = ' '
                                CALL ER_WMSG  ( 'SFGRAM', -5, p, ier )
                                ntparm ( j, i ) = ntparm ( j, i ) - 1
                            END IF
			  ELSE
			    prmtyp ( k, j, i ) = 'X'
			END IF
		    END IF
		END DO
C
C*		Get the colors in the array.
C
		CALL IN_COLR  ( tarr (2), n, icolor (1, j, i), ier )
C
C*		Save the information for the range and witnes lines.
C
		range  ( j, i ) = tarr (3)
		witnes ( j, i ) = tarr (4)
	    END DO
	END DO
C
C*	If there are no parameters, return an error.
C
	IF  ( nparms .eq. 0 )  THEN
	    iret = -15
	    RETURN
	END IF
C
C*	Determine calculable parameters.
C
	CALL PC_DFLS  ( nparms, ppp, chrflg, cmpflg, levflg, np, ier )
C
C*	Check for invalid parameters and set flag for real or character
C*	data type.
C
	DO  ii = 1, nparms
	    k = ksave (ii)
	    j = jsave (ii)
	    i = isave (ii)
	    IF  ( cmpflg (ii) )  THEN
		IF  ( chrflg (ii) )  THEN
		    prmtyp ( k, j, i ) = 'C'
		  ELSE IF  ( prmtyp ( k, j, i ) .eq. 'X' )  THEN
		    prmtyp ( k, j, i ) = 'R'
		END IF
	      ELSE 
		CALL ER_WMSG  ( 'SFGRAM', -7, ppp (ii), ier )
		ntparm ( j, i ) = ntparm ( j, i ) - 1
		parms  ( k, j, i ) = ' '
		iptprm ( k, j, i ) = 0
		prmtyp ( k, j, i ) = ' '
	    END IF
	END DO
C*
	RETURN
	END
