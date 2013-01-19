	SUBROUTINE SNPPLN  ( ithtal, ithtel, imixrl, pname1, pname2,
     +			     tunit, vcoord, ystrt,  ystop, xstrt, xstop,
     +			     iret )
C************************************************************************
C* SNPPLN								*
C*									*
C* This subroutine draws theta, theta-e and mixing ratio lines for	*
C* SNPROF.								*
C*									*
C* SNPPLN  ( ITHTAL, ITHTEL, IMIXRL, PNAME1, PNAME2, TUNIT, VCOORD,	*
C*           YSTRT, YSTOP, XSTRT, XSTOP, IRET )				*
C*									*
C* Input parameters:							*
C*	ITHTAL (3)	INTEGER		Theta color/ type/ width	*
C*	ITHTEL (3)	INTEGER		Theta-e color/ type/ width	*
C*	IMIXRL (3)	INTEGER		Mixing ratio color/ type/ width	*
C*	PNAME1		CHAR*		Parameter1 name			*
C*	PNAME2		CHAR*		Parameter2 name			*
C*	TUNIT		CHAR*1		Temperature unit (K,C,F)	*
C*	VCOORD		CHAR*		Vertical coordinate		*
C*	YSTRT		REAL		Bottom y			*
C*	YSTOP		REAL		Top y				*
C*	XSTRT		REAL		Left x				*
C*	XSTOP		REAL		Right x				*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	 8/89	Use 0 for hardware lines		*
C* K. Brill/GSC		09/89   Specified limits for thta, thte, mixr	*
C* K. Brill/NMC		02/92	Added TUNIT				*
C* K. Brill/NMC		02/92	+6 error message rather than -8		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		ithtal (*), ithtel (*), imixrl (*)
	CHARACTER*(*)	pname1, pname2, vcoord, tunit
C*
	REAL		pres (50), temp (50)
C*
	REAL		rmline (11)
	DATA		rmline  / .1, .4, 1., 2., 4., 7., 10., 16., 24.,
     +				  32., 40. /
C----------------------------------------------------------------------
	iret = 0
C
C*	Check to see if some lines are to be plotted.
C
	IF  ( ( ithtal (1) .le. 0 ) .and. ( ithtel (1) .le. 0 ) .and.
     +	      ( imixrl (1) .le. 0 ) )  RETURN
C
C*	Check that the vertical coordinate is PRES.
C
	IF  ( vcoord .ne. 'PRES' )  THEN
	    iret = +6
	    CALL ER_WMSG  ( 'SNPROF', iret, vcoord, ier )
	    RETURN
	END IF
C
C*	Get range of pressures to use for lines.
C
	ppp   = 1050.
	npres = 1
	pres (1) = ystrt
	DO  WHILE  ( ppp .ge. ystop )
C
C*	    Add to list.
C
	    IF  ( ppp .lt. ystrt )  THEN
		npres = npres + 1
		pres ( npres ) = ppp
	    END IF
C
C*	    Decrement pressure.
C
	    ppp = ppp - 25.
	END DO
C
C*	Add top pressure to list.
C
	IF  ( npres .eq. 0 )  RETURN
	IF  ( ystop .ne. pres (npres) )  THEN
	    npres = npres + 1
	    pres ( npres ) = ystop
	END IF
C
C*	Save line characteristics.
C
	CALL GQLINE  ( i1, i2, i3, i4, ier )
C
C*	Draw theta lines.
C
	IF  ( ithtal (1) .gt. 0 )  THEN
C
C*	    Set line.
C
	    CALL GSCOLR  ( ithtal (1), ier )
	    CALL GSLINE  ( ithtal (2), 0, ithtal (3), 0, ier )
C
C*	    Loop through theta lines.
C
	    DO  itheta = ithtal ( 4 ), ithtal ( 5 ), ithtal ( 6 )
		theta = itheta
C
C*		Get value for each pressure.
C
		DO  ipres = 1, npres
		    pressr = pres ( ipres )
		    ttt    = PR_TMPK  ( pressr, theta )
		    IF  ( tunit .eq. 'C' )  THEN
			temp ( ipres ) = PR_TMKC  ( ttt )
		      ELSE IF ( tunit .eq. 'F' ) THEN
			temp ( ipres ) = PR_TMKF  ( ttt )
		      ELSE
			temp ( ipres ) = ttt
		    END IF
		END DO
C
C*		Draw line.
C
		CALL GLINE  ( 'M', npres, temp, pres, ier )
	    END DO
	END IF
C
C*	Draw theta-e lines.
C
	IF  ( ithtel (1) .gt. 0 )  THEN
C
C*	    Set line.
C
	    CALL GSCOLR  ( ithtel (1), ier )
	    CALL GSLINE  ( ithtel (2), 0, ithtel (3), 0, ier )
C
C*	    Loop through theta-e lines.
C
	    DO  itheta = ithtel ( 4 ), ithtel ( 5 ), ithtel ( 6 )
		theta = itheta
C
C*		Get value for each pressure.
C
		tg = 0.
		DO  ipres = 1, npres
		    pressr = pres ( ipres )
		    ttt    = PR_TMST  ( theta, pressr, tg)
		    tg     = ttt
		    IF  ( tunit .eq. 'C' )  THEN
			temp ( ipres ) = PR_TMKC  ( ttt )
		      ELSE IF ( tunit .eq. 'F' ) THEN
			temp ( ipres ) = PR_TMKF  ( ttt )
		      ELSE
			temp ( ipres ) = ttt
		    END IF
		END DO
C
C*		Draw line.
C
		CALL GLINE  ( 'M', npres, temp, pres, ier )
	    END DO
	END IF
C
C*	Draw default mixing ratio lines.
C
	IF  ( imixrl (1) .gt. 0 .and. imixrl (4) .eq. 0 )  THEN
C
C*	    Set line.
C
	    CALL GSCOLR  ( imixrl (1), ier )
	    CALL GSLINE  ( imixrl (2), 0, imixrl (3), 0, ier )
C
C*	    Loop through default mixing ratio lines.
C
	    DO  imixr = 1, 11
		rmix = rmline ( imixr )
C
C*		Get value for each pressure.
C
		DO  ipres = 1, npres
		    pressr = pres ( ipres )
		    ttt    = PR_DWPT  ( rmix, pressr )
		    IF  ( tunit .eq. 'K' )  THEN
			temp ( ipres ) = PR_TMCK  ( ttt )
		      ELSE IF ( tunit .eq. 'F' ) THEN
			temp ( ipres ) = PR_TMCF  ( ttt )
		      ELSE
			temp ( ipres ) = ttt
		    END IF
		END DO
C
C*		Draw line.
C
		CALL GLINE  ( 'M', npres, temp, pres, ier )
	    END DO
	END IF
C
C*	Draw user specified mixing ratio lines.
C
	IF  ( imixrl (1) .gt. 0 .and. imixrl (4) .ne. 0 )  THEN
C
C*	    Set line.
C
	    CALL GSCOLR  ( imixrl (1), ier )
	    CALL GSLINE  ( imixrl (2), 0, imixrl (3), 0, ier )
C
C*	    Loop through mixing ratio lines.
C
	    DO  imixr = imixrl ( 4 ), imixrl ( 5 ), imixrl ( 6 )
		rmix = FLOAT ( imixr )
C
C*		Get value for each pressure.
C
		DO  ipres = 1, npres
		    pressr = pres ( ipres )
		    ttt    = PR_DWPT  ( rmix, pressr )
		    IF  ( tunit .eq. 'K' )  THEN
			temp ( ipres ) = PR_TMCK  ( ttt )
		      ELSE IF ( tunit .eq. 'F' ) THEN
			temp ( ipres ) = PR_TMCF  ( ttt )
		      ELSE
			temp ( ipres ) = ttt
		    END IF
		END DO
C
C*		Draw line.
C
		CALL GLINE  ( 'M', npres, temp, pres, ier )
	    END DO
	END IF
C
C*	Reset line type.
C
	CALL GSLINE  ( i1, 0, i3, 0, ier )
C*
	RETURN
	END
