	SUBROUTINE SNDTRP ( tropht, trpint, delz, nlun, lun, nparms,
     +			    nlevel, hdata, iret )
C************************************************************************
C* SNDTRP								*
C*									*
C* This routine will compute the mean wind and lapse rate above and	*
C* below the tropopause.						*
C*									*
C* SNDTRP ( TROPHT, TRPINT, DELZ, NLUN, LUN, NPARMS, NLEVEL, HDATA,	*
C*	    IRET )							*
C*									*
C* Input parameters:							*
C*	TROPHT		CHAR*		Height of the tropopause	*
C*	TRPINT		CHAR*		Interval above and below trop	*
C*	DELZ		CHAR*		Height interval			*
C*	NLUN		INTEGER		Number of file numbers		*
C*	LUN (NLUN)	INTEGER		File numbers			*
C*	NPARMS		INTEGER		Number of parameters		*
C*	NLEVEL		INTEGER		Number of levels		*
C*	HDATA (LLMXLV)	REAL		Interpolated data		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sndiag.prm'
C*
	CHARACTER*(*)	tropht, trpint, delz
	INTEGER		lun(*)
	REAL		hdata(*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Initialize sums.
C
	iret  = 0
	sumbu = 0.
	sumbv = 0.
	sumbd = 0.
	sumbl = 0.
	sumbn = 0.
	sumau = 0.
	sumav = 0.
	sumad = 0.
	sumal = 0.
	suman = 0.
C
C*	Get user input.
C
	CALL ST_CRNM ( tropht, trop, ier )
	IF  ( ier .ne. 0 )  trop = 10000.
	CALL ST_CRNM ( trpint, tropint, ier )
	IF  ( ier .ne. 0 )  tropint = 1000.
	CALL ST_CRNM ( delz, deltaz, ier )
	IF  ( ier .ne. 0 )  deltaz = 500.
C
C*	Loop through the levels.
C
	DO  i = 2, nlevel-1
	    IF  ( .not. ERMISS(hdata((i-1+1)*nparms+ITEMP)) .and.
     +		  .not. ERMISS(hdata((i-1-1)*nparms+ITEMP)) )  THEN
		xlapse = ( hdata((i-1+1)*nparms+ITEMP) -
     +		           hdata((i-1-1)*nparms+ITEMP) ) * 1000. /
     +		         ( 2. * deltaz )
	    ELSE
		xlapse = RMISSD
	    END IF
	    pres = hdata((i-1)*nparms+IPRES)
	    tmpk = PR_TVRK ( hdata((i-1)*nparms+ITEMP),
     +			     hdata((i-1)*nparms+IDWPT),
     +			     hdata((i-1)*nparms+IPRES) )
	    tmpv = PR_TMKC ( tmpk )
	    dens = PR_DDEN ( pres, tmpv )
C
C*	    Find the sums for the appropriate area of the sounding.
C
	    IF  ( hdata((i-1)*nparms+IHGHT) .ge. (trop-tropint) .and.
     +		  hdata((i-1)*nparms+IHGHT) .lt. trop )  THEN
		IF  ( .not. ERMISS(hdata((i-1)*nparms+IUWND)) )  THEN
		    sumbu = sumbu + hdata((i-1)*nparms+IUWND) * dens
		    sumbv = sumbv + hdata((i-1)*nparms+IVWND) * dens
		    sumbd = sumbd + dens
		END IF
		IF  ( .not. ERMISS(xlapse) )  THEN
		    sumbl = sumbl + xlapse * dens
		    sumbn = sumbn + dens
		END IF
	    ELSE IF  ( hdata((i-1)*nparms+IHGHT) .le. (trop+tropint)
     +		 .and. hdata((i-1)*nparms+IHGHT) .gt. trop )  THEN
		IF  ( .not. ERMISS(hdata((i-1)*nparms+IUWND)) )  THEN
		    sumau = sumau + hdata((i-1)*nparms+IUWND) * dens
		    sumav = sumav + hdata((i-1)*nparms+IVWND) * dens
		    sumad = sumad + dens
		END IF
		IF  ( .not. ERMISS(xlapse) )  THEN
		    sumal = sumal + xlapse * dens
		    suman = suman + dens
		END IF
	    END IF
	END DO
C
C*	Compute the mean wind and lapse rate for below and above the 
C*	tropopause.
C
	IF  ( sumbd .ne. 0. )  THEN
	    vb = sumbv / sumbd
	    ub = sumbu / sumbd
	    IF  ( ub .eq. 0. .and. vb .eq. 0. )  THEN
		bspd = 0.
		bdir = 0.
	    ELSE
		bspd = PR_SPED ( ub, vb )
		bdir = PR_DRCT ( ub, vb )
	    END IF
	END IF
	IF  ( sumbn .ne. 0. .and. sumbl .ne. 0.)  THEN
	    blps = sumbl / sumbn
	END IF
C
	IF  ( sumad .ne. 0. )  THEN
	    va = sumav / sumad
	    ua = sumau / sumad
	    IF  ( ua .eq. 0. .and. va .eq. 0. )  THEN
		aspd = 0.
		adir = 0.
	    ELSE
		aspd = PR_SPED ( ua, va )
		adir = PR_DRCT ( ua, va )
	    END IF
	END IF
	IF  ( suman .ne. 0. .and. sumal .ne. 0.)  THEN
	    alps = sumal / suman
	END IF
C
C*	Write the output.
C
	DO  k = 1, nlun
	    WRITE ( lun(k), 1000 ) trop, tropint, bspd, bdir, blps,
     +					 tropint, aspd, adir, alps
	END DO
1000	FORMAT ( /, '   TROPOPAUSE MEAN WIND AND LAPSE RATE', /,/,
     +              ' Tropopause Height - Input : ',F10.2,' m',/,
     +              ' Values in layer ',F10.2,' m below tropopause:',/,
     +              '     Mean Wind Speed     : ',F10.2,' m/s',/,
     +              '     Mean Wind Direction : ',F10.2,' degrees',/,
     +              '     Lapse Rate          : ',F10.2,' C/km',/,
     +              ' Values in layer ',F10.2,' m above tropopause:',/,
     +              '     Mean Wind Speed     : ',F10.2,' m/s',/,
     +              '     Mean Wind Direction : ',F10.2,' degrees',/,
     +              '     Lapse Rate          : ',F10.2,' C/km' )
C*
	RETURN
	END
