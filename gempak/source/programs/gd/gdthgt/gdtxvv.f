	SUBROUTINE GDTXVV ( igdfln, timfnd, npts, gvcord, ystrt, ystop,
     +			    iyaxis, gvect, ivcord, gpoint, levels,
     +			    nlev, grdoutu, grdoutv, qvlvls, nvvout,
     +			    iret )
C************************************************************************
C* GDTXVV								*
C*									*
C* This subroutine gets the wind data for a time height section.	*
C*									*
C* GDTXVV ( IGDFLN, TIMFND, NTPS, GVCORD, YSTRT, YSTOP, IYAXIS, GVECT,	*
C*	    IVCORD, GPOINT, LEVELS, NLEV, GRDOUTU,GRDOUTV, QVLVLS, 	*
C*	    NVVOUT, IRET )						*
C*									*
C* Input parameters:							*
C*	IGDFLN		  INTEGER	Grid file number		*
C*	TIMFND(NPTS)	  CHAR*		List of times in section	*
C*	NPTS		  INTEGER	# of times in section		*
C*	GVCORD		  CHAR*		User input vert coord		*
C*      YSTRT             REAL          Bottom vert coord value		*
C*      YSTOP             REAL          Top vert coord value		*
C*	IYAXIS		  INTEGER	Y axis type for final section	*
C*	GVECT	 	  CHAR*		User input function		*
C*	IVCORD		  INTEGER	Vertical coordinate 		*
C*	GPOINT		  CHAR*		User input location		*
C*	LEVELS(NLEV)	  INTEGER	Levels for input data		*
C*	NLEV		  INTEGER	# of input data levels		*
C*									*
C* Output parameters:							*
C*      GRDOUTU(NPTS,NVVOUT)REAL	Time section data on output	*
C*      GRDOUTV(NPTS,NVVOUT)REAL	Time section data on output	*
C*	QVLVLS(NVVOUT)	  REAL		Vertical levels on output	*
C*	NVVOUT		  INTEGER	# of output levels		*
C*					if nvout is non-zero on entry   *
C*					then qlvls and nvout are input  *
C*					(not output) and unchanged      *
C*	IRET		  INTEGER	Return code			*
C*					  7 = GFUNC not specified	*
C*					  0 = normal return		*
C*					 -6 = GVCORD is invalid		*
C*					-12 = no levels found		*
C*					-13 = @level not allowed	*
C*					-14 = %vcord not allowed	*
C*				        -19 = GFUNC is not valid	*
C**									*
C* Log:									*
C* T.W.Barker/WR/SSD	 8/91	Created from GDXDTA			*
C* K. Brill/NMC		11/92	Remove print *,				*
C* P. Bruehl/NWS	 7/96	Initialized lvert to false & added "/G" *
C*				for grid relative winds			*
C* D. Keiser/GSC	 8/96	Clean up				*
C* T. Lee/GSC		 1/99	Changed timfnd(LLMXTM) to timfnd(*)	*
C* K. Brill/HPC		10/04	Changes for new files/times management  *
C* S. Gilbert/NCEP      08/07   Checked return code of DG_CXGP          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gvcord, gvect, timfnd (*), gpoint
	CHARACTER	time (2)*20
	REAL		rlvl ( LLMXLV ), qvlvls (*)
C*
	LOGICAL		lavflg , lvlflg, lvert, gottm
	CHARACTER	gvecx*80
	INTEGER		levels (LLMXLV)
	REAL		grdoutu (*)
	REAL		grdoutv (*), valu (LLMXLV), valv (LLMXLV)
	REAL		grd1 (LLMXLV), uu (1), vv (1)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	lvert = .false.
C
C*	Check to see if the function involves a layer average.
C
	CALL GDTXLA ( gvect, lavflg, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Don't allow CIRC functions
C
	call ST_LCUC ( gvect, gvecx, iret )
	islsh = INDEX ( gvecx, 'CIRC' )
	IF ( islsh .ne. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Check for /v in gvect (from gdcross program) not sure if needed.
C*	Also, check for /G indicating grid relative vectors.
C
	islsh = INDEX ( gvecx, '/V' )
	islshg = INDEX ( gvecx, '/G' )
	IF ( islsh .ne. 0 ) THEN
	    lvert = .true.
	    gvecx = gvecx ( : islsh - 1 )
	END IF
	IF ( islshg .ne. 0 ) THEN
	   lvert = .true.
	   gvecx = gvecx ( : islshg - 1 )
	END IF
C
C*      Set interpolation type.
C
        inttyp = 1
C
C*  	Reset the internal time pointer.
C
	CALL DG_RSTM ( iret )
C
C*	Loop through times.
C
	i = 0
	gottm = .true.
	DO WHILE ( gottm )
	    i = i + 1
	    ilev = 0
	    CALL DG_NTIM ( .true., .true., time, gottm, ier )
	    IF ( ier .eq. 0 .and. gottm ) THEN
C
C*  	      Transform point location to grid coordinates:
C
	      CALL DG_CXGP  ( gpoint, ' ', 1, np, rgx, rgy,
     +                        rlat, rlon, iret )
              IF ( iret .ne. 0 ) RETURN
	      levlst = 1
C
C*	      Loop through levels
C
	      DO j = 1, nlev
		IF ( ( .not. lavflg ) .or. ( j .gt. 1 ) ) THEN
		    levt = levels (j)
		    levb = levels (j)
		    IF ( lavflg ) levb = levels ( j - 1 )
		    CALL GDTXRV ( time, gvcord, gvecx, levt, levb,
     +				  lavflg, rgx, rgy, inttyp, lvert, uu,
     +				  vv, iret )
		    IF ( ERMISS ( uu(1) ) .and.
     +			 ( ERMISS ( vv(1) ) ) .and.
     +			 ( lavflg ) .and.
     +			 ( ( j - 1 ) .gt. levlst ) ) THEN
			ifound = 0
			DO k = 2, j - levlst
			    IF ( ifound .eq. 0 ) THEN
				kk = j - k
				levt = levels (j)
				levb = levels (kk)
				CALL GDTXRV ( time, gvcord, gvecx, levt,
     +					      levb, lavflg, rgx, rgy,
     +					      inttyp, lvert, uu, vv,
     +					      iret )
				IF ( ( .not. ERMISS ( uu (1) ) ) .and.
     +				     ( .not. ERMISS ( vv (1) ) ) ) THEN
				    ifound = 1
				    levlst = j
				END IF
			    END IF
			END DO
		    END IF
		    IF ( ( .not. ERMISS ( uu (1) ) ) .and.
     +			 ( .not. ERMISS ( vv (1) ) ) ) THEN
			ilev = ilev + 1
			IF (lavflg) THEN
			    IF (ivcord.eq.1) THEN
				rlvl (ilev) =
     +				    sqrt ( FLOAT (levt) * FLOAT (levb) )
			    ELSE
				rlvl (ilev) =
     +				    0.5 *
     +				    ( FLOAT (levt) * FLOAT (levb) )
			    END IF
			ELSE
			    rlvl (ilev) = FLOAT (levt)
			END IF	
			valu (ilev) = uu (1)
			valv (ilev) = vv (1)
		    END IF
		END IF
	      END DO
	    END IF
	    IF ( nvvout .ne. 0 ) THEN
		lvlflg = .true.
	    ELSE
		lvlflg = .false.
	    END IF
	    IF ( ilev .gt. 1 ) THEN
		CALL GDTXGD ( valu, 1, ilev, ivcord, 1, rlvl, ystrt,
     +			      ystop, lvlflg, grd1, qvlvls, nvvout,
     +			      iret )
		DO j = 1, nvvout
		    indx = ( ( j - 1 ) * npts ) + i
		    grdoutu (indx) = grd1 (j)
		END DO
		CALL GDTXGD ( valv, 1, ilev, ivcord, 1, rlvl, ystrt,
     +			      ystop, lvlflg, grd1, qvlvls, nvvout,
     +			      iret )
		DO j = 1, nvvout
		    indx = ( ( j - 1 ) * npts ) + i
		    grdoutv (indx) = grd1 (j)
		END DO
	    ELSE IF ( i .le. npts ) THEN
		DO j = 1, nvvout
		    indx = ( ( j - 1 ) * npts ) + i
		    grdoutu (indx) = RMISSD
		    grdoutv (indx) = RMISSD
		END DO
	    END IF
	END DO
	RETURN
	END
