	SUBROUTINE GDTXDA ( igdfln, timfnd, npts, gvcord,
     +			    ystrt, ystop, iyaxis, gfunc,
     +			    ivcord, gpoint, levels, nlev,
     +			    grdout, qlvls, nvout, iret )
C************************************************************************
C* GDTXDA								*
C*									*
C* This subroutine gets the data to contour for a time/height section.	*
C*									*
C* GDTXDA( IGDFLN, TIMFND, NTPS, GVCORD, YSTRT, YSTOP, IYAXIS, GFUNC,   *
C*	   IVCORD, GPOINT, LEVELS, NLEV, GRDOUT, QLVLS, NVOUT, IRET)	*
C*									*
C* Input parameters:							*
C*	IGDFLN		  INTEGER	Grid file number		*
C*	TIMFND(NPTS)	  CHAR*		List of times in section	*
C*	NPTS		  INTEGER	# of times in section		*
C*	GVCORD		  CHAR*		User input vert coord		*
C*      YSTRT             REAL          Bottom vert coord value		*
C*      YSTOP             REAL          Top vert coord value		*
C*	IYAXIS		  INTEGER	Y axis type for final section	*
C*	GFUNC	 	  CHAR*		User input function		*
C*	IVCORD		  INTEGER	Vertical coordinate 		*
C*	GPOINT		  CHAR*		User input location		*
C*	LEVELS(NLEV)	  INTEGER	levels for input data		*
C*	NLEV		  INTEGER	# of input data levels		*
C*									*
C* Output parameters:							*
C*      GRDOUT(NPTS,NVOUT)REAL		Time section data on output	*
C*	QLVLS(NVOUT)	  REAL		Vertical levels on output	*
C*	NVOUT		  INTEGER	# of output levels		*
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
C* T. Lee/GSC		 1/99	Changed timfnd (LLMXTM) to timfnd(*)	*
C* K. Brill/HPC		10/04	Changes for new files/times management  *
C* S. Gilbert/NCEP	08/07	Checked return code of DG_CXGP		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gvcord, gfunc, timfnd (*), gpoint
	CHARACTER	time (2)*20
	REAL		rlvl ( LLMXLV ), qlvls (*)
C*
	LOGICAL		lavflg , lvlflg, gottm
	INTEGER		levels (LLMXLV)
	REAL		val (LLMXLV), grdout (*)
	REAL		grd1 (LLMXLV)
	INCLUDE		'ERMISS.FNC'
C*
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Check to see if the function involves a layer average.
C
	CALL GDTXLA ( gfunc, lavflg, iret )
	IF ( iret .ne. 0 ) RETURN
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
              if ( iret .ne. 0 ) RETURN
	      levlst = 1
C
C*	      Loop through levels
C
	      DO j = 1, nlev
		IF ( ( .not. lavflg ) .or. ( j .gt. 1 ) ) THEN
		    levt = levels (j)
		    levb = levels (j)
		    IF ( lavflg ) levb = levels ( j - 1 )
		    CALL GDTXRD ( time, gvcord, gfunc, levt, levb,
     +				  lavflg, rgx, rgy, inttyp, yy, iret )
		    IF ( ERMISS ( yy ) .and. ( lavflg ) .and.
     +			 ( ( j - 1 ) .gt. levlst ) ) THEN
			ifound = 0
			DO k = 2, j - levlst
			    IF ( ifound .eq. 0 ) THEN
				kk = j - k
				levt = levels (j)
				levb = levels (kk)
				CALL GDTXRD ( time, gvcord, gfunc,
     +					      levt, levb, lavflg, rgx,
     +					      rgy, inttyp, yy, iret )
				IF ( .not. ERMISS ( yy ) ) THEN
				    ifound=1
				    levlst=j
				END IF
			    END IF
			END DO
		    END IF
		    IF ( .not. ERMISS ( yy ) ) THEN
			ilev = ilev + 1
			IF ( lavflg ) THEN
			    IF ( ivcord .eq. 1 ) THEN
				rlvl (ilev) =
     +				    sqrt ( float (levt) * float (levb) )
			    ELSE
				rlvl (ilev) =
     +				    0.5 *
     +				    ( float (levt) * float (levb) )
			    END IF
			ELSE
			    rlvl (ilev) = float (levt)
			END IF
			val (ilev) = yy
		    END IF
		END IF
	      END DO
	    END IF
	    IF ( nvout .ne. 0 ) THEN
		lvlflg = .true.
	    ELSE
		lvlflg = .false.
	    END IF
	    IF ( ilev .gt. 1 ) THEN
		CALL GDTXGD ( val, 1, ilev, ivcord, iyaxis, rlvl, ystrt,
     +			      ystop, lvlflg, grd1, qlvls, nvout, iret )
		DO j = 1, nvout
		    indx = ( ( j - 1 ) * npts ) + i
		    grdout (indx) = grd1 (j)
		END DO
	    ELSE IF ( i .le. npts ) THEN
		DO j = 1, nvout
		    indx = ( ( j - 1 ) * npts ) + i
		    grdout (indx) = RMISSD
		END DO
	    END IF
	END DO
	RETURN
	END
