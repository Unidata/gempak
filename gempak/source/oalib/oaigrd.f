	SUBROUTINE OA_IGRD  ( ngrid, kex, key, kexkey, iextnd, iglvl,
     +	                      igvcr, gsflag, gsdttm, gfuncs, grid, iret )
C************************************************************************
C* OA_IGRD								*
C*									*
C* This subroutine initializes grid data to either zero or first-guess	*
C* grid values.								*
C*									*
C* OA_IGRD ( NGRID, KEX, KEY, KEXKEY, IEXTND, IGLVL, IGVCR, GSFLAG,	*
C*           GSDTTM, GFUNCS, GRID, IRET )				*
C*									*
C* Input parameters:							*
C*	NGRID		INTEGER		Number of grids			*
C*	KEX		INTEGER		X dimension of extend grid	*
C*	KEY		INTEGER		Y dimension of extend grid	*
C*	KEXKEY		INTEGER		KEX * KEY			*
C*	IEXTND (4)	INTEGER		Extend region specification	*
C*	IGLVL (NGRID)	INTEGER		Levels of grids			*
C*	IGVCR (NGRID)	INTEGER		Vertical coordinates		*
C*					  0 = NONE			*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*	GSFLAG		LOGICAL		Flag for first guess		*
C*      GSDTTM          CHAR*           Guess time                      *
C*	GFUNCS (NGRID)	CHAR*		Guess functions			*
C*									*
C* Output parameters:							*
C*	GRID		REAL		Grid data			*
C*	 (NGRID,KEXKEY)							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = guess does not exist	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/GSC		 4/90	Rewrote for first guess			*
C* J. Nielsen/SUNYA	10/90	Corrected call to DG_GRID		*
C*				Changed call to OA_GUES			*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* T. Tian/SAIC		 3/05	Removed GADTTM, GPARM, added GFUNCS	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gsdttm, gfuncs (*)
	INTEGER		iglvl (*), igvcr (*), iextnd (*)
	REAL		grid ( ngrid, kexkey )
C*
	INTEGER		level (2)
	REAL		tmpgrd (LLMXGD)
	CHARACTER*12	glevel, gvcord, parm
	CHARACTER	pfunc*72, gfunc*72, time (2)*20
	LOGICAL		gsflag
C------------------------------------------------------------------------
	iret = 0
C
C*	Set the grids to zero or missing depending on whether or not a
C*	first guess field is being used.
C
	IF ( gsflag ) THEN
	  value = RMISSD
	ELSE
	  value = 0.0
	END IF
C
C*	Initialize each grid--loop over the grids.
C
	DO  ing = 1, ngrid
C
C*	  Set grid points to VALUE set above.
C
	  DO i = 1, kexkey
	    grid ( ing, i ) = value
	  END DO
C*
	  IF ( gsflag ) THEN
C
C*	    Convert levels and vertical coordinate values from integer
C*	    to character strings for DG_GRID.
C
            CALL ST_INCH ( iglvl (ing), glevel, ier )
	    IF ( igvcr (ing) .eq. 1 ) THEN
	        gvcord  = 'PRES'
            ELSE IF ( igvcr (ing) .eq. 2 ) THEN
	        gvcord  = 'THTA'
	    ELSE IF ( igvcr (ing) .eq. 3 ) THEN
	        gvcord  = 'HGHT'
	    ELSE
	        gvcord  = 'NONE'
	    END IF
C
C*	    Call DG subroutine to get the first guess grid.
C
            gfunc = gfuncs (ing)
	    CALL DG_GRID ( gsdttm, glevel, gvcord, gfunc,
     +			   pfunc, tmpgrd, igx, igy, time, level,
     +                     ivcord, parm, iret )
	    IF ( iret .ne. 0 ) THEN
	      CALL ER_WMSG ( 'DG', iret, ' ', ier )
	      iret = -5
	      CALL ER_WMSG ( 'OA', iret, ' ', ier )
	      RETURN
	    END IF
C
C*	    Set the guess grid onto the analysis grid.
C
	    CALL OA_GUES ( tmpgrd, igx, igy, iextnd, grid, ing,
     +			   ngrid, kex, key, iret )
C*
	  END IF
C*
	END DO
C*
	RETURN
	END
