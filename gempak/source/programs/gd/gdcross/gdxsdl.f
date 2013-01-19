	SUBROUTINE GDXSDL  ( border, nx, ny, grd, nlvl, clvl, clbl,
     +			     labflg, iret )
C************************************************************************
C* GDXSDL								*
C*									*
C* This subroutine puts contour labels along the right side of a	*
C* cross section.							*
C*									*
C* GDXSDL  ( BORDER, NX, NY, GRD, NLVL, CLVL, LABFLG, IRET )		*
C*									*
C* Input parameters:							*
C*	BORDER		CHAR*		Background attributes		*
C* 	NX		INTEGER         Number of values along X	*
C*	NY		INTEGER         Number of values along Y	*
C*	GRD   (NX, NY)	REAL		Grid of cross section THTA	*
C*      NLVL		INTEGER         Number of contour levels	*
C*      CLVL  (NLVL)	REAL		Contour levels			*
C*									*
C* Input and output parameters:						*
C*	LABFLG (NLVL)   INTEGER		Label flags			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC		 7/90						*
C* K. Brill/NMC 	10/90	Check for missing values		*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C* S. Jacobs/NCEP	10/97	Added bg attr for setting label color	*
C* K. Brill/EMC		 6/98	Declare border as CHAR			*
C* S. Jacobs/NCEP	 1/99	Changed call to IN_LINE			*
C* S. Jacobs/NCEP	 5/99	Changed call to IN_LINE			*
C* C. Bailey/HPC	10/06	Add clbl to calling seq., plot as label	*
C* C. Bailey/HPC	10/06	Changed call to IN_LINE			*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*)	border, clbl (*)
	REAL		grd ( nx, ny ), clvl ( nlvl )
	INTEGER		labflg ( nlvl )
C*
	CHARACTER       label*4
	LOGICAL		scflag
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Parse the background attributes.
C
	values = 0.
	CALL IN_LINE  ( border, values, 1, ibcolr, ibtyp, ibwid, iblab,
     +			smth, fltr, scflag, ier )
	CALL GSCOLR ( ibcolr, ier )
C*
	CALL GQSYSZ ( rxszmk, ryszmk, rxsztx, rysztx, rxszwb,
     +                ryszwb, ier )
	xr = FLOAT ( nx )
	yvold = - 1.0
	nym1 = ny - 1
	DO ii = 1, nlvl
	  IF ( labflg (ii) .ne. 0 ) THEN
	    th = clvl (ii)
C
C*	    Find the grid relative position of theta level.
C
	    DO i = 1, nym1
	      riy = -9999.
	      IF ( ( 
     +            ( th .ge. grd (nx, i) .and. th .lt. grd (nx, i+1) )  
     +	      .or. ( th .le. grd (nx, i) .and. th .gt. grd (nx, i+1) ) )
     +        .and. ( ( .not. ERMISS ( grd (nx, i) ) )
     +        .and. ( .not. ERMISS ( grd (nx, i+1) ) ) ) )  THEN
	        riy = FLOAT ( i )
	        frc = ( th - grd (nx, i) ) /
     +                     ( grd (nx, i+1) - grd (nx, i) )
	        riy = riy + frc
	      END IF
	      IF ( i .eq. nym1 .and. th .eq. grd (nx, i+1) )
     +          riy = FLOAT ( ny )
C*
	      IF ( riy .gt. 0. ) THEN
C
C*	        Plot this label.
C
		label = clbl(ii)(1:4)
	        CALL GTRANS ( 'G', 'V', 1, xr, riy, xv, yv, ier )
	        hght = ABS ( yv - yvold )
	        IF ( hght .gt. rysztx ) THEN
     	            CALL GTEXT ( 'V', xv, yv, label, 0., 2, 0 ,ier )
	            yvold = yv
	        END IF
	      END IF
	    END DO
	  END IF
	  labflg ( ii ) = 0
	END DO
C*
	RETURN
	END
