	SUBROUTINE ND_GNAV  ( proj, kxky, gdarea, cprj, kx, ky,  
     +			     grdout, rnvblk, iret )
C************************************************************************
C* ND_GNAV								*
C*									*
C* This subroutine takes the user input for PROJ and KXKY and 		*
C* makes a grid navigation block.    					*
C*									*
C* ND_GNAV  ( PROJ, KXKY, GDAREA, CPRJ, KX, KY, GRDOUT, RNVBLK, IRET )	*
C*									*
C* Input parameters:							*
C*	PROJ		CHAR*		User input for PROJ		*
C*	KXKY		CHAR*		User input for KXKY		*
C*	GDAREA		CHAR*		User input for GDAREA		*
C*									*
C* Output parameters:							*
C*	CPRJ		CHAR*		Grid projection			*
C*	KX		INTEGER		Number of points in x dir	*
C*	KY		INTEGER		Number of points in y dir	*
C*	GRDOUT  (4)	REAL		Grid corners			*
C*	RNVBLK  (13)	REAL		Grid navigation block		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = invalid navigation	*
C*					 -5 = invalid grid area		*
C*					 -6 = invalid grid size		*
C**									*
C* Log:									*
C* S. Jacobs/EAI	 7/93	Copied from GDCNAV			*
C* G. Krueger/EAI	 6/96	Add default projection			*
C************************************************************************
	CHARACTER*(*)	proj, kxky, gdarea, cprj
	REAL		rnvblk (*)
C*
	CHARACTER	cdproj*30
	REAL		zmarg (4), rarr (2), rltln (4), grdout (4),
     +			angle (3), centrd (2)
	INTEGER		iarr  (2)
	LOGICAL		angflg, space
C------------------------------------------------------------------------
	iret = 0
C
C*	Translate grid projection information.
C
	CALL GG_PROJ  ( proj, cprj, angle, zmarg, angflg, ier )
	angle1 = angle (1)
	angle2 = angle (2)
	angle3 = angle (3)
C
C*	Check for error.
C
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'GG', ier, proj, ier1 )
	    iret = -4
	    RETURN
	END IF
C
C*	Translate grid area.
C
	CALL LC_GARE  ( gdarea, rltln, cdproj, centrd, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -5
	    CALL ER_WMSG  ( 'NAGRIB', iret, gdarea, ier )
	    RETURN
	END IF
C
C*	Translate the input for KXKY.
C
	IF  ( ( cprj .eq. 'CED' ) .and. ( kxky (1:1) .eq. '#' ) )  THEN
	    space = .true.
	  ELSE
	    space = .false.
	END IF
C
C*	Check for input as grid spacing.
C
	IF  ( space )  THEN
C
C*	    Get x- and y- spacing and check that there are two numbers.
C
	    CALL ST_RLST  ( kxky (2: ), ';', 0., 2, rarr, num, ier )
	    IF  ( ( rarr (1) .le. 0. ) .or. ( rarr (2) .le. 0. ) )  THEN
		iret = -6
		CALL ER_WMSG  ( 'NAGRIB', iret, ' ', ier )
		RETURN
	      ELSE
		xspace = rarr (1)
		yspace = rarr (2)
	    END IF
C
C*	    Align on grid points and exit for error.
C
	    CALL GR_ALGN ( rltln, xspace, yspace, grdout, kx, ky, iret )
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG  ( 'GR', iret, ' ', ier )
		iret = -4
		RETURN
	    END IF
C
C*	    Otherwise, find kx, ky.
C
	  ELSE
	    DO  i = 1, 4
		grdout (i) = rltln (i)
	    END DO
C
C*	    Get two numbers and check for error.
C
	    CALL ST_ILST  ( kxky, ';', 0, 2, iarr, num, ier )
	    kx = iarr (1)
	    ky = iarr (2)
	    IF  ( ( kx .lt. 2 ) .or. ( ky .lt. 2 ) )  THEN
		iret = -6
		CALL ER_WMSG  ( 'NAGRIB', iret, ' ', ier )
		RETURN
	    END IF
	END IF
C
C*	Fill navigation block.
C
	CALL GR_MNAV  ( cprj, kx, ky, grdout (1), grdout (2),
     +			grdout (3), grdout (4), angle1, angle2,
     +			angle3, angflg, rnvblk, ier )
C*
	RETURN
	END
