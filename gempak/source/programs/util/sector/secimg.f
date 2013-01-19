	SUBROUTINE SECIMG  ( garea, pixres, rnvblk, iret )
C************************************************************************
C* SECIMG								*
C*									*
C* This subroutine parses the input for the subset area and the pixel	*
C* resolution for the new image.					*
C*									*
C* SECIMG  ( GAREA, PIXRES, RNVBLK, IRET )				*
C*									*
C* Input parameters:							*
C*	GAREA		CHAR*		Graphics area input		*
C*	PIXRES		CHAR*		Pixel resolution input		*
C*									*
C* Input/Output parameters:						*
C*	RNVBLK (LLNNAV)	REAL		Navigation block		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = Normal return		*
C*					 -6 = Too few points		*
C*					 -7 = No points from original	*
C*					 -8 = Bad bounds		*
C*					 -9 = Invalid pixel resolution	*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 7/96						*
C* S. Jacobs/NCEP	 8/96	Fixed error codes for bad pixres and	*
C*				invalid bounds for the new image	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	garea, pixres
	REAL		rnvblk (*)
C*
	CHARACTER	cdproj*30, tarea*72
	REAL		centrd (2), gltln (4), xin (2), yin (2),
     +			xout (2), yout (2)
	LOGICAL		cont
C*
C------------------------------------------------------------------------
	iret  = 0
	tarea = ' '
	cont  = .true.
C
C*	Parse the graphics area.
C
	CALL ST_LCUC ( garea, tarea, ier )
	IF  ( tarea .ne. 'DSET' )  THEN
	    CALL LC_GARE ( tarea, gltln, cdproj, centrd, ier )
C
C*	    Set the navigation block values.
C
	    rnvblk ( 7) = gltln (1)
	    rnvblk ( 8) = gltln (2)
	    rnvblk ( 9) = gltln (3)
	    rnvblk (10) = gltln (4)
C
C*	    Find the normal coordinates for the requested area.
C
	    xin (1) = gltln (1)
	    yin (1) = gltln (2)
	    xin (2) = gltln (3)
	    yin (2) = gltln (4)
	    CALL GTRANS ( 'M', 'N', 2, xin, yin, xout, yout, ier )
C
C*	    Check for bad garea plot bounds.
C
	    IF  ( ( ier .ne. 0 ) .or.
     +		  ( xout (1) .eq. RMISSD ) .or.
     +		  ( xout (2) .eq. RMISSD ) .or.
     +		  ( yout (1) .eq. RMISSD ) .or. 
     +		  ( yout (2) .eq. RMISSD ) ) THEN
		cont = .false.
	    END IF
C
C*	    Get the plot bounds.
C
	    IF  ( cont )  THEN
		CALL GQBND ( 'P', xl, yb, xr, yt, ier )
C
C*		Get the image size from the navigation block.
C
		imxsiz = rnvblk (5)
		imysiz = rnvblk (6)
C
C*		Get the new image bounds.
C
		ixlef = NINT ( imxsiz - (xr - xout (1)) / (xr - xl) *
     +				(imxsiz - 1) )
		iytop = NINT ( 1 - (yt - yout (2)) / (yt - yb) *
     +				(1 - imysiz) )
		ixrit = NINT ( imxsiz - (xr - xout (2)) / (xr - xl) *
     +				(imxsiz - 1) )
		iybot = NINT ( 1 - (yt - yout (1)) / (yt - yb) *
     +				(1 - imysiz) ) 
C
C*		Reset the image size.
C
		rnvblk (5) = ixrit - ixlef + 1
		rnvblk (6) = iybot - iytop + 1
	    END IF
	  ELSE
C
C*		Set the image size from the navigation block.
C
		imxsiz = rnvblk (5)
		imysiz = rnvblk (6)
C
C*		Set the new image bounds to the original image.
C
		ixlef = 1
		iytop = 1
		ixrit = imxsiz
		iybot = imysiz
	END IF
C
C*	Check the new image bounds.
C*	Is ixlef < ixrit and iytop < iybot?
C*	Is ixlef to the right of the original image?
C*	Is ixrit to the left of the original image?
C*	Is iytop below the original image?
C*	Is iybot above the original image?
C
	IF  ( ( ixlef .ge. ixrit ) .or. ( iytop .ge. iybot ) )  THEN
	    iret = -8
	    RETURN
	END IF
	IF  ( ( ixlef .ge. imxsiz ) .or. ( ixrit .le. 1 ) .or.
     +	      ( iytop .ge. imysiz ) .or. ( iybot .le. 1 ) )  THEN
	    iret = -7
	    RETURN
	END IF
C
C*	Set the pixel resolution.
C
	CALL ST_NUMB ( pixres, ipix, ier )
	IF  ( ( ier .ne. 0 ) .or. ( ipix .lt. 1 ) )  THEN
	    iret = -9
	    RETURN
	END IF
C
C*	Reset the image size. Make sure that the image has at least
C*	16 pixels in each direction.
C
	rnvblk (5) = INT ( rnvblk (5) / ipix )
	rnvblk (6) = INT ( rnvblk (6) / ipix )
	IF  ( ( rnvblk (5) .lt. 16 ) .or.
     +	      ( rnvblk (6) .lt. 16 ) )  THEN
	    iret = -6
	    RETURN
	END IF
C
C*	Sectorize the image.
C
	IF  ( cont )  THEN
	    CALL IM_SBGN  ( imxsiz, imysiz, ixlef, iytop, ixrit,
     +			    iybot, ipix, ier )
	END IF
C*
	RETURN
	END
