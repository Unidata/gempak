	SUBROUTINE GSATIM ( filnam, iret )
C************************************************************************
C* GSATIM								*
C*									*
C* This subroutine displays a satellite image from a file.		*
C*									*
C* GSATIM  ( FILNAM, IRET ) 						*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		File name 			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Goodman/RDS	 9/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	 6/88	Clean up				*
C* G. Krueger/EAI	12/93	Modified GREST -> GSATIM		*
C* S. Jacobs/NMC	 2/94	Clean up; Changed call seq. to DSATIM	*
C* J. Cowie/COMET	 1/95	Get image bounds from  common,		*
C*				changed calling sequence to DSATIM	*
C* J. Cowie/COMET	 5/95	Removed image bounds from DSATIM calling*
C*				sequence.				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER 	filnam*(*)
C*
	REAL		xin (2), yin (2), xout (2), yout (2)
C------------------------------------------------------------------------
C*	Check that a device has been set.
C
	IF  (  ddev .ne. ' ' ) THEN
	    iret = NORMAL
	  ELSE
	    iret = NDVICE
	    RETURN
	END IF
C
C*	Set the image and view region bounding values.
C
	CALL GQBND ( 'P', xl, yb, xr, yt, iret )
	IF ( iret .ne. NORMAL ) RETURN
	xin (1) = xl
	yin (1) = yb
	xin (2) = xr
	yin (2) = yt
	CALL GTRANS ( 'P', 'D', 2, xin, yin, xout, yout, iret )
	IF ( iret .ne. NORMAL ) RETURN
	ixout0 = xout (1)
	iyout0 = yout (1)
	ixout1 = xout (2)
	iyout1 = yout (2)
	CALL GQBND ( 'N', vxl, vyb, vxr, vyt, iret )
	IF ( iret .ne. NORMAL ) RETURN
	xin (1) = vxl
	yin (1) = vyb
	xin (2) = vxr
	yin (2) = vyt
	CALL GTRANS ( 'N', 'D', 2, xin, yin, xout, yout, iret )
	IF ( iret .ne. NORMAL ) RETURN
	ixspc0 = xout (1)
	iyspc0 = yout (1)
	ixspc1 = xout (2)
	iyspc1 = yout (2)
C
C*	Drop the image.
C
	CALL DSATIM ( filnam, ixout0, iyout0, ixout1, iyout1,
     +		      ixspc0, iyspc0, ixspc1, iyspc1,
     +		      iret )
C
	RETURN
	END
