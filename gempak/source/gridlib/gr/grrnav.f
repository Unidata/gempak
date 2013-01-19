	SUBROUTINE GR_RNAV  ( rnvblk, proj, kx, ky, iret )
C************************************************************************
C* GR_RNAV								*
C*									*
C* This subroutine gets the projection and grid size from a grid	*
C* navigation block.							*
C*									*
C* GR_RNAV  ( RNVBLK, PROJ, KX, KY, IRET )				*
C*									*
C* Input parameters:							*
C*	RNVBLK (LLNNAV)	REAL		Navigation block		*
C*									*
C* Output parameters:							*
C*	PROJ		CHAR*		Projection name			*
C*	KX		INTEGER		Number of points in x dir	*
C*	KY		INTEGER		Number of points in y dir	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = invalid navigation	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/88	Modified from GR_RSNV			*
C* M. Linda/GSC		 9/97	Corrected right border of prologue	*
C************************************************************************
	CHARACTER*(*)	proj
	REAL		rnvblk (*)
C*
	CHARACTER	cproj*4
C-----------------------------------------------------------------------
	iret = 0
C
C*	Projection is in word 2.
C
	CALL ST_ITOC  ( rnvblk (2), 1, cproj, ier )
	proj = cproj
C
C*	Get grid sizes.
C
	kx = rnvblk (5) 
	ky = rnvblk (6) 
C
C*	Check for obvious errors.
C
	IF  ( (ier .ne. 0) .or. (kx .le. 2) .or. (ky .le. 2) )  THEN
	    iret = -6
	    RETURN
	END IF
C*
	RETURN
	END
