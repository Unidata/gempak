	SUBROUTINE GSATPX ( sys, x, y, offx, offy, imgfil, iarea, mode, 
     &			    ipix, dxo, dyo, iret )
C************************************************************************
C* GSATPX								*
C*									*
C* This subroutine gets a pixel value from an image image.		*
C*									*
C* GSATPX  ( SYS, X, Y, OFFX, OFFY, IMGFIL, IAREA, MODE, 		*
C*           IPIX, DXO, DYO, IRET )					*
C*									*
C* Input parameters:							*
C*	SYS		CHAR*		Coordinate system               *
C*					  'S' = screen coordinates      *
C*					  'D' = device coordinates      *
C*					  'N' = normalized coordinates  *
C*					  'V' = view coordinates        *
C*					  'P' = plot coordinates        *
C*					  'M' = map coordinates         *
C*					  'G' = grid coordinates        *
C*	X		REAL		X coordinates / latitudes	*
C*	Y		REAL		Y coordinates / longitudes	*
C*      OFFX            INTEGER		Image pixel offset, x		*
C*      OFFY            INTEGER		Image pixel offset, y		*
C*	IMGFIL		CHAR*		Image file name			*
C*	IAREA		INTEGER		Pixel area indicator		*
C*	MODE		INTEGER		Pixel mode indicator		*
C*									*
C* Output parameters:							*
C*	IPIX		INTEGER		Pixel value			*
C*	DXO             FLOAT           Device coordinate X at offset	*
C*	DYO             FLOAT           Device coordinate Y at offset	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* T. Lee/GSC		 9/99	Created					*
C* T. Lee/GSC		12/99	Added pixel area and mode retrieval	*
C* D.W.Plummer/NCEP      6/04   Added offx,offy and (dxo,dyo)           *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*) 	sys, imgfil
C*
	PARAMETER	(NCH = 132, NWD = NCH/4)
	CHARACTER	file*132
	INTEGER		isend (3), ifile (NWD)
	REAL		rsend (2)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check validity of the coordinate system.
C
	isys = INDEX ( syslo, sys ) + INDEX ( sysup, sys )
	IF  ( isys .eq. 0 )  THEN
	    iret = NOCORD
	    RETURN
	END IF
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 3 + 2 + 2 + NWD + 2
	isend (2) = FSATPX
	isend (3) = isys
	CALL GPUT  ( isend, 3, iret )
C 
	rsend (1) = x
	rsend (2) = y
	CALL GPUTR  ( rsend, 2, iret )
C
	isend (1) = offx
	isend (2) = offy
	CALL GPUT  ( isend, 2, iret )
C
C*	Convert the character string to an integer array and send it.
C
	file = imgfil
	CALL ST_STOI ( file, NCH, nw, ifile, iret )
	CALL GPUT    ( ifile, NWD, iret ) 
C
C*	Load pixel information.
C
	isend (1) = iarea
	isend (2) = mode
	CALL GPUT ( isend, 2, iret )
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  THEN
	    iret = ier
	    RETURN
	END IF
C
	CALL GGET  ( ipix, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C
	CALL GGETR  ( dxo, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
	CALL GGETR  ( dyo, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
