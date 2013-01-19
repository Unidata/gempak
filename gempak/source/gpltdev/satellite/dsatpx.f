	SUBROUTINE DSATPX ( dx, dy, offx, offy, imgfil, 
     +			    ixout0, iyout0, ixout1, iyout1, 
     +			    iarea, mode, ipix, dxo, dyo, iret )
C************************************************************************
C* DSATPX								*
C*									*
C* This subroutine gets a pixel value from an image file.		*
C*									*
C* DSATPX ( DX, DY, OFFX, OFFY, IMGFIL, IXOUT0, IYOUT0, IXOUT1, IYOUT1, *
C*          IAREA, MODE, IPIX, DXO, DYO, IRET )				*
C*									*
C* Input parameters:							*
C*	DX		REAL		X coordinates			*
C*	DY		REAL		Y coordinates			*
C*      OFFX            INTEGER         Image pixel offset, x           *
C*      OFFY            INTEGER         Image pixel offset, y           *
C*	IMGFIL		CHAR*		Image file name			*
C*	IXOUT0		INTEGER		Left of the image		*
C*	IYOUT0		INTEGER		Bottom of the image		*
C*	IXOUT1		INTEGER		Right of the image		*
C*	IYOUT1		INTEGER		Top of the image		*
C*	IAREA		INTEGER		Pixel area indicator		*
C*	MODE		INTEGER		Pixel mode indicator		*
C*									*
C* Output parameters:							*
C*	IPIX		INTEGER		Pixel value			*
C*      DXO             FLOAT           Device coordinate X at offset   *
C*      DYO             FLOAT           Device coordinate Y at offset   *
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* T. Lee/GSC		 9/99	Created					*
C* T. Lee/GSC		12/99	Added pixel area and mode retrieval	*
C* D.W.Plummer/NCEP      6/04   Added offx,offy and (dxo,dyo)           *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*) 	imgfil
	PARAMETER	(NCH = 132, NWD = NCH/4)
C*
	CHARACTER	file*132
	INTEGER		isend (6), ifile (NWD)
	REAL		rsend (2)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 2 + 2 + 2 + NWD + 6
	isend (2) = CSATPX
	CALL GPUT  ( isend, 2, iret )
C
	rsend (1) = dx
	rsend (2) = dy
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
	isend (1) = ixout0
	isend (2) = iyout0
	isend (3) = ixout1
	isend (4) = iyout1
	isend (5) = iarea
	isend (6) = mode
	CALL GPUT  ( isend, 6, iret )
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
