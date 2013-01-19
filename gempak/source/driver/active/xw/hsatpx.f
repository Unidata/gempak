	SUBROUTINE HSATPX ( dx, dy, offx, offy, fil, 
     +			    ixout0, iyout0, ixout1, iyout1, 
     +			    iarea, mode, ipix, dxo, dyo, iret )
C************************************************************************
C* HSATPX - XW								*
C*									*
C* This subroutine gets a pixel value from an image file.		*
C*									*
C* HSATPX ( DX, DY, OFFX, OFFY, FIL, IXOUT0, IYOUT0, IXOUT1, IYOUT1, 	*
C*          IAREA, MODE, IPIX, DXO, DYO, IRET )				*
C*									*
C* Input parameters:							*
C*	DX		REAL		X coordinates			*
C*	DY		REAL		Y coordinates			*
C*      OFFX            INTEGER         Image pixel offset, x           *
C*      OFFY            INTEGER         Image pixel offset, y           *
C*	FIL		CHAR*		Image file name			*
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
C* T. Piper/SAIC	09/06	Replaced st_lstr with st_null		*
C************************************************************************
	CHARACTER*(*) 	fil
C*
	CHARACTER	imgnam*133
C------------------------------------------------------------------------
C
C*	Get the pixel value.
C
	CALL ST_NULL ( fil, imgnam, ilen, ier )
	CALL XSATPX   ( dx, dy, offx, offy, imgnam,
     +			ixout0, iyout0, ixout1, iyout1, 
     +			iarea, mode, ipix, dxo, dyo, iret )
C*
	RETURN
	END
