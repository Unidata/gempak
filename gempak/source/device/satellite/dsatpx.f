	SUBROUTINE DSATPX ( dx, dy, offx, offy, imgfil, 
     +			    ixout0, iyout0, ixout1, iyout1, 
     +			    iarea, mode, ipix, dxo, dyo, iret )
C************************************************************************
C* DSATPX								*
C*									*
C* This subroutine gets a pixel value from an image file.		*
C*									*
C* DSATPX ( DX, DY, IMGFIL, IXOUT0, IYOUT0, IXOUT1, IYOUT1, IAREA,	*
C*	    MODE, IPIX, IRET )						*
C*									*
C* Input parameters:							*
C*	DX		REAL		X coordinates			*
C*	DY		REAL		Y coordinates			*
C*	OFFX		INTEGER		Offset (x) from (dx,dy)		*
C*	OFFY		INTEGER		Offset (y) from (dx,dy)		*
C*	IMGFIL		CHAR*		Image file name			*
C*	IXOUT0		INTEGER		Left of the image		*
C*	IYOUT0		INTEGER		Bottom of the image		*
C*	IXOUT1		INTEGER		Right of the image		*
C*	IYOUT1		INTEGER		Top of the imaeg		*
C*	IAREA		INTEGER		Pixel area indicator		*
C*	MODE		INTEGER		Pixel mode indicator		*
C*									*
C* Output parameters:							*
C*	IPIX		INTEGER		Pixel value			*
C*	DXO		REAL		Device X at offset point	*
C*	DYO		REAL		Device Y at offset point	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* T. Lee/GSC		 9/99	Created					*
C* T. Lee/GSC		12/99	Added pixel area and mode retrieval	*
C* D.W.Plummer/NCEP	 6/04	Added offx,offy and (dxo,dyo)		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*) 	imgfil
C------------------------------------------------------------------------
	iret = NORMAL
C
	CALL HSATPX   ( dx, dy, offx, offy, imgfil, 
     +			ixout0, iyout0, ixout1, iyout1, 
     +			iarea, mode, ipix, dxo, dyo, iret )
C*
	RETURN
	END
