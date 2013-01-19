	SUBROUTINE ND_GGDS ( gdsarr, cprj, kx, ky, grdout, rnvblk,
     +			    iret )
C************************************************************************
C* ND_GGDS								*
C*									*
C* This subroutine takes the GDS information and constructs a 		*
C* navigation block for a GEMPAK grid file.				*
C*									*
C* ND_GGDS ( GDSARR, CPRJ, KX, KY, GRDOUT, RNVBLK, IRET )		*
C*									*
C* Input parameters:							*
C*	GDSARR (10)	REAL		GRIB GDS projection info	*
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
C**									*
C* Log:									*
C* T. Piper/SAIC	10/02	Modified from NAGGDS for GRIB2		*
C************************************************************************
	REAL		gdsarr (*), grdout (*), rnvblk (*)
	CHARACTER*(*)	cprj
C*
	LOGICAL		angflg
C------------------------------------------------------------------------
	iret = 0
C
C*	Set the projection.
C
	IF  ( NINT ( gdsarr (1) ) .eq. 0 )  THEN
	    cprj = 'CED'
	ELSE IF  ( NINT ( gdsarr (1) ) .eq. 10 )  THEN
	    cprj = 'MER'
	ELSE IF  ( NINT ( gdsarr (1) ) .eq. 20 )  THEN
	    cprj = 'STR'
	ELSE IF  ( NINT ( gdsarr (1) ) .eq. 30 )  THEN
	    cprj = 'LCC'
	ELSE
	    iret = -4
	    RETURN
	END IF
C
C*	Set the number of columns and rows.
	kx = NINT ( gdsarr (2) )
	ky = NINT ( gdsarr (3) )
C
C*	Set the lat/lon values of the corners.
C
	grdout (1) = gdsarr (4)
	grdout (2) = gdsarr (5)
	grdout (3) = gdsarr (6)
	grdout (4) = gdsarr (7)
C
C*	Set the projection angles.
C
	angle1 = gdsarr ( 8)
	angle2 = gdsarr ( 9)
	angle3 = gdsarr (10)
	angflg = .true.
C
C*	Fill navigation block.
C
	CALL GR_MNAV  ( cprj, kx, ky, grdout (1), grdout (2),
     +			grdout (3), grdout (4), angle1, angle2,
     +			angle3, angflg, rnvblk, ier )
C*
	RETURN
	END
