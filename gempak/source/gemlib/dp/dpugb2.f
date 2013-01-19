	SUBROUTINE DP_UGB2 ( idata, iarray, rarray, kxky, grid, iret )
C************************************************************************
C* DP_UGB2								*
C*									*
C* This subroutine unpacks a GEMPAK GRIB2 format product.		*
C*									*
C* DP_UGB2  ( IDATA, IARRAY, RARRAY, KXKY, GRID, IRET )			*
C*									*
C* Input parameters:							*
C*	IDATA (*)	INTEGER		Packed data			*
C*	IARRAY (4)	INTEGER		iuscal, kx, ky, iscan_mode	*
C*	RMSVAL (1)	REAL		Missing data value		*
C*									*
C* Output parameters:							*
C*	KXKY		INTEGER		Number of grid points		*
C*	GRID (KXKY)	REAL		Grid data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		idata (*), iarray(*), iuscal, kx, ky, kxky, 
     +			iscan_mode, iret
	REAL		grid  (*), rarray(*), rmsval
C------------------------------------------------------------------------
	iret = 0
	kxky = 0
C
	iuscal = iarray(1)
	kx = iarray(2)
	ky = iarray(3)
	iscan_mode = iarray(4)
C
	rmsval = rarray(1)
C
	CALL GB2_UGEM ( idata, iuscal, rmsval, kx, ky, iscan_mode,
     +			kxky, grid, iret )
	IF ( kx * ky .ne. kxky ) THEN
	    write(*,*) 'Unexpected KX * KY ',kx,ky,kxky
	END IF
C*
	RETURN
	END
