	SUBROUTINE GD_WPGDW  ( iacss, grid, igx, igy, ighdr, gdattm1,
     +			       gdattm2, level1, level2, ivcord, parm,
     +			       rewrit, ipktyp, nbits, iret )
C************************************************************************
C* GD_WPGDW								*
C*									*
C* This subroutine packs an input grid of real values and writes it	*
C* to a grid file.  IPKTYP should be one of the following parameter	*
C* names from GEMPRM.PRM:						*
C*									*
C*         MDGNON        No grid packing				*
C*         MDGGRB        Pack in GEMPAK GRIB format given nbits		*
C*         MDGDEC        Pack in GEMPAK GRIB format given precision	*
C*         MDGDIF        Pack in GEMPAK DIF format given nbits		*
C*									*
C* If the packing type is MDGNON, the real data will be stored as if	*
C* GD_WDAT were called.  If MDGGRB or MDGDIF is specified, the		*
C* number of bits given in NBITS will be used to store the data.	*
C* For packing type MDGDEC, NBITS is the precision.  The grid data	*
C* is multiplied by 10 ** NBITS and rounded to the nearest integer.	*
C* The actual number of bits used to store the data is the minimum	*
C* number required to store the resulting integers.			*
C*									*
C* GD_WPGDW  ( IACSS, GRID, IGX, IGY, IGHDR, GDATTM1, GDATTM2, LEVEL1,	*
C*	       LEVEL2, IVCORD,PARM, REWRIT, IPKTYP, NBITS, IRET )	*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*	GRID (IGX,IGY)	REAL		Grid data			*
C*	IGX		INTEGER		Number of horizontal points	*
C*	IGY		INTEGER		Number of vertical points 	*
C*	IGHDR (IHDRSZ)	INTEGER		Grid header			*
C*	GDATTM1		CHAR*20		GEMPAK times			*
C*	GDATTM2		CHAR*20		GEMPAK times			*
C*	LEVEL1		INTEGER		Vertical levels			*
C*	LEVEL2		INTEGER		Vertical levels			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*				  	   0 = NONE			*
C*				  	   1 = PRES			*
C*					   2 = THTA			*
C*					   3 = HGHT			*
C*	PARM		CHAR*12		Parameter name			*
C*	REWRIT		LOGICAL		Flag to replace existing grid	*
C*	IPKTYP		INTEGER		Packing type			*
C*	NBITS		INTEGER		Number of bits / precision	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -5 = no write access		*
C*					 -6 = read/ write error		*
C*					 -9 = invalid grid size		*
C*					-10 = grid already exists	*
C*					-11 = grid file is full		*
C**									*
C* Log:									*
C* R. Tian/SAIC          3/06						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid (*)
	CHARACTER*(*)	gdattm1, gdattm2, parm
	INTEGER		ighdr (*)
	LOGICAL		rewrit
C*
	CHARACTER	gdattm (2)*20
	INTEGER		level (2)
C------------------------------------------------------------------------
	gdattm (1) = gdattm1
	gdattm (2) = gdattm2
	level (1) = level1
	level (2) = level2
	CALL GD_WPGD ( iacss, grid, igx, igy, ighdr, gdattm,
     +		       level, ivcord, parm, rewrit, ipktyp, nbits, iret )
C*
	RETURN
	END
