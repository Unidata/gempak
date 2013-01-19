	SUBROUTINE GD_WPPGW ( iacss, igrid, lengrd, igx, igy, ighdr, 
     +			      gdattm1, gdattm2, level1, level2, ivcord,
     +                        parm, rewrit, ipktyp, nbits, misflg, ref,
     +                        scale, difmin, iret )
C************************************************************************
C* GD_WPPGW								*
C*									*
C* This subroutine writes a grid that is already packed to a grid	*
C* file.  IPKTYP should be one of the following parameter names:	*
C*         MDGGRB        Packed in GEMPAK GRIB format			*
C*                         REF    = minimum value			*
C*                         SCALE  = 2 ** N				*
C*         MDGNMC        Packed in NMC format				*
C*                         REF    = average value			*
C*                         SCALE  = 1 / 2 ** N				*
C*         MDGDIF        Packed in GEMPAK DIF format			*
C*                         REF    = first non-missing point in grid	*
C*                         SCALE  = scaling term for differences	*
C*                         DIFMIN = minimum value of difference field	*
C*									*
C* GD_WPPGW ( IACSS, IGRID, LENGRD, IGX, IGY, IGHDR, GDATTM1, GDATTM2,	*
C*            LEVEL1, LEVEL2, IVCORD, PARM, REWRIT, IPKTYP, NBITS,	*
C*            MISFLG, REF, SCALE, DIFMIN, IRET )			*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*	IGRID (LENGRD)	INTEGER		Packed grid data		*
C*	LENGRD		INTEGER		Number of 32-bit words in grid	*
C*	IGX		INTEGER		Number of horizontal points	*
C*	IGY		INTEGER		Number of vertical points 	*
C*	IGHDR (IHDRSZ)	INTEGER		Grid header			*
C*	GDATTM1   	CHAR*20		GEMPAK times			*
C*	GDATTM2   	CHAR*20		GEMPAK times			*
C*	LEVEL1    	INTEGER		Vertical levels			*
C*	LEVEL2    	INTEGER		Vertical levels			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*				  	   0 = NONE			*
C*				  	   1 = PRES			*
C*					   2 = THTA			*
C*					   3 = HGHT			*
C*	PARM		CHAR*12		Parameter name			*
C*	REWRIT		LOGICAL		Flag to replace existing grid	*
C*	IPKTYP		INTEGER		Packing type			*
C*	NBITS		INTEGER		Number of bits 			*
C*	MISFLG		LOGICAL		Missing data flag		*
C*	REF		REAL		Reference value			*
C*	SCALE		REAL		Scaling factor			*
C*	DIFMIN		REAL		DIF reference value		*
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
C* R. Tian/SAIC          8/06						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		igrid (*)
	CHARACTER*(*)	gdattm1, gdattm2, parm
	INTEGER		ighdr (*)
	LOGICAL		rewrit, misflg
C*
	CHARACTER	gdattm (2)*20
	INTEGER		level (2)
C------------------------------------------------------------------------
	gdattm (1) = gdattm1
	gdattm (2) = gdattm2
	level (1) = level1
	level (2) = level2
	CALL GD_WPPG ( iacss, igrid, lengrd, igx, igy, ighdr, gdattm,
     +	               level, ivcord, parm, rewrit, ipktyp, nbits,
     +                 misflg, ref, scale, difmin, iret )
C*
	RETURN
	END
