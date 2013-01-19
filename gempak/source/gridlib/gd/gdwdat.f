	SUBROUTINE GD_WDAT  ( iacss, grid, igx, igy, ighdr, gdattm,
     +			      level, ivcord, parm, rewrit, iret )
C************************************************************************
C* GD_WDAT								*
C*									*
C* This subroutine writes a grid into a grid file.			*
C*									*
C* GD_WDAT  ( IACSS, GRID, IGX, IGY, IGHDR, GDATTM, LEVEL, IVCORD,	*
C*            PARM, REWRIT, IRET )					*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*	GRID (IGX,IGY)	REAL		Grid data			*
C*	IGX		INTEGER		Number of horizontal points	*
C*	IGY		INTEGER		Number of vertical points 	*
C*	IGHDR (IHDRSZ)	INTEGER		Grid header			*
C*	GDATTM (2)	CHAR*20		GEMPAK times	 		*
C*	LEVEL  (2)	INTEGER		Vertical levels			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*				  	   0 = NONE			*
C*				  	   1 = PRES			*
C*					   2 = THTA			*
C*					   3 = HGHT			*
C*	PARM		CHAR*12		Parameter name			*
C*	REWRIT		LOGICAL		Flag to replace existing grid	*
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
C* M. desJardins/GSFC	 6/87						*
C* I. Graffman/RDS	 1/88	Parm changed to upper case always	*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 3/89	Modified for grid packing		*
C* R. Tian/SAIC          1/04   Added GD_FCHK call                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	REAL		grid (*)
	CHARACTER*(*)	gdattm (2), parm
	INTEGER		ighdr (*), level (*)
	LOGICAL		rewrit
C------------------------------------------------------------------------
        iret = 0
C
C*      Convert access number to DM number.
C
        CALL GD_FCHK ( iacss, igdfln, iret )
        IF ( iret .ne. 0 ) THEN
            RETURN
        END IF
C
C*	Call the packed grid write subroutine with no packing specified.
C
	ipktyp = MDGNON
	nbits  = 0
	CALL GD_WPGD  ( iacss, grid, igx, igy, ighdr, gdattm, level,
     +			ivcord, parm, rewrit, ipktyp, nbits, iret )
C*
	RETURN
	END
