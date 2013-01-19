	SUBROUTINE GD_GGRD  ( iacss, ignum, gdattm, level, ivcord, 
     +			      parm, grid, igx, igy, ighdr, iret )
C************************************************************************
C* GD_GGRD								*
C*									*
C* This subroutine reads the requested grid from a grid file given	*
C* the grid number.							*
C*									*
C* GD_GGRD  ( IACSS, IGNUM, GDATTM, LEVEL, IVCORD, PARM, GRID, IGX,	*
C*            IGY, IGHDR, IRET )					*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*	IGNUM		INTEGER		Grid number			*
C*									*
C* Output parameters:							*
C*	GDATTM (2)	CHAR*20		GEMPAK times			*
C*	LEVEL  (2)	INTEGER		Vertical levels			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*	PARM		CHAR*12		Parameter name			*
C*	GRID (IGX,IGY)	REAL		Grid data			*
C*	IGX		INTEGER		Number of horizontal points	*
C*	IGY		INTEGER		Number of vertical points 	*
C*	IGHDR (IHDRSZ)	INTEGER		Grid header			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read error		*
C*					-12 = grid does not exist	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* M. desJardins/GSFC	 4/89	Changed sorting routines		*
C* K. Brill/NMC          9/90   Fixed calling sequence to GD_GIDN	*
C* R. Tian/SAIC          1/04   Added GD_FCHK call                      *
C* T. Lee/SAIC		12/05	Initialized ighdr			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER*(*)	gdattm (2), parm
	INTEGER		level (2), ighdr (*)
	REAL		grid (*)
C-----------------------------------------------------------------------
        iret = 0
C	
C*	Initialize header.
C
	DO ii = 1, LLGDHD
	    ighdr ( ii ) = 0
	END DO
C
C*      Convert access number to DM number.
C
        CALL GD_FCHK ( iacss, igdfln, iret )
        IF ( iret .ne. 0 ) THEN
            RETURN
        END IF
C
C*	Get grid identifier and check that this is a valid number.
C
	CALL GD_GIDN  ( iacss, ignum, gdattm, level, ivcord, parm,
     +			iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Get column for grid.
C
	icol = ksrtl ( 1, ignum, igdfln )
C
C*	Read in the data.
C
	irow = 1
	CALL DM_RDTR  ( igdfln, irow, icol, 'GRID', kbfhdr, grid, ikxy,
     +			iret )
C
C*	Read kx and ky from grid header.
C
	IF  ( iret .eq. 0 )  THEN
	    igx = kbfhdr (1)
	    igy = kbfhdr (2)
	    DO  i = 1, khdrln ( igdfln )
		ighdr (i) = kbfhdr (i+2)
	    END DO
	  ELSE
	    iret = -6
	END IF
C*
	RETURN
	END
