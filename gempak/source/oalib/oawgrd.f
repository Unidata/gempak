	SUBROUTINE OA_WGRD  ( gdattm, ngrid, parm, level,
     +			      ivcord, grid, kex, key, iextnd, iret )
C************************************************************************
C* OA_WGRD								*
C*									*
C* This subroutine writes grids computed in an objective analysis to	*
C* the grid file.  Although the data in GRID have been computed for the	*
C* extend area, only the data in the grid area are written to the file.	*
C*									*
C* OA_WGRD ( GDATTM, NGRID, PARM, LEVEL, IVCORD, GRID, KEX, KEY,	*
C*           IEXTND, IRET )						*
C*									*
C* Input parameters:							*
C*	GDATTM		CHAR*		Date/time for grids		*
C*	NGRID		INTEGER		Number of grids			*
C*	PARM   (NGRID)	CHAR*		Parameters			*
C*	LEVEL  (NGRID)	INTEGER		Levels				*
C*	IVCORD (NGRID)	INTEGER		Vertical coordinate		*
C*	GRID		REAL		Grid data			*
C*	 (NGRID,KEX,KEY)						*
C*	KEX		INTEGER		X points in extend area		*
C*	KEY		INTEGER		Y points in extend area		*
C*	IEXTND (4)	INTEGER		Grid extension			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/86						*
C* M. desJardins/GSFC	 7/87	Added new GD calls for GEMPAK4		*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* T. Piper/SAIC	 1/02	Set correct length for ighdr		*
C* R. Tian/SAIC		 5/02	Removed IGDFLN, changed to call DG_NWDT	*
C* T. Lee/SAIC		12/05	Initialized ighdr			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid ( NGRID, KEX, KEY )
	CHARACTER*(*)	parm (*), gdattm
	INTEGER		level (*), iextnd (*), ivcord (*)
C*
	CHARACTER	gtime (2)*20, gparm*12, gpack*12
	INTEGER		levelg (2), ighdr (LLGDHD) 
C*
	REAL		gwork (LLMXGD)
C*
	DATA		gpack / ' ' /
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Set up grid identifier.
C
	gtime  (1) = gdattm
	gtime  (2) = ' '
	levelg (2) = -1
C
C*	Define grid subset area.
C
	isx = iextnd (1) + 1
	isy = iextnd (2) + 1
	iex = kex - iextnd (3)
	iey = key - iextnd (4)
	igx = iex - isx + 1
	igy = iey - isy + 1
C
C*	Loop through all the parameters and levels.
C
	DO  ii = 1, LLGDHD
	    ighdr ( ii ) = 0
	END DO
C
	DO  igrid = 1, ngrid
C
C*	    Get the parameter name.
C
	    gparm = parm (igrid)
C
C*	    Define the level to be used.
C
	    levelg (1) = level (igrid)
C
C*	    Get the vertical coordinate.
C
	    jvcord = ivcord (igrid)
C
C*	    Move the data into a singly dimensioned array.
C
	    knt = 0
	    DO  iy = isy, iey
		DO  ix = isx, iex
		    knt = knt + 1
		    gwork (knt) = grid ( igrid, ix, iy )
		END DO
	    END DO
C
C*	    Write the grid to the grid file.
C
	    CALL DG_NWDT ( gwork, gtime, levelg, jvcord, gparm,
     +	                   ighdr, gpack, .true., iret )
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG ( 'DG', iret, ' ', ier )
	    END IF
	END DO
C*
	RETURN
	END
