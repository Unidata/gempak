	SUBROUTINE GD_WPGD  ( iacss, grid, igx, igy, ighdr, gdattm,
     +			      level, ivcord, parm, rewrit, ipktyp, 
     +			      nbits, iret )
C************************************************************************
C* GD_WPGD								*
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
C* GD_WPGD  ( IACSS, GRID, IGX, IGY, IGHDR, GDATTM, LEVEL, IVCORD,	*
C*            PARM, REWRIT, IPKTYP, NBITS, IRET )			*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*	GRID (IGX,IGY)	REAL		Grid data			*
C*	IGX		INTEGER		Number of horizontal points	*
C*	IGY		INTEGER		Number of vertical points 	*
C*	IGHDR (IHDRSZ)	INTEGER		Grid header			*
C*	GDATTM (2)	CHAR*20		GEMPAK times			*
C*	LEVEL  (2)	INTEGER		Vertical levels			*
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
C* M. desJardins/GSFC	 6/87						*
C* I. Graffman/RDS	 1/88	Parm changed to upper case always	*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* M. desJardins/GSFC	 3/89	Modified for grid packing		*
C* M. desJardins/GSFC	 4/89	Changed sorting				*
C* J. Whistler/SSAI	 4/91	Sent intdft to GD_ADDT instead of	*
C*				ihdarr					*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C* S. Jacobs/NCEP	 2/01	Initialized mmm and r before DM_SGPK	*
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
C*
	CHARACTER	prm*12
	LOGICAL		found, mmm, new
	INTEGER		ihdarr (10), keyloc (10), intdft (3)
C*
	DATA		keyloc / 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /
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
C*	Check for write access.
C
	IF  ( .not. gdwrt ( igdfln ) )  THEN
	    iret = -5
	    RETURN
	END IF
C
C*	Check that IGX and IGY are nonnegative.
C
	IF  ( ( igx .le. 0 ) .or. ( igy .le. 0 ) )  THEN
	    iret = -9
	    RETURN
	END IF
C
C*	Turn the grid identifier into a column header.
C
	CALL ST_LCUC  ( parm, prm, ier)
	CALL GD_ITOH  ( gdattm, level, ivcord, prm, ihdarr, ier )
C
C*	Search for grid with this header.
C
	CALL DM_SRCH  ( igdfln, 'COL', 10, keyloc, ihdarr, icol, ier )
C
C*	Check to see if the grid was found.
C
	IF  ( ier .eq. 0 )  THEN
	    found = .true.
	  ELSE
	    found = .false.
	END IF
C
C*	Return error if grid is in file and is not to be replaced.
C
	IF  (  found .and. ( .not. rewrit ) )  THEN
	    iret = -10
	    RETURN
	END IF
C
C*	Get column to write grid.
C
	IF  ( found )  THEN
C
C*	    For grid already in file, time is in sorted list.
C
	    new  = .false.
C
C*	    Otherwise, add new column header.  Return on error.
C
	  ELSE
	    new = .true.
	    CALL DM_WCLH  ( igdfln, 0, ihdarr, icol, ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG  ( 'DM', iret, ' ', iret )
		iret = -6
		IF  ( ier .eq. -12 ) iret = -11
		RETURN
	    END IF
	END IF
C
C*	Add kx and ky to the grid header.
C
	kbfhdr (1) = igx
	kbfhdr (2) = igy
	DO  i = 1, khdrln ( igdfln )
	    kbfhdr (i+2) = ighdr (i)
	END DO
C
C*	Write the grid to the file.
C
	irow = 1
	igxy = igx * igy
	mmm  = .TRUE.
	r    = RMISSD
	CALL DM_SGPK  ( igx, igy, ipktyp, nbits, mmm, r, r, r, ier )
	CALL DM_WDTR  ( igdfln, irow, icol, 'GRID', kbfhdr, grid, igxy,
     +			iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DM', iret, ' ', ier )
	    iret = -6
	    RETURN
	END IF
C
C*	Flush the write buffers.
C
	CALL DM_FWRT  ( igdfln, iret )
C
C*	Finally, add the grid to the sorted list.
C
	IF  ( new )  THEN
	    CALL TG_FTOI  ( ihdarr, intdft, ier )
	    CALL GD_ADDT  ( iacss, intdft, ier )
	END IF
C*
	RETURN
	END
