	SUBROUTINE GD_WPPG  ( iacss, igrid, lengrd, igx, igy, ighdr, 
     +			      gdattm, level, ivcord, parm, rewrit, 
     +			      ipktyp, nbits, misflg, ref, scale, 
     +			      difmin, iret )
C************************************************************************
C* GD_WPPG								*
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
C* GD_WPPG  ( IACSS, IGRID, LENGRD, IGX, IGY, IGHDR, GDATTM, LEVEL,	*
C*            IVCORD, PARM, REWRIT, IPKTYP, NBITS, MISFLG, REF,		*
C*            SCALE, DIFMIN, IRET )					*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*	IGRID (LENGRD)	INTEGER		Packed grid data		*
C*	LENGRD		INTEGER		Number of 32-bit words in grid	*
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
C* M. desJardins/GSFC	 3/89	From GD_WDAT				*
C* M. desJardins/GSFC	 4/89	Changed sorting				*
C* K. Brill/NMC		 4/91	Update documentation; declare MISFLG	*
C* J. Whistler/SSAI	 4/91	Sent intdft to GD_ADDT instead of	*
C*				ihdarr					*
C* R. Tian/SAIC          1/04   Added GD_FCHK call                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	INTEGER		igrid (*)
	CHARACTER*(*)	gdattm (2), parm
	INTEGER		ighdr (*), level (*)
	LOGICAL		rewrit, misflg
C*
	CHARACTER	prm*12
	LOGICAL		found, new
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
C*	    For grid already in file, column is in sorted list.
C
	    new  = .false.
C
C*	    Otherwise, add new column header.  Return on error.
C
	  ELSE
	    new  = .true.
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
	CALL DM_SGPK  ( igx, igy, ipktyp, nbits, misflg, ref, scale,
     +			difmin, ier )
	CALL DM_WDTI  ( igdfln, irow, icol, 'GRID', kbfhdr, igrid, 
     +			lengrd, iret )
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
