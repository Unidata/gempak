	SUBROUTINE VG_READ ( igdfln, vc, level, parm, 
     +                       sfcval, grid_buf, grid_out, iret )
C************************************************************************
C* VG_READ								*
C*									*
C* This subroutine reads in a parameter given the vertical coordinate	*
C* VC, the level LEVEL and the parameter name PARM.  GEMPAK files are	*
C* assumed.								*
C*									*
C* VG_READ ( IGDFLN, VC, LEVEL, PARM, SFCVAL, GRID_BUF, GRID, IRET )	*
C*									*
C* Input parameters:							*
C*	IGDFLN		INTEGER		Grid file number		*
C*	VC		CHAR*4		Vertical coordinate		*
C*	LEVEL		INTEGER		Level 				*
C*	PARM		CHAR*4		Parameter name			*
C* Input/Ouput parameters:                                              *
C*      SFCVAL(kxky,np) REAL            surface value of parms          *
C*      GRID_BUF(kxy)   REAL            temp buf for the input data     *
C*									*
C* Output parameters:							*
C*	GRID(*)		REAL 		Output field			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-98 = grid is missing		*
C**									*
C* Log:									*
C* K. Brill/NMC		05/92						*
C* K. Brill/NMC		08/92	Added HGHT from ZAGL coordinate		*
C* K. Brill/NMC		08/92	Added subsetting			*
C* K. Brill/NMC		08/94	Add unscaling of PSYM grids		*
C* G. Hull/SAIC         03/08   add cmn_data to remove Grid size limit  *
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
	CHARACTER*(*)	vc, parm
	REAL		sfcval (kxky,np)
	REAL		grid_buf (kxin*kyin)
	REAL		grid_out (*)
C*
	INTEGER		lev (2), ighdr (128)
	CHARACTER*12	vp
	CHARACTER*20	timlst (256)
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Read in the data from a GEMPAK grid file.
C
	CALL LV_CORD ( vc, vp, ivc, ier )
	lev (1) = level
	lev (2) = -1
	CALL GD_RDAT ( igdfln, gdttm, lev, ivc, parm, grid_buf, ix, iy,
     +		       ighdr, iret )
C*
	IF ( iret .ne. 0 .and. parm .eq. NHGHT .and. lev (1) .eq. 0 )
     +	     THEN
C
C*	    Try to get surface elevation at the first time in the file.
C
	    CALL GD_GTIM ( igdfln, 256, timlst, ntimes, iret )
	    timlst (2) = ' '
	    IF ( iret .eq. 0 ) THEN
		CALL GD_RDAT ( igdfln, timlst (1), lev, ivc, parm,
     +			       grid_buf, ix, iy, ighdr, iret )
	    END IF
	END IF
C*
	IF ( iret .eq. 0 .and. parm .eq. NPSYM ) THEN
	    DO ij = 1, ix * iy
		IF ( .not. ERMISS ( grid_buf (ij) ) )
     +		    grid_buf (ij) = grid_buf (ij) * 100.
	    END DO
	END IF
C*
	IF ( iret .eq. 0 ) THEN
C
C*	    Subset the grid, if necessary.
C
	    IF ( ix * iy .ne. kxky ) THEN
		iout = 0
		DO jj = jssll, jssur
		   DO ii = issll, issur
			indx = ( jj - 1 ) * kxin + ii
			iout = iout + 1
			grid_out (iout) = grid_buf (indx)
		   END DO
		END DO
C           
C*          Else copy the grid to the output buffer
C
	    ELSE  
		DO ij = 1, kxky
	           grid_out(ij) = grid_buf(ij) 
		END DO
	    END IF
C*
	ELSE IF (  parm .eq. NHGHT .and. vc .eq. NZAGL ) THEN
C
C*	    Check for height on ZAGL surface.
C
	    IF ( .not. lpmrds (3) ) THEN
		iret = -98
		RETURN
	    END IF
	    zed = FLOAT ( level )
	    DO ij = 1, kxky
		IF ( .not. ERMISS ( sfcval (ij,3) ) ) THEN
		    grid_out (ij) = zed + sfcval (ij,3)
		ELSE
		    grid_out (ij) = RMISSD
		END IF
	    END DO
	ELSE IF 
     +	    ( vc .eq. NPRES .and. parm .eq. vc .and. level .eq. 0 ) THEN
C
C*	    Do not allow zero assignment to pressure.
C
	    DO i = 1, kxky
		grid_out (i) = RMISSD
	    END DO
	ELSE IF ( parm .eq. vc ) THEN
C
C*	    If the parameter name and vertical coordinate are the same,
C*	    just set the whole grid to the level value.
C
	    DO i = 1, kxky
		grid_out (i) = FLOAT ( level )
	    END DO
	    iret = 0
	ELSE 
	    DO i = 1, kxky
		grid_out (i) = RMISSD
	    END DO
	END IF
C*
	RETURN
	END
