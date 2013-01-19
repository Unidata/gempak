	SUBROUTINE GD_GLEV  ( iacss, gdattm, ivcord, maxlev, levarr,
     +			      nlev, iret )
C************************************************************************
C* GD_GLEV								*
C*									*
C* This subroutine returns all the levels present in a grid file for	*
C* a given date and vertical coordinate.  The levels returned are	*
C* not sorted.								*
C*									*
C* GD_GLEV  ( IACSS, GDATTM, IVCORD, MAXLEV, LEVARR, NLEV, IRET )	*
C*									*
C* Input parameters:							*
C*	IACSS		INTEGER		Grid access number		*
C*	GDATTM (2)	CHAR*20		GEMPAK times			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*	MAXLEV		INTEGER		Maximum number of levels	*
C*									*
C* Output parameters:							*
C*	LEVARR (2,NLEV)	INTEGER		Levels found			*
C*	NLEV		INTEGER		Number of levels found		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read/write error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/85						*
C* M. desJardins/GSFC	 4/89	Changed sorting				*
C* R. Tian/SAIC		 1/04	Changed GD_FCHK call			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER*(*)	gdattm (2)
	INTEGER		levarr (2,*)
C*
	INTEGER		ihival (5), iloval (5), intdtf (3), lev (2),
     +			ihdarr (10)
	LOGICAL		found
	CHARACTER	keynam (5)*4
C*
	DATA		keynam /'GDT1', 'GTM1', 'GDT2', 'GTM2', 'GVCD'/
C-----------------------------------------------------------------------
	iret = 0
	nlev = 0
C
C*      Convert access number to DM number.
C
        CALL GD_FCHK ( iacss, igdfln, iret )
        IF ( iret .ne. 0 ) THEN
            RETURN
        END IF
C
C*	Convert times to the two integer used in file and add to hival
C*	and loval arrays.
C
	CALL TG_CTOI  ( gdattm (1), intdtf, ier )
	CALL TG_ITOF  ( intdtf, iloval (1), ier )
	CALL TG_CTOI  ( gdattm (2), intdtf, ier )
	CALL TG_ITOF  ( intdtf, iloval (3), ier )
	iloval (5) = ivcord
	DO  i = 1, 5
	    ihival (i) = iloval (i)
	END DO
C
C*	Set the search for these times and vertical coordinate.
C
	CALL DM_PSRC  ( igdfln, 5, keynam, iloval, ihival, ier )
	CALL DM_BEGS  ( igdfln, ier )
C
C*	Get all the grids corresponding to these times and vert coord.
C
	ierr = 0
	DO WHILE  ( ierr .eq. 0 )
C
C*	    Get next column.
C
	    CALL DM_NEXT  ( igdfln, irow, icol, ierr )
	    IF  ( ierr .eq. 0 )  THEN
C
C*		Read in column header.
C
		CALL DM_RCLH  ( igdfln, icol, ihdarr, ier )
C
C*		Extract levels.
C
		lev (1) = ihdarr (5)
		lev (2) = ihdarr (6)
C
C*		Check whether time should be added to array.
C
		found = .false.
		klev  = 1
		DO WHILE  ( ( klev .le. nlev ) .and. ( .not. found ) )
		    IF  ( ( lev (1) .eq. levarr (1,klev) ) .and.
     +			  ( lev (2) .eq. levarr (2,klev) ) )  THEN
			found = .true.
		      ELSE
			klev = klev + 1
		    END IF
		END DO
C
C*		Add to list if not already there.
C
		IF  ( ( .not. found ) .and. ( nlev .lt. maxlev ) )  THEN
		    nlev = nlev + 1
		    levarr ( 1, nlev ) = lev (1)
		    levarr ( 2, nlev ) = lev (2)
		END IF
	    END IF
	END DO
C*
	RETURN
	END
