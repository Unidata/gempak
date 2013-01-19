	SUBROUTINE GD_SRTL  ( iacss, iret )
C************************************************************************
C* GD_SRTL								*
C*									*
C* This subroutine sorts the grid identifiers by first time and 	*
C* first level and maintains the sorted list in common.			*
C*									*
C* GD_SRTL  ( IACSS, IRET )						*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/89						*
C* T. Lee/GSC		10/00	Fixed zero index bug			*
C* R. Tian/SAIC          1/04   Added GD_FCHK call                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	INTEGER		ihdarr (10), ihival (2), iloval (2)
	CHARACTER	keynam (2)*4
C*
	DATA		keynam  / 'GDT1', 'GTM1' /
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
C*	Loop through all the times.
C
	igrid  = 0
	istart = 1
	DO  i = 1, ktgrid ( igdfln )
C
C*	    Set up search for this time.
C
	    CALL TG_ITOF  ( igdatm (1,i,igdfln), iloval, ier )
	    ihival (1) = iloval (1)
	    ihival (2) = iloval (2)
	    CALL DM_PSRC  ( igdfln, 2, keynam, iloval, ihival, ier )
	    CALL DM_BEGS  ( igdfln, ier )
C
C*	    Loop through all the grids corresponding to this time.
C
	    ierr = 0
	    DO WHILE  ( ierr .eq. 0 )
C
C*	        Get next column.
C
	        CALL DM_NEXT  ( igdfln, irow, icol, ierr )
		IF  ( ierr .eq. 0 )  THEN
C
C*		    Read in column header.
C
		    CALL DM_RCLH  ( igdfln, icol, ihdarr, ier )
C
C*		    Extract first level and vertical coordinate.
C
		    ilevel = ihdarr (5)
		    ivcord = ihdarr (7)
		    ilistt = ivcord * 1000000 + ilevel
		    IF  ( ilevel .eq. 0 )  ilistt = ilistt + 100000
C
C*		    Find place in the list given the time already
C*		    found.
C
		    iloc = igrid
		    DO WHILE  ( ( iloc .ge. istart )
     +					.and. 
     +			( ilistt .gt. ksrtl (2,iloc,igdfln) )  )
			    iloc = iloc - 1
		    END DO
C
C*		    Add grid to sorted list.
C
		    iloc = iloc + 1
		    DO  j = igrid, iloc, -1
			ksrtl (1,j+1,igdfln) = ksrtl (1,j,igdfln)
			ksrtl (2,j+1,igdfln) = ksrtl (2,j,igdfln)
		    END DO
		    ksrtl (1,iloc,igdfln) = icol
		    ksrtl (2,iloc,igdfln) = ilistt
		    igrid = igrid + 1
		END IF
	    END DO
	    istart = igrid + 1
	END DO
C*
	RETURN
	END
