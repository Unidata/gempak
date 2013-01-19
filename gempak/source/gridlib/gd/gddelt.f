	SUBROUTINE GD_DELT  ( iacss, gdattm, icol, iret )
C************************************************************************
C* GD_DELT								*
C*									*
C* This subroutine deletes a grid from the time list and the grid	*
C* list.								*
C*									*
C* GD_DELT  ( IACSS, GDATTM, ICOL, IRET )				*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*	GDATTM		CHAR*		First grid time			*
C*	ICOL		INTEGER		Column in DM file		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* M. desJardins/GSFC	 4/89	New time sorting			*
C* S. Jacobs/NMC	 7/94	Added declaration of gdattm		*
C* R. Tian/SAIC          1/04   Added GD_FCHK call                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER*(*)	gdattm
C*
	INTEGER		intdtf (3)
	LOGICAL		ftime, found
C------------------------------------------------------------------------
	iret  = 0
C
C*      Convert access number to DM number.
C
        CALL GD_FCHK ( iacss, igdfln, iret )
        IF ( iret .ne. 0 ) THEN
            RETURN
        END IF
C
C*	Break the time into three integers.
C
	CALL TG_CTOI  ( gdattm, intdtf, ier )
C
C*	Find this time in the time array.
C
	itime = 1
	ftime = .false.
	DO WHILE  ( ( .not. ftime ) .and. 
     +		    ( itime .le. ktgrid (igdfln) ) )
	    IF  ( ( igdatm (1,itime,igdfln) .eq. intdtf (1) ) .and.
     +		  ( igdatm (2,itime,igdfln) .eq. intdtf (2) ) .and.
     +		  ( igdatm (3,itime,igdfln) .eq. intdtf (3) ) )  THEN
		ftime = .true.
	      ELSE
		itime = itime + 1
	    END IF
	END DO
C
C*	Check for error.
C
	IF  ( .not. ftime )  RETURN
C
C*	Decrement counter for number of times.
C
	ndattm ( itime, igdfln ) = ndattm ( itime, igdfln ) - 1
	IF  ( ndattm ( itime, igdfln ) .eq. 0 )  THEN
	    DO  i = itime + 1, ktgrid ( igdfln )
		igdatm (1,i-1,igdfln) = igdatm (1,i,igdfln)
		igdatm (2,i-1,igdfln) = igdatm (2,i,igdfln)
		igdatm (3,i-1,igdfln) = igdatm (3,i,igdfln)
	    END DO
	    ktgrid ( igdfln ) = ktgrid ( igdfln ) - 1
	END IF
C
C*	If the file was sorted, delete this grid by column.
C
	IF  ( ksrtl (1,1,igdfln) .ne. 0 )  THEN
	    found = .false.
	    igrid = 1
	    DO WHILE  ( ( .not. found ) .and. 
     +			( igrid .le. kgrid (igdfln) ) )
		IF  ( ksrtl (1,igrid,igdfln) .eq. icol )  THEN
		    found = .true.
		    DO  i = igrid, kgrid (igdfln) - 1
			ksrtl (1,i,igdfln) = ksrtl (1,i+1,igdfln)
			ksrtl (2,i,igdfln) = ksrtl (2,i+1,igdfln)
		    END DO
		  ELSE
		    igrid = igrid + 1
		END IF
	    END DO
	END IF
C
C*	Now decrement total number of grids by 1.
C
	kgrid ( igdfln ) = kgrid ( igdfln ) - 1
C*
	RETURN
	END
