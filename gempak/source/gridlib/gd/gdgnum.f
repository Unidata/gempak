	SUBROUTINE GD_GNUM  ( iacss, gdattm, level, ivcord, parm, 
     +			      ignum, iret )
C************************************************************************
C* GD_GNUM								*
C*									*
C* This subroutine gets the grid number for the requested grid.		*
C*									*
C* GD_GNUM  ( IACSS, GDATTM, LEVEL, IVCORD, PARM, IGNUM, IRET )		*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		File acess number		*
C*	GDATTM (2)	CHAR*20		GEMPAK times			*
C*	LEVEL  (2)	INTEGER		Vertical levels			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*					  0 = NONE			*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*	PARM		CHAR*12		Parameter name			*
C*									*
C* Output parameters:							*
C*	IGNUM		INTEGER		Grid number			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read/write error		*
C*					-12 = grid does not exist	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* M. desJardins/GSFC	 4/89	Changed sorting				*
C* R. Tian/SAIC          1/04   Added GD_FCHK call                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'grdcmn.cmn'
C*
	CHARACTER*(*)	gdattm (2), parm
	INTEGER		level (2)
C*
	INTEGER		ihdarr (10), iheadr (10)
	LOGICAL		found, equal
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
C*	Convert grid identifier to a column header.
C
	CALL GD_ITOH  ( gdattm, level, ivcord, parm, ihdarr, ier )
C
C*	Check that grid list is sorted.
C
	IF  ( ksrtl (1,1,igdfln) .eq. 0 )  CALL GD_SRTL ( iacss, ier )
C
C*	Search for grid with this header.
C
	found = .false.
	ipos  = 1
	DO WHILE  ( ( ipos .le. kgrid (igdfln) ) .and. ( .not. found ) )
C
C*	    Read identifier for next grid.
C
	    icol = ksrtl  ( 1, ipos, igdfln )
	    CALL DM_RCLH  ( igdfln, icol, iheadr, ier )
C
C*	    Check to see if this identifier is equal to current id.
C
	    equal = .true.
	    i     =  1
	    DO WHILE  ( equal .and. ( i .le. 10 ) )
		IF  ( ihdarr ( i ) .ne. iheadr ( i ) )  equal = .false.
		i = i +  1
	    END DO
	    IF  ( equal )  THEN
		found = .true.
	      ELSE
		ipos  = ipos + 1
	    END IF
	END DO
C
C*	If the grid was found, return the grid number.  Otherwise,
C*	return an error.
C
	IF  ( found )  THEN
	    ignum = ipos
	  ELSE
	    iret = -12
	END IF
C*
	RETURN
	END
