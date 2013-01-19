	SUBROUTINE GD_GIDN  ( iacss, ignum, gdattm, level, ivcord, 
     +			      parm, iret )
C************************************************************************
C* GD_GIDN								*
C*									*
C* This subroutine returns a grid identifier given the grid number.	*
C*									*
C* GD_GIDN  ( IACSS, IGNUM, GDATTM, LEVEL, IVCORD, PARM, IRET )	*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid access number		*
C*	IGNUM		INTEGER		Grid number			*
C*									*
C* Output parameters:							*
C*	GDATTM (2)	CHAR*20		GEMPAK times			*
C*	LEVEL  (2)	INTEGER 	Vertical levels			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*	PARM		CHAR*12		Parameter name			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = read/write error		*
C*					-12 = invalid grid number	*
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
	INTEGER		level  (2)
C*
	INTEGER		iheadr (10)
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
C*	Check that list of grids has been sorted.
C
	IF  ( ksrtl (1,1,igdfln) .eq. 0 )  CALL GD_SRTL  ( iacss, ier )
C
C*	Check for valid number in sorted list.
C
	IF  ( ( ignum .gt. kgrid (igdfln) ) .or. ( ignum .le. 0 ) ) THEN
	    iret = -12
	    RETURN
	END IF
C
C*	Read in the column header.
C
	icol = ksrtl  ( 1, ignum, igdfln )
	CALL DM_RCLH  ( igdfln, icol, iheadr, iret )
	IF  ( iret .ne. 0 ) iret = -6
C
C*	Convert the header to a grid identifier.
C
	CALL GD_HTOI  ( iheadr, gdattm, level, ivcord, parm, ier )
C*
	RETURN
	END
