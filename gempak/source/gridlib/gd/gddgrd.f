	SUBROUTINE GD_DGRD ( iacss, gdattm, level, ivcord, parm, iret )
C************************************************************************
C* GD_DGRD								*
C*									*
C* This subroutine deletes a grid from a grid file.			*
C*									*
C* GD_DGRD  ( IACSS, GDATTM, LEVEL, IVCORD, PARM, IRET )		*
C*									*
C* Input parameters:							*
C*	IACSS 		INTEGER		Grid file number		*
C*	GDATTM (2)	CHAR*20		GEMPAK time			*
C*	LEVEL  (2)	INTEGER		Vertical level			*
C*	IVCORD		INTEGER		Vertical coordinate		*
C*					  0 = none			*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*	PARM		CHAR*12		Parameter name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -5 = no write access to file	*
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
	INTEGER		level  (2)
C*
	INTEGER		iheadr (10)
	INTEGER		keyloc (10)
C*
	DATA		keyloc / 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /
C-----------------------------------------------------------------------
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
C*	Convert the grid identifier to integer column header.
C
	CALL GD_ITOH  ( gdattm, level, ivcord, parm, iheadr, iret )
C
C*	Search for grid with this header.
C
	CALL DM_SRCH  ( igdfln, 'COL', 10, keyloc, iheadr, icol, ier )
C
C*	Check to see if the grid was found.
C
	IF  ( ier .ne. 0 )  THEN
	    iret = -12
	    RETURN
	END IF
C
C*	First, delete data.  Then delete column header.
C
	irow = 1
	CALL DM_DDAT  ( igdfln, irow, icol, 'GRID', iret1 )
	CALL DM_DCLH  ( igdfln, icol, iret2 )
C
C*	Check for error deleting data.
C
	IF  ( ( iret1 .ne. 0 ) .or. ( iret2 .ne. 0 ) )  THEN
	    iret = -6
	    RETURN
	END IF
C
C*	Flush the write buffers so that file is updated correctly.
C
	CALL DM_FWRT  ( igdfln, ier )
C
C*	Update list of sorted identifiers.
C
	CALL GD_DELT  ( iacss, gdattm, icol, ier )
C*
	RETURN
	END
