	SUBROUTINE GDMDSP  ( gdfile, gdtime, level, ivcord, parm,
     +			     garea, iscale, contin, iret )
C************************************************************************
C* GDMDSP								*
C*									*
C* This subroutine allows the user to accept parameters for GDMAP.	*
C*									*
C* GDMDSP  ( GDFILE, GDTIME, LEVEL, IVCORD, PARM, GAREA, ISCALE,	*
C*	     CONTIN, IRET )						*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file			*
C*	GDTIME(2)	CHAR*		Grid time			*
C*	LEVEL(2)	INTEGER		Grid level			*
C*	IVCORD		INTEGER		Grid vertical coordinate	*
C*	PARM		CHAR*		Grid parameter			*
C*	GAREA		CHAR*		Graphics area			*
C*	ISCALE		INTEGER		Scaling factor			*
C*	CONTIN		LOGICAL		Flag for asking user to continue*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  3 = user entered "EXIT"	*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* I. Graffman/RDS	 3/88	Standard respnd handling		*
C* M. desJardins/GSFC	 8/88	Cleaned up				*
C* S. Schotz/GSC	 6/90	Get respnd locally from IP_RESP		*
C* J. Whistler/SSAI	 6/91	Added SCALE to display			*
C* T. Lee/GSC		 7/01	Added contin				*
C************************************************************************
	CHARACTER*(*)	gdfile, gdtime(2), parm, garea
	INTEGER		level(2)
	LOGICAL		respnd, contin
C-----------------------------------------------------------------------
	iret = 0
C
C*	Write out the grid file name.
C
	WRITE  ( 6, 1000)  gdfile
1000	FORMAT ( / ' Grid file: ', A )
C
C*	Write out the grid identifier.
C
	WRITE  ( 6, 2000 )
2000	FORMAT ( ' GRID IDENTIFIER: ' )
	CALL GR_WTRM  ( 6, .true., 0, gdtime, level, ivcord, parm, ier )
C
C*	Write out the graphics area.
C
	WRITE  ( 6, 3000 ) garea, iscale
3000	FORMAT ( / ' GAREA:     ', A, ' SCALE: ', I2 ) 
C
C*	If respond is set, wait for user to accept parameters.
C
	IF  ( contin )  THEN
	    CALL IP_RESP ( respnd, ier )
	    IF  ( respnd )  THEN
		CALL TM_ACCP  ( ier )
		IF  ( ier .eq. 2 )  iret = 3
	    END IF
	END IF
C*
	RETURN
	END
                                                   
