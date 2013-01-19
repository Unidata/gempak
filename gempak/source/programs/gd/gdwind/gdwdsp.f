	SUBROUTINE GDWDSP  ( gdfile, gdtime, level, ivcord, wparm, wind,
     +			     garea, iscale, dmin, dmax, contin, iret )
C************************************************************************
C* GDWDSP								*
C*									*
C* This subroutine allows the user to accept parameters for the GDWIND	*
C* program.								*
C*									*
C* GDWDSP  ( GDFILE, GDTIME, LEVEL, IVCORD, WPARM, WIND, GAREA,		*
C*           ISCALE, DMIN, DMAX, CONTIN, IRET )				*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file			*
C*	GDTIME(2)	CHAR*		Grid time			*
C*	LEVEL(2)	INTEGER		Grid level			*
C*	IVCORD		INTEGER		Grid vertical coordinate	*
C*	WPARM		CHAR*		Vector name			*
C*	WIND		CHAR*		Wind type input by user		*
C*	GAREA		CHAR*		Graphics area			*
C*	ISCALE		INTEGER		Scaling factor			*
C*	DMIN		REAL		Minimum data value		*
C*	DMAX		REAL		Maximun data value		*
C*	CONTIN		LOGICAL		Flag for asking user to continue*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  3 = user entered "EXIT"	*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* S. Schotz/GSC	 6/90	Get respnd locally from IP_RESP		*
C* J. Whistler/SSAI	 4/91	Added iscale, dmin, and dmax		*
C* J. Whistler/SSAI	 6/91	Changed wording of MAX/MIN print out	*
C* S. Jacobs/NMC	 9/94	Changed format for scale		*
C* T. Lee/GSC		 7/01	Added contin				*
C************************************************************************
	CHARACTER*(*)	gdfile, gdtime(2), wparm, garea, wind
	INTEGER		level(2)
	LOGICAL		respnd, contin
C-----------------------------------------------------------------------
	iret = 0
	WRITE  ( 6, 1001 )
1001	FORMAT ( / ' PARAMETERS FOR GDWIND: ' / )
C
C*	Write out the grid file name.
C
	WRITE  ( 6, 1000 )  gdfile
1000	FORMAT ( / ' Grid file: ', A )
C
C*	Write out the grid identifier.
C
	WRITE  ( 6, 2000 )
2000	FORMAT ( ' GRID IDENTIFIER: ' )
	CALL GR_WTRM ( 6, .true., 0, gdtime, level, ivcord, wparm, ier )
C
C*	Write out the graphics area.
C
	WRITE  ( 6, 3000 ) garea, iscale, dmin, dmax
3000	FORMAT ( / ' GAREA:  ', A, / ' SCALE: ', I4, //
     +           'MINIMUM AND MAXIMUM VECTOR MAGNITUDE', 2F9.2 )
C
C*	Write out wind type.
C
	WRITE  ( 6, 3001 ) wind
3001	FORMAT ( / ' Wind type: ', A )
C
C*	If respond is set, wait for user to accept parameters.
C
	IF  ( contin )  THEN
	    CALL IP_RESP ( respnd, ier )
	    IF  ( respnd )  CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  iret = 3
	END IF
C*
	RETURN
	END
