	SUBROUTINE GDSDSP  ( gdfile, gdtime, level, ivcord, wparm,
     +                       wind, icolor, garea, contin, iret )
C************************************************************************
C* GDSDSP								*
C*									*
C* This subroutine allows the user to accept parameters for the 	*
C* GDSTREAM program.							*
C*									*
C* GDSDSP ( GDFILE, GDTIME, LEVEL, IVCORD, WPARM, WIND, ICOLOR, GAREA, 	*
C*          CONTIN, IRET )						*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file			*
C*	GDTIME(2)	CHAR*		Grid time			*
C*	LEVEL(2)	INTEGER		Grid level			*
C*	IVCORD		INTEGER		Grid vertical coordinate	*
C*	WPARM		CHAR*		Wind components			*
C*	WIND		CHAR*		Wind type input by user		*
C*	ICOLOR		INTEGER		Color number			*
C*	GAREA		CHAR*		Graphics area			*
C*	CONTIN		LOGICAL		Flag for asking user to continue*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  3 = user entered "EXIT"	*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* M. desJardins/GSFC	 3/88	Fixed bad correction made in Jan.	*
C* S. Schotz/GSC	 6/90	Get respnd locally from IP_RESP		*
C* J. Whistler/SSAI	 4/91	Added WIND				*
C* T. Lee/GSC		 7/01	Added contin				*
C************************************************************************
	CHARACTER*(*)	gdfile, gdtime(2), wparm, garea, wind
	INTEGER		level(2)
	LOGICAL		respnd, contin
C-----------------------------------------------------------------------
	iret = 0
C
C*	Write out the grid file name.
C
	WRITE  ( 6, 1000) gdfile
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
	WRITE  ( 6, 3000 ) garea
3000	FORMAT ( / ' GAREA:     ', A )
C
C*	Write out streamline color.
C
	WRITE  ( 6, 3001 ) icolor
3001	FORMAT ( / ' Streamline color:', I5 )
C
C*	Write out wind.
C
	WRITE  ( 6, 3002 ) wind
3002	FORMAT ( / ' Wind type: ', A )
C
C*	If respond is set, wait for user to accept parameters.
C
	IF  ( contin )  THEN
	    CALL IP_RESP ( respnd, ier )
	    IF  ( respnd ) THEN
		CALL TM_ACCP  ( ier )
		IF  ( ier .eq. 2 )  iret = 3
	    END IF
	END IF
C*
	RETURN
	END
