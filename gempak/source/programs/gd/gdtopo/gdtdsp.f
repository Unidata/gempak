	SUBROUTINE GDTDSP ( gdfile, topofl, garea, gltln, ikx, iky,
     +			    rmin, rmax, iret )
C************************************************************************
C* GDTDSP								*
C*									*
C* This subroutine allows the user to accept parameters for the GDLIST 	*
C* program.								*
C*									*
C* GDTDSP  ( GDFILE, TOPOFL, GAREA, GLTLN, IKX, IKY, RMIN, RMAX, IRET )	*
C*									*
C* Input parameters:							*
C*	GDFILE		CHAR*		Grid file			*
C*	TOPOFL		CHAR*		Topography file			*
C*	GAREA		CHAR*		Graphics area			*
C*	GLTLN(*)	REAL		Lat/lon boundary		*
C*	IKX		INTEGER		Number of X points		*
C*	IKY		INTEGER		Number of Y points		*
C*	RMIN		REAL		Minimum value of topography	*
C*	RMAX		REAL		Maximum value of topography	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  3 = user entered "EXIT"	*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	11/91						*
C************************************************************************
	CHARACTER*(*)	gdfile, topofl, garea
	REAL		gltln(*)
	LOGICAL		respnd
C*
	CHARACTER	ggarea*48
C-----------------------------------------------------------------------
	iret = 0
	ier  = 0
C
C*	Write out program name.
C
	WRITE  ( 6, 1001 )
1001	FORMAT ( // ' GDTOPO PARAMETERS: ' )
C
C*	Write out the grid file name.
C
	WRITE  ( 6, 1000 )  gdfile
1000	FORMAT ( / ' Grid file:       ', A )
C
C*	Write out the topography file name.
C
	WRITE  ( 6, 1002 )  topofl
1002	FORMAT ( / ' Topography file: ', A )
C
C*	Write out area. Both as user input and as calculated values.
C
	ggarea = garea
	WRITE  ( 6, 5000, IOSTAT = iostat )  ggarea, gltln(1), gltln(2),
     +						     gltln(3), gltln(4)
5000	FORMAT ( /, ' GAREA:              ', A
     +           /, ' Lower left corner:  ', 2F11.4
     +           /, ' Upper right corner: ', 2F11.4 )
C
C*	Write out number of X/Y grid points.
C
	WRITE  ( 6, 3000, IOSTAT = iostat )  ikx, iky, ikx*iky
3000	FORMAT ( / ' Number of X/Y grid points: ', 2I6
     +           /, '     Number of Grid points: ', I12 )
C
C*	Write out mininum and maximum values.
C
	WRITE  ( 6, 4000, IOSTAT = iostat )  rmin, rmax
4000	FORMAT ( / ' Minimum and Maximum values', 2F9.2 / )
C
C*	If respond is set, wait for user to accept parameters.
C
	CALL IP_RESP ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier. eq. 2 )  iret = 3
	END IF
C*
	RETURN
	END
