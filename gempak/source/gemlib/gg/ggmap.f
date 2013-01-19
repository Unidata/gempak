	SUBROUTINE GG_MAP  ( map, iret )
C************************************************************************
C* GG_MAP								*
C*									*
C* This subroutine parses user input to draw a map on the graphics	*
C* device.  The MAP string should contain the map color, line type,	*
C* line width and a point filter flag separated by slashes (/).  If	*
C* the line type or width is zero or undefined, the current value is	*
C* used.  If MAP is a blank, a default color of 1 will be used.  The 	*
C* point filter can be YES or NO.  If the point filter is turned on,	*
C* the number of points drawn to the device will be reduced.		*
C*									*
C* GG_MAP  ( MAP, IRET )						*
C*									*
C* Input parameters:							*
C*	MAP		CHAR*		Map color/line type/width/filter*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				  	  0 = normal return		*
C*				 	 -7 = map not drawn		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/85						*
C* I. Graffman/RDS	 6/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	 6/86	Added line width			*
C* I. Graffman/RDS	 2/88	Added GERROR call			*
C* M. desJardins/GSFC	 5/88	Fixed map width problems		*
C* M. desJardins/GSFC	 8/89	Set hardware type to no change		*
C* M. desJardins/GSFC	 7/90	Eliminate saving line type/width	*
C* S. Schotz/GSC	10/90	Call IN_LINE for map string		*
C* M. desJardins	02/91   Add map file option 			*
C* K. Brill/NMC		01/92	Replace GERROR with ER_WMSG		*
C* S. Jacobs/NCEP	 5/96	Added checks for mutiple map files;	*
C*				Removed IN_LINE and added parsing code	*
C* S. Maxwell/GSC	 1/97   Added call to GG_DMAP			*
C* S. Jacobs/NCEP	 5/99	Added map point filter			*
C* T. Piper/GSC		09/99	Increased mapfil and mparr to 128	*
C************************************************************************
	PARAMETER	( MXMP = 20 )
C*
	CHARACTER*(*)	map
C*
	CHARACTER	mapfil*128, mparr(MXMP)*128, mpatt(MXMP)*72,
     +			group(4)*72
C-----------------------------------------------------------------------
	iret = 0
C
C*	In map mode, extract the global map file name and set the file.
C
	CALL IP_MFIL  ( mapfil, ierr )
C
C*	Parse the input for MAPFIL.
C
	CALL ST_CLST  ( mapfil, '+', ' ', MXMP, mparr, num, ier )
C
C*	If mapfil is empty, do the loop once, using the defaults.
C
	IF  ( num .eq. 0 )  num = 1
C
C*	Parse the input for MAP.
C
	CALL ST_CLST  ( map, '+', ' ', num, mpatt, numa, ier )
C
C*	Loop over all the map files.
C
	DO  i = 1, num
C
C*	    Decode the map string.
C
	    IF  ( i .ne. 1 )  THEN
		IF  ( mpatt (i) .eq. ' ' )  THEN
		    mpatt (i) = mpatt (i-1)
	    	END IF
	    END IF
C
C*	    Break list into three groups.
C
	    CALL ST_CLST  ( mpatt(i), '/', '1', 4, group, ng, ier )
C
C*	    Decode color, type and width parts.
C
	    CALL IN_COLR ( group (1), 1, mapcol, ier )
	    CALL ST_ILST ( group (2), ';', 0, 1, maptyp,  ntyp, ier )
	    CALL ST_ILST ( group (3), ';', 0, 1, mapwid, nwid, ier )
C
C*	    Check the map point filter flag.
C
	    IF  ( ( group(4)(1:1) .eq. 'Y' ) .or.
     +		  ( group(4)(1:1) .eq. 'y' ) )  THEN
		mapflt = 1
	      ELSE
		mapflt = 0
	    END IF
C
C*	    Draw the map.
C
	    CALL GG_DMAP ( mparr(i), mapcol, maptyp, mapwid, mapflt,
     +			   iret )
C
	END DO
C*
	RETURN
	END
