	SUBROUTINE GG_LTLN  ( latlon, iret )
C************************************************************************
C* GG_LTLN								*
C*									*
C* This subroutine parses user input to draw latitude/longitude lines	* 
C* on the graphics device.  The LATLON string should contain the line	* 
C* color, line type, line width, label frequency and latlon increment	* 
C* information separated by slashes.  The latter consists of the	* 
C* latitude and	longitude increments separated by semicolons.  If LATLON* 
C* is blank, no lines will be drawn.					*
C*									*
C* GG_LTLN  ( LATLON, IRET )						*
C*									*
C* Input parameters:							*
C*	LATLON		CHAR*		Line col/typ/wdth/lblfr/inc	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				  	  0 = normal return		*
C*				 	-13 = lines not drawn		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/90						*
C* K. Brill/NMC          9/90	Put label frq before inc		*
C* S. Schotz/GSC	10/90	Call IN_COLOR to parse color		*
C* J. Whistler/SSAI	 8/91	Added Ability to draw axis for graph    *
C*				type projections and made IFREQ an array*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C* S. Jacobs/EAI	12/92	Clean up parsing of LATLON variable	*
C* K. Tyle/GSC		 5/96	Added check for increment = 0		*
C* S. Maxwell/GSC	 1/97	Added call to GG_DLTN			*
C* A. Hardy/GSC		12/00   Added lat/lon position and format       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	latlon
C*
	CHARACTER	llarr(7)*28
C*
	INTEGER		ifreq (2)
	REAL		rinc (2), dlltln (2), aloc (2)
C-----------------------------------------------------------------------
	iret = 0
C*
	CALL ST_CLST ( latlon, '/', ' ', 7, llarr, num, ier )
C
C*	Decode color string and return if color is not specified
C
	CALL IN_COLR ( llarr(1), 1, lincol, ier )
	IF  ( lincol .le. 0 )  RETURN
C
C*	Decode line attributes
C
	CALL ST_NUMB ( llarr(2), lintyp, ier )
	CALL ST_NUMB ( llarr(3), linwid, ier )
C
C*      Decode label frequency
C
	CALL ST_ILST  ( llarr(4), ';', 1, 2, ifreq, numf, ier )
C
	IF ( numf .eq. 1 )  ifreq (2) = ifreq (1)
C
C*	Decode increment
C
	CALL ST_RLST ( llarr(5), ';', 10., 2, rinc, num, ier )
	dlltln(1) = rinc (1)
	dlltln(2) = rinc (2)
	IF ( dlltln(1) .lt. 0.1 ) dlltln(1) = 1.0
	IF ( dlltln(2) .lt. 0.1 ) dlltln(2) = 1.0
C
	CALL ST_RLST ( llarr(6), ';', RMISSD, 2, aloc, num, ier )
	CALL ST_NUMB ( llarr(7), ifrmat, ier )
C
C*	Save current line information.
C
	CALL GG_DLTN ( lincol, lintyp, linwid, ifreq, dlltln, 
     +                 aloc, ifrmat, iret )
C*
	RETURN
	END
