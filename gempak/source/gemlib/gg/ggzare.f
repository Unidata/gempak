        SUBROUTINE GG_ZARE  ( x, y, garea, proj, iret ) 
C************************************************************************
C* GG_ZARE								*
C*                                                                      *
C* This subroutine controls the zooming for the GEMPAK programs.	*
C*                                                                      *
C* GG_ZARE ( X, Y, GAREA, PROJ, IRET )					*
C*                                                                      *
C* Input parameters:                                                    *
C*	X(*)		REAL		Latitudes of GAREA bounds	*
C*	Y(*)		REAL		Longitude of GAREA bounds	*
C*                                                                      *
C* Output parameters:                                                   *
C*	GAREA		CHAR*		Return string for GAREA		*
C*	PROJ		CHAR*		Return string for PROJ		*
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                      -11 = Invalid parameter name    *
C*                                      -41 = Invalid cursor points     *
C**                                                                     *
C* Log:                                                                 *
C* S. Maxwell/GSC	 1/97						*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C************************************************************************
        CHARACTER*(*)   proj, garea
	REAL            x(*), y(*)
C*
        CHARACTER       strout*160, prj*72
	REAL            gltln(4), ang(3)
C------------------------------------------------------------------------
C*      Set the value of the parameter.
C
	gltln(1) = x(1)
	gltln(2) = y(1)
	gltln(3) = x(2)
	gltln(4) = y(2)
C
        CALL ST_LSTF ( gltln, 4, ';', 1, garea, iret)
C
C*      Get current map projection if not a SAT projection.
C
        CALL GQMPRJ ( prj, ang(1), ang(2), ang(3), dlt1,
     +                dln1, dlt2, dln2, ier )

C*      Build a string made up of the 3 projection angles.
C
        CALL ST_LSTF ( ang, 3, ';', 1, strout, ier )
        CALL ST_LSTR ( prj, len, ier )

C*      Redefine value of PROJ.
C
        proj = prj ( 1 : len ) // '/' // strout
C
C*
	RETURN
	END
