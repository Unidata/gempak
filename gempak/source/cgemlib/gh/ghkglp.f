	SUBROUTINE GH_KGLP ( type, sname, rmnlat, rmnlon, rmxlat,
     +                       rmxlon,namstr, xkey, ykey, iret )
C************************************************************************
C* GH_KGLP								*
C*									*
C* This subroutine determines in which corner of the plot to place the  *
C* legend box (key) and gets the normalized coordinates of the legend   *
C* box.  It also creates the storm type/name string.                    *
C*									*
C* GH_KGLP ( TYPE, SNAME,RMNLAT,RMNLON, RMXLAT, RMXLON                  *
C*           NAMSTR, XKEY, YKEY, IRET )	                                *
C*									*
C* Input parameters:							*
C*	DDEV		CHAR*		Device driver 			*
C*	TYPE		CHAR*		Storm type			*
C*	SNAME		CHAR*		Storm name			*
C*	RMNLAT 		REAL		Minimum y axis map value	*
C*	RMNLON 		REAL		Minimum x axis map value	*
C*	RMXLAT 		REAL		Maximum y axis map value	*
C*	RMXLON 		REAL		Maximum x axis map value	*
C*									*
C* Output parameters:							*
C*	NAMSTR		CHAR*		Storm type/name string          *
C*	XKEY (*)	REAL		Norm. x coords of legend box    *
C*	YKEY (*)	REAL		Norm. y coords of legend box    *
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return            *
C*					  -1 = box overlaps track area  *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 4/02	Extracted from GH_KGLB                  *
C* D. Kidwell/NCEP	 4/02	Added alat, alon to GH_KGIP call seq.   *
C* A. Hardy/NCEP	 9/02   Added 'Subtropical Depression/storm'    *
C* J. Wu/SAIC		 8/03   Increased box length for day 4-5 track 	*
C* D. Kidwell/NCEP	 1/04	Incr box length for 'TPC' line & intens *
C* D. Kidwell/NCEP	 4/04	CSC - added jflags processing           *
C* S. Gilbert/NCEP	 7/06	Added new argument origc                *
C* m.gamazaychikov/SAIC	04/08	Added new flag iextra			*
C* m.gamazaychikov/SAIC	08/08	Removed forcing the last character of   *
C*                              unnamed storm in EP/CP to E		*
C* m.gamazaychikov/SAIC	09/08	CSC - added nabk, abklat, abklon	*
C* S. Jacobs/NCEP	 8/09	Make the storm name mixed case		*
C* X. Guo/CWS 		03/10  Removed unused arguments                 *
C* A. Krautkramer/NHC   09/11  Check 'DB' and 'PT'                      * 
C* M. Sardi/NHC         08/15  For EPAC TD tname strings, leave the 'E' *
C*                             to the right of the hypen capitalized.   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	type, sname, namstr 
        REAL		xkey (*), ykey (*)
C*
	CHARACTER	storm*24, tname*24
C-----------------------------------------------------------------------
C*      Create the storm type/name string.
C
	IF ( type .eq. 'HU' ) THEN
            storm = 'Hurricane'
	  ELSE IF ( type .eq. 'DB' )  THEN
	    storm = 'Remnants of'
	  ELSE IF ( type .eq. 'PT' )  THEN
	    storm = 'Post-Tropical Cyclone' 
          ELSE IF ( type .eq. 'TS' )  THEN 
            storm = 'Tropical Storm'
          ELSE IF ( type .eq. 'TD' ) THEN 
            storm = 'Tropical Depression'
          ELSE IF ( type .eq. 'SS' ) THEN 
            storm = 'Subtropical Storm'
          ELSE IF ( type .eq. 'SD' ) THEN 
            storm = 'Subtropical Depression'
        END IF
C
	tname = sname
        CALL ST_LSTR ( tname, lens, ier )
C
        IF  ( tname (lens-1:lens-1) .eq. '-' ) THEN
            CALL ST_UCLC ( tname(2:lens-2), tname(2:lens-2), ier )
        ELSE
            CALL ST_UCLC ( tname(2:), tname(2:), ier )
        END IF
C
        CALL ST_LSTR ( storm, lens, ier )
        namstr = storm(:lens)//' '//tname
C
C*	Get the normalized legend box coordinates.
C
        xkey ( 1 ) = 0.0
        xkey ( 2 ) = rmxlat - 0.0001
        xkey ( 3 ) = xkey ( 2 )
        xkey ( 4 ) = 0.0
        xkey ( 5 ) = 0.0
        ykey ( 1 ) = rmnlon
        ykey ( 2 ) = rmnlon
        ykey ( 3 ) = 0.0
        ykey ( 4 ) = 0.0
        ykey ( 5 ) = rmnlon

C*
	RETURN
	END
