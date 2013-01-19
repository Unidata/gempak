        SUBROUTINE  MA_ELEV ( stnid, iret )
C************************************************************************
C* MA_ELEV                                                              *
C*                                                                      *
C* This subroutine sets the elevation of the ship or fixed buoy to 0.0  *
C* meters.  If it is a Great Lakes fixed buoy, the elevation is gotten  *
C* from the station table file.  These data are stored in the elev      *
C* array.                                                               *
C*                                                                      *
C* MA_ELEV  ( STNID, IRET )                                             *
C*                                                                      *
C* Input parameters:                                                    *
C*      STNID          CHAR*             Station ID                     *
C*      IFBUOY         INTEGER           Set to 0, if fixed buoy        *
C*                                       report; set to 1, if not       *
C*      JSTN           INTEGER           Number of stations in table    *
C*      JSTNID (*)     CHAR*             Station IDs                    *
C*      ELEV (*)       INTEGER           Elevs of stations in meters    *
C*      RIVALS(IRSLAT) REAL              Latitude in degrees            *
C*      RIVALS(IRSLON) REAL              Longitude in degrees           *
C*					                                *
C* Output parameters:                                                   *
C*      RIVALS(IRSELV) REAL              Elevation of station in meters *
C*      IRET           INTEGER           Return code                    *
C*                                         0 = normal return            *
C*                                                                      *
C**								        *
C* Log:							                *
C* R. Hollern/NCEP       6/96                                           *
C* R. Hollern/NCEP      11/96   Added call to UT_BSRC                   *
C* D. Kidwell/NCEP	 4/97	Changed interface, cleaned up code,     *
C*				changed UT_BSRC to MA_BSRC              *
C* T. Lee/GSC		 9/97	Included GEMPRM.PRM			*
C* D. Kidwell/NCEP	10/97	Changed interface, cleaned up code      *
C* D. Kidwell/NCEP      12/97   Replaced call to MA_BSRC with DO loop   *
C* R. Hollern/NCEP       9/99   Computed elev for ships in Great Lakes  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE  	'macmn.cmn'
C*
        CHARACTER*(*)   stnid
C------------------------------------------------------------------------
        iret = 0
C
        IF ( ifbuoy .eq. 1 ) THEN
C
            xla = rivals ( irslat )
            xlo = rivals ( irslon )
C           
            IF ( ( xlo .ge. -92.5  .and. xlo .le. -84.52 )  .and.
     +           ( xla .ge.  46.48 .and. xla .le.  49.0 ) ) THEN 
C
C*              Ship located in Lake Superior.
C
                rivals ( irselv ) = 183.
C
              ELSE IF ( ( xlo .ge. -88.1 .and. xlo .le. -84.8 ) .and.
     +                  ( xla .ge. 41.2  .and. xla .le.  46.2 ) ) THEN 
C
C*              Ship located in Lake Michigan.
C
                rivals ( irselv ) = 176.
C
              ELSE IF ( ( xlo .ge. -84.8 .and. xlo .le. -79.79 ) .and.
     +                  ( xla .ge. 43.0  .and. xla .le.  46.48 ) ) THEN 
C
C*              Ship located in Lake Huron or Georgian Bay.
C
                rivals ( irselv ) = 176.
C
              ELSE IF ( ( xlo .ge. -84.0 .and. xlo .le. -78.0 ) .and.
     +                  ( xla .ge. 41.0  .and. xla .le.  42.9 ) ) THEN 
C
C*              Ship located in Lake Erie.
C
                rivals ( irselv ) = 174.
C
              ELSE IF ( ( xlo .ge. -80.0 .and. xlo .le. -76.0 ) .and.
     +                  ( xla .ge. 43.1  .and. xla .le.  44.23 ) ) THEN 
C
C*              Ship located in Lake Ontario.
C
                rivals ( irselv ) = 74.
C
              ELSE 
C
C*              It is a ship report not located in the Great Lakes.
C    
                rivals ( irselv ) = 0.0
            END IF
C
          ELSE IF ( stnid ( 1:2 ) .eq. '45' ) THEN
C
C*          It is a Great Lakes fixed buoy.  Get the station
C*          elevation from the station table data.
C
            ipos = 0
C
            DO WHILE ( ipos .lt. jstn )
C
                ipos = ipos + 1
                IF ( stnid ( 1:5 ) .eq. jstnid (ipos) ( 1:5 ) ) THEN
                    rivals ( irselv ) = elev ( ipos )
                    ipos = jstn + 1
                  ELSE IF ( ipos .eq. jstn ) THEN
C
C*                  Great Lakes buoy is not in table.	
C
                    CALL DC_WLOG ( 2, 'MA', 2, stnid ( 1:5 ), ierwlg )
                END IF
C
            END DO
C
          ELSE
C
C*          It is a fixed buoy, but not located in Great Lakes.
C
            rivals ( irselv ) = 0.0
        END IF
C*
        RETURN
        END
