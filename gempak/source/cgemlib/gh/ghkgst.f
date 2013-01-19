       SUBROUTINE GH_KGST ( iblk, iret )
C************************************************************************
C* GH_KGST								*
C*									*
C* This subroutine plots the U.S. two character state identifiers,	*
C* tropical and Carribean country names.				*
C*									*
C* GH_KGST ( IBLK, IRET ) 						*
C* 									*
C* Input parameters:							*
C* 	IBLK		INTEGER		Black color value		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return             *
C*									*
C**									*
C* Log:									*
C* A. Hardy/SAIC	 8/01	Created					*
C* D. Kidwell/NCEP	 4/02	Moved P.R. lon -66 to -65               *
C* D. Kidwell/NCEP	 8/02	Corrected Columbia to Colombia          *
C* S. Gilbert/NCEP	04/07	Added labels for islands (etc) for CPHC *
C************************************************************************
	PARAMETER	( NMCON = 25 )
C*
        CHARACTER       states(50)*2, cdproj*20, stid*2, 
     +                  contry(NMCON)*22
	REAL		conlat(NMCON), conlon(NMCON), fsize(NMCON)
	INTEGER		ixoff(NMCON), iyoff(NMCON)
C*
       	DATA   states / 'AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 
     +                  'FL', 'GA', 'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 
     +                  'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 
     +                  'MO', 'MT', 'NE', 'NV', 'NH', 'NJ','NM', 'NY', 
     +                  'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC',
     +                  'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 
     +                  'WI', 'WY' /
C
	DATA   contry / 'Bahamas', 'Bermuda', 'Belize', 'Colombia', 
     +                  'Costa Rica', 'Cuba', 'Dominican Rep.', 
     +                  'El Salvador', 'Guatemala', 'Haiti', 'Honduras',
     +                  'Jamaica', 'Mexico', 'Nicaragua', 'Panama', 
     +                  'Puerto Rico', 'Venezuela', 'Johnston Atoll',
     +                  'Nihoa', 'French Frigate\nShoals', 'Maro Reef',
     +                  'Lisianski', 'Pearl and\nHermes Atoll', 
     +                  'Midway', 'Kure Atoll' /
C
	DATA   conlat /  24.50,   32.00,  17.10,   9.00, 
     +                    9.90,   22.00,  18.70,   
     +                   13.78,   15.50,  19.30,  14.90,
     +                   17.50,   23.50,  12.80,   8.50,
     +                   18.90,    9.00,  16.75, 
     +                   23.06,   23.85,  25.33,
     +                   26.08,   27.83,
     +                   28.21,   28.41 /
C
	DATA   conlon / -73.80,   -63.70,  -88.50, -75.00,
     +                  -84.00,   -79.50,  -69.50, 
     +                  -88.90,   -90.30,  -72.50, -86.70,
     +                  -77.00,  -100.30,  -85.50, -80.00,
     +                  -65.00,   -67.50, -169.51, 
     +                  -161.91, -166.26, -170.50, 
     +                  -174.00, -175.83, 
     +                  -177.36, -178.33 / 
C
	DATA   fsize  / 1.0,   1.0,  1.0,  1.0,
     +                  1.0,   1.0,  1.0, 
     +                  1.0,   1.0,  1.0,  1.0,
     +                  1.0,   1.0,  1.0,  1.0,
     +                  1.0,   0.8,  0.8, 
     +                  0.8,   0.8,  0.8, 
     +                  0.8,   0.8, 
     +                  0.8,   0.8 / 
C
	DATA   ixoff  / 0,   0,  0,  0,
     +                  0,   0,  0, 
     +                  0,   0,  0,  0,
     +                  0,   0,  0,  0,
     +                  0,   0,  0, 
     +                  0,   0,  0, 
     +                  0,   0, 
     +                  6,   -10 / 
C
	DATA   iyoff  / 0,   0,  0,  0,
     +                  0,   0,  0, 
     +                  0,   0,  0,  0,
     +                  0,   0,  0,  0,
     +                  0,   0,  0, 
     +                  0,   0,  0, 
     +                  0,   -2, 
     +                  1,   1 / 
C------------------------------------------------------------------------
	iret = 0
C
C*	Query color and line attributes.
C
	CALL GH_SAVE ( ier )
C
C*	Set text attributes.
C
        CALL GSCOLR ( iblk, ier )
        CALL GSTEXT ( 2, 2, 1.0, 1, 111, 1, 2, ier )
C
C*      Find state ID in the geographic file list.
C
        DO ii = 1, 50
            stid = states(ii)
            CALL TB_FGEO  ( stid, tlatll, tlonll, tlatur, tlonur, 
     +                      cdproj, cenlat, cenlon, ier )
            CALL GTEXT ( 'M', cenlat, cenlon, stid, 0.0,
     +                           0, 0, ier )
        END DO
C
        DO ii = 1, NMCON
            CALL ST_LSTR ( contry(ii), lens, ier )
            CALL GSTEXT ( 2, 2, fsize(ii), 1, 111, 1, 2, ier )
            CALL GTEXT ( 'M', conlat(ii), conlon(ii), contry(ii)(:lens), 
     +                   0.0,  ixoff(ii), iyoff(ii), ier )
        END DO
C
C*	Restore color and line attributes.
C
	CALL GH_REST ( ier )
C*
	RETURN
	END
