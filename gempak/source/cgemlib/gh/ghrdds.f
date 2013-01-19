	SUBROUTINE GH_RDDS ( advise, lenw, slat, slon, wdir, wsped, 
     +                       wpres, wwnd, tmptim, idisp, iret )
C************************************************************************
C* GH_RDDS 								*
C*									*
C* This subroutine determins if the tropical storm is dissipating at	*
C* the current advisory time.						*
C*                                                                      *
C* GH_RDDS ( ADVISE, LENW, SLAT, SLON, WDIR, WSPED, WPRES, WWND,        *
C*           TMPTIM, IDISP, IRET ) 					*
C*									*
C* Input parameters:							*
C*	ADVISE		CHAR*		Report advisory bulletin	*
C*	LENW		INTEGER		Length of bulletin              *
C*	SLAT 		REAL		Latitude of points              *
C*	SLON 		REAL		Longitude of points             *
C*	WDIR 		CHAR*		Direction of storm movement(deg)*
C*	WSPED 		CHAR*		Speed of movement (knots)	*
C*	WPRES 		CHAR*		Minimum central pressure (mb)   *
C*	WWND		CHAR*		Max sustained wind		*
C*	TMPTIM		CHAR*		Valid location time		*
C*									*
C* Output parameters:							*
C*	IDISP		INTEGER		Storm dissipation flag		*
C*					  2 = label, marker, statement	*
C*					  1 = marker, statement		*
C*					  0 = label,marker (normal map)	*
C*					 -1 = statement			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 5/01						*
C* A. Hardy/GSC		 6/01		Cleaned up prolog		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE         'BRIDGE.PRM'
C*
	CHARACTER*(*)	advise,  wdir, wsped, wpres, wwnd, tmptim
C*
	LOGICAL         label, mrkr, hvdis
C------------------------------------------------------------------------
        iret = 0 
        mrkr  = .true. 
        label = .true. 
        hvdis = .false. 
C        
C*      Search for dissipating in report.
C
	CALL ST_LSTR ( tmptim, lent, ier)
        CALL ST_NOCC (advise (:lenw), tmptim(:lent), 2, ipoint, ier )
	idiss = INDEX( advise(ipoint:lenw), 'DISSIP' ) 
        IF ( idiss .gt. 0 ) hvdis = .true.
C
C*	Verify valid storm information.
C
        CALL ST_NUMB ( wdir, idir, ier )
        CALL ST_NUMB ( wsped, isped, ier )
C
C*      Missing current location information.
C
	IF ( ( slat .eq. RMISSD) .or. ( slon .eq. RMISSD ) ) THEN
               mrkr = .false.
               label = .false.
        END IF
C
C*      Missing storm information.
C
	IF ( ( idir .lt. 0 ) .or. ( isped .lt. 0 ) .or.
     +       ( wpres(:3) .eq. 'XXX') .or. (wwnd(:3) .eq. 'XXX' ) ) THEN
               label = .false.
        END IF
C
C*      Determine if label box, current location or dissipation 
C*      statement gets plotted.
C
	IF ( ( .not. hvdis ) .and. ( label ) .and. ( mrkr ) ) THEN
            idisp = 0 
	  ELSE IF ( ( hvdis ) .and. ( .not. label ) .and. 
     +                                        ( .not. mrkr ) ) THEN
            idisp = -1
	  ELSE IF ( ( hvdis ) .and. ( .not. label ) .and. 
     +                                              ( mrkr ) ) THEN
            idisp = 1
	  ELSE IF ( ( hvdis ) .and. ( label ) .and. ( mrkr ) ) THEN
            idisp = 2
        END IF
C*
	RETURN
	END
