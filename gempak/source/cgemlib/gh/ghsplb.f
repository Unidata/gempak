	SUBROUTINE GH_SPLB ( tname, dattim, tadnm, curlon, pllat, pllon,
     +			     contur, ilcolr, iret ) 
C************************************************************************
C* GH_SPLB								*
C*									*
C* This subroutine plots the text label and legend information for the  *
C* TPC strike probability graphic.					*
C*									*
C* GH_SPLB ( TNAME, DATTIM, TADNM, CURLON, PLLAT, PLLON, CONTUR,        *
C*	     ILCOLR, IRET )	                                        *
C*									*
C* Input parameters:							*
C*	TNAME		CHAR*		Tropical storm name		*
C*	DATTIM		CHAR*		Current advisory time           *
C*	TADNM		CHAR*		Current advisory number		*
C*	CURLON		REAL		Current longitude		*
C*	PLLAT		REAL		Latitude for text               *
C*	PLLON		REAL		Longitude for text		*
C*	CONTUR		LOGICAL		Flag for contour presence       *
C*	ILCOLR (*)	INTEGER		Colors for legend               *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 4/01   					*
C* D. Kidwell/NCEP	 6/01 	Modified latitude for text 		*
C* A. Hardy/GSC		 6/01   Added color tag; GH_SAVE, GH_REST	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	tname, dattim, tadnm
	INTEGER		ilcolr (*)
	LOGICAL		contur
C*
	CHARACTER	fulstr*1000, legend (4)*8, timstr*100, tzone*1,
     +			zone2*3, fmonth*10, fmon*3, fday*10, ampm*2,
     +			year*4, day*2, hour*2, mins*2, coltag*33,
     +                  prbclr (4)*15
	INTEGER		jtarr (5)
	REAL		xx (5), yy (5)
C*
	DATA		legend / '10-19%', '20-49%', '50-99%', '100%' /
	DATA		prbclr / 'p_prob_lt_20', 'p_prob_lt_50', 
     +                           'p_prob_lt_100', 'p_prob_eq_100' /
C-----------------------------------------------------------------------
	iret = 0
C
C*	Get the time elements required.
C
	IF ( curlon .gt. -85.0 ) THEN
	    tzone = 'E'
	  ELSE
	    tzone = 'C'
	END IF
	CALL GH_TIME ( dattim, tzone, jtarr, hours, zone2, fmonth, fmon, 
     +		       fday, ampm, ier )
	CALL ST_INCH ( jtarr ( 1 ), year, ier )
	CALL ST_INCH ( jtarr ( 3 ), day, ier )
	IF ( jtarr ( 3 ) .lt. 10 ) day  = '0' // day ( :1 )
	CALL ST_INCH ( jtarr ( 4 ), hour, ier )
	IF ( jtarr ( 4 ) .lt. 10 ) hour = ' ' // hour ( :1 )
	CALL ST_INCH ( jtarr ( 5 ), mins, ier )
	IF ( jtarr ( 5 ) .lt. 10 ) mins = '0' // mins ( :1 )
	CALL ST_LCUC ( fday, fday, ier )
C
C*	Assemble the date/time string.
C
	timstr = hour // ':' // mins // ' ' // ampm // ' ' // zone2 
     +		 // ' ' // fday (:3) // ' ' // fmon // ' ' // day 
     +		 // ' ' // year
C
C*	Create the label strings for the text in the box.
C
	CALL ST_LSTR ( tname, lenm, ier )
	CALL ST_LSTR ( timstr, lent, ier )
	CALL ST_LSTR ( tadnm, lenn, ier )
C
	fulstr = 'NATIONAL WEATHER SERVICE/NATIONAL HURRICANE CENTER'
     +		 // ' ADVISORY NUMBER  ' // tadnm ( :lenn ) // CHCR
     +		 // CHCR // 'PROBABILITY THAT CENTER OF '
     +		 // tname ( :lenm ) 
     +		 // ' WILL PASS WITHIN 75 STATUTE MILES' // CHCR // CHCR
     +		 // ' DURING THE 72 HOURS STARTING AT '
     +		 // timstr ( :lent ) // CHCR
	CALL ST_LSTR ( fulstr, lenf, ier )
	IF ( contur ) THEN
	    fulstr = fulstr ( :lenf ) // CHCR // CHCR
	    CALL ST_LSTR ( fulstr, lenf, ier )
	END IF
C
C*	Plot the text.  Use normalized coordinates for consistency.
C
	ixoff = 0
 	iyoff = -5
        CALL GSTEXT ( 22, 2, 1.0, 1, 221, 1, 2, ier )
	CALL GTRANS ( 'M', 'N', 1, pllat, pllon, xxx, yyy, ier )
	CALL GQSYSZ ( rxszmk,ryszmk,rxsztx,rysztx,rxszwb,ryszwb,ier )
	yyy = yyy - 7. * rysztx
	IF ( yyy .gt. .74 ) yyy = .74
 	CALL GTEXT ( 'N', xxx, yyy, fulstr ( :lenf ), 0.0, ixoff, 
     +               iyoff, ier )
C
	IF ( contur ) THEN
            CALL GSTEXT ( 22, 2, 1.0, 1, 121, 1, 1, ier )
C
C*	    Loop to plot color legends.
C
	    xx ( 1 ) = xxx - .22
	    yy ( 1 ) = yyy - 5.7 * rysztx
	    yy ( 2 ) = yy ( 1 )
	    yy ( 3 ) = yy ( 1 ) + .015
	    yy ( 4 ) = yy ( 3 )
	    yy ( 5 ) = yy ( 1 )
	    yl = ( yy ( 1 ) + yy ( 3 ) ) * .5
	    DO ii = 1, 4
	        xx ( 2 ) = xx ( 1 ) + .033
	        xx ( 3 ) = xx ( 2 )
	        xx ( 4 ) = xx ( 1 )
	        xx ( 5 ) = xx ( 1 )
C
      		coltag = prbclr ( ii )
        	CALL ST_LSTR ( coltag, lens, ier )
        	CALL GH_COLR ( coltag(:lens), ilcolr(ii), ier)
	        CALL GFILL ( 'N', 5, xx, yy, ier)
C
      		coltag = 'p_label_box_text'
        	CALL ST_LSTR ( coltag, lens, ier )
        	CALL GH_COLR ( coltag(:lens), 1, ier)

	        xl = xx ( 2 ) + .01
	        CALL GTEXT ( 'N', xl, yl, legend (ii), 0.0, 0, 0, ier )
	        xx ( 1 ) = xx ( 1 ) + .13
	    END DO
	END IF
C*
        RETURN
	END
