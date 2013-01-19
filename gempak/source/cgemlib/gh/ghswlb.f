	SUBROUTINE GH_SWLB ( origc, tname, aadnm, tadnm, xlab,ylab,iret)
C************************************************************************
C* GH_SWLB								*
C*									*
C* This subroutine creates the text label information for the TPC wind  *
C* swath graphic.							*
C*									*
C* GH_SWLB ( ORIGC, TNAME, AADNM, TADNM, XLAB, YLAB, IRET) 		*
C*									*
C* Input parameters:							*
C*	ORIGC		CHAR*		Issuing center			*
C*	TNAME		CHAR*		Tropical storm name		*
C*      AADNM		CHAR*		Earliest advisory number	*
C*	TADNM		CHAR*		Most current advisory number	*
C*	XLAB		REAL		Latitude for centering label	*
C*	YLAB		REAL		Longitude for centering label	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 2/01   					*
C* A. Hardy/GSC		 6/01  	Added color tag 			*
C* A. Hardy/SAIC	10/01  	Input center lat/lon for label box      *
C* m.gamazaychikov/SAIC	06/06	Added wocen to CS, option for nws string*
C* S. Gilbert/NCEP      07/06   Changed storm id (wocen) to issuing     *
C*                              center (origc)                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	tname, aadnm, tadnm, origc
        INTEGER         lenf
C*
	CHARACTER 	nws*79 ,tpstm*20, hrcn*20, advis*30,
     +                  thru*10, fulstr*300, force*50, coltag*33
        REAL            xx(5), yy(5)
C-----------------------------------------------------------------------
	iret = 0
C
C*	Query attributes.
C
	CALL GQTEXT ( jtxfn, jtxhw, sztext, jtxwid, jbrdr, jrrotn,
     +		      jjust, ier )
C
C*	Set nws string based on the basin
C
	IF ( origc (:4) .eq. 'CPHC' ) THEN
           nws = '        NATIONAL WEATHER SERVICE/'
     +           // 'CENTRAL PACIFIC HURRICANE CENTER'
         ELSE
           nws = '               NATIONAL WEATHER SERVICE/'
     +           // 'NATIONAL HURRICANE CENTER'
	END IF
C
        advis = 'FROM ADVISORIES '	
        thru  = 'THROUGH '	
C
	CALL ST_LSTR ( tname, lenm, ier )
        tpstm = 'TROPICAL STORM'
        hrcn = 'AND HURRICANE'
        force = 'FORCE WIND SWATHS OF '// tname(:lenm)

C
	CALL ST_LSTR ( tpstm, lent, ier )
        CALL ST_LSTR ( hrcn, lenh, ier )
        CALL ST_LSTR ( force, lenc, ier )
C
	CALL ST_LSTR ( advis, lena, ier )
	CALL ST_LSTR ( thru, lenu, ier )
	CALL ST_LSTR ( aadnm, len, ier )
	CALL ST_LSTR ( tadnm, lend, ier )
C
 	fulstr = nws // CHCR // CHCR //  CHCR  // CHCR // advis(:lena) 
     +          // ' ' // aadnm(:len) // ' ' // thru(:lenu)
     +          // ' ' // tadnm(:lend)
 	CALL ST_LSTR ( fulstr, lenf, ier )
C
C*	Plot the text box and text in normalized coordinates.  
C
 	ixoff  = 0
 	iyoff  = -5
C
        coltag = 's_swath_box_text'
        CALL ST_LSTR ( coltag, lens, ier )
        CALL GH_COLR ( coltag(:lens), 1, ier )

        CALL GSTEXT ( 22, 2, 1.0, 1, 221, 1, 2, ier )
        CALL GTRANS ( 'M', 'N', 1, xlab, ylab, xxx, yyy, ier )
 	CALL GTEXT ( 'N', xxx, yyy, fulstr(:lenf), 0.0, ixoff, 
     +               iyoff, ier )
C
	CALL GQSYSZ ( rxszmk,ryszmk,rxsztx,rysztx,rxszwb,ryszwb,ier )
        CALL GSTEXT ( 22, 2, 1.0, 1, 121, 1, 1, ier )
        space = 2 * rxsztx
        iavch = 15
C
C*	Plot 'hurricane' text.
C
        xhr = (xxx - (space + iavch * rxsztx ) )
        yyz = yyy - .007
 	CALL GTEXT ( 'N', xhr, yyz, hrcn(:lenh), 0.0, ixoff, 
     +               iyoff, ier )
C
C*      Plot 'storm' text.
C
        xxz = xhr - (space + 0.033) - ( iavch * rxsztx )
 	CALL GTEXT ( 'N', xxz, yyz, tpstm(:lent), 0.0, ixoff, 
     +               iyoff, ier )
C
C*	Plot color legend for tropical storm.
C
        xx (1) = xhr - .017
        xx ( 2 ) = xx ( 1 ) - .033
	xx ( 3 ) = xx ( 2 )
	xx ( 4 ) = xx ( 1 )
	xx ( 5 ) = xx ( 1 )
 	yy ( 1 ) = yyz - 3.1 * rysztx
	yy ( 2 ) = yy ( 1 )
	yy ( 3 ) = yy ( 1 ) + .015
	yy ( 4 ) = yy ( 3 )
	yy ( 5 ) = yy ( 1 )

        coltag = 's_ts_wind'
        CALL ST_LSTR ( coltag, lens, ier )
        CALL GH_COLR ( coltag(:lens), 18, ier )

	CALL GFILL ( 'N', 5, xx, yy, ier)
C
C*      Plot 'force' text line.
C
        coltag = 's_swath_box_text'
        CALL ST_LSTR ( coltag, lens, ier )
        CALL GH_COLR ( coltag(:lens), 1, ier )

        xxz = xxx  + .03
 	CALL GTEXT ( 'N', xxz, yyz, force(:lenc), 0.0, ixoff, 
     +               iyoff, ier )
C
C*	Plot color legend for hurricane.
C
        xx (1) = xxz - .018
        xx ( 2 ) = xx ( 1 ) - .033
	xx ( 3 ) = xx ( 2 )
	xx ( 4 ) = xx ( 1 )
	xx ( 5 ) = xx ( 1 )
C       
        coltag = 's_hur_wind'
        CALL ST_LSTR ( coltag, lens, ier )
        CALL GH_COLR ( coltag(:lens), 2, ier )

        CALL GFILL ( 'N', 5, xx, yy, ier)
C
C*	Restore attributes.
C
	CALL GSTEXT ( jtxfn, jtxhw, sztext, jtxwid, jbrdr, jrrotn,
     +		      jjust, ier )
C*
        RETURN
	END
