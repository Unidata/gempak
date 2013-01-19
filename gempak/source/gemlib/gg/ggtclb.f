	SUBROUTINE GG_TCLB ( valid, ibasin, iret )
C************************************************************************
C* GG_TCLB								*
C*									*
C* This subroutine plots the legends for the tropical cyclone marine    *
C* graphic.                                                             *
C*									*
C* GG_TCLB ( VALID, IBASIN, IRET )                                      *
C*									*
C* Input parameters:							*
C*	VALID		CHAR*		Full valid time string          *
C*	IBASIN 		INTEGER		Indicator for ocean             *
C*					  1 = Atlantic - TPC            *
C*					  2 = Pacific - TPC             *
C*					  3 = Pacific - CPHC            *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/01	                                        *
C* D. Kidwell/NCEP	 2/02	Expanded TCMG legend text; added CPHC   *
C* D. Kidwell/NCEP	 2/03	Removed 'EXPERIMENTAL' label            *
C* NHC/NCEP             09/11   Changed 'TROPICAL PREDICTION CENTER' to *
C*                              'NATIONAL HURRICANE CENTER'             * 
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	valid
C*
	CHARACTER	wlabel*160
	REAL		darlat (3), darlon (3), vldlat (3), vldlon (3)
C*
    	DATA		darlat /   35.,   36. ,   37.5 /
    	DATA		darlon /  -20., -162. , -171.5 /
    	DATA		vldlat /    3.,    2.5,     .5 /
    	DATA		vldlon /  -48., -135. , -160.  /
C-----------------------------------------------------------------------
	iret = 0
C
C*	Query attributes.
C
	CALL GQTEXT ( jtxfn, jtxhw, sztext, jtxwid, jbrdr, jrrotn,
     +		      jjust, ier )
C
C*	Set attributes.
C
	CALL GSTEXT ( 1, 0, 2., 0, 221, 0, 2, ier )
	wlabel = 'SHADED REGION' // CHCR // 'IS' // CHCR // 
     +		 'DANGER AREA'
	CALL GTEXT ( 'M', darlat ( ibasin ), darlon ( ibasin ), wlabel,
     +		     0., 0, 0, ier )
C
	CALL GSTEXT ( 0, 0, 1., 0, 0, 0, 0, ier )
	CALL ST_LSTR ( valid, len, ier )
	IF ( ibasin .le. 2 ) THEN
	    wlabel = 'TROPICAL CYCLONE MARINE GRAPHIC' // CHCR //
     +		     'NATIONAL HURRICANE CENTER' // CHCR //
     +		     'TROPICAL ANALYSIS AND FORECAST BRANCH' // CHCR //
     +		     'MIAMI FLORIDA   33165' // CHCR // CHCR //
     +		     valid (:len)
	  ELSE
	    wlabel = 'TROPICAL CYCLONE MARINE GRAPHIC' // CHCR //
     +		     'CENTRAL PACIFIC HURRICANE CENTER' // CHCR //
     +		     'HONOLULU HAWAII   96822' // CHCR // CHCR //
     +		     valid (:len)
	END IF
	CALL GTEXT ( 'M', vldlat ( ibasin ), vldlon ( ibasin ), wlabel,
     +		     0., 0, 0, ier )
C
C*	Restore attributes.
C
	CALL GSTEXT ( jtxfn, jtxhw, sztext, jtxwid, jbrdr, jrrotn,
     +		      jjust, ier )
C*
	RETURN
	END
