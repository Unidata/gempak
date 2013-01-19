	SUBROUTINE GG_CWCP ( bndnam, nmfp, fipsin, npout, latout, 
     +			     lonout, iret )
C************************************************************************
C* GG_CWCP								*
C*									*
C* This subroutine takes the input FIPS array and they are converted 	*
C* into a new lat/lon string.						*
C*									*
C* GG_CWCP ( BNDNAM, NMFP, FIPSIN, NPOUT, LATOUT, LONOUT, IRET )	*
C*									*
C* Input parameters:							*
C*	BNDNAM		CHAR*		Name of bounds file		*
C*	NMFP		INTEGER		Number of counties in array	*
C*	FIPSIN (NMFP)	CHAR*		Array of counties		*
C*									*
C* Output parameters:							*
C*	NPOUT 		INTEGER		Num. of lat/lon point pairs	*
C*	LATOUT (NPOUT)	INTEGER		Latitude array			*
C*	LONOUT (NPOUT)	INTEGER		Longitude array			*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	 4/05						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'

	PARAMETER	( NW = 1000 )
C*
	CHARACTER*(*)	bndnam
	INTEGER		fipsin(*)
	REAL		latout(*), lonout(*)
C*
	INTEGER		expand, debug
	CHARACTER	device*12, dfilnam*8, pro*3

C-----------------------------------------------------------------------
	iret   = 0
	debug  = 0
	expand = 1
	npout  = 0
C
        mode = 1
        CALL GINITP ( mode, istat, ier )
C
        device =  'GN' 
        iunit = 1
        dfilnam =  'REBUNDLE' 
        itype = 1
        xsize = 500.0
        ysize = 500.0
        CALL GSDEVA ( device, iunit, dfilnam, itype, xsize, ysize, ier)
C
        allat = 0.0
        allon = -150.0
        urlat = 80.0
        urlon = 40.0
        pro = 'str'
        prjang1 = 90.0  
	prjang2 = -105.0  
	prjang3 = 0.0
        CALL GSMPRJ ( pro, prjang1, prjang2, prjang3, allat, allon, 
     +		 urlat, urlon, ier )

C*
	CALL ST_NULL ( bndnam, bndnam, ilens, ier )
 	CALL CVG_REBUN ( nmfp, fipsin, expand, debug, bndnam(:ilens), 
     +			 npout, latout, lonout, ier )
	CALL GENDP   ( 0, ier )
C*
        RETURN
        END
