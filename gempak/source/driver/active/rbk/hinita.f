	SUBROUTINE HINITA  ( dev, cdev, iunit, filnam, itype,
     +			     xsize, ysize, ncurwn, iret )
C************************************************************************
C* HINITA - RBK 				                        * 
C*									*
C* This subroutine is called to initialize a new device driver.		*
C*									*
C* HINITA  ( DEV, CDEV, IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN,	*
C*	     IRET )							*
C*									*
C* Input parameters:							*
C*	DEV		CHAR*		Device name			*
C*	CDEV            CHAR*           Current device name		*
C*	IUNIT		INTEGER		Type of output device		*
C*					  (not used for UTF)	        *
C*	FILNAM		CHAR*		File name of output		*
C* 	ITYPE		INTEGER		Output file format		*
C*					    2 = NAFOS format		*
C*					  <>2 = AFOS format		*
C*	XSIZE		REAL		X size in pixels		*
C*	YSIZE		REAL		Y size in pixels		*
C*									*
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C*					  (not used for UTF)		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC		9/98		Modified from utf's HINITA      *
C* A. Hardy/GSC		9/00		Modified base time YYMMDD/HHNN  *
C* B. Yin/SAIC          3/04   		Changed SS_GTIM to CSS_GTIM     *
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
	INCLUDE         'ERROR.PRM'
	INCLUDE         'DVWNDW.CMN'
	INCLUDE         'DEVCHR.CMN'
	INCLUDE         'DEVACT.CMN'
C*
	CHARACTER*(*)	dev, cdev, filnam
C*
	CHARACTER	carr(2)*160, fname*160, ttime*20, ctime*20
	CHARACTER	hourst*4, datest*2
	INTEGER         ibase(5), itime
C------------------------------------------------------------------------
	iret   = NORMAL
	ncurwn = 0
C
C*	Save device common area.
C
	ddev   = dev
	curdev = cdev
	niunit = iunit
C
C*	Initialize device characteristics common area for specific
C*	device.
C
	CALL HINIT ( iret)
C
C*	Check to make sure bscalh has been set in device driver. If
C*	it has not, set it to a reasonable value.
C
	IF  ((bscalh .le. 0.) .or. (bscalh .gt. 1000.)) bscalh = 3.0
C
C*	Initialize the UTF output file.
C
	CALL ST_CLST ( filnam, ';', ' ', 2, carr, num, ierr )
	CALL ST_NULL ( carr(1), fname, lenf, ier )
C
C*	Retrieving full GEMPAK date/time base time string.
C*      If base time is given as DDHHNN convert to DD/HHNN.
C
        CALL ST_NOCC ( carr(2), '/', 1, ipntr, ier)
        IF ( ipntr .eq. 0 ) THEN
	    CALL ST_LSTR ( carr(2), leng, ier)
	    IF ( leng .eq. 6 ) THEN
              datest = carr(2)(1:2)
              hourst = carr(2)(3:6)
 	      ELSE IF (leng .eq. 5) THEN
              datest = '0' // carr(2)(1:1)
              hourst = carr(2)(2:5)
            END IF
            carr(2) = datest// '/' //hourst
        END IF
C
	itime = 1
        CALL CSS_GTIM ( itime, ctime, ier)
        CALL TI_STAN ( carr(2), ctime, ttime, ierr)
        IF ((ierr .eq. 0 ) .and. ( ttime(3:4) .le. '12') ) THEN
	    CALL TI_CTOI ( ttime, ibase, ier)
	  ELSE
	    DO i = 1,5
                ibase(i) = IMISSD
            END DO
            CALL ER_WMSG ('RBK', 1, ' ', ier) 
        END IF
C
	CALL AINITA ( iunit, fname, lenf, itype, ibase, xsize, ysize,
     +		      ileft, ibot, iright, itop, nncolr, iret )
C
C*      isxoff = x offset of the 'S' coord relative to 'D' coord
C*      isyoff = y offset of the 'S' coord relative to 'D' coord
C*      iswdht = width of the 'S' coord system
C*      ishght = height of the  'S' coord system
C*      not used in PS driver
C
        isxoff  = 0
        isyoff  = 0
        iswdth  = ABS ( iright - ileft )
        ishght  = ABS ( itop - ibot )
C
C*	Do not change anything below this line.
C------------------------------------------------------------------------
	ispanx = ISIGN ( 1, ( iright - ileft ) )
	ispany = ISIGN ( 1, ( itop - ibot ) )
C*
	RETURN
	END
