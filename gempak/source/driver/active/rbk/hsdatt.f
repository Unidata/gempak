	SUBROUTINE HSDATT  ( iunit, filnam, itype, xsize, ysize,
     +			     ncurwn, iret )
C************************************************************************
C* HSDATT - RBK								*
C* 									*
C* This subroutine defines the RBK device attributes.			*
C* 									*
C* HSDATT  ( IUNIT, FILNAM, ITYPE, XSIZE, YSIZE, NCURWN, IRET )		*
C*									*
C* Input parameters:							*
C*	IUNIT		INTEGER		Type of output device		*
C*	FILNAM		CHAR*		Name of file to use as output	*
C*      ITYPE           INTEGER         Output file format		*
C*					     2 = NAFOS format		*
C*					   <>2 = AFOS format		*
C*      XSIZE           REAL            X size in pixels		*
C*      YSIZE           REAL            Y size in pixels		*
C*                                                                      *
C* Output parameters:							*
C*	NCURWN		INTEGER		Current window number		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC		9/98		Modified from utf's HSDATT      *
C* A. Hardy/GSC		9/00		Modified base time YYMMDD/HHNN  *
C* B. Yin/SAIC          3/04		Changed SS_GTIM to CSS_GTIM     *
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C*
	CHARACTER*(*)	filnam
C*
	CHARACTER	carr(2)*160, fname*160, ttime*20, ctime*20
	CHARACTER	hourst*4, datest*2
        INTEGER         ibase(5), itime 
C------------------------------------------------------------------------
	iret   = NORMAL
	ncurwn = 0
C*
	CALL ST_CLST ( filnam, ';', ' ', 2, carr, num, ierr )
	CALL ST_NULL ( carr(1), fname, lenf, ier )
C
C*	Retrieving full GEMPAK date/time base time string.
C*      If base time is given as DDHHNN conver to DD/HHNN.
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
	CALL ASDATT ( iunit, fname, lenf, itype, ibase, xsize, ysize,
     +		      ileft, ibot, iright, itop, nncolr, iret )
        isxoff  = 0
        isyoff  = 0
        iswdth  = ABS ( iright - ileft )
        ishght  = ABS ( itop - ibot )
C*
	RETURN
	END
