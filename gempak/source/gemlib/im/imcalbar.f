	SUBROUTINE IM_CALBAR ( minpx, maxpx, minval, maxval, iret)
C************************************************************************
C* IM_CALBAR                                                            *
C*                                                                      *
C* This subroutine computes the calibration levels for given data 	*
C* ranges. This routine can be called multiple times for different	*
C* calibration ranges when the calibration "curve" consists of more	*
C* than one segment. IM_CALBAR_INIT should be called first.		*
C* Common block parameter cmblev stores the calibrated levels.		*
C*                                                                      *
C* IM_CALBAR  ( MINPX, MAXPX, MINVAL, MAXVAL, IRET )			*
C*                                                                      *
C* Input parameters:                                                    *
C*      MINPX		REAL		Minimum pixel value 		*
C*      MAXPX		REAL		Maximum pixel value 		*
C*      MINVAL		REAL		Minimum data value 		*
C*      MAXVAL		REAL		Maximum data value 		*
C*									*
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*					 -1 = bad brightness range	*
C**                                                                     *
C* Log:                                                                 *
C* Chiz/Unidata		04/02		Created 			*
C************************************************************************

	INTEGER		iret
	REAL		minpx, maxpx, minval, maxval
	CHARACTER*20	tmplev

	INCLUDE		'IMGDEF.CMN'

	iscaleval = 1
	scaleval = 1.
	iminval = NINT(minval)
	imaxval = NINT(maxval)

	IF ( ( maxpx .lt. 0 ).or.( maxpx .gt. 255 )) THEN
	   iret = -1
	   RETURN
	END IF
	IF ( ( minpx .lt. 0 ).or.( minpx .gt. 255 )) THEN
	   iret = -1
	   RETURN
	END IF

C
C*	Determine colorbar levels and store common block values.
C 
	ratio = (maxval - minval)/(maxpx - minpx)
	IF (ratio .lt. 1 ) THEN
	   np = ABS(LOG10(ABS(ratio)) - .5)
	ELSE
	   np = 0
	END IF

        iminpix = int(minpx + 1)
        imaxpix = int(maxpx + 1)

	DO i=iminpix, imaxpix
           IF ( ( i .ge. iminpix ) .and. ( i .le. imaxpix ) ) THEN
                 IF ( ( mod ( i - 1, 5 ) .eq. 0).or.
     +		    ( i .eq. imaxpix ) ) THEN
		     IF (np .eq. 0) THEN
                        level = nint( (i-iminpix) * ratio) + iminval
                        CALL ST_INCH ( level/iscaleval, 
     +                              cmblev (i), ier )
		     ELSE
                        flevel = (i - 1 - minpx) * ratio + minval
                        CALL ST_RLCH ( flevel/scaleval, np,
     +                              tmplev, ier )
			cmblev(i) = tmplev(1:8)
		     END IF
                 ELSE
                     cmblev (i) = ' '
                 END IF
           END IF
        END DO

	iret = 0

	RETURN
	END



	SUBROUTINE	IM_CALBAR_INIT (iret)
C************************************************************************
C* IM_CALBAR_INIT                                                       *
C*                                                                      *
C* This subroutine initializes the calibration level array.		*
C*                                                                      *
C* IM_CALBAR_INIT  ( IRET )						*
C*                                                                      *
C* Input parameters:                                                    *
C*	None								*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C**                                                                     *
C* Log:                                                                 *
C* Chiz/Unidata         04/02           Created                         *
C************************************************************************
C
	INCLUDE         'IMGDEF.CMN'
C
	iret = 0

	DO i=1,256
	   cmblev(i) = ' '
	END DO

	RETURN
	END
