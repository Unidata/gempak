	SUBROUTINE GDMGLV  ( iflno, gvcord, time, ystrt, ystop,
     +			     istrt, istop, rlvl,  iret )
C************************************************************************
C* GDMGLV								*
C*									*
C* This subroutine gets the data to plot for a profile.			*
C*									*
C* GDMGLV  ( IFLNO, GDATIM, GVCORD, GFUNC, GPOINT, TIME, IVCORD,	*
C*           RGX, RGY, RLAT, RLON, NPTS, X, Y, PARM, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		Grid file number		*
C*	GVCORD		CHAR*		User input vert coord		*
C*	TIME 		CHAR*		Time to search for levels	*
C*      YSTRT	 	REAL            Starting vert coord value	*
C*      YSTOP           REAL            Stopping vert coord value       *
C*									*
C* Output parameters:							*
C*	ISTRT		INTEGER		First level to start with	*
C*	ISTOP		INTEGER		Last level to plot		*
C*	RLVL (*)	REAL		List of levels			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = invalid grid point	*
C*					 -9 = no valid points		*
C*					-10 = no levels at this time	*
C**									*
C* Log:									*
C* J. Whistler/NSSFC	12/94						*
C* L. Hinson/AWC         7/06          Updated for 5.9.4 NAWIPS         *
C*                                     Revision mades with regards to   *
C*                                     DG calls                         *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gvcord, time
	REAL		rlvl ( * )
C*
	CHARACTER	dattim (2)*20, ccord*48
C*
	INTEGER		level ( 2, LLMXLV )
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	npts = 0
C
C*	Check that vertical coordinate is valid.
C
	CALL ST_LCUC  ( gvcord, ccord, ier )
	CALL LV_CORD  ( ccord, ccord, ivcord, ier )
C
C*	Get levels which might have data.
C*	First translate date/time and vertical coordinate.
C
	dattim ( 1 ) = time
	dattim ( 2 ) = ' '
        CALL DG_GLEV ( iflno, dattim, ivcord, 
     +                 LLMXLV, level, nlev, ier )   
	IF  ( nlev .eq. 0 )  THEN
	    iret = -10
	    CALL ER_WMSG  ( 'GDMLEV', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Float the levels for sorting.
C
	knt = 0
	DO i = 1, nlev
	  IF ( level(1,i) .ne. 0 ) THEN
	      knt = knt + 1
	      rlvl ( knt ) = FLOAT ( level ( 1, i ) )
	  END IF
	END DO
	nlev = knt
C*
	CALL LV_SORT ( ivcord, nlev, rlvl, iret )
C*
	ys1 = ystrt
C*
	i = 1
	istrt = 0
	istop = 0
	DO WHILE ( i .lt. nlev .and.
     +           (istrt .eq. 0 .or. istop .eq. 0 ) )
	  i = i + 1
	  IF ( (ys1 .ge. rlvl ( i-1 ) .and. ys1 .lt. rlvl ( i ) )
     +                               .or.
     +         (ys1 .le. rlvl ( i-1 ) .and. ys1 .gt. rlvl ( i ) ) ) THEN
              istrt = i - 1
	  END IF
	  IF ( (ystop .ge. rlvl ( i-1 ) .and. ystop .lt. rlvl ( i ) )
     +                               .or.
     +         (ystop .le. rlvl ( i-1 ) .and. ystop .gt. rlvl ( i ) ) )
     +      istop = i - 1
	END DO
C*
	IF ( istrt .eq. 0 ) istrt = 1
	IF ( istop .eq. 0 ) istop = nlev
C*
	RETURN
	END
