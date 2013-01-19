	SUBROUTINE HSDATT  ( iunit, fname, ictype, xsize, ysize, 
     +			     ncurwn, iret )
C************************************************************************
C* HSDATT - GIF 							*
C* 									*
C* This subroutine defines the device attributes.			*
C* 									*
C* HSDATT  ( IUNIT, FNAME, ICTYPE, XSIZE, YSIZE, 			*
C*		NCURWN, IRET )						*
C*									*
C* Input parameters:							*
C*	IUNIT		INTEGER		Unit				*
C*	FNAME		CHAR*		File/window name		*
C*      ICTYPE          INTEGER         Device type (color,bw,etc)      *
C*      XSIZE           REAL            X size in inches or pixels      *
C*      YSIZE           REAL            Y size in inches or pixels      *
C*                                                                      *
C* Output parameters:							*
C*	NCURWN		INTEGER		Window number			*
C* 	IRET		INTEGER		Return code			*
C*					  NORMAL = no change to att's	*
C*					  NWSIZE = Device att's changed	*
C*					  NIDSIZ = Invalid device size	*
C**									*
C* Log:									*
C* J. Cowie/COMET	 9/95	Added other attributes.			*
C* J. Cowie/COMET	 2/96	Fixed pop mode change bug		*
C* J. Nielsen-G/TAMU	12/96	Modified for GIF driver			*
C* T. Lee/GSC		 7/00	Called wclosp				*
C* R. Tian/SAIC		05/02	Fixed memory fault			*
C* S. Jacobs/NCEP	 2/03	Changed return value to not be from 	*
C*				HSCINT, but for new window or size	*
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'driver.cmn'
C*
	CHARACTER*(*)	fname
C*
C------------------------------------------------------------------------
	iret = NORMAL
	ncurwn = 1
        IF ( ( xsize .GT. 1 ) .AND. ( xsize .LT. 5001 )
     +       .AND. ( ysize .GT. 1 ) .AND. ( ysize .LT. 5001 ) )  THEN
          ir = NINT ( xsize ) - 1
          ib = NINT ( ysize ) - 1
        ELSE
	  ir = 639
	  ib = 479
        END IF
C
C*	See if the GIF characteristics have changed.  If so, dump out to
C*	the current file if needed.
C
        IF ( ( (fname .ne. ' ') .and. (fname .ne. filnam) ) .or. 
     +	     ( ( xsize .ne. RMISSD .or. ysize .ne. RMISSD ) .and. 
     +	       ( ir .ne. iright .or. ib .ne. ibot ) ) )  THEN 

            CALL WCLOSP ( filnam, gfplot, iret )
            gfplot = .false.
C
C*	    Update filename.
C
	    IF ( fname .ne. ' ') THEN 
		filnam = fname 
		iret   = NEWWIN
	    ELSE
		filnam = 'gempak.gif'
		iret   = NEWWIN
	    END IF
C
C*	    Update window size.
C
	    IF ( (xsize .ne. RMISSD) .or. (ysize .ne. RMISSD) ) THEN
		iright = ir
		ibot   = ib
		iret   = NWSIZE
	    END IF	

C
C*	    Update unit.
C
	    IF ( (iunit .NE. RMISSD) .and. (iunit .GT. 0) )  THEN
		niunit = iunit
	    END IF

C
C*	    Open plot and reset colors
C
	    opnfil = .false.
	    gfplot = .false.
C
	    CALL HSCINT ( ier )
	END IF
C*
	RETURN
	END
