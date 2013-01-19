	SUBROUTINE GG_WTCH ( date, icolor, ssize, iwidth, iflags, iret )
C************************************************************************
C* GG_WTCH								*
C*									*
C* This subroutine plots the current thunderstorm and tornado watches.	*
C*									*
C* GG_WTCH ( DATE, ICOLOR, SSIZE, IWIDTH, IFLAGS, IRET )		*
C*									*
C* Input parameters:							*
C*	DATE		CHAR*		Ending time for watches		*
C*	ICOLOR (3)	INTEGER		Line colors			*
C*	SSIZE  (3)	REAL		Line sizes for status lines	*
C*	IWIDTH (3)	INTEGER		Line widths			*
C*	IFLAGS (6)	INTEGER		Flags for labels		*
C*					  0 = false			*
C*					  1 = true			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 9/98	Created					*
C* S. Jacobs/NCEP	 9/98	Fixed increment for stepping thru file	*
C* S. Jacobs/NCEP	 9/98	Added parameter for the num of watches	*
C* S. Jacobs/NCEP	 9/98	Added check for start time when plotting*
C* S. Jacobs/NCEP	 9/98	Fixed loop to read reports from files	*
C* D. Kidwell/NCEP	 4/99	Fixed for Y2K; added GG_WLBL call; fixed*
C*                              watch cancel check; GTEXTC -> GTEXT     *
C* D. Kidwell/NCEP	 4/99	Remove trailing blanks from time labels *
C* S. Jacobs/NCEP	 5/99	Modified to read "decoded" data files	*
C* A. Hardy/GSC		 6/99   Added ilbflg - label flag               *
C* S. Jacobs/NCEP	 6/99	Increased array sizes, NW, to 500	*
C* S. Jacobs/NCEP	 7/99	Fixed for replacement watch type RP     *
C* S. Jacobs/NCEP	 8/99	Added plotting of status line		*
C* S. Jacobs/NCEP	 8/99	Changed call to FL_SCND			*
C* S. Jacobs/NCEP	 8/99	Changed call to FL_TMPL			*
C* S. Jacobs/NCEP	 9/99	Fixed reading of status lines when first*
C* D. Kidwell/NCEP	 9/99	Fixed for location of status line label *
C* S. Jacobs/NCEP	11/99	Increased time range by 1 hour for check*
C* S. Jacobs/NCEP	12/99	Fixed time range check			*
C* D. Kidwell/NCEP	 1/00	Fixed bugs - cancel, replace, compare   *
C* D. Kidwell/NCEP	 3/00	Added checks for corrn and cancel flags *
C* S. Jacobs/NCEP	 3/00	Changed calling sequence		*
C* D. Kidwell/NCEP	 4/00	Added processing for TEST watches       *
C* M. Li/GSC		 5/00	Added MXNMFL and MXFLSZ			*
C* A. Hardy/GSC		 7/00	Increased the lat/lon array 7 -> 20 pts.*
C* J. Wu/GSC             7/00   Added checks for TI_STAN's return status*
C* S. Jacobs/NCEP	 3/01	Increased file template to 48 chars	*
C* S. Jacobs/NCEP	 4/01	Changed flstrt check from gt to ge	*
C* D. Kidwell/NCEP	 5/01	Changed line type for watch near expire *
C* A. Hardy/GSC		 6/01   Separated the file read into GG_WCUR	*
C* M. Li/SAIC		 3/03	Set colors for watch			*
C* M. Li/SAIC		 4/03   Set colors for watch xxx0		*
C* A. Hardy/NCEP	 5/03   Removed addition of 60 mins to timstr   *
C* M. Li/SAIC		12/04	Modified watch flags			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE         'ggcmn.cmn'
C*
	PARAMETER	( JSVR = 1, JTOR = 2, JSTT = 3 )
	PARAMETER	( JWTM = 1, JSTM = 2, JWNM = 3, JSNM = 4,
     +                    JCLC = 5, JMRS = 6 )
C*
        CHARACTER*(*)   date
	INTEGER		icolor(*), iwidth(*), iflags(*)
	REAL		ssize(*)
C*
	CHARACTER	wlabel*80, tmstp4*20, tmstr4*20, 
     +			temp4*20, t90*20, slabel*80 
C*
	INTEGER		istrt(5), iuwtch(1000)
C*
	INTEGER		imrsl(NW)
	CHARACTER	temp(NW)*4
	LOGICAL		plot
C-----------------------------------------------------------------------
	iret = 0
        CALL GG_WCUR ( date, iuwtch, iunum, iret )
C
C*	Query current settings.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQLINE ( jtype, jtyhw, jwidth, jwdhw, ier )
	CALL GQSPLN ( jsltyp, jslstr, jsldir, sizsp, jslwid, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
C
C*	Set attributes and defaults.
C
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
	DO  i = 1, 3
	    IF  ( ( iwidth(i) .le.  0 ) .or.
     +		  ( iwidth(i) .gt. 10 ) )  iwidth(i) = 3
	END DO
C
	IF  ( ( ssize(JSTT) .le.  0.0 ) .or.
     +	      ( ssize(JSTT) .gt. 10.0 ) )  ssize(JSTT) = 0.7
C
C*	Seek the most recent status line(s)
C
	CALL ST_SORT ( 2, nwtch, wnum, np, temp, ier )
	DO ii = 1, np
	    DO ip = 1, nwtch
		IF ( wnum(ip) .eq. temp(ii) ) imrsl(ii) = ip
	    END DO
	END DO
C
C*	Plot the graphic for each valid report.
C
	DO ip = 1, nwtch
	    plot = .false.
	    DO ii = 1, np
		IF ( ip .eq. imrsl(ii) ) plot = .true.
	    END DO
C
 	    CALL TI_DTM4 ( timstp ( ip ), tmstp4, ier )
 	    CALL TI_DTM4 ( timstr(ip), tmstr4, ier )
C
  	    IF  ( ( dattim .eq. 'ALL' ) .or. 
     +              ( ( tmstp4 .gt. dattm4 ) .and.
     +		    ( tmstr4 .le. dattm4 ) ) )  THEN

 	    	CALL TI_CTOI ( timstr(ip), istrt, ier )
C
C*		Set the line type for actual or test watch.
C
		IF ( itest (ip) .eq. 0 ) THEN
		    iltyp  = 1
		    isltyp = 6
C
C*		    Check if the watch is within 90 minutes of 
C*		    expiration and change the line type.
C
		    CALL TI_CTOI ( tmstp4, istrt, ier )
		    CALL TI_SUBM ( istrt, 90, istrt, ier )
		    CALL TI_ITOC ( istrt, temp4, ier )
		    CALL TI_DTM4 ( temp4, t90, ier )
		    IF ( t90 .le. dattm4 ) iltyp = 12
		  ELSE
		    iltyp  = 2
		    isltyp = 21
		END IF
C
C*		Set the color based on the type of watch if the color
C*		code is 0, or on the last digit of watch number if the
C*		color code is 1.
C
	  	CALL ST_RMBL ( wnum(ip), wnum(ip), lenw, ier )
		CALL ST_NUMB ( wnum(ip)(lenw:lenw), iwn, ier )
		idx = iwn + 4
		IF  ( wtype(ip) .eq. 'TN' )  THEN
		    IF ( iflags(JCLC) .eq. 0 ) idx = JTOR 
		    ic = icolor(idx)
		    CALL GSLINE ( iltyp, 0, iwidth(idx), 0, ier )
		  ELSE IF  ( wtype(ip) .eq. 'TS' )  THEN
		    IF ( iflags(JCLC) .eq. 0 ) idx = JSVR
                    ic = icolor(idx)
		    CALL GSLINE ( iltyp, 0, iwidth(idx), 0, ier )
		  ELSE
		    IF ( iflags(JCLC) .eq. 0 ) idx = JSTT
		    ic = icolor(idx)
		    CALL GSSPLN ( isltyp, 0, 1, ssize(idx),
     +				  iwidth(idx), ier )
		END IF
C
C*		Draw the box or status line.
C
		IF  ( ic .ne. 0 )  THEN
		    CALL GSCOLR ( ic, ier )
		    IF  ( wtype(ip) .eq. 'ST' )  THEN
			IF ( ( iflags(JMRS) .eq. 0 ) .or.
     +			     ( iflags(JMRS) .ne. 0 .and. plot ) )
     +			    CALL GSPLN ( 'M', npt(ip), rlat(1,ip),
     +			                 rlon(1,ip), ier )
		      ELSE
			CALL GLINE ( 'M', npt(ip), rlat(1,ip),
     +			             rlon(1,ip), ier )
		    END IF
C
C*		    Plot the text at the lower left corner of the box.
C
		    IF  ( wtype(ip) .ne. 'ST' )  THEN
			CALL GG_WLBL ( npt ( ip ), rlat ( 1,ip ),
     +				       rlon ( 1,ip ), alat, alon,
     +				       ier )
		    END IF
C
		    IF  ( ( iflags(JWTM) .eq. 0 ) .and. 
     +		          ( iflags(JWNM) .eq. 0 ) )   THEN
			wlabel = ' '
C
		      ELSE IF  ( ( iflags(JWTM) .eq. 0 ) .and. 
     +			         ( iflags(JWNM) .ne. 0 ) )   THEN
			iyoff  = -2
			wlabel = wnum(ip)
C
		      ELSE IF ( ( iflags(JWTM) .ne. 0 ) .and. 
     +			        ( iflags(JWNM) .eq. 0 ) )   THEN
			iyoff  = -2
		        wlabel = timstr(ip) (10:13) // '-' //
     +		                 timstp(ip) (10:13)
C
		      ELSE IF ( ( iflags(JWTM) .ne. 0 ) .and. 
     +			        ( iflags(JWNM) .ne. 0 ) )   THEN
			iyoff  = -3
			wlabel = wnum(ip) // CHCR //
     +				 timstr(ip) (10:13) // '-' //
     +				 timstp(ip) (10:13)
		    END IF
	
C
C*		    Plot the text label for status line.
C	
                    IF  ( ( iflags(JSTM) .eq. 0 ) .and.
     +                    ( iflags(JSNM) .eq. 0 ) )   THEN
                        slabel = ' '
C
                      ELSE IF  ( ( iflags(JSTM) .eq. 0 ) .and.
     +                           ( iflags(JSNM) .ne. 0 ) )   THEN
                        isoff  = -2
                        slabel = wnum(ip)
C
                      ELSE IF ( ( iflags(JSTM) .ne. 0 ) .and.
     +                          ( iflags(JSNM) .eq. 0 ) )   THEN
                        isoff  = -2
                        slabel = timstr(ip) (10:13)
C
                      ELSE IF ( ( iflags(JSTM) .ne. 0 ) .and.
     +                          ( iflags(JSNM) .ne. 0 ) )   THEN
                        isoff  = -3
                        slabel = wnum(ip) // CHCR // timstr(ip) (10:13)
                    END IF

		    IF  ( wtype(ip) .eq. 'ST' )  THEN
			IF  ( npt(ip) .gt. 0 )  THEN
			    IF ( ( iflags(JMRS) .eq. 0 ) .or.
     +				 ( iflags(JMRS) .ne. 0 .and. plot ) ) 
     +			        CALL GTEXT ( 'M', rlat ( 1,ip ),
     +					     rlon ( 1,ip ), slabel,
     +					     0.0, 0, isoff, ier )
			END IF
		      ELSE
			CALL GTEXT ( 'M', alat, alon, wlabel,
     +			             0.0, 0, iyoff, ier )
		    END IF
		END IF
  	    END IF
	END DO
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSLINE ( jtype, jtyhw, jwidth, jwdhw, ier )
	CALL GSSPLN ( jsltyp, jslstr, jsldir, sizsp, jslwid, ier )
	CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
C*
	RETURN
	END
