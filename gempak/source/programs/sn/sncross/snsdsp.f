	SUBROUTINE SNSDSP  ( timflg, nstn, stns, topwnd, toptmp, parm,
     +			     vcord, clear, isnflg, nlvl, clvl, icolor,
     +			     iline, ilwid, ilabel, nflvl, flvl, ifcolr,
     +			     iflabl, ifltyp, time, statn, iret )
C************************************************************************
C* SNSDSP								*
C*									*
C* This subroutine gives the user an option to exit SNCROSS.		*
C*									*
C* SNSDSP  ( TIMFLG, NSTN, STNS, TOPWND, TOPTMP, PARM, VCORD, CLEAR,	*
C*           ISNFLG, NLVL, CLVL, ICOLOR, ILINE, ILWID, ILABEL, NFLVL, 	*
C*	     FLVL, IFCOLR, IFLABL, IFLTYP, TIME, STATN, IRET )		*
C*									*
C* Input parameters:							*
C*	TIMFLG		LOGICAL		Time section flag		*
C*	NSTN		INTEGER		Number of stations		*
C*	STNS (NSTN)	CHAR*		List of stations / times	*
C*	TOPWND (NSTN)	REAL		Top of wind reports		*
C*	TOPTMP (NSTN)	REAL		Top of temperature reports	*
C*	PARM		CHAR*		Parameter to be displayed	*
C*	VCORD		CHAR*		Vertical coordinate		*
C*	CLEAR		LOGICAL		Clear screen flag		*
C*	ISNFLG		LOGICAL		Isentrope flag			*
C*	NLVL		INTEGER		Number of contour levels	*
C*	CLVL   (NLVL)	REAL		Contour levels			*
C*	ICOLOR (NLVL)	INTEGER		Contour colors			*
C*	ILINE  (NLVL)	INTEGER		Contour line types		*
C*	ILWID  (NLVL)	INTEGER		Contour line widths		*
C*	ILABEL (NLVL)	INTEGER		Contour label values		*
C*	NFLVL		INTEGER		Number of fill levels		*
C*	FLVL   (NFLVL)	REAL		Fill levels			*
C*	IFCOLR (NFLVL+1)INTEGER		Fill colors			*
C*	IFLABL (NFLVL+1)INTEGER		Fill label values		*
C*	IFLTYP (NFLVL+1)INTEGER		Fill types			*
C*	TIME(*)		CHAR*		Time				*
C*	STATN		CHAR*		Station				*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = exit from program		*
C** Log:								*
C* I. Graffman/RDS	11/86						*
C* S. Schotz/GSC	 5/90	Get respnd locally from IP_RESP		*
C* M. desJardins/GSFC	 9/90	GEMPAK 5				*
C* J. Whistler/SSAI	 4/91	Added declaration of logical isnflg	*
C* K. Brill/NMC		01/92	Added color fill output			*
C* K. Brill/NMC		04/92	Display times for time section		*
C* T. Lee/SAIC		11/01	Displayed contour fill types		*
C************************************************************************
	CHARACTER* (*)	stns (*), parm, vcord, time (*), statn
	REAL		topwnd (*), toptmp (*), clvl (*), flvl (*)
	LOGICAL		clear, timflg
	INTEGER		iline (*), ilwid (*), ilabel (*), icolor (*)
	INTEGER		ifcolr (*), iflabl (*), ifltyp (*)
C*
	CHARACTER	clr*4
	LOGICAL		respnd, isnflg
C------------------------------------------------------------------------
C*	Write out the stations and the top of the reports.
C
	WRITE  ( 6, 1000 )
1000	FORMAT ( /'     SNCROSS PARAMETERS'/ )
	IF  ( timflg )  THEN
	    WRITE  ( 6, 1001 )
1001	    FORMAT ( '  TIMES ', 7X, 'TOP LVL FOR TEMP', 2X,
     +               'TOP LVL FOR WIND' )
	  ELSE
	    WRITE  ( 6, 1002 )
1002	    FORMAT ( ' STATION', 7X, 'TOP LVL FOR TEMP', 2X,
     +               'TOP LVL FOR WIND' )
	END IF
	DO  i = 1, nstn
	    itemp = toptmp (i)
	    iwind = topwnd (i)
	    IF ( timflg ) THEN
	      WRITE  ( 6, 1010 ) time (i) (1:14),itemp, iwind
	    ELSE
	      WRITE  ( 6, 1010 ) stns (i) (1:14),itemp, iwind
	    END IF
1010	    FORMAT ( 1X, A, 4X, I8, 10X, I8 )
	END DO
C*
	IF  ( clear )  THEN
 	    clr = 'YES'
	ELSE
	    clr = 'NO'
	END IF
	WRITE  ( 6, 1020 ) parm, vcord, clr
1020	FORMAT ( ' PARM:      ', A /
     +           ' VCOORD:    ', A /
     +           ' CLEAR:     ', A )
	IF  ( timflg )  THEN
	    WRITE  ( 6, 1030 )  statn
1030	    FORMAT ( ' STATION:   ', A )
	  ELSE
	    WRITE  ( 6, 1031 )  time (1)
1031	    FORMAT ( ' TIME:      ', A )
	END IF
C
C*	Write out contour levels in groups of 7.
C
	IF  ( .not. isnflg .and. nlvl .gt. 0 .and. 
     +	      parm .ne. ' ' )  THEN
	    nblk = ( nlvl - 1 ) / 7 + 1
	    i1 = 1
	    i2 = 7
	    DO i = 1, nblk
		IF  ( i2 .gt. nlvl )  i2 = nlvl
		WRITE ( 6, 2010 ) ( clvl   (j), j = i1, i2 )
		WRITE ( 6, 2011 ) ( icolor (j), j = i1, i2 )
		WRITE ( 6, 2012 ) ( iline  (j), j = i1, i2 )
		WRITE ( 6, 2013 ) ( ilwid  (j), j = i1, i2 )
		WRITE ( 6, 2014 ) ( ilabel (j), j = i1, i2 )
2010		FORMAT ( /' LEVELS:    ',7F9.2 )
2011		FORMAT (  ' COLORS:    ',7I9 )
2012		FORMAT (  ' LINTYP:    ',7I9 )
2013		FORMAT (  ' LINWID:    ',7I9 )
2014		FORMAT (  ' LABEL:     ',7I9 )
		i1 = i1 + 7
		i2 = i2 + 7
	    END DO
	END IF
C
C*      Write out contour fill information.
C
        IF  ( .not. isnflg .and. nflvl .gt. 0 .and.
     +	      parm .ne. ' ' )  THEN
          WRITE ( 6, 2015 ) 
2015      FORMAT ( /, ' FILLED CONTOURS: ' )
          nblk = ( nflvl - 1 ) / 7 + 1
          i1 = 1
          i2 = 7
          DO  i = 1, nblk
            i3 = i2
            IF  ( i2 .ge. nflvl )  THEN
                i2 = nflvl
                i3 = nflvl + 1
            END IF
            WRITE (6, 2010 ) ( flvl  (j), j = i1, i2 )
            WRITE (6, 3011 ) ( ifcolr (j), j = i1, i3 )
3011        FORMAT ( ' COLORS:', 8 ( I7, 2X ) )
            WRITE (6, 3012 ) ( ifltyp (j), j = i1, i3 )
3012        FORMAT ( '  TYPES:', 8 ( I7, 2X ) )
            i1 = i1 + 7
            i2 = i2 + 7
          END DO
        END IF
C
C*	Prompt for response.
C
	CALL IP_RESP  ( respnd, ier )
	IF  ( respnd )  THEN
	    CALL TM_ACCP  ( ier )
	    IF  ( ier .eq. 2 )  THEN
		iret = -1
	      ELSE
		iret = 0
	    END IF
	END IF
C*
	RETURN
	END
