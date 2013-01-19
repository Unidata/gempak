	SUBROUTINE RS_DECO  ( ob, endob, obtype, day, hour, wndcod,
     +			      defalt, name, data, iret )
C************************************************************************
C* RS_DECO								*
C*									*
C*  This subroutine decodes a report in WMO international code form 	*
C*  FM 12-VIII Ext. SYNOP or FM 13-VIII Ext. SHIP, following the Manual	*
C*  on Codes, WMO no. 306, vol. 1, 1984, with supplements through	*
C*  supplement 3, 1987.  The expected form of the message is as follows,*
C*  with subscripts within parentheses and asterisks indicating groups 	*
C*  which are not decoded.  Up arrows indicate groups which must be 	*
C*  present for ship and buoy obs and must not be present for syoptic   *
C*  and cman obs.							*
C*									*
C*   D.....D or A(1)b(w)n(b)n(b)n(b) or IIiii   YYGGi(w)  99L(a)L(a)L(a)*
C*									*
C*   Q(c)L(o)L(o)L(o)L(o)  i(R)i(x)hVV   Nddff   00fff  1s(n)TTT	*
C*   ^^^^^^^^^^^^^^^^^^^^						*
C*   2s(n)T(d)T(d)T(d) or 29UUU   3P(o)P(o)P(o)P(o)   4PPPP or 4a(3)hhh	*
C*                                                             ********	*
C*   5appp   6RRRt(R)   7wwW(1)W(2) or 7w(a)w(a)//   8N(h)C(L)C(M)C(H)	*
C*									*
C*   9hh//   222D(s)v(s)   0s(n)T(w)T(w)T(w)   1P(wa)P(wa)H(wa)H(wa)	*
C*   or 9hrmn (CMAN)							*
C*   2P(w)P(w)H(w)H(w)   3d(w1)d(w1)d(w2)d(w2)   4P(w1)P(w1)H(w1)H(w1)	*
C*									*
C*   5P(w2)P(w2)H(w2)H(w2)   6I(s)E(s)E(s)R(s)   70H(wa)H(wa)H(wa)	*
C*									*
C*   333   0C(s)D(L)D(M)D(H)   1s(n)T(x)T(x)T(x)   2s(n)T(n)T(n)T(n)	*
C*									*
C*   3Ejjj              4E'sss   5j(1)j(2)j(3)j(4) j(5)j(6)j(7)j(8)j(9)	*
C*     *** (E encoded as 10+E to distinguish from E')			*
C*   6RRRt(R)   7R(24)R(24)R(24)R(24)   8N(s)Ch(s)h(s)   8N(s)Ch(s)h(s)	*
C*									*
C*   8N(s)Ch(s)h(s)   8N(s)Ch(s)h(s)    901nn   902nn   903nn   904nn	*
C*									*
C*   905R(t)d(c)   921ff   9S(P)S(P)s(p)s(p)  444  N'C'H'H'C(t)		*
C*                         *****************				*
C*   For buoy and cman stations:					*
C*   555   11fff   22fff   3nnnn   4nnnn   9GGgg			*
C*                         *****   *****				*
C*   For synoptic stations:						*
C*   555   0i(t)t(d)t(d)t(d)  						*
C*									*
C*   1T(c)T(c)T(c)  T(cx)T(cx)T(cx)T(cn)T(cn)T(cn)			*
C*       								*
C*   Special regional codes used by the United States and surrounding 	*
C*   regions are not decoded (except for the city temperatures and 	*
C*   max/min, which are encoded in the format shown in the last line 	*
C*   above.  A broader and more general decoding capability could 	*
C*   easily be written into the program.  Observations commonly found	*
C*   in this code form include land-based synoptic observations, ship 	*
C*   observations, buoy observations, and c-man offshore platform 	*
C*   observations.							*
C*									*
C*  RS_DECO  (  OB, ENDOB, OBTYPE, DAY, HOUR, WNDCOD, DEFALT, NAME,	*
C*		DATA, IRET )						*
C*									*
C* Input parameters:							*
C*	OB		CHAR*		Raw report			*
C*	ENDOB		INTEGER		Number of characters in report	*
C*	OBTYPE		CHAR*4		Type of (from RS_DLN2)		*
C*	DAY		INTEGER		Day (from RS_DHDR or RS_DLN2)	*
C*	HOUR		INTEGER		Hour (from RS_DHDR or RS_DLN2)	*
C*	WNDCOD		INTEGER		Wind units code (from RS_DLN2)	*
C*	DEFALT		INTEGER		Missing data value		*
C*									*
C* Output parameters:							*
C*	NAME		CHAR*7		Name of station or ship		*
C*	DATA (*)	INTEGER		Decoded data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = Normal completion		*
C*					 -5 = Did not find T, P, or SST	*
C**									*
C* Log:									*
C* J. Nielsen/MIT	10/86						*
C* J. Nielsen/MIT	 2/89						*
C* F. Cuq/UCLA		 8/90	Integer*2 -> integer*4			*
C* J. Nielsen/MIT	 2/92	Gempacized				*
C* S. Jacobs/NMC	 9/95	Fixed calling sequence to a RS_HHDE call*
C************************************************************************
	CHARACTER	ob*(*), obtype*4
	INTEGER		endob, day, hour, wndcod, defalt
C*
	CHARACTER	name*(*)
	INTEGER		data(*), iret
C*
	INTEGER		temp, loc, i, shift
	LOGICAL		wmps
	LOGICAL		RS_NMER
C------------------------------------------------------------------------
c  Goto conventions:
c   9999: Exit immediately
c   999:  Exit after checking for 333, 444, or 555 groups
C------------------------------------------------------------------------
	iret = -5
	loc = 1
	name = 'XXXXXXX'
	DO  i = 1, 200
	    data(i) = defalt
	END DO
C
C************************************************************************
C									*
C*	Decode start of report.						*
C*	If an SM report, read the five-digit station identifier and use	*
C*	the input values of day, hour, and wndcod.  If a US, Canadian, 	*
C*	or Caribbean station, assume wind is in knots; otherwise, m/s.  *
C*	If a CMAN report, read the five-character station identifier    *
C*	and use the input values of day, hour, and wndcod.  If a ship   *
C*	or buoy report, read the unknown length character station 	*
C*	identifier and decode the latitude, longitude, day, hour, and	*
C*	wndcod.  If an unknown report type, assume an SM.		*
C									*
C************************************************************************
C
C*	First, try to pin a label on unknown observation types
C
	IF  ( obtype .eq. 'XXXX' )  THEN
C
C*	    Look for 99 (latitude group) 
C
	    i = 10
	    DO WHILE  ( ob(loc+i:loc+i) .ne. ' ' )
		i = i + 1
		IF  ( i .gt. 14 )  GOTO 9999
	    END DO
	    DO WHILE  ( ob(loc+i:loc+i) .eq. ' ' )
		i = i + 1
		IF  ( i .ge. endob )  GOTO 9999
	    END DO
	    IF  ( ob(loc+i:loc+i+1) .eq. '99' )  THEN
		obtype = 'SHBY'
	    ELSE
		obtype = 'SMNA'
	    ENDIF
	ENDIF
C________________________________________________________________________
C
C*	Decode start of synoptic obs (assume unknown reports are synops)
C
10	IF  ( ( obtype .eq. 'SMUS' ) .or.
     +	      ( obtype .eq. 'SMNA' ) .or.
     +	      ( obtype .eq. 'XXXX' ) )  THEN
C
C*	    Decode five-digit station identifier
C
	    IF  ( RS_NMER ( ob (loc:), 5 ) )  THEN
		CALL RS_NUMD ( ob (loc:), 2, defalt, data (1), ier1 )
		CALL RS_NUMD ( ob (loc+2:), 3, defalt, data (2), ier2 )
	    ENDIF
	    IF  ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) )  GOTO 9999
	    name = ob(loc:loc+4) // '  '
	    loc = loc + 5
C
C*	    Special case: foolish Alaskans
C
	    IF  ( ob(loc+1:loc+2) .eq. 'SM' )  THEN
C
C*		Skip past SM, see what comes next
C
		CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
		IF  ( ier .ne. 0 )  GOTO 9999
		CALL RS_NUMD ( ob (loc:), 2, defalt, temp, ier )
		IF  ( temp .eq. data(1) )  THEN
C
C*		    Same WMO group - see if station id matches
C
		    CALL RS_NUMD ( ob (loc+2:), 3, defalt, temp, ier )
		    IF  ( temp .ne. data(2) )  GOTO 10
C
C*		    It matches; skip over numbers
C
		    loc = loc + 5
		ELSE
C
C*		    It's not a station identifier; hope it's start of 
C*		    the data
C
		    loc = loc + 1
		ENDIF
	    ENDIF
C
C*	    Determine most likely wind units (knots or m/s)
C
	    wmps = .true.
	    IF  ( ( wndcod .gt. 1 ) .and. ( wndcod .ne. defalt ) )
     +		wmps = .false.
	    IF  ( ( ( data (1) .eq. 70 ) .or. 
     +		    ( data (1) .eq. 71 ) .or.
     +		    ( data (1) .eq. 72 ) .or.
     +		    ( data (1) .eq. 74 ) .or.
     +		    ( data (1) .eq. 78 ) ) .and.
     +		  ( wndcod .ne. 1 ) )  
     +		wmps = .false.
C
C*	    Get day and hour from input parameters
C
	    data (3) = day
	    data (4) = hour
C________________________________________________________________________
C
C*	Decode CMAN observations
C
	ELSE IF  ( obtype .eq. 'CMAN' )  THEN
	    name = ob ( loc:loc+4 ) // '  '
	    loc = loc + 5
	    data (3) = day
	    data (4) = hour
	    IF  ( wndcod .ne. 1 )  wmps = .false.
C________________________________________________________________________
C
C*	Decode ship and buoy observations
C
	ELSE
C
C*	    'BBXX' might precede any buoy ob
C
	    IF  ( ob ( loc:loc+3 ) .eq. 'BBXX' )  THEN
		loc = loc + 4
	    	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	    	IF  ( ier .ne. 0 )  GOTO 9999
	    END IF
C
C*	    Try skipping up to two spaces
C
	    IF  ( ob (loc:loc) .eq. ' ' )  loc = loc + 1
	    IF  ( ob (loc:loc) .eq. ' ' )  loc = loc + 1
C
C*	    Look for ship or buoy name
C
	    IF  ( ob (loc+4:loc+4) .eq. ' ' )  THEN
		name = ob (loc:loc+3) // '   '
		loc = loc + 4
	    ELSE IF  ( ob (loc+5:loc+5) .eq. ' ' )  THEN
		name = ob (loc:loc+4) // '  '
	        IF  ( RS_NMER ( ob (loc:), 5 ) ) THEN
C A(1)b(w)
		  CALL RS_NUMD ( ob (loc: ), 2, defalt, data(1), ier )
C n(b)n(b)n(b)
		  CALL RS_NUMD ( ob (loc+2: ), 3, defalt, data(2), ier )
		ENDIF
		loc = loc + 5
	    ELSE IF  ( ob (loc+6:loc+6) .eq. ' ' )  THEN
		name = ob (loc:loc+5) // ' '
	 	loc = loc + 6
	    ELSE IF  ( ( ob (loc+7:loc+7) .eq. ' ' ) .or.
     +		       ( ob (loc+8:loc+8) .eq. ' ' ) )  THEN
		name = ob (loc:loc+6)
		loc = loc + 8
C
C*	    Allow possibility that space is missing after ship name
C
	    ELSE IF  ( ob (loc+9:loc+9) .eq. ' ' )  THEN
		name = ob (loc:loc+3) // '   '
		loc = loc + 4
	    ELSE IF  ( ob (loc+10:loc+10) .eq. ' ' )  THEN
		name = ob (loc:loc+4) // '  '
		IF  ( RS_NMER ( ob (loc:), 5 ) ) THEN
C A(1)b(w)
		  CALL RS_NUMD ( ob (loc: ), 2, defalt, data(1), ier )
C n(b)n(b)n(b)
		  CALL RS_NUMD ( ob (loc+2: ), 3, defalt, data(2), ier )
		END IF
		loc = loc + 5
	    ELSE IF  ( ob (loc+11:loc+11) .eq. ' ')  THEN
		name = ob (loc:loc+5) // ' '
		loc = loc + 6
	    ELSE
		goto 9999
	    ENDIF
C________________________________________________________________________
C
C*	    Next group: day, hour, wind indicator
C
	    CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	    IF  ( ier .ne. 0 )  GOTO 999
C YY
	    CALL RS_NUMD ( ob ( loc: ), 2, defalt, data (3), ier )
C GG
	    CALL RS_NUMD ( ob ( loc+2: ), 2, defalt, data(4), ier )
C
C*	    Consistency check on day and hour
C
	    IF  ( ( data(3) .gt. 31 ) .or.
     +		  ( ( day - data(3) .gt. 3 ) .and.
     +		    	( day .gt. 3 ) ) .or.
     +		  ( data(3) - day .gt. 25 ) .or.
     +		  ( data(4) .gt. 24 ) )  THEN
		data(3) = defalt
		data(4) = defalt
		GOTO 9999
	    ENDIF
C i(w)
	    CALL RS_NUMD ( ob (loc+4:), 1, defalt, temp, ier )
	    IF  ( temp .le. 1 )  THEN
		wmps = .true.
	    ELSE
		wmps = .false.
	    ENDIF
	    loc = loc + 5
C________________________________________________________________________
C
C*	    Next groups: latitude and longitude
C	
	    CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	    IF  ( ier .ne. 0 )  GOTO 9999
	    IF  ( ob (loc:loc+1) .ne. '99' )  GOTO 9999
C L(a)L(a)L(a)
	    CALL RS_NUMD ( ob (loc+2:), 3, defalt, data(5), ier )
	    IF  ( ier .ne. 0 )  GOTO 9999
	    loc = loc + 5
	    CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	    IF  ( ier .ne. 0 )  GOTO 9999
C L(o)L(o)L(o)L(o)
	    CALL RS_NUMD ( ob (loc+1:), 4, defalt, data(6), ier )
	    IF  ( ier .ne. 0 )  GOTO 9999
C Q(c)
	    CALL RS_NUMD ( ob (loc:), 1, defalt, temp, ier )
C
C*	    Determine quadrant, adjust lat and lon accordingly
C
	    IF  ( temp .ne. 1 )  THEN
		IF  ( temp .eq. 3 )  THEN
		    data(5) = - data(5)
		ELSE IF ( temp .eq. 5 )  THEN
		    data(5) = - data(5)
		    data(6) = - data(6)
		ELSE
		    data(6) = - data(6)
		ENDIF
	    ENDIF
	    loc = loc + 5
	ENDIF
C************************************************************************
C*									*
C* MAIN SECTION OF DECODER						*
C*									*
C* Organization:  Each group is given a two-digit code 'Sn', where S is *
C* the section number and n is the group code number.  (Exception: the	*
C* visibility and wind groups are together given code 10.)  Within each	*
C* group decoder, there are typically three GOTO's:			*
C*	Sn0:	Decode group						*
C*	Sn8:	Search for beginning of next group			*
C*	Sn9:	Check first digit of next group.  Pass it along to	*
C*		  S(n+1)0 or S(n+1)9, as appropriate.			*
C* 									*
C* Sections 1 and 2 are decoded contiguously.  Control is passed to	*
C* line 999 at the end of sections 2, 3, and 4, whereupon the three-	*
C* digit section code is decoded and control is passed to the start of	*
C* section line S000, where S = 3, 4, or 5.				*
C									*
C************************************************************************
C
C* SECTION 1
C
C* Visibility group i(R)i(x)hVV
C
100	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  THEN
C
C*	    Another special case: check for NIL [333].  If found, first 
C*	    report was missing, so try to decode 2nd report.
C
	    IF  ( ob(loc:loc+2) .eq. 'NIL' )  THEN
		loc = loc + 4
		CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	    ENDIF
	    IF  ( ( ob(loc:loc+2) .eq. '333' ) .and. 
     +		  ( ier .ne. 0 ) ) THEN
		loc = loc + 3
		CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	    ENDIF
	    CALL RS_NUMD ( ob(loc:), 2, defalt, temp, ier )
	    IF  ( temp .eq. data(1) )  THEN
C
C*		Just as we suspected: a stupid table of obs
C
		GOTO 10
	    ENDIF
	    GOTO 9999
	ENDIF
	IF  ( shift .ge. 4 )  THEN
C
C*	    Check for similar special case
C
	    CALL RS_NUMD ( ob(loc:), 2, defalt, temp, ier )
	    IF  ( temp .eq. data(1) )  THEN
		GOTO 10
	    ENDIF
C
C*	    Panic if we had to scan too far
C
	    GOTO 9999
	ENDIF
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
C
C*	If no rain observed, assume six hour dry period
C
	IF  ( temp .eq. 3 )  THEN
	    data(7) = 0
	    data(8) = 0
	ENDIF
C
	CALL RS_NUMD ( ob(loc+1:), 1, defalt, temp, ier )
	IF  ( ( temp .eq. 2 ) .or. ( temp .eq. 5 ) )  THEN
C
C*	    No significant current or past weather
C
	    data(9) = 0
	    data(10) = 0
	    data(11) = 0
	END IF
C
C*	Ceiling code
C
	CALL RS_NUMD ( ob(loc+2:), 1, defalt, temp, ier )
	IF  ( ier .eq. 0 )  CALL RS_HHDE ( temp, 1, data(12), ier )
C
C*	Visibility code
C
	CALL RS_NUMD ( ob(loc+3:), 2, defalt, temp, ier )
	IF  ( ier .eq. 0 )  CALL RS_VVDE ( temp, data(13), ier )
	loc = loc + 5
C________________________________________________________________________
C
C*	Wind group  Nddff
C
	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
C
C*	Panic if we had to go too far
C
	IF  ( shift .ge. 3 ) GOTO 9999
C
C*	Sky cover code
C
	CALL RS_NUMD ( ob(loc:), 1, defalt, data(14), ier )
C
C*	Wind direction
C
	CALL RS_NUMD ( ob(loc+1:), 2, defalt, temp, ier )
	IF  ( ier .eq. 0 )  data(15) = 10 * temp
	IF  ( temp .eq. 99 )  data(15) = defalt
C
C*	Wind speed (m/s; encode knots by adding 500)
C
	CALL RS_NUMD ( ob(loc+3:), 2, defalt, data(16), ier )
	IF  ( ier .eq. 0 ) THEN
	    IF  ( .not. wmps )  data(16) = data(16) + 500
	    IF  ( ( data(15) .gt. 360 ) .and. 
     +		  ( data(15) .ne. defalt ) )  THEN
C
C*	    	Bad wind direction; better zap everything so far
C
		DO  i = 7, 16
		    data(i) = defalt
		END DO
	    ENDIF
	ENDIF
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
108	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 108
	ENDIF
109	IF ( temp .eq. 0 )  THEN
	    IF  ( ( data(16) .eq. 99 ) .or. ( data(16) .eq. 599 ) )
     +          GOTO 1010
	    loc = loc + 5
	    GOTO 108
	ENDIF	
	GOTO 1019
C________________________________________________________________________
C
C*	Excessive wind group  00fff
C
1010	IF  ( ob ( loc:loc+1 ) .ne. '00' )  GOTO 9999
	CALL RS_NUMD ( ob(loc+2:), 3, defalt, data(16), ier )
	IF  ( ier .ne. 0 )  THEN
	    IF  ( .not. wmps )  data(16) = data(16) + 500
	ENDIF
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
1018	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 1018
	ENDIF
1019	IF ( temp .eq. 1 )  GOTO 110
	GOTO 119
C________________________________________________________________________
C
C*	Temperature group  1s(n)TTT
C*	Decode in 10ths of degrees Celcius
C
C*	First, assert that decoding is successful
C
110	iret = 0
	CALL RS_NUMD ( ob(loc+2:), 3, defalt, data(17), ier )
	IF  ( ( ier .eq. 0 ) .and. ( ob(loc+1:loc+1) .eq. '1' ) ) 
     +	    data(17) = - data(17)
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
118	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 118
	ENDIF
119	IF ( temp .eq. 2 )  GOTO 120
	GOTO 129
C________________________________________________________________________
C
C*	Dew point group  2s(n)T(d)T(d)T(d) or 29UUU
C*	Almost same code as previous group
C
120	CALL RS_NUMD ( ob(loc+2:), 3, defalt, data(18), ier )
	IF  ( ier .eq. 0 )  THEN
C
	    IF  ( ob(loc+1:loc+1) .eq. '1' )  THEN
	      data(18) = - data(18)
	    ELSE IF  ( ob(loc+1:loc+1) .eq. '9' )  THEN
C
	      IF ( ( data(18) .le. 100 ) .and.
     +		   ( data(17) .ne. defalt ) )  THEN
C
C*		Relative humidity reported; estimate dew point
C
		data(18) = data(17) +
     +		  NINT ( 13.78 * ALOG ( FLOAT ( data(18) ) / 100. ) )
	      ELSE
		data(18) = defalt
	      ENDIF
C
	    ENDIF
	ENDIF
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
128	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 128
	ENDIF
129	IF ( temp .eq. 3 )  GOTO 130
	GOTO 139
C________________________________________________________________________
C	
C*	Station pressure group  3P(o)P(o)P(o)P(o)
C*	Decode in 10ths of millibars
C
130	CALL RS_NUMD ( ob(loc+1:), 4, defalt, data(19), ier )
	IF  ( ( ier .eq. 0 ) .and. ( data(19) .le. 1500 ) )  
     +	  data(19) = data(19) + 10000
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
138	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 138
	ENDIF
139	IF ( temp .eq. 4 )  GOTO 140
	GOTO 149
C________________________________________________________________________
C
C*	Sea level pressure group  4PPPP or 4a(3)hhh
C*	Only decode 4PPPP form, in 10ths of millibars
C*	Set iret to 0 to confirm success
C
140	IF  ( ( ob(loc+1:loc+1) .eq. '0' ) .or.
     +	      ( ob(loc+1:loc+1) .eq. '9' ) )  THEN
	    CALL RS_NUMD ( ob(loc+1:), 4, defalt, data(20), ier )
	    IF  ( ( ier .eq. 0 ) .and. ( data(20) .lt. 1000 ) )  
     +	      data(20) = data(20) + 10000
	ENDIF
	iret = 0
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
148	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 148
	ENDIF
149	IF ( temp .eq. 5 )  GOTO 150
	GOTO 159
C________________________________________________________________________
C
C*	Pressure tendency group  5appp
C*	Decode in 10ths of millibars, with proper sign
C
150	CALL RS_NUMD ( ob(loc+1:), 1, defalt, data(21), ier )
	CALL RS_NUMD ( ob(loc+2:), 3, defalt, data(22), ier )
	IF  ( data(21) .ge. 5 ) data(22) = - data(22)
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
158	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 158
	ENDIF
159	IF ( temp .eq. 6 )  GOTO 160
	GOTO 169
C________________________________________________________________________
C
C*	First rainfall group 6RRRt(R)
C* 	Decode rainfall in 10ths of mm, period of rainfall in hours
C
160	CALL RS_NUMD ( ob(loc+1:), 3, defalt, temp, ier )
	IF  ( ier .eq. 0 )  CALL RS_RRRD ( temp, data(7), ier )
	CALL RS_NUMD ( ob(loc+3:), 1, defalt, temp, ier )
	IF  ( ier .eq. 0 )  data(8) = 6 * temp
C
C*	If period was 24 hours, load element 26 too
C
	IF  ( temp .eq. 4 ) data(26) = data(7)
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
168	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 168
	ENDIF
169	IF ( temp .eq. 7 )  GOTO 170
	GOTO 179
C________________________________________________________________________
C
C*	Weather group  7wwW(1)W(2) or 7w(a)w(a)//
C
C*	Present weather
C
170	CALL RS_NUMD ( ob(loc+1:), 2, defalt, data(9), ier )
C
C*	Past weathers
C
	CALL RS_NUMD ( ob(loc+3:), 1, defalt, data(10), ier )
	CALL RS_NUMD ( ob(loc+4:), 1, defalt, data(11), ier )
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
178	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 178
	ENDIF
179	IF ( temp .eq. 8 )  GOTO 180
	GOTO 189
C________________________________________________________________________
C 
C*	Cloud type group  8N(h)C(L)C(M)C(H)
C*	Cloud amt code, and low, middle, and high cloud types
C
180	CALL RS_NUMD ( ob(loc+1:), 1, defalt, data(51), ier )
	CALL RS_NUMD ( ob(loc+2:), 1, defalt, data(52), ier )
	CALL RS_NUMD ( ob(loc+3:), 1, defalt, data(53), ier )
	CALL RS_NUMD ( ob(loc+4:), 1, defalt, data(54), ier )
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
188	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 188
	ENDIF
189	IF ( temp .eq. 9 )  GOTO 190
	GOTO 199
C________________________________________________________________________
C	
C*	Precise cloud height group 9hh//
C
190	IF  ( obtype .eq. 'CMAN' )  GOTO 191
	CALL RS_NUMD ( ob(loc+1:), 2, defalt, temp, ier )
	IF  ( ier .eq. 0 )  CALL RS_HHDE ( temp, 2, data(12), ier )
	loc = loc + 5
	GOTO 198
C________________________________________________________________________
C
C*	Cman hour/minute of ob  9hhmm
C
191	CALL RS_NUMD ( ob(loc+1:), 4, defalt, data(91), ier )
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
198	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 198
	ENDIF
199	IF  ( ob(loc:loc+2) .eq. '222' )  THEN
	    GOTO 2000
	ELSE
C
C*	    Unknown group, give up
C
	    GOTO 9999
	ENDIF
C************************************************************************
C
C*	Section 2 (got here from line 199)
C
C************************************************************************
C
C*	Motion of ship group  222D(s)v(s)
C
C*	Direction of motion (degrees)
C
2000	CALL RS_NUMD ( ob(loc+3:), 1, defalt, temp, ier )
	IF  ( ( temp .ge. 0 ) .and. ( temp .le. 8 ) ) 
     +	  data(29) = 45 * temp
C
C*	Speed of motion (m/s)
C
	CALL RS_NUMD ( ob(loc+4:), 1, defalt, temp, ier )
	IF  ( ier .eq. 0 )  data(30) = ( temp * 5 ) / 2
	CALL RS_NUMD ( ob(loc+4:), 1, defalt, data(54), ier )
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
2008	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 2008
	ENDIF
2009	IF ( temp .eq. 0 )  GOTO 200
	GOTO 209
C________________________________________________________________________
C
C*	Water temperature group  0s(n)T(w)T(w)T(w)
C*	Decode in 10ths of degrees Celsius
C*	Set iret = 0, showing faith in our decoding
C
200	CALL RS_NUMD ( ob(loc+2:), 3, defalt, data(31), ier )
	CALL RS_NUMD ( ob(loc+1:), 1, defalt, temp, ier )
	IF  ( temp .eq. 1 )  data(31) = - data(31)
	iret = 0
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
208	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 208
	ENDIF
209	IF ( temp .eq. 1 )  GOTO 210
	GOTO 219
C________________________________________________________________________
C
C*	Wind wave height group  1P(wa)P(wa)H(wa)H(wa)
C*	Automated wind wave height group  2P(w)P(w)H(w)H(w)
C
C*	Period in seconds
C
210	CALL RS_NUMD ( ob(loc+1:), 2, defalt, data(32), ier )
C
C*	Height in 10ths of m
C
	CALL RS_NUMD ( ob(loc+3:), 2, defalt, temp, ier )
	IF  ( ier .eq. 0 )  data(33) = temp * 5
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
218	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 218
	ENDIF
219	IF  ( temp .eq. 2 )  GOTO 210
	IF  ( temp .eq. 3 )  GOTO 230
	GOTO 239
C________________________________________________________________________
C
C*	Swell direction group  3d(w1)d(w1)d(w2)d(w2)
C*	Decode in degrees
C
230	CALL RS_NUMD ( ob(loc+1:), 2, defalt, temp, ier )
	IF  ( ier .eq. 0 )  data(34) = temp * 10
	CALL RS_NUMD ( ob(loc+3:), 2, defalt, temp, ier )
	IF  ( ier .eq. 0 )  data(37) = temp * 10
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
238	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 238
	ENDIF
239	IF  ( temp .eq. 4 )  GOTO 240
	GOTO 249
C________________________________________________________________________
C
C*	First swell group  4P(w1)P(w1)H(w1)H(w1)
C
C*	Period in seconds
C
240	CALL RS_NUMD ( ob(loc+1:), 2, defalt, data(35), ier )
C
C*	Height in 10ths of meters
C
	CALL RS_NUMD ( ob(loc+3:), 2, defalt, temp, ier )
	IF  ( ier .eq. 0 )  data(36) = temp * 5
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
248	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 248
	ENDIF
249	IF  ( temp .eq. 5 )  GOTO 250
	GOTO 259
C________________________________________________________________________
C
C*	Second swell group  5P(w2)P(w2)H(w2)H(w2)
C*	Decode like first swell group
C
250	CALL RS_NUMD ( ob(loc+1:), 2, defalt, data(38), ier )
	CALL RS_NUMD ( ob(loc+3:), 2, defalt, temp, ier )
	IF  ( ier .eq. 0 )  data(39) = temp * 5
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
258	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 258
	ENDIF
259	IF  ( temp .eq. 6 )  GOTO 260
	GOTO 269
C________________________________________________________________________
C
C*	Ice group  6I(s)E(s)E(s)R(s)
C
C*	Source of ice code
C
260	CALL RS_NUMD ( ob(loc+1:), 1, defalt, data(40), ier )
C
C*	Ice thickness (cm)
C
	CALL RS_NUMD ( ob(loc+2:), 2, defalt, data(41), ier )
C
C*	Rate of accretion code
C
	CALL RS_NUMD ( ob(loc+4:), 1, defalt, data(42), ier )
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
268	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 268
	ENDIF
269	IF  ( temp .eq. 7 )  GOTO 270
	GOTO 279
C________________________________________________________________________
C
C*	Accurate wave height group  70H(wa)H(wa)H(wa)
C*	(10ths of m)
C
270	IF  ( ob(loc:loc+1) .eq. '70' )  THEN
	    CALL RS_NUMD  ( ob(loc+2:), 3, defalt, data(33), ier )
	ENDIF
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
278	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
C
C*	Should have gone to line 999 by now.  Unexpected code group
C*	encountered.  Skip forward, and if don't get to end of ob or
C*	encounter 333, 444, or 555, then give up.
C
279	loc = loc + 5
	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	GOTO 9999
C************************************************************************
C									*
C*	Section 3  (got here from line 999)				*
C									*
C************************************************************************
3000	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 3000
	END IF
	IF  ( temp .eq. 0 )  GOTO 300
	GOTO 309
C________________________________________________________________________
C
C*	Tropical cloud motions (Region 4)  0C(s)D(L)D(M)D(H)
C
C*	Tropical sky code
C
300	CALL RS_NUMD ( ob(loc+1:), 1, defalt, data(58), ier )
C
C*	Low cloud direction of motion (degrees)
C
	CALL RS_NUMD ( ob(loc+2:), 1, defalt, temp, ier )
	IF  ( ( temp .ge. 0 ) .and. (temp .le. 8 ) ) 
     +	  data(59) = temp * 45
C
C*	Middle cloud direction of motion (degrees)
C
	CALL RS_NUMD ( ob(loc+3:), 1, defalt, temp, ier )
	IF  ( ( temp .ge. 0 ) .and. (temp .le. 8 ) ) 
     +	  data(60) = temp * 45
C
C*	High cloud direction of motion (degrees)
C
	CALL RS_NUMD ( ob(loc+4:), 1, defalt, temp, ier )
	IF  ( ( temp .ge. 0 ) .and. (temp .le. 8 ) ) 
     +	  data(61) = temp * 45
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
308	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 308
	ENDIF
309	IF  ( temp .eq. 1 )  GOTO 310
	GOTO 319
C________________________________________________________________________
C
C*	Maximum temperature group  1s(n)T(x)T(x)T(x)
C	Decode in 10ths of degrees Celsius
C
310	CALL RS_NUMD ( ob(loc+2:), 3, defalt, data(24), ier )
	IF  ( ( ier .eq. 0 ) .and. ( ob(loc+1:loc+1) .eq. '1' ) ) 
     +	    data(24) = - data(24)
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
318	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 318
	ENDIF
319	IF  ( temp .eq. 2 )  GOTO 320
	GOTO 329
C________________________________________________________________________
C
C*	Minimum temperature group  2s(n)T(n)T(n)T(n)
C*	Decode in 10ths of degrees Celsius
C
320	CALL RS_NUMD ( ob(loc+2:), 3, defalt, data(25), ier )
	IF  ( ( ier .eq. 0 ) .and. ( ob(loc+1:loc+1) .eq. '1' ) )
     +	    data(25) = - data(25)
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
328	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 328
	ENDIF
329	IF  ( temp .eq. 3 )  GOTO 330
	GOTO 339
C________________________________________________________________________
C
C*	Bare ground group  3Ejjj
C*	Retrieve only the first code digit
C*	Add 10 to combine with E'
C
330	CALL RS_NUMD ( ob(loc+1:), 1, defalt, temp, ier )
	IF  ( ier .eq. 0 )  data(28) = temp + 10
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
338	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 338
	ENDIF
339	IF  ( temp .eq. 4 )  GOTO 340
	GOTO 349
C________________________________________________________________________
C
C*	Icy ground group  4E'sss
C
C*	Icy ground status code
C
340	CALL RS_NUMD ( ob(loc+1:), 1, defalt, data(28), ier )
C
C*	Snow cover (cm)
C
	CALL RS_NUMD ( ob(loc+2:), 3, defalt, temp, ier )
	IF  ( ier .eq. 0 )  THEN
	    IF  ( ( temp .eq. 997 ) .or.
     +		  ( temp .eq. 998 ) )  THEN
		data(27) = 0
	    ELSE
		IF  ( temp .le. 996 )  data(27) = temp
	    END IF
	END IF
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
348	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 348
	ENDIF
349	IF  ( temp .eq. 5 )  GOTO 350
	GOTO 359
C________________________________________________________________________
C
C*	Surprising weather group  5j(1)j(2)j(3)j(4) j(5)j(6)j(7)j(8)j(9)
C*	Several possibilities exist, depending on the value of j(1)
C
C*	Find and store j(1) indicator
C
350	CALL RS_NUMD ( ob(loc+1:), 1, defalt, data(43), ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 5
	    GOTO 358
	ENDIF
C........................................................................
C
C*	Wind shift?
C
	IF  ( data(43) .le. 3 )  THEN
C
C*	    Wind ddff before shift
C
	    CALL RS_NUMD ( ob(loc+1:), 4, defalt, data(44), ier )
C
	    loc = loc + 5
	    CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	    IF  ( ier .ne. 0 )  GOTO 999
C
C*	    Time of wind shift (coded)
C
	    CALL RS_NUMD ( ob(loc:), 1, defalt, data(46), ier )
C
C*	    Wind ddff after shift
C
	    CALL RS_NUMD ( ob(loc+1:), 4, defalt, data(45), ier )
C........................................................................
C
C*	Temperature change?
C
	ELSE IF  ( data(43) .eq. 4 )  THEN
C
C*	    Time of temperature change (coded)
C
	    CALL RS_NUMD ( ob(loc+2:), 1, defalt, data(48), ier )
C
C*	    Amount of change (degrees Celsius)
C
	    CALL RS_NUMD ( ob(loc+4:), 1, defalt, data(47), ier )
	    IF  ( ier .eq. 0 )  THEN
		IF  ( data(47) .lt. 5 )  data(47) = data(47) + 10
		IF  ( ob(loc+3:loc+3) .eq. '1' )  data(47) = - data(47)
	    ENDIF
C........................................................................
C
C*	Maximum wind?
C
	ELSE IF  ( data(43) .eq. 5 )  THEN
C
C*	    Time of maximum wind
C
	    CALL RS_NUMD ( ob(loc+4:), 1, defalt, data(50), ier )
C
C*	    Speed of maximum wind (decode)
C
	    CALL RS_NUMD ( ob(loc+2:), 2, defalt, data(49), ier )
	    IF  ( ( ier .eq. 0 ) .and. ( .not. wmps ) ) 
     +		data(49) = 500 + data(49)
C
C*	    Look for group with maximum wind greater than 100 knots
C
	    IF  ( ( data(49) .eq. defalt ) .or.
     +		  ( data(49) .eq. 99 ) )  THEN
	        loc = loc + 5
	        CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
		IF  ( ier .ne. 0 )  GOTO 999
		IF  ( ob(loc:loc+1) .eq. '55' )  THEN
C
C*		  Maximum wind greater than 100 knots (decode)
C
		  CALL RS_NUMD ( ob(loc+2:), 3, defalt, data(49), ier )
		  IF  ( ( ier .eq. 0 ) .and. ( .not. wmps ) )
     +		      data(49) = 500 + data(49)
		ELSE
C
C*		  Found some other group instead
C
		  CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
		  GOTO 359
C
		ENDIF
	    ENDIF
C........................................................................
C
C*	Cloud motion group
C
	ELSE IF  ( data(43) .eq. 6 )  THEN
C
C*	    Low cloud motion (degrees)
C
	    CALL RS_NUMD ( ob(loc+2:), 1, defalt, temp, ier )
	    IF  ( ( temp .ge. 0 ) .and. ( temp .le. 8 ) )
     +		data(55) = temp * 45
C
C*	    Middle cloud motion (degrees)
C
	    CALL RS_NUMD ( ob(loc+3:), 1, defalt, temp, ier )
	    IF  ( ( temp .ge. 0 ) .and. ( temp .le. 8 ) )
     +		data(56) = temp * 45
C
C*	    High cloud motion (degrees)
C
	    CALL RS_NUMD ( ob(loc+4:), 1, defalt, temp, ier )
	    IF  ( ( temp .ge. 0 ) .and. ( temp .le. 8 ) )
     +		data(57) = temp * 45
C........................................................................
C
C*	Tall cloud group
C
	ELSE IF  ( data(43) .eq. 7 )  THEN
C
C*	    Cloud genus code
C
	    CALL RS_NUMD ( ob(loc+2:), 1, defalt, data(55), ier )
C
C*	    Cloud motion (degrees)
C
	    CALL RS_NUMD ( ob(loc+3:), 1, defalt, temp, ier )
	    IF  ( ( temp .ge. 0 ) .and. ( temp .le. 8 ) )
     +		data(56) = temp * 45
C
C*	    Cloud top elevation angle (degrees)
C
	    CALL RS_NUMD ( ob(loc+4:), 1, defalt, temp, ier )
	    IF  ( ( temp .gt. 0 ) .and. ( temp .ne. defalt ) )  THEN
		IF  ( temp .eq. 1 )  THEN
		    data(57) = 45
		ELSE IF ( temp .le. 3 )  THEN
		    data(57) = 50 - ( temp * 10 )
		ELSE IF ( temp .le. 6 )  THEN
		    data(57) = 27 - ( temp * 4 )
		ELSE
		    data(57) = 14 - temp
		ENDIF
	    ENDIF
C........................................................................
C
C*	Pressure rise (10ths of mb)
C
	ELSE IF  ( data(43) .eq. 8 )  THEN
	    CALL RS_NUMD ( ob(loc+2:), 3, defalt, data(23), ier )
C........................................................................
C
C*	Pressure fall (10ths of mb)
C
	ELSE IF  ( data(43) .eq. 9 )  THEN
	    CALL RS_NUMD ( ob(loc+2:), 3, defalt, temp, ier )
	    IF  ( ier .eq. 0 )  data(23) = - temp
	ENDIF
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
358	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 358
	ENDIF
359	IF  ( temp .eq. 5 )  GOTO 350
	IF  ( temp .eq. 6 )  GOTO 360
	GOTO 369
C________________________________________________________________________
C
C*	First rainfall group  6RRRt(R)
C
C*	Precipitation amount (10ths of mm)
C
360	CALL RS_NUMD ( ob(loc+1:), 3, defalt, temp, ier )
	IF  ( ier .eq. 0 )  CALL RS_RRRD ( temp, data(7), ier )
C
C*	Period of measurement (hours)
C
	CALL RS_NUMD ( ob(loc+4:), 1, defalt, temp, ier )
	IF  ( ier .eq. 0 )  data(8) = 6 * temp
C
C*	Check for 24 hour total
C
	IF  ( temp .eq. 4 )  data(26) = data(7)
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
368	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 368
	ENDIF
369	IF  ( temp .eq. 7 )  GOTO 370
	GOTO 379
C________________________________________________________________________
C
C*	24-hour precipitation group, 10ths of mm  7R(24)R(24)R(24)R(24)
C
370	CALL RS_NUMD ( ob(loc+1:), 4, defalt, data(26), ier )
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
378	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 378
	ENDIF
379	IF  ( temp .eq. 8 )  GOTO 380
	GOTO 389
C________________________________________________________________________
C
C*	Individual cloud group  8N(s)Ch(s)h(s)
C*	Decode up to 4 such groups: cloud amount (code), cloud type
C*	(code), and cloud height.
C
380	i = 0
381	CALL RS_NUMD ( ob(loc+1:), 1, defalt, data(62+i), ier)
	CALL RS_NUMD ( ob(loc+2:), 1, defalt, data(63+i), ier)
	CALL RS_NUMD ( ob(loc+3:), 2, defalt, temp, ier )
	IF  ( ier .eq. 0 )  CALL RS_HHDE ( temp, 2, data(64+i), ier )
	i = i + 3
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
388	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 388
	ENDIF
389	IF  ( temp .eq. 8 )  THEN
	    IF  ( i .le. 9 )  GOTO 381
	    loc = loc + 5
	    GOTO 388
	ENDIF
	IF  ( temp .eq. 9 )  GOTO 390
	GOTO 399
C________________________________________________________________________
C
C*	Special weather group 9xxnn
C
C*	Six hour snow, cm
C
390	IF  ( ob(loc+1:loc+2) .eq. '01' )  THEN
	    CALL RS_NUMD ( ob(loc+3:), 2, defalt, data(78), ier )
C
C*	Water equivalent, mm
C
	ELSE IF  ( ob(loc+1:loc+2) .eq. '02' )  THEN
	    CALL RS_NUMD ( ob(loc+3:), 2, defalt, data(79), ier )
C
C*	Water equivalent (decode to mm)
C
	ELSE IF  ( ob(loc+1:loc+2) .eq. '03' )  THEN
	    CALL RS_NUMD ( ob(loc+3:), 2, defalt, temp, ier )
	    IF  ( ier .eq. 0 )  data(79) = temp * 10
C
C*	Total snow, cm
C
 	ELSE IF  ( ob(loc+1:loc+2) .eq. '04' )  THEN
	    CALL RS_NUMD ( ob(loc+3:), 2, defalt, data(27), ier )
C
C*	Precipitation period code, start/end (hours)
C
 	ELSE IF  ( ob(loc+1:loc+2) .eq. '05' )  THEN
	    CALL RS_NUMD ( ob(loc+4:), 1, defalt, data(80), ier )
	    CALL RS_NUMD ( ob(loc+3:), 1, defalt, temp, ier )
	    IF  ( ier .eq. 0 )  THEN
		IF  ( ( temp .eq. 0 ) .or. ( temp .eq. 9 ) )  THEN
		    data(81) = defalt
		ELSE IF  ( temp .eq. 8 )  THEN
		    data(81) = 24
		ELSE IF  ( temp .eq. 7 )  THEN
		    data(81) = 12
		ELSE
		    data(81) = temp
		ENDIF
	    ENDIF
C
C*	Maximum wind speed
C
 	ELSE IF  ( ob(loc+1:loc+2) .eq. '12' )  THEN
	    CALL RS_NUMD ( ob(loc+3:), 2, defalt, data(88), ier )
	    IF  ( ( ier .eq. 0 ) .and. ( .not. wmps ) )  
     +		data(88) = 500 + data(88)
	ENDIF
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
398	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 398
	ENDIF
	IF  ( temp .eq. 9 )  GOTO 390
399	GOTO 9999
C************************************************************************
C*									*
C*	Section 4  (Got here from line 999)				*
C*									*
C************************************************************************
4000	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 4000
	ENDIF
C________________________________________________________________________
C
C*	Cloud below station group  N'C'H'H'C(t)
C*	Amount, cloud type, height, description
C
	CALL RS_NUMD ( ob(loc:), 1, defalt, data(74), ier )
	CALL RS_NUMD ( ob(loc+1:), 1, defalt, data(75), ier )
	CALL RS_NUMD ( ob(loc+2:), 2, defalt, temp, ier )
	IF  ( ier .ne. 0 )  CALL RS_HHDE ( temp, 2, data(76), ier )
	CALL RS_NUMD ( ob(loc+4:), 1, defalt, data(77), ier )
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
498	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	GOTO 9999
C************************************************************************
C*									*
C*	Section 5, United States code groups (got here from line 999)	*
C*									*
C************************************************************************
C
C*	Try hard to find good codes, since we are near the end of the ob
C*	and numbers no longer come in groups of 5
C
5000	CALL RS_GGRP ( ob, loc, endob, 4, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 5000
	ENDIF
C
	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( shift .eq. 0 )  THEN
	    IF  ( temp .eq. 0 )  GOTO 500
	    GOTO 529
	ELSE IF  ( temp .eq. 1 )  THEN
	    loc = loc - shift
	    GOTO 510
	ENDIF
	GOTO 9999
C________________________________________________________________________
C
C*	Tide group   0i(t)t(d)t(d)t(d)
C
C*	Tide code
C
500	CALL RS_NUMD ( ob(loc+1:), 1, defalt, data(82), ier )
C
C*	Tide departure magnitude (10ths of m)
	CALL RS_NUMD ( ob(loc+2:), 3, defalt, data(83), ier )
	IF  ( ier .eq. 0 )  THEN
	    IF  ( ( data(82) .eq. 1 )  .or. ( data(82) .eq. 4 ) .or. 
     +		  ( data(82) .eq. 7 ) )  data(83) = - data(83)
	ENDIF
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
	GOTO 5000
C________________________________________________________________________
C
C*	City weather group
C
C*	City temperature, 10ths of degrees Celsius
C
510	CALL RS_NUMD ( ob(loc+2:), 2, defalt, temp, ier )
	IF  ( ier .eq. 0 )  THEN
	    IF  ( ob(loc+1:loc+1) .eq. '1' )  temp = - temp
	    IF  ( ( data(17) .ge. 150 ) .and. ( temp .le. 25 ) )
     +	        temp = temp + 100
	    data(84) = ( ( temp - 32 ) * 50 ) / 9
	ENDIF
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 4
	CALL RS_GGRP ( ob, loc, endob, 6, shift, ier ) 
	IF  ( ier .ne. 0 )  GOTO 518
C________________________________________________________________________
C
C*	City maximum temperature, 10ths of degrees Celsius
C
	CALL RS_NUMD  ( ob(loc+1:), 2, defalt, temp, ier )
	IF  ( ier .eq. 0 )  THEN
	    IF  ( ob(loc:loc) .eq. '1' )  temp = - temp
	    IF  ( ( data(17) .ge. 150 ) .and. ( temp .le. 25 ) )
     +	        temp = temp + 100
	    data(85) = ( ( temp - 32 ) * 50 ) / 9
	ENDIF
C
C*	City minimum temperature, 10ths of degrees Celsius
C
	CALL RS_NUMD  ( ob(loc+4:), 2, defalt, temp, ier )
	IF  ( ier .eq. 0 )  THEN
	    IF  ( ob(loc+3:loc+3) .eq. '1' )  temp = - temp
	    data(86) = ( ( temp - 32 ) * 50 ) / 9
	ENDIF
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 6
518	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 9999
	CALL RS_NUMD ( ob(loc:loc), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 518
	ENDIF
519	IF  ( temp .eq. 2 )  GOTO 520
	GOTO 529
C________________________________________________________________________
C
C*	City rainfall (units?)
C
520	CALL RS_NUMD ( ob(loc+1:), 4, defalt, data(79), ier )
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
528	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 9999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 528
	ENDIF
C
C*	Check for misplaced 9 group
C
529	IF  ( temp .eq. 9 )  GOTO 390
	GOTO 9999
C************************************************************************
C*									*
C*	Section five for buoys and cman obs  (here from line 999)	*
C*									*
C************************************************************************
6000	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 999
	CALL RS_NUMD ( ob(loc:), 2, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 6000
	ENDIF
	IF  ( temp .eq. 11 )  GOTO 610
	GOTO 619
C________________________________________________________________________
C
C*	Minimum recent wind group (m/s)  11fff
C
610	CALL RS_NUMD ( ob(loc+2:), 3, defalt, temp, ier )
	IF  ( ier .eq. 0 )  THEN
	    IF  ( obtype .ne. 'CMAN' )  temp = temp / 10
	    IF  ( wmps ) THEN
		data(89) = temp
	    ELSE
		data(89) = 500 + temp
	    ENDIF
	ENDIF
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
618	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 9999
	CALL RS_NUMD ( ob(loc:loc), 2, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 618
	ENDIF
619	IF  ( temp .eq. 22 )  GOTO 620
	CALL RS_NUMD ( ob(loc:loc), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 619
	ENDIF
	IF  ( temp .eq. 9 )  GOTO 690
	GOTO 9999
C________________________________________________________________________
C
C*	Maximum recent wind group
C		
620	CALL RS_NUMD ( ob(loc+2:), 3, defalt, temp, ier )
	IF  ( ier .eq. 0 )  THEN
	    IF  ( obtype .ne. 'CMAN' )  temp = temp / 10
	    IF  ( wmps ) THEN
		data(90) = temp
	    ELSE
		data(90) = 500 + temp
	    ENDIF
	ENDIF
C''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
	loc = loc + 5
628	CALL RS_GGRP ( ob, loc, endob, 5, shift, ier )
	IF  ( ier .ne. 0 )  GOTO 9999
	CALL RS_NUMD ( ob(loc:), 1, defalt, temp, ier )
	IF  ( ier .ne. 0 )  THEN
	    loc = loc + 1
	    GOTO 628
	ENDIF
629	IF  ( temp .eq. 9 )  GOTO 690
	GOTO 9999
C________________________________________________________________________
C
C*	Time of observation group
C
690	CALL RS_NUMD ( ob(loc+1:), 4, defalt, data(91), ier )
	GOTO 9999
C
C************************************************************************
C									*
C*				ROUTER					*
C									*
C*	This segment is invoked when a new section is found by RS_GGRP.	*
C*	Ier is checked for the RS_GGRP return code and control is 	*
C*	passed accordingly.						*
C									*
C************************************************************************
999	IF  ( ier .gt. 0 )  THEN
	    loc = loc + 3
	    IF  ( ier .eq. 3 )  GOTO 3000
	    IF  ( ier .eq. 4 )  GOTO 4000
	    IF  ( ier .eq. 5 )  THEN
		IF  ( obtype .eq. 'SMUS' )  GOTO 5000
		IF  ( obtype .eq. 'CMAN' )  GOTO 6000
		IF  ( obtype .eq. 'SHBY' )  GOTO 6000
	    ENDIF
	ENDIF
C
C*	Last try; maybe no space after '333' group
C
	CALL RS_GGRP ( ob, loc, endob, 8, shift, ier )
	IF  ( ier .eq. 0 )  THEN
	    CALL RS_GGRP ( ob, loc, endob, 9, shift, ier )
	    IF  ( ier .ne. 0 )  THEN
		IF  ( ob(loc:loc+2) .eq. '333' )  THEN
		    loc = loc + 3
		    GOTO 3000
		ELSE IF  ( ob(loc:loc+2) .eq. '444' )  THEN
		    loc = loc + 3
		    GOTO 4000
		ELSE IF  ( ob(loc:loc+2) .eq. '555' )  THEN
		    loc = loc + 3
		    IF  ( obtype .eq. 'SMUS' )  GOTO 5000
		    IF  ( obtype .eq. 'CMAN' )  GOTO 6000
		    IF  ( obtype .eq. 'SHBY' )  GOTO 6000
		ENDIF
	    ENDIF
	ENDIF
C
C************************************************************************
C
9999	RETURN
	END

