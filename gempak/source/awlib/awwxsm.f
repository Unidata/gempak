	SUBROUTINE AW_WXSM ( bdata, ibeg, ispch, chsiz, xlat, ylon,
     +                       ixoff, iyoff, iret )
C************************************************************************
C* AW_WXSM                                                              *
C*                                                                      *
C* This subroutine plots the weather symbols for AWIPS graphics.        *
C* 								        *
C* AW_WXSM ( BDATA, IBEG, ISPCH, CHSIZ, XLAT, YLON, IXOFF, IYOFF, IRET )*
C*								        *
C* Input parameters: 						        *
C*      BDATA(*)	BYTE		Byte data array                 *
C*      IBEG  		INTEGER         Byte position in block          *
C*      ISPCH           INTEGER         Hex value of weather symbol #   *
C*      CHSIZ           INTEGER		Character size                  *
C*      XLAT            REAL		X position                      *
C*      YLON            REAL		Y position                      *
C*      IXOFF           INTEGER         X offset position               *
C*      IYOFF           INTEGER         Y offset position               *
C*								        *
C* Output parameters:						        *
C*      IRET            INTEGER          Return code                    *
C**								        *
C* Log:									*
C* A. Hardy/GSC          8/98                                           *
C* T. Piper/GSC		11/98	Updated prolog				*
C* A. Hardy/GSC         11/98   Corrected gicng->gsicng, modified gpgm  *
C* A. Hardy/GSC		 3/99	Updated prolog				*
C* A. Hardy/GSC		 5/00   Force large char size to be equal to 1  *
C* A. Hardy/GSC         10/00   Put file number and dbug in awcmn.cmn	*
C* T. Piper/SAIC	 2/03	Changed b to bdata 			*
C************************************************************************
 	INCLUDE         'awcmn.cmn'
C*
	BYTE		bdata(*)
        INTEGER         ispch, ibeg, chsiz
C*
	CHARACTER*3     gpgm (128), outpgm
	INTEGER         gsym (128)
	REAL            wxnum, temp
C*
        DATA gpgm /
     1              '000','SKY','SKY','SKY','SKY','SKY','SPC','SPC',
     2              '000','000','000','000','000','000','SKY',
     2              'SKY',
     3              'SKY','000','000','SKY','SKY','SKY','PTN','PTN', 
     4              'PTN','PTN','PTN','PTN','WTH','WTH','WTH','WTH',
     5              'WTH','WTH','WTH','WTH','WTH','WTH','WTH','WTH',
     6              'WTH','PWX','WTH','WTH','WTH','WTH','WTH','WTH',
     7              'SPC','SPC','SPC','SPC','SPC','WTH','PWX','WTH',
     8              'SPC','SPC','WTH','WTH','WTH','WTH','WTH','WTH',
     9              'WTH','WTH','WTH','WTH','WTH','WTH','WTH','WTH',
     A              'WTH','WTH','WTH','WTH','WTH','WTH','WTH','WTH',
     B              'SPC','SPC','SPC','WTH','SPC','SPC','SPC','SPC',
     C              'WTH','WTH','SPC','SPC','SPC','CTP','CTP','CTP',
     D              'CTP','CTP','CTP','CTP','CTP','CTP','CTP','CTP', 
     E              'CTP','CTP','CTP','CTP','CTP','CTP','CTP','CTP', 
     F              'CTP','CTP','CTP','CTP','CTP','CTP','CTP','CTP',
     G              'SPC','SPC','TRB','TRB','TRB','ICE','ICE','ICE' /
C
         DATA gsym / 
     1              0, 6, 7, 8, 9, 10, 0, 8,
     2              0, 0, 0, 0, 0, 0, 0, 1,
     3              2, 0, 0, 3, 4, 5, 999, 1999,
     4              5999, 6999, 3999, 8999, 4, 5, 6, 7,
     5              8, 9, 10, 11, 12, 13, 14, 15,
     6              16, 9, 18, 19, 50, 60, 70, 68,
     7              30, 14, 16, 15, 4, 45, 8, 31,
     8              18, 17, 34, 36, 37, 38, 39, 41,
     9              44, 48, 49, 52, 56, 57, 66, 67,
     A              47, 58, 76, 77, 78, 79, 81, 62,
     B              4, 19, 20, 43, 21, 23, 22, 24,
     C              97, 72, 12, 13, 29, 11, 12, 13, 
     D              14, 15, 16, 17, 18, 19, 1, 2,
     E              3, 4, 5, 6, 7, 8, 9, 21,
     F              22, 23, 24, 25, 26, 27, 28, 29,
     G              25, 26, 2, 4, 6, 3, 5, 8 /
C------------------------------------------------------------------------
        IF ( dbug .eq. 'y' ) THEN
            write(flun,*)'In WXSM'
	END IF
C
C*      Locating the weather symbol number and plot function.
C
	n = bdata(ibeg)
	wxnum = gsym ( n+1 )
	outpgm = gpgm ( n+1 )
        IF ( dbug .eq. 'y' ) THEN
	    write(flun,111)'WEATHER SYMBOL NUMBER','G FUNCTION PROGRAM'
	    write(flun,123) ispch, wxnum, outpgm
 111        format(a21,5x,a20)
 123        format(Z2.2,3x,f3.0,25x,a4)
	END IF
C
C*      Setting defaults for variables for number of symbols 
C*      to be plotted and for character size.
C
        np = 1	
	sf = 0.25
	temp = 0.8
	IF ( (chsiz .eq. 0 ) .or. ( chsiz .eq. 3 ) ) chsiz = 1
C
C*      Plotting sky cover.
C
	IF ( outpgm .eq. 'SKY' ) THEN
            IF ( dbug .eq. 'y' ) THEN
	        write(flun,*)'In SKY'
	    END IF
	    isktyp = 0
	    iskwid = 0
	    szsky = temp + ( chsiz * sf ) - 0.05
	    rcode = wxnum
	    CALL GSSKY ( szsky, isktyp, iskwid, iret )
	    CALL GSKY ( 'G', np, rcode, xlat, ylon, ixoff, iyoff, iret )
C
C*        Plotting past weather.
C
	  ELSE IF ( outpgm .eq. 'PWX' ) THEN
            IF ( dbug .eq. 'y' ) THEN
	        write(flun,*)'In PWX'
	    END IF
            ipwwid = 0
	    szpwth = temp + ( chsiz * sf )
	    rcode = wxnum
	    CALL GSPWTH ( szpwth, ipwwid, ier )
	    CALL GPWTH ( 'G', np, rcode, xlat, ylon, ixoff, iyoff, ier )
C
C*        Plotting pressure tendency.
C
	  ELSE IF ( outpgm .eq. 'PTN' ) THEN
            IF ( dbug .eq. 'y' ) THEN
	        write(flun,*)'In PTN'
	    END IF
            iptwid = 0
	    szptnd = temp + ( chsiz * sf )
	    rcode =  wxnum
	    CALL GSPTND ( szptnd, iptwid, ier )
	    CALL GPTND ( 'G', np, rcode, xlat, ylon, ixoff, iyoff, ier )
C
C*        Plotting WMO weather symbols.
C
	  ELSE IF ( outpgm .eq. 'WTH' ) THEN
            IF ( dbug .eq. 'y' ) THEN
	        write(flun,*)'In WTH'
	    END IF
            iwtwid = 0
	    IF ( (wxnum .eq. 70) .or. (wxnum .eq. 72) ) THEN
	        szwthr = temp + ( chsiz * sf ) + 0.5
	      ELSE
	        szwthr = temp + ( chsiz * sf )
            END IF
	        rcode = wxnum
	        CALL GSWTHR ( szwthr, iwtwid, ier )
	        CALL GWTHR ( 'G', np, rcode, xlat, ylon, 
     +                          ixoff, iyoff, ier )
C
C*        Plotting cloud type.
C
	  ELSE IF ( outpgm .eq. 'CTP' ) THEN
            IF ( dbug .eq. 'y' ) THEN
	        write(flun,*)'In CTP'
	    END IF
	    ictwid = 0
	    szctyp = temp + ( chsiz * sf )
	    rcode =  wxnum
	    CALL GSCTYP ( szctyp, ictwid, ier )
	    CALL GCTYP ( 'G', np, rcode, xlat, ylon, ixoff, iyoff, ier )
C
C*        Plotting special symbols.
C
	  ELSE IF ( outpgm .eq. 'SPC' ) THEN
            IF ( dbug .eq. 'y' ) THEN
	        write(flun,*)'In SPC'
	    END IF
	    ispwid = 0
	    IF ( (wxnum .eq. 12.0) .or. (wxnum .eq. 13.0) ) THEN
                szspcl = temp + ( chsiz * sf ) - 0.3
              ELSE IF ( wxnum  .eq. 0.0 ) THEN
                szspcl = temp + ( chsiz * sf ) - 0.4
              ELSE 
                szspcl = temp + ( chsiz * sf )
            END IF
	    rcode = wxnum
            CALL GSSPCL ( szspcl, ispwid, ier )
            CALL GSPCL ( 'G', np, rcode, xlat, ylon, ixoff, iyoff, ier )
C
C*        Plotting turbulence.
C
	  ELSE IF ( outpgm .eq. 'TRB' ) THEN
            IF ( dbug .eq. 'y' ) THEN
	         write(flun,*)'In TRB'
	    END IF
	    ituwid = 0
	    szturb = temp + ( chsiz *sf ) - 0.5
	    rcode =  wxnum
	    CALL GSTURB ( szturb, ituwid, ier )
	    CALL GTURB ( 'G', np, rcode, xlat, ylon, ixoff, iyoff, ier )
C
C*        Plotting icing.
C
	  ELSE IF ( outpgm .eq. 'ICE' ) THEN
            IF ( dbug .eq. 'y' ) THEN
	        write(flun,*)'In ICE'
	    END IF
	    icewid = 0
	    szicng = temp + ( chsiz * sf ) - 0.5
	    rcode = wxnum
	    CALL GSICNG( szicng, icewid, ier ) 
	    CALL GICNG( 'G', np, rcode, xlat, ylon, ixoff, iyoff, ier )
        END IF
C*      
	RETURN
	END
