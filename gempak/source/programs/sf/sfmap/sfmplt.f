	SUBROUTINE SFMPLT  ( icolr, sx, sy, slat, slon, chflg, prmlst,
     +			     ncprm, outd, chd, ccvals, icclrs, numccc,
     +			     icrprm, tsize, jwide, endflg, iret )
C************************************************************************
C* SFMPLT								*
C*									*
C* This subroutine plots data for one station.				*
C*									*
C* SFMPLT  ( ICOLR, SX, SY, SLAT, SLON, CHFLG, PRMLST, NCPRM, OUTD,	*
C*	     CHD, CCVALS, ICCLRS, NUMCCC, ICRPRM, TSIZE, JWIDE,   	*
C*	     ENDFLG, IRET )       				        *
C*									*
C* Input parameters:							*
C*	ICOLR (*)	INTEGER		Color array			*
C*	SX		REAL		Station x position (plot coord) *
C*	SY		REAL		Station y position (plot coord) *
C*	SLAT		REAL		Station x position (map coord)	*
C*	SLON		REAL		Station y position (map coord)	*
C*	CHFLG (*)	LOGICAL		Character data flag		*
C*      PRMLST (*)	CHAR*	        Parameter list			*
C*      NCPRM		INTEGER		Number of parameters		*
C*	OUTD (*)	REAL		Real station data		*
C*	CHD(*)		CHAR*		Character station data		*
C*      CCVALS (*)      REAL            Values for color coding         *
C*      ICCLRS (*)      INTEGER         Colors for color coding         *
C*      NUMCCC (*)      INTEGER         Number of colors for coding     *
C*      ICRPRM (*)      INTEGER         Pointers to reference parms     *
C*	TSIZE  (26)      REAL		Text size for numericals	*
C*	JWIDE  (26)	INTEGER		Text width for numericals	* 
C*	ENDFLG		CHAR*		Value range end point flag	*
C*					  L = Lower data range		*
C*					  U = Upper data range		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C* I. Graffman/RDS	 6/85	GEMPLT Version 3.1 			*
C* M. desJardins/GSFC	 1/86	Changed x offsets to plot closer	*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* I. Graffman/RDS	12/86	Added positions 9 & 10			*
C* M. desJardins/GSFC	 6/88	Eliminated in-line encoding		*
C* M. desJardins/GSFC	 1/90	Added SKPMIS				*
C* S. Schotz/GSC	 4/90   Added plotting of weather/cloud symbols	*
C* S. Schotz/GSC	 5/90	Updated GSKY,GWTHR calling sequence	*
C* S. Schotz/GSC	 8/90	Removed SCALE				*
C* J. Whistler/SSAI	 7/91	Moved parm filter up into SFMAP		*
C* M. desJardins/NMC	10/91	Added sky cover symbols			*
C* K. Brill/NMC		10/91	Added cloud symbols			*
C* K. Brill/NMC		11/91	Removed wind flags and moved barb and   *
C*				arrow drawing to parameter loop		*
C* K. Brill/NMC		11/91	Added check for PTSY			*
C* K. Brill/NMC		12/91   Finished adding checks for PTSY		*
C* K. Brill/NMC		03/92	Added DARR				*
C* S. Jacobs/EAI	 6/92	Fixed plotting of wind to be done in 	*
C*				map coordinates; added check for 	*
C*				missing winds				*
C* J. Whistler/SSAI	 3/94	Plot markers of stations that have data *
C*				when color is set to 0			*
C* S. Maxwell/GSC        3/97   Added call to GMARK                     *
C* S. Maxwell/GSC        3/97   Removed imark and skpmis                *
C* D. Kidwell/NCEP      10/97   Added turbulence and icing symbols      *
C* D. Kidwell/NCEP       2/98   Added processing of color coding info   *
C* D. Kidwell/NCEP       3/98   Corrected processing for coded color 0  *
C* D. Kidwell/NCEP       5/98   Changed DARR processing, added DAWV     *
C* A. Hardy/GSC          2/99   Increased number of offsets from 10->24 *
C* S. Jacobs/NCEP	 3/99	Added endflg; Check for value range ends*
C* S. Jacobs/NCEP	 3/99	Changed chbuf from 8 char to 12 char	*
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *
C*                              DATA statement                          *
C* S. Jacobs/NCEP	 2/01	Allow the missing SKYC symbol to plot	*
C* D. Kidwell/NCEP       3/02   Added DASH                              *
C* D. Kidwell/NCEP       9/02   Added TWSY, BRGK and TSKC               *
C* D. Kidwell/NCEP       5/03   Added TPWS, AWSY, VWSY and WSKC         *
C* D. Kidwell/NCEP      10/04   Added TCSL                              *
C* D. Kidwell/NCEP       4/05   Added BRPK                              *
C* R. Jones/NCEP	 2/06	Added fractional visibility plotting	*
C* R. Jones/NCEP	 5/06	Added option for variable text sizes	*
C* R. Jones/NCEP	 7/06	Added query of text info for var sizes	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	prmlst (*), chd (*), endflg
	INTEGER		icolr (*), icclrs (*), numccc (*), icrprm (*)
        INTEGER         jwide (26)
	LOGICAL		chflg (*)
	REAL		outd (*), ccvals (*), tsize (26)
C*
	CHARACTER	chbuf*12
        CHARACTER       cfrac*4 
	INTEGER		ixof (24), iyof (24)
	LOGICAL		datflg
C*
	INCLUDE		'ERMISS.FNC'
C*
	DATA		ixof  / 0, 0,  2,  2,  0,  2,  0,  0,  0,  0, 
     +                         -4, 6,  0,  0,  2,  2, -4, -4, -4, -4,
     +				6, 6,  6,  6 /
	DATA		iyof  / 2, 0,  2,  0, -2, -2,  4, -4,  2, -2,
     +                          0, 0,  4, -4,  4, -4,  4,  2, -2, -4,
     +                          4, 2, -2, -4  /
C------------------------------------------------------------------------
	iret   = 0
	datflg = .false.
C        
C*	Plot each parameter.
C
	DO  ip = 1, ncprm
C
C*          Station position is one less than the element number
C
            ipos = ip - 1
C
C*          Check for weather symbols, set size for offset
C
	    IF  ( ( prmlst (ip) .eq. 'WSYM' ) .or. 
     +		  ( prmlst (ip) .eq. 'TWSY' ) .or.
     +		  ( prmlst (ip) .eq. 'TPWS' ) .or.
     +		  ( prmlst (ip) .eq. 'AWSY' ) .or.
     +		  ( prmlst (ip) .eq. 'VWSY' ) .or.
     +		  ( prmlst (ip) .eq. 'PTND' ) .or.
     +		  ( prmlst (ip) .eq. 'PTSY' ) .or.
     +		  ( prmlst (ip) .eq. 'PWTH' ) .or.
     +		  ( prmlst (ip) .eq. 'CTYP' ) .or.
     +		  ( prmlst (ip) .eq. 'CSYL' ) .or.
     +		  ( prmlst (ip) .eq. 'CSYM' ) .or.
     +		  ( prmlst (ip) .eq. 'CSYH' ) .or.
     +		  ( prmlst (ip) .eq. 'CSYT' ) .or.
     +		  ( prmlst (ip) .eq. 'TCSL' ) .or.
     +		  ( prmlst (ip) .eq. 'DARR' ) .or.
     +		  ( prmlst (ip) .eq. 'TBSY' ) .or.
     +		  ( prmlst (ip) .eq. 'ICSY' ) .or.
     +		  ( prmlst (ip) .eq. 'TSKC' ) .or.
     +		  ( prmlst (ip) .eq. 'WSKC' ) .or.
     +		  ( prmlst (ip) .eq. 'DAWV' ) .or.
     +		  ( prmlst (ip) .eq. 'DASH' ) .or.
     +		  ( prmlst (ip) .eq. 'BRGK' ) .or.
     +		  ( prmlst (ip) .eq. 'BRPK' ) .or.
     +		  ( prmlst (ip) (1:3) .eq. 'BRB' ) .or.
     +		  ( prmlst (ip) (1:3) .eq. 'ARR' ) )  THEN
		IF  ( .not. ERMISS ( outd (ip) ) )  THEN
		    isiz = 1
		  ELSE
		    isiz = 0
		END IF
	      ELSE IF ( ( prmlst (ip) .eq. 'MARK' ) .or.
     +		        ( prmlst (ip) .eq. 'SKYC' ) )  THEN
		isiz = 1
	      ELSE
C
C*		Get number of characters to plot
C
		chbuf = ' '
		IF  ( .not. chflg (ip) )  THEN
		    IF ( .not. ERMISS ( outd (ip) ) )  THEN
			intg = NINT ( outd (ip)  )
			CALL ST_INLN  ( intg, chbuf, isiz, ier )
		      ELSE
			chbuf = ' '
			isiz  = 0
		    END IF
		  ELSE
		    chbuf = chd (ip)
		    CALL ST_LDSP  ( chbuf, chbuf, isiz, ier )
		END IF
	    END IF
C
C*	    Set offsets for station positions and plot data if 
C*	    string is not blank.  Weather symbols are offset
C*	    in the x direction to be centered on two characters
C*	    for positions to the left and right of the station.
C
	    IF  ( isiz .gt. 0 )  THEN
C*
		datflg = .true.
		IF  ( icolr (ip) .gt. 0 )  THEN
		    CALL GSCOLR  ( icolr (ip), ier )
	          ELSE IF ( icolr (ip) .lt. 0 ) THEN
C
C*		    Check reference value for color coding.
C
		    indx = IABS ( icolr (ip) )
		    refval = outd ( icrprm ( indx ) ) 
		    IF ( .not. ERMISS (refval) ) THEN
		        iend = numccc (1) - 1
		        DO i = 2, indx
		            iend = iend + numccc (i)
		        END DO
		        istrt = iend - numccc (indx) + 2
			IF  ( endflg .eq. 'U' )  THEN
		          IF ( refval .lt. ccvals (istrt) ) THEN
		            icolor = icclrs (istrt)
		           ELSE IF ( refval .ge. ccvals (iend) ) THEN
		            icolor = icclrs (iend + 1)
		           ELSE
		            ict = istrt
		            DO WHILE ( ict .lt. iend )
			        IF ( ( refval .ge. ccvals (ict) ) .and.
     +			         ( refval .lt. ccvals (ict + 1) ) ) THEN
			            icolor = icclrs (ict + 1)
			            ict = iend
			          ELSE
			            ict = ict + 1
			        END IF
		            END DO
		          END IF
			 ELSE
		          IF ( refval .le. ccvals (istrt) ) THEN
		            icolor = icclrs (istrt)
		           ELSE IF ( refval .gt. ccvals (iend) ) THEN
		            icolor = icclrs (iend + 1)
		           ELSE
		            ict = istrt
		            DO WHILE ( ict .lt. iend )
			        IF ( ( refval .gt. ccvals (ict) ) .and.
     +			         ( refval .le. ccvals (ict + 1) ) ) THEN
			            icolor = icclrs (ict + 1)
			            ict = iend
			          ELSE
			            ict = ict + 1
			        END IF
		            END DO
		          END IF
		        END IF
			IF ( icolor .gt. 0 ) THEN
		            CALL GSCOLR  ( icolor, ier )
			  ELSE
C
C*			    If coded color is 0, do not plot parm.
C
			    datflg = .false.
			END IF
		      ELSE
C
C*			If reference value is missing, do not plot parm.
C
			datflg = .false.
		    END IF
	          ELSE
		    datflg = .false.
	        END IF
C*
	        IF ( datflg ) THEN
		    IF  ( ipos .eq. 0 )  THEN
                        ix = -isiz + 1
                        ixs = 0
                        iy = 0
C*
		      ELSE IF  ( ( ipos .eq. 1 ) .or. ( ipos .eq. 2 ) 
     +				.or. ( ipos .eq. 5 ) .or. 
     +                          ( ipos .eq. 13 ) .or. ( ipos .eq. 14 )
     +                         )  THEN
	                ix = ixof (ipos) - (isiz * 2) 
                        ixs = -3
                        iy = iyof (ipos)
C*
		      ELSE IF  ( ( ipos .eq. 3 ) .or. ( ipos .eq. 4 ) 
     +				.or.  ( ipos .eq. 6 ) .or.
     +                          ( ipos .eq. 15 ) .or. ( ipos .eq. 16 )
     +                         )  THEN
                        ix = ixof (ipos)
                        ixs = 3
                        iy = iyof (ipos)
C*
		      ELSE IF  ( ( ipos .eq. 7 ) .or. ( ipos .eq. 8 ) 
     +				.or.  ( ipos .eq. 9 ) .or. 
     +                          ( ipos .eq. 10 ) ) THEN
                        ix = -isiz + 1
                        ixs = 0
                        iy = iyof (ipos)
		      ELSE IF  ( ( ipos .eq. 11 ) .or. ( ipos .eq. 17 ) 
     +				.or.  ( ipos .eq. 18 ) .or. 
     +                          ( ipos .eq. 19 ) .or. ( ipos .eq. 20 ) 
     +                         ) THEN
                        ix = ixof (ipos) - (isiz * 2)
                        ixs = -3 
                        iy = iyof (ipos)
		      ELSE IF  ( ( ipos .eq. 12 ) .or. ( ipos .eq. 21 ) 
     +				.or.  ( ipos .eq. 22 ) .or. 
     +                          ( ipos .eq. 23 ) .or. ( ipos .eq. 24 ) 
     +                         ) THEN
                        ix = ixof (ipos)
                        ixs = 3
                        iy = iyof (ipos)
	            END IF
C*
                    IF  ( prmlst (ip) .eq. 'WSYM' .or.
     +			  prmlst (ip) .eq. 'TWSY' .or.
     +			  prmlst (ip) .eq. 'TPWS' .or.
     +			  prmlst (ip) .eq. 'AWSY' .or.
     +			  prmlst (ip) .eq. 'VWSY' ) THEN
                        CALL GWTHR ( 'P', 1, outd (ip), sx, sy,
     +                               ixs, iy, iret )
                      ELSE IF ( prmlst (ip) .eq. 'SKYC' .or.
     +				prmlst (ip) .eq. 'TSKC' .or.
     +				prmlst (ip) .eq. 'WSKC' ) THEN
                        CALL GSKY  ( 'P', 1, outd (ip), sx, sy, 
     +                                ixs, iy, iret )
                      ELSE IF ( prmlst (ip) .eq. 'PTND' .or.
     +				prmlst (ip) .eq. 'PTSY' ) THEN
                        CALL GPTND ( 'P', 1, outd (ip), sx, sy, 
     +                                ixs, iy, iret )
                      ELSE IF ( prmlst (ip) .eq. 'MARK' ) THEN
                        CALL GMARK ( 'P', 1, sx, sy, iret )
                      ELSE IF ( prmlst (ip) .eq. 'PWTH' ) THEN
                        CALL GPWTH ( 'P', 1, outd (ip), sx, sy, 
     +                                ixs, iy, iret )
		      ELSE IF ( prmlst (ip) .eq. 'CSYL' .or.
     +				prmlst (ip) .eq. 'CSYM' .or.
     +				prmlst (ip) .eq. 'CSYH' .or.
     +				prmlst (ip) .eq. 'CSYT' .or.
     +				prmlst (ip) .eq. 'TCSL' ) THEN
			CALL GCTYP ( 'P', 1, outd (ip), sx, sy,
     +				     ixs, iy, iret )
		      ELSE IF ( prmlst (ip) (1:3) .eq. 'BRB' .or.
     +				prmlst (ip) .eq. 'BRGK' .or.
     +				prmlst (ip) .eq. 'BRPK' ) THEN
		        spd = FLOAT ( INT ( outd (ip) / 1000. ) )
			dir = FLOAT ( MOD ( INT ( outd (ip) ), 1000 ))
			CALL GBARB ( 'M', 1, slat, slon, spd, dir,
     +				     iret )
		      ELSE IF ( prmlst (ip) (1:3) .eq. 'ARR' ) THEN
		        spd = FLOAT ( INT ( outd (ip) / 1000. ) )
			dir = FLOAT ( MOD ( INT ( outd (ip) ), 1000 ))
			CALL GARRW ( 'M', 1, slat, slon, spd, dir,
     +				     iret )
		      ELSE IF ( prmlst (ip) .eq. 'DARR' .or.
     +				prmlst (ip) .eq. 'DAWV' .or.
     +				prmlst (ip) .eq. 'DASH' ) THEN
			CALL GDARR ( 'M', 1, slat, slon, outd (ip), 
     +				     iret )
                      ELSE IF ( prmlst (ip) .eq. 'TBSY' ) THEN
                        CALL GTURB ( 'P', 1, outd (ip), sx, sy, 
     +                                ixs, iy, iret )
                      ELSE IF ( prmlst (ip) .eq. 'ICSY' ) THEN
                        CALL GICNG ( 'P', 1, outd (ip), sx, sy, 
     +                                ixs, iy, iret )
C
C*		      Process the fractional visibility.
C
                      ELSE IF ( prmlst (ip) .eq. 'VSBC' ) THEN
C 
                        IF ( ipos .eq. 0 ) THEN 
                          ix = 0
                          iy = 0
                        ELSE
                          ix = ixof (ipos)
                          iy = iyof (ipos)
                        END IF
C
                        ix2 = 2 * ix
                        iy2 = 2 * iy
C
                        IF ( isiz .gt. 2 ) THEN
C
                          IF ( isiz .eq. 3 ) THEN
                            ifrac = 1
                            ilen = 3
                          ELSE IF ( isiz .eq. 4 ) THEN
                            ifrac = 1
                            ilen = 4
                          ELSE IF ( isiz .eq. 5 ) THEN
                            ifrac = 3
                            ilen = 3
                          ELSE IF ( isiz .eq. 6 ) THEN
                            ifrac = 3
                            ilen = 4
                          END IF
C
                          cfrac(1:ilen) = chbuf (ifrac:isiz)
                          iwhol = isiz - 4
C
                          CALL GQTEXT ( i1, i2, sztext, i3, i4, i5,
     +                                  i6, ier )
                          szwhol = sztext
                          szfrac = 0.5 * sztext
C
C*			  Plot the visibility in whole miles
C*			  (one character), if applicable.
C
                          IF ( iwhol .ge. 1 ) THEN 
                            jx = ix - 1
                            CALL GTEXT  ( 'P',  sx, sy, chbuf (1:1),
     +                                     0., jx, iy, ier )
                          ENDIF
C
C*			  Plot the fractional part.
C
C*			  Plot the virgule (full size).
C
                          jx = ix + 1
			  IF ( isiz .eq. 3 .or. isiz .eq. 4 ) jx = ix
                          jy = iy
C
                          CALL GTEXT ( 'P', sx, sy, cfrac (2:2),
     +                                  0., jx, jy, ier )
C
C*			  Set the text size to half the original size.
C
                          CALL GSTEXT ( i1, i2, szfrac, i3, i4,
     +				        i5, i6, ier )
C
C*			  Plot the numerator.
C
                          jx = ix2
                          if ( isiz .eq. 3 ) jx = ix2 - 1
                          if ( isiz .eq. 4 ) jx = ix2 - 2
                          if ( isiz .eq. 5 ) jx = ix2 + 1
                          if ( isiz .eq. 6 ) jx = ix2 + 1
                          jy = iy2 + 1
C
                          CALL GTEXT ( 'P', sx, sy, cfrac (1:1),
     +                                  0., jx, jy, ier )
C
C*			  Plot the denominator
C*			  (one or two characters, i.e. 2,4,8 or 16).
C
                          jx = ix2 + 1
                          if ( isiz .eq. 5 ) jx = ix2 + 4
                          if ( isiz .eq. 6 ) jx = ix2 + 3
                          jy = iy2 - 1
C
                          CALL GTEXT ( 'P', sx, sy, cfrac (3:ilen),
     +                                  0., jx, jy, ier )
C
C*			  Restore the original text size.
C
                          CALL GSTEXT ( i1, i2, sztext, i3, i4,
     +					i5, i6, ier )
C
                        ELSE
C             
C*			  Plot the visibility in whole miles with
C*			  no fraction.
C
                          if ( isiz .eq. 2 ) ix = ix - 1
C
                          CALL GTEXT  ( 'P',  sx, sy, chbuf (1:isiz),
     +                                   0., ix, iy, ier )
C
                        ENDIF
C
		      ELSE
C
C*			Plot all other text.
C
C*			Retrieve active text attributes
C
			CALL GQTEXT ( i1, i2, r1, i3, i4, i5,
     +				      i6, ier )
C				 
                        tsmax = tsize (26)
                        IF ( tsize ( ip ) .lt. 0 ) THEN
C                      
C*			  Use the current text size for each offset.
C
                          tsiz = ABS ( tsize (ip) )
C
                          CALL GSTEXT ( i1, i2, tsiz, jwide (ip),
     +                                  i4, i5, i6, ier )
C
                          CALL GTEXT  ( 'P',  sx, sy, chbuf (1:isiz), 
     +				        0., ix, iy, ier )
C
                        ELSE
C
C*			  Use a constant text size for the offsets.
C
                          tsiz = tsize (ip) 
C
                          CALL GSTEXT ( i1, i2, tsmax, jwide (ip),
     +                                  i4, i5, i6, ier )
C
                          CALL GQTEXT ( i1, i2, sl, i3, i4,
     +				        i5, i6, ier )
                          CALL GQSYSZ ( xszmk, yszmk, xsztx1, ysztx1,
     +                                  xszwb, yszwb, ier )
                          CALL GSTEXT ( i1, i2, tsiz, jwide (ip),
     +                                  i4, i5, i6, ier )
                          CALL GQTEXT ( i1, i2, ss, i3, i4,
     +					i5, i6, ier )
                          CALL GQSYSZ ( xszmk, yszmk, xsztx2, ysztx2,
     +                                  xszwb, yszwb, ier )
                          term = 0.6666
                          IF ( tsiz .eq. tsmax ) term = 1.0
                          ratx = term * xsztx1/xsztx2
                          raty = term * ysztx1/ysztx2
                          irnd = 0.5
                          jrnd = 0.5
                          IF ( ix .lt. 0 ) irnd = -0.5
                          IF ( iy .lt. 0 ) jrnd = -0.5
                          ixsm = ix * ratx + irnd
                          jysm = iy * raty + jrnd
                          IF ( ixs .eq. 0 ) ixsm = ix
                          if(ip .eq. 11) ixsm = -2
C
                          CALL GQTEXT (i1, i2, sq, i3, i4,
     +				       i5, i6, ier )	
C
                          CALL GTEXT ( 'P', sx, sy, chbuf (1:isiz),
     +                                  0., ixsm, jysm, ier )
C
                        END IF
C
                      END IF
		END IF
	    END IF
	END DO
C*
	RETURN
	END
