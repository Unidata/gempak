	SUBROUTINE GG_WPLT ( filtyp, timstr, timstp, dattim, dattm4, 
     +                     timisu, wtype, mrktyp, ssize, iwidth, icolor, 
     +			   icolr2, itest, icancl, iflags, num, nwtch, 
     +			   wstn, npt, nfips, cnnam, rlat, rlon, iret )
C************************************************************************
C* GG_WPLT								*
C*									*
C* This subroutine plots markers and/or outlines and/or fill the union 	*
C* or counties for either the current weather watch outline update 	*
C* (WOU) or the current weather watch county notifications (WCN).	*
C*									*
C* GG_WPLT ( FILTYP, TIMSTR, TIMSTP, DATTIM, DATTM4, TIMISU, WTYPE, 	*
C*	     MRKTYP, SSIZE, IWIDTH, ICOLOR, ICOLR2, ITEST, ICANCL, 	*
C*	     IFLAGS, NUM, NWTCH, NPT, NFIPS, CNNAM, RLAT, RLON, IRET )	*
C*									*
C* Input parameters:							*
C*      FILTYP          CHAR*           File type 'WOU' or 'WCN'	*
C*	TIMSTR (NUM)	CHAR*		Starting time watch		*
C*	TIMSTP (NUM)	CHAR*		Ending time watch		*
C*	DATTIM		CHAR*		End time for watch		*
C*	DATTM4		CHAR*		Ending time (YYYYMMDD/HHNN)	*
C*	DATISU (NUM)	CHAR*		Issued time			*
C*	WTYPE  (NUM)	CHAR*		Watch type			*
C*	MRKTYP (NUM)	INTEGER		Marker symbol types		*
C*	SSIZE  (NUM)	REAL		Marker symbol sizes		*
C*	IWIDTH (NUM)	INTEGER		Marker symbol widths		*
C*      ICOLOR (NUM)    INTEGER         Marker and outline colors       *
C*      ICOLR2 (NUM)    INTEGER         Fill colors                     *
C*      ITEST  (NUM)	INTEGER		Test flag			*
C*	ICANCL (NUM)    INTEGER		Cancellation flag		*
C*	IFLAGS (7)	INTEGER		Flags for labels, outline, etc.	*
C*					  0 = false			*
C*					  1 = true			*
C*	NUM		INTEGER		Number of markers		*
C*	NWTCH		INTEGER		Number of decoded reports 	*
C*	WSTN (NWTCH)	CHAR*		Watch numbers			*
C*	NPT  (NWTCH)	INTEGER		Number of counties		*
C*      NFIPS(NPT, NWTCH) CHAR*         Fips numbers array		*
C*      CNNAM(NPT, NWTCH) CHAR*         County names array		*
C*      RLAT(NPT, NWTCH)  FLOAT         County centroid lats. array	*
C*      RLON(NPT, NWTCH)  FLOAT         County centroid lons. array	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC     	03/03	Modified from GG_WARN			*
C* M. Li/SAIC		03/03	Set colors & marker selection 		*
C* S. Jacobs/NCEP	 3/03	Removed check for plotting Test prods	*
C* M. Li/SAIC		 4/03  	Set colors for watch xxx0		*
C* S. Jacobs/NCEP	 4/03	Changed check for last digit		*
C* A. Hardy/NCEP	 3/04   Changed CNTY_BNDS -> WBCMZ_BNDS		*
C* M. Li/SAIC           01/05   Added new input parameter ICOLR2        *
C* F. J. Yen/NCEP	 4/05   Added union flag			*
C* F. J. Yen/NCEP	 5/05   Disabled union flag			*
C* F. J. Yen/NCEP	 8/05   Re-enabled union flag; rm duplicates for*
C*				performance; get new label pos for union*
C* M. Li/SAIC		07/06	Add while loop condition irt = 2	*
C* F. J. Yen/NCEP	 1/08	Correct dimen in prolog. Increase MXW.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
    	PARAMETER	( NW = LLMXPT, NC = 500, MXW = 1000 )
C*
	PARAMETER	( JTSM = 1, JTOR = 2  )
	PARAMETER	( JTIM = 1, JLAB = 2, JNUM = 3, JCLC = 4, 
     +			  JMKR = 5, JOUT = 6, JFIL = 7, JUNN = 8 )
C*
	CHARACTER*(*)	filtyp, dattim, dattm4, timstr(*), timstp(*), 
     +			wtype(*), cnnam(NC,*), wstn(*), timisu(*) 
	INTEGER		icolor(*), mrktyp(*), iwidth(*), iflags(*), 
     +                  itest(*), icancl(*), npt(*), nfips(NC,*),
     + 			icolr2(*) 
	REAL		ssize(*), rlat(NC,*), rlon(NC,*)
	REAL		px(NW), py(NW)
C*
	CHARACTER	wlabel*80, tmstp4*20, tmstr4*20
C*
	CHARACTER	cfips*12, bndtyp*30, ulabl(MXW)*80
	INTEGER		nskip(NC), mrktst(17:21)
	INTEGER		iuyoff(MXW), iuclr(MXW)
	REAL		xcl(MXW), ycl(MXW)
C*     
        DATA            mrktst / 2, 3, 4, 6, 14 /
C-----------------------------------------------------------------------
	iret = 0
C
C*	Query current settings.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GQLINE ( jtyp, jthw, jwid, jwhw, ier )
	CALL GQMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
	CALL GQTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
C
C*	Set attributes and defaults.
C
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C 
	DO  i = 1, num
            IF  ( ( mrktyp(i) .lt. 17 ) .or.
     +            ( mrktyp(i) .gt. 21 ) ) THEN
            	IF ( filtyp .eq. 'WOU' ) THEN
                    mrktyp(i) = 19
                  ELSE
                    mrktyp(i) = 17
                END IF
            END IF
C
	    IF  ( ( ssize(i) .le.  0.0 ) .or.
     +		  ( ssize(i) .gt. 10.0 ) )  ssize(i) = 1.0
C
	    IF  ( ( iwidth(i) .le.  0 ) .or.
     +		  ( iwidth(i) .gt. 10 ) )  iwidth(i) = 3
	END DO
C
C*	Find the unique watch numbers for WOU
C
	kk = 0
     	IF ( filtyp .eq. 'WOU' .and. nwtch .gt. 1 ) THEN	
          DO ii = 1, nwtch -1
            DO jj = ii+1, nwtch
                IF ( wstn(jj) .eq. wstn(ii) ) THEN
                    CALL TI_DIFF ( timisu(jj), timisu(ii), mindif, ier )
                    IF ( mindif .ge. 0 ) THEN
			kk = kk + 1
			nskip(kk) = ii
		    ELSE IF ( mindif .lt. 0 ) THEN
			kk = kk + 1
			nskip(kk) = jj
		    END IF
		END IF
	    END DO
	  END DO
	END IF
C
C*	Plot the graphic for each valid report.
C
	nunn= 0
	ioutsv = iflags(JOUT)
	ilabsv = iflags(JLAB)
	IF ( iflags(JUNN) .eq. 1 ) iflags(JLAB) = 0
 	DO ip = 1, nwtch
	    IF ( kk .gt. 0 ) THEN
		DO ii = 1, kk
		    IF ( ip .eq. nskip(ii) ) GOTO 900
		END DO
	    END IF
	    IF ( icancl(ip) .ne. 0 ) GOTO 900
C
	    CALL TI_DTM4 ( timstp ( ip ), tmstp4, ier )
	    CALL TI_DTM4 ( timstr ( ip ), tmstr4, ier )

 	    IF  ( ( dattim .eq. 'ALL' ) .or.
     +		  ( ( tmstp4 .gt. dattm4 ) .and.
     +		    ( tmstr4 .le. dattm4 ) ) )  THEN
C
C*		Set the color and marker
C
		IF  ( wtype(ip) .eq. 'TOR' .or.
     +				wtype(ip) .eq. 'TN' )  THEN
		    jtype = JTOR
		  ELSE IF  ( wtype(ip) .eq. 'TSM' .or.
     +				wtype(ip) .eq. 'TS' )  THEN
		    jtype = JTSM
		  ELSE IF  ( wtype(ip) .eq. 'SVR' .or.
     +                          wtype(ip) .eq. 'SV' )  THEN
                    jtype = JTSM
		  ELSE
		    jtype = 0
		END IF
C
		IF ( jtype .gt. 0 ) THEN
C
C*		    Set colors, based on the last digit of the watch 
C*		    number if the color code is true, otherwise, 
C*		    based on watch type (default).
C
		    CALL ST_NUMB ( wstn (ip), iwn, ier ) 
		    IF ( iflags(JCLC) .eq. 0 ) THEN 
			idx = jtype
		      ELSE
		    	idx = MOD(iwn,10) + 3
		    END IF
		    ic = icolor ( idx )
		    ifc = icolr2 ( idx )
C
C*		    Set marker based on type of watch and itest
C
		    IF ( itest ( ip ) .eq. 0 ) THEN
			mrkr = mrktyp ( idx )
		      ELSE
			mrkr = mrktst ( mrktyp ( idx ) )
		    END IF

		    CALL GSMRKR ( mrkr, 0, ssize(idx), iwidth(idx), 
     +			          ier )
		  ELSE
		    ic = 0
		END IF
C
C*		Plot each county in the WOU or WCN area.
C
		IF  ( ic .ne. 0 )  THEN
		  CALL GSCOLR ( ic, ier )
		  IF ( npt(ip) .gt. NC ) THEN
		      npt(ip) = NC
      		      CALL ER_WMSG ( 'GG', 2, 'GGWPLT', ierr )
		  END IF
                  IF ( npt (ip) .gt. 0 ) THEN
		      IF ( iflags(JUNN) .eq. 0) THEN
		        IF ( iflags(JFIL) .eq. 1 ) THEN
C
C*		          Plot county fill.
C
		          DO  im = 1, npt(ip)
			      CALL ST_INCH ( nfips(im,ip), cfips, ier )
                              bndtyp = 'WBCMZ_BNDS|<FIPS>'//cfips
                              CALL GPLBND ( bndtyp,
     +                              ifc,0,0,0,0,0,0,0,
     +                              ' ',0,0,0.,0, ier )
		          END DO
		        END IF
		       ELSE
C
C*		        Union flag is set, so get union of FIPS in a watch
C
C*			For now if union is set and both
C*			outline and fill are not, set to outline
C
			IF ( iflags(JFIL) .eq. 0 .and.
     +			         iflags(JOUT) .eq. 0 ) THEN
			    iflags(JOUT) = 1
			END IF
			
		        np = npt (ip)
     		        irt = 0
			areasv = 0.
		        DO WHILE ( irt .eq. 0 .or. irt .eq. 2 )			
			  nwm1 = NW - 1
		          CALL GG_WLSO ( np, nfips(1,ip), nwm1, nunion,
     +			    nv, px, py, xcn, ycn, area, irt )
			  np = 0			
		          IF ( irt .eq. 0 ) THEN
C 
C*			    Save the centroid of the largest polygon
C*			    for labeling unions
C
			    if ( area .lt. 0. ) area = -area
			    IF ( area .gt. areasv ) THEN
				areasv = area
				xc = xcn
				yc = ycn
			    END IF
			    CALL GQCOLR ( kcolr, ier )
  			    IF ( iflags(JFIL) .eq. 1 ) THEN
C
C*			      Fill Union
C
			      CALL GSCOLR ( ifc, ier )
			      CALL GFILL ( 'M', nv, px, py, ier )
			    END IF
			    IF ( iflags(JOUT) .eq. 1 ) THEN
C
C*			      Outline Union
C
			      nv = nv + 1
			      px (nv) = px (1)
			      py (nv) = py (1)
			      CALL GSCOLR ( ic, ier )
			      CALL GSLINE ( 1, 0, iwidth (idx), 0, ier )
			      CALL GLINE ( 'M', nv, px, py, ier )
			    END IF
			    CALL GSCOLR ( kcolr, ier )
			   ELSE IF ( irt .le. -21) THEN
			    CALL ER_WMSG ( 'GG', irt, ' ', ier )
			    IF ( irt .le. -22 ) RETURN
				CALL GSCOLR ( jcolr, ier )
        			CALL GSLINE ( jtyp, -1, jwid, -1, ier )
				CALL GSMRKR ( jmark, jmkhw, szmark,
     +					jmkwid, ier )
				CALL GSTEXT ( jtxfn, jtxhw, siztx,
     +					jtxwid, jbrdr,
     +		      			jrrotn, jjust, ier )
				iflags(JOUT) = ioutsv
				iflags(JLAB) = ilabsv
		          END IF
       		        END DO
C
		      
		      END IF
		      CALL ST_LSTR ( wstn (ip), lenwnm, ier)
		      DO  im = 1, npt(ip)
C
C*		          Draw the marker or outline the county.
C
			  CALL ST_INCH ( nfips(im,ip), cfips, ier )
			  bndtyp = 'WBCMZ_BNDS|<FIPS>'//cfips
C
C*			  Removed check for ITEST. This check used
C*			  to force Test products to always plot
C*			  as a marker. Since all of the WOU and WCN
C*			  products are Tests until 10/2003, removing
C*			  the restriction will allow the user to
C*			  choose to plot the county outlines.
C
C
C*			  Plot marker.
C
			  IF ( iflags(JMKR) .eq. 1 )
     +			    CALL GPLBND ( bndtyp,
     +				0,0,0,0,0, 0,0,0, 
     +				'MARK', ic, mrkr, ssize(idx), 
     +				iwidth(idx), ier)
C
C*			  If outline flag is set but union flag
C*			  is not set, then plot outline of counties .
C
        		  IF ( iflags(JOUT) .eq. 1 .and.
     +			       iflags(JUNN) .eq. 0 ) 
     +			    CALL GPLBND ( bndtyp, 
     +				0,0,0,0,0, ic, 1, iwidth(idx),
     +			        ' ',0,0,0.,0, ier)
C
C*		          Plot the text.
C
			  CALL ST_LSTR ( cnnam (im,ip), lencn, ier )
C
		            IF ( ( iflags(JNUM) .eq. 0 ) .and.
     +			         ( iflags(JTIM) .eq. 0 ) .and. 
     +			         ( iflags(JLAB) .eq. 0 ) ) THEN
			        iyoff  = -1
			        wlabel = ' '
C
		              ELSE IF  ( ( iflags(JNUM) .eq. 0 ) .and.
     +                                   ( iflags(JTIM) .eq. 0 ) .and. 
     +				         ( iflags(JLAB) .ne. 0 ) ) THEN
			        iyoff  = -2
			        wlabel = cnnam(im,ip)(:lencn)
C
		              ELSE IF ( ( iflags(JNUM) .eq. 0 ) .and.
     +                                  ( iflags(JTIM) .ne. 0 ) .and. 
     +				        ( iflags(JLAB) .eq. 0 ) ) THEN
			        iyoff  = -3
			        wlabel = timstr(ip) (8:11) // '-' //
     +				 timstp(ip) (8:11)
C
		              ELSE IF ( ( iflags(JNUM) .eq. 0 ) .and.
     +                                  ( iflags(JTIM) .ne. 0 ) .and. 
     +				        ( iflags(JLAB) .ne. 0 ) ) THEN
			        iyoff  = -3
			        wlabel = cnnam(im,ip) (:lencn) // CHCR
     +				     // timstr(ip) (8:11) // '-' //
     +				     timstp(ip) (8:11)
C
                              ELSE IF ( ( iflags(JNUM) .ne. 0 ) .and.
     +                                  ( iflags(JLAB) .eq. 0 ) .and.
     +					( iflags(JTIM) .eq. 0 ) ) THEN
                                iyoff  = -2
                                wlabel = wstn(ip)(:lenwnm) 
C
                              ELSE IF ( ( iflags(JNUM) .ne. 0 ) .and.
     +                                  ( iflags(JLAB) .ne. 0 ) .and.
     +                                  ( iflags(JTIM) .eq. 0 ) ) THEN
                                iyoff  = -3
                                wlabel = wstn(ip)(:lenwnm) // CHCR
     +				      // cnnam(im,ip)(:lencn)
C
                              ELSE IF ( ( iflags(JNUM) .ne. 0 ) .and.
     +                                  ( iflags(JLAB) .eq. 0 ) .and.
     +                                  ( iflags(JTIM) .ne. 0 ) ) THEN
                                iyoff  = -3
                                wlabel = wstn(ip)(:lenwnm) // CHCR
     +                                // timstr(ip) (8:11) // '-' // 
     +                               timstp(ip) (8:11) 
C
                              ELSE IF ( ( iflags(JNUM) .ne. 0 ) .and.
     +                                  ( iflags(JLAB) .ne. 0 ) .and.
     +                                  ( iflags(JTIM) .ne. 0 ) ) THEN
                                iyoff  = -3
                                wlabel = wstn(ip)(:lenwnm) // CHCR
     +                                // cnnam(im,ip)(:lencn) // CHCR
     +                                // timstr(ip) (8:11) // '-' //
     +                                   timstp(ip) (8:11)
C
		            END IF
C
			    IF ( iflags(JUNN) .eq. 0 ) THEN 
		                CALL GTEXT ( 'M', rlat(im,ip),
     +				        rlon(im,ip), wlabel, 0.0, 0,
     +					iyoff, ier )
			      ELSE
C
C*				If union flag is set, then save 
C*				labeling info to be plotted later
C*				so that fill will not overwrite it.
C
				IF ( im .eq. 1 ) THEN
				  IF ( nunn .lt. MXW ) THEN
				    nunn = nunn + 1
				   ELSE
				    CALL ER_WMSG ( 'GG', 6, 'GGWPLT',
     +						   ierr )
				  END IF
				  IF ( areasv .le. 0. ) THEN
				    xcl(nunn) = rlat(1,ip)
				    ycl(nunn) = rlon(1,ip)
				   ELSE
				    xcl(nunn) = xc
				    ycl(nunn) = yc
				  END IF 
				  CALL ST_LSTR ( wlabel, lenstr, ier )
				  ulabl(nunn) = wlabel(1:lenstr)
				  iuclr(nunn) = ic
C
C*				  Adjust iyoff for union labels
C
				  iuyoff(nunn) = iyoff + 1
				END IF
			    END IF
		        END DO
		  END IF
		END IF
 	    END IF
900	END DO
C
	IF ( iflags(JUNN) .eq. 1 ) THEN
	    IF ( iflags (JNUM) .eq. 1 .or. iflags (JTIM) .eq. 1) THEN
C
C*		Label the union of each watch	
C

		DO ipn = 1, nunn
		    CALL GSCOLR ( iuclr(ipn), ier )
		    CALL GTEXT ( 'M', xcl(ipn), ycl(ipn), ulabl(ipn),
     +				 0.0, -2, iuyoff(ipn), ier )
		END DO
	    END IF
	END IF
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSLINE ( jtyp, jthw, jwid, jwhw, ier )
	CALL GSMRKR ( jmark, jmkhw, szmark, jmkwid, ier )
	CALL GSTEXT ( jtxfn, jtxhw, siztx, jtxwid, jbrdr,
     +		      jrrotn, jjust, ier )
	iflags(JOUT) = ioutsv
	iflags(JLAB) = ilabsv
C*
	RETURN
	END
