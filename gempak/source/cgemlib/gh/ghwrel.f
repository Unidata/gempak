	SUBROUTINE GH_WREL  ( origc, ddev, parms, data, ntime, xmndst, 
     +                        xval, iyaxis, prblvl, itwnd, iend, iret )
C************************************************************************
C* GH_WREL								*
C*									*
C* This subroutine plots real data for wind speed forecast and 		*
C* probability TPC graphic.						*
C*									*
C* GH_WREL  ( ORIGC, DDEV, PARMS, DATA, NTIME, XMNDST, XVAL, IYAXIS,	*
C*	      PRBLVL, ITWND, IEND, IRET )				*
C*									*
C* Input parameters:							*
C*	ORIGC		CHAR*		Issuing center			*
C*	DDEV		CHAR*		Device parameter		*
C*	PARMS		CHAR*		Parm*condition			*
C*	DATA (NTIME)	REAL		Data				*
C*	NTIME		INTEGER		Number of times			*
C*	XMNDST		REAL		Max space for connecting pts	*
C*	XVAL (NTIME)	REAL		Points on x axis		*
C*      IYAXIS (5,*)	INTEGER         Forecast lines			*
C*	PRBLVL (*)	INTEGER		Probability lines' labels	*
C* 	ITWND		INTEGER		Total number of forecast winds  *
C*	IEND		INTEGER	        All points for probability lines*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* A. Hardy/GSC		 4/01	Modified from SFXREL			*
C* A. Hardy/GSC		 6/01	Added GQLINE, modified GSLINE		*
C* A. Hardy/GSC		 6/01	Added color tag; fixed prolog		*
C* A. Hardy/SAIC	 8/01	Added ck for PS for 'NHC' line width    *
C* D. Kidwell/NCEP       4/02   Dropped iwht, iblk from call sequence   *
C* m.gamazaychikov/SAIC	06/06	Added wocen to CS, change lblstr setting*
C* S. Gilbert/NCEP       7/06   changed storm id (wocen) to issuing     *
C*                              center (origc)                          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parms, ddev, origc
	REAL		xval (*), data (*) 
        INTEGER		iyaxis(5,*), prblvl(*)
C*
	CHARACTER	condtn*8, ppp, lblstr*4, coltag*33
	REAL		xxx (LLMXTM), yyy (LLMXTM), ytmp (LLMXTM),
     +                  xlab(2), ylab(2)
C------------------------------------------------------------------------
	iret  = 0
	xdist = 3. * xmndst
C
        CALL GQLINE ( jltyp, jlthw, jwidth, jwhw, ier )
C
C*	Get the condition and parameter parts.
C
	condtn = parms (5: )
	ppp    = parms
C
C*	Get conditions.
C
	CALL GH_WCND  ( condtn, size, iwidth, ier )
	lintyp = size
C
C*	Set the line type and width.
C
	CALL GQLINE  ( ilntyp, ilthw, jwidth, iwhw, iret )
	CALL GSLINE  ( lintyp, 0, iwidth, 0, iret )
C
C*	Loop through points connecting lines.
C
        ii = 1
        DO npt = 1, iend
 		yyy (npt) = data (npt)
            IF ( npt .lt. itwnd+1 )  THEN
                xxx (npt) = xval (npt)
              ELSE IF ( npt .eq. itwnd+1)  THEN
                xxx (npt) = xval (npt-1)
              ELSE IF ( (npt .gt. itwnd+1) .and. (npt .lt. iend ) )THEN
                ii = ii + 2
                xxx (npt) = xval (npt - ii )
              ELSE IF ( npt .eq. iend )  THEN
                xxx (npt) = xval (1)
            END IF
        END DO
C
C*	Draw fills and probability lines.
C
	IF  ( npt .gt. 0 )  THEN
	    CALL GFILL  ( 'M', iend, xxx, yyy, ier )
C
C*          Set up line array. 
C
            DO jj = 1,5
                DO ii = 1, itwnd
                    ytmp(ii) = iyaxis(jj,ii)
                END DO
C
C*		Draw probability and forecast lines.
C
		coltag = 'i_forecast_lines'
       		CALL ST_LSTR ( coltag, lens, ier )
        	CALL GH_COLR ( coltag(:lens), 4, ier)
C
                IF ( jj .lt. 5 ) lwidth = 1
                IF ( jj .eq. 5 ) THEN
                    IF  ( ddev(:2) .ne. 'PS') THEN
                        lwidth = 3
                      ELSE 
                        lwidth = 12
                    END IF
                END IF
	        CALL GSLINE  ( lintyp, 0, lwidth, 0, iret )
	        CALL GLINE  ( 'M', itwnd, xval, ytmp, ier )
C
C*              Place labels.  The label color will be vanilla except
C*		for postscript, which will be black.
C
                CALL GSCOLR ( 1, ier )
                CALL GSTEXT ( 2, 2, 1.286, 1, 121, 1, 2, ier )
                xlab(1) = ( xval(2) + xval(3) ) / 2
                ylab(1) = ( ytmp(2) + ytmp(3) ) / 2
                IF ( itwnd .eq. 6 ) THEN
                    xlab(2) = ( xval(5) + xval(6) ) / 2
                    ylab(2) = ( ytmp(5) + ytmp(6) ) / 2
                    ifini = 2
                  ELSE
                    ifini = 1
                    IF ( ntime .eq. 5 ) THEN
                        xlab(1) = ( xval(1) + xval(2) ) / 2
                        ylab(1) = ( ytmp(1) + ytmp(2) ) / 2
                    END IF
                END IF 
C
                IF ( (jj .eq. 1 ) .or. ( jj .eq. 4) ) THEN
                    lblstr = '10%'
                  ELSE IF ( (jj .eq. 2 ) .or. ( jj .eq. 3) )THEN
                    IF ( (prblvl(jj) .eq. 2) .or. 
     +                     (prblvl(jj) .eq. 5) )   lblstr = '20%'
                    IF ( (prblvl(jj) .eq. 3) .or. 
     +                     (prblvl(jj) .eq. 4) )   lblstr = '30%'
                  ELSE IF  (jj .eq. 5 ) THEN
                    lblstr = 'NHC'
                    IF ( origc (:4) .eq. 'CPHC' ) lblstr = 'CPHC'
                END IF
                DO nn = 1, ifini
 		    CALL GTEXT  ( 'M', xlab(nn), ylab(nn), 
     +                            lblstr, 0.0,0,0, ier)
                END DO
            END DO
	END IF
C
C*	Reset line attributes.
C
        CALL GSLINE ( jltyp, jlthw, jwidth, jwhw, ier )
C*
	RETURN
	END
