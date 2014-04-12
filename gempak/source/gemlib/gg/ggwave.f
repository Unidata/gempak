	SUBROUTINE GG_WAVE ( filtyp, dattim, maxback, kwninc, kcolrs,
     +			     numc, mrktyp, sizmrk, mrkwid, iskip,
     +			     interv, itmclr, iret )
C************************************************************************
C* GG_WAVE								*
C*									*
C* This subroutine sets up the times and attributes for plotting	*
C* Significant Wave Height or Sea-Surface Height Anamoly data.		*
C*									*
C* GG_WAVE ( FILTYP, DATTIM, KWNINC, KCOLRS, NUMC, MRKTYP, SIZMRK,	*
C*	     MRKWID, ISKIP, INTERV, ITMCLR, ITMWID, IRET )		*
C*									*
C* Input parameters:							*
C*	FILTYP		CHAR*		File type; i.e., 'SGWH', 'SSHA'	*
C*	DATTIM		CHAR*		Ending time for WAVE		*
C*	MAXBACK		CHAR*		Time back to display (mins)	*
C*	KWNINC(*)	INTEGER		WAVE increments			*
C*	KCOLRS(*)	INTEGER		Color for each WAVE increment	*
C*	NUMC		INTEGER		Number of WAVE intervals	*
C*	MRKTYP		INTEGER		Marker type			*
C*	SIZMRK		REAL		Marker size			*
C*	MRKWID		INTEGER		Marker width			*
C*	ISKIP		INTEGER		Skip value			*
C*	INTERV		INTEGER		Time stamp interval		*
C*	ITMCLR		INTEGER		Time stamp color		*
C*	ITMWID		INTEGER		Line width of time stamp	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* T. Piper/SAIC	03/06	Modeled after gg_qsct.			*
C* G. McFadden/SAIC	 1/08	Added support for ENVI (SGWHE) and GFO	*
C*                              (SGWHG) significant wave height.        *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C* G. McFadden/SAIC	 6/08	Added support for Jason2 (SGWH2)	*
C*                              significant wave height.	        *
C* G. McFadden/IMSG	 7/11	Added support for CRYOSAT (SGWHC)	*
C*                              significant wave height.	        *
C* G. McFadden/IMSG	 7/13	Added support for Altika         	*
C*                              significant wave height (SGWHA) and 	*
C*                              wind speed (WSPDA).                 	*
C* G. McFadden/IMSG     11/13   Moved support for Altika windspeed to   *
C*                              GG_WSPD                                 *
C* M. James/Unidata      4/14   Added input parm for maximum time back  *
C*                              to display.                             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filtyp, dattim, maxback
	INTEGER		kwninc (*), kcolrs (*)
C*
	CHARACTER	path*25, templ*(MXTMPL), cday*3, cdttm*20,
     +			dattm2*20, tfile*128, cyyyy*4,
     +			flstrt*160
	CHARACTER*(MXFLSZ)      filnam, files(MXNMFL), fnull
	CHARACTER cstmin*(20), cval*(20), stime*(20)
	INTEGER		icolrs(LLCLEV), itarr(5), jtarr(5), idtarr(3)
	REAL		shainc(LLCLEV)
        LOGICAL         done
C-----------------------------------------------------------------------
	IF ( filtyp .ne. "SGWH" .and. filtyp .ne. "SSHA" .and.
     +       filtyp .ne. "SGWHG" .and. filtyp .ne. "SGWHE" .and.
     +       filtyp .ne. "SGWH2" .and. filtyp .ne. "SGWHC" .and.
     +       filtyp .ne. "SGWHA" )  THEN
	    print *, "filtyp '", filtyp, "' unknown!"
	    iret = -1
	    return
	END IF
	iret = 0
C
	numclr = numc
	IF ( interv .le. 0 ) interv = 30
	CALL ST_LCUC ( filtyp, filtyp, ier )
	IF ( numclr .gt. 0 ) THEN
 	    DO ii = 1, numclr
		shainc ( ii ) = kwninc ( ii )
		icolrs ( ii ) = kcolrs ( ii )
	    END DO
	ELSE
C
C*  No speeds or colors were specified.  Set defaults.
C
	    numclr = 1
	    shainc ( 1 ) = 200.
	    icolrs ( 1 ) = 3
	END IF
	shainc (numclr+1) = shainc (numclr)
	icolrs (numclr+1) = 0
        nexp   = MXNMFL
	iorder = 1
C
C*  Check for the last file requested by the user.
C
	CALL ST_LCUC ( dattim, dattim, ier )
	itype = 1
	IF  ( dattim .eq. 'LAST' ) THEN
	    CALL CSS_GTIM ( itype, dattm2, ier )
	  ELSE IF ( dattim .eq. 'ALL' ) THEN
	    RETURN
	  ELSE
	    CALL CSS_GTIM ( itype, cdttm, ier )
	    CALL TI_STAN ( dattim, cdttm, dattm2, ier )
	    IF ( ier .ne. 0 ) THEN
	    	CALL ER_WMSG ( 'TI', ier, dattim, ierr )
	    	iret = ier
	    	return
	    ENDIF	    	
	END IF
C
C*      Compute stime, the start time of the range by subtracting
C*      minutes in SAT_WIND_START from the frame time.
C
        cval = ' '
        IF ( maxback .eq. '' ) THEN
                CALL ST_NULL ( 'SAT_WIND_START', cstmin, lens, ier )
                CALL CTB_PFSTR ( cstmin, cval, ier1 )
                CALL ST_NUMB ( cval, mins, ier2 )
                IF ((ier1.ne.0) .or. (ier2.ne.0) .or. (mins.lt.0)) THEN
                    mins = 6 * 60
                END IF
        ELSE
                cstmin = maxback
                CALL ST_NUMB ( cstmin, mins, ier2 )
        END IF
        CALL TI_CTOI ( dattm2, itarr, ier )
        CALL TI_SUBM ( itarr, mins, jtarr, ier )
        CALL TI_ITOC ( jtarr, stime, ier )
C
C*      Compute the new end time of the range by adding
C*      minutes in SAT_WIND_END to the frame time.
C
        CALL ST_NULL ( 'SAT_WIND_END', cstmin, lens, ier )
        cval = ' '
        CALL CTB_PFSTR ( cstmin, cval, ier1 )
        CALL ST_NUMB ( cval, mins, ier2 )
        IF ( (ier1 .ne. 0) .or. (ier2 .ne. 0) .or. (mins .lt. 0) ) THEN
            mins = 0
        END IF
        CALL TI_ADDM ( itarr, mins, jtarr, ier )
        CALL TI_ITOC ( jtarr, dattm2, ier )
C
C*  Set the color attributes.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GSCOLR(itmclr, ier)
C
C*  Set the marker attributes for SSHA data.
C
	IF ( filtyp .eq. 'SSHA' )  THEN
	    CALL GQMRKR ( imark, imkhw, szmark, imkwid, ier )
	    IF  ( sizmrk .le. 0.0 )  THEN
                sizmrk =  0.20
            END IF
            IF  ( mrkwid .le. 0 )  THEN
                mrkwid = 1
            END IF
	    CALL GSMRKR ( mrktyp, imkhw, sizmrk, mrkwid, ier )
	END IF
C
C*  Set the text attributes for the time stamps.
C
	CALL GQTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr,
     +                irrotn, ijust, ier )
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
C*  Scan the directory for all of the data files.
C
	CALL ST_NULL ( filtyp, fnull, nf, ier )
	path  = ' '
	templ = ' '
	CALL CTB_DTGET ( fnull, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
	CALL ST_RNUL ( path, path, lens, ier )
	CALL ST_RNUL ( templ, templ, lens, ier )
	CALL ST_LSTR ( path, lenp, ier )

	CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, ier )
C
C*  Display altimeter (Significant Wave Height) or Altika
C*  wind speed data.
C
	IF ( ier .eq. 0 .and. ( filtyp .eq. "SGWH" .or. 
     +       filtyp .eq. "SGWH2" .or. filtyp .eq. "SGWHE" .or.
     +       filtyp .eq. "SGWHG" .or. filtyp .eq. "SGWHC" .or.
     +       filtyp .eq. "SGWHA" )   ) THEN
C
C*  This data source exists.  Make the file name for the
C*  last file requested.
C
            CALL FL_MNAM ( dattm2, templ, filnam, ier )
C
C*          Find the earliest file to start searching.
C
            CALL FL_MNAM ( stime, templ, flstrt, ier )
C
C*          Decode each file until the end time is reached.
C
            done = .false.
            ifl  = 1
            DO WHILE ( ( ifl .le. nfile ) .and. ( .not. done ) )
                IF  ( files(ifl) .gt. filnam )  THEN

                    done = .true.
                ELSE
                    IF  ( files(ifl) .ge. flstrt )  THEN
                        tfile = path(:lenp) // '/' // files(ifl)
                        CALL ST_NULL ( tfile,  tfile,  lens, ier )
                        CALL GG_RWAV ( fnull, tfile, shainc,
     +                                 icolrs, numclr, iskip,
     +                                 interv, itmclr, ier )    
		    END IF
		END IF
            ifl = ifl + 1
	    END DO
C
C*  Display SSHA data.
C
	ELSE IF ( ier .eq. 0 .and. filtyp .eq. "SSHA" ) THEN
            CALL TI_C2I( dattm2, iyyyy, immdd, ihhmm, ier)
            DO ii = 1, nfile
                CALL ST_INCH(iyyyy, cyyyy, ier)
                ipos = INDEX(files(ii), cyyyy)
                IF ( ipos .gt. 0 )  THEN
                    ipos2 = INDEX(files(ii), "_")
                    cday = files(ii)(ipos2+1:ipos2+ 4)
                    CALL ST_INTG(cday, jday, ier)
                    CALL TI_JTOI(iyyyy, jday, idtarr, ier)
                    imnth = immdd/100
                    iday = immdd - imnth*100
                    IF ( ( idtarr(2) .eq.  imnth ) .and.
     +                   ( idtarr(3) .eq.  iday ) )  THEN
                        tfile = path(:lenp) // '/' // files(ii)
                        CALL ST_NULL ( tfile,  tfile,  lens, ier )
                        CALL GG_RWAV ( fnull, tfile, shainc,
     +                                 icolrs, numclr, iskip,
     +                                 interv, itmclr, ier )
                    END IF
                END IF
	    END DO
	END IF
C
C*  Draw the color bar.
C
	CALL GG_CBAR ( '1/H/UR/.95;.95/.50;.01/1|1/21//111///hw',
     +				numclr, shainc, icolrs, ier )
C
C*  Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	IF ( filtyp .eq. 'SSHA' )  THEN
	    CALL GSMRKR ( imark, imkhw, szmark, imkwid, ier )
	END IF
	CALL GSTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr,
     +			irrotn, ijust, ier )
C
	RETURN
	END
