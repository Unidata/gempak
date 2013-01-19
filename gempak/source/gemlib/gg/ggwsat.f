	SUBROUTINE GG_WSAT ( filtyp, dattim, kwninc, kcolrs, kcolrs2,
     +	                     numc, brbsiz, ibwid, hdsiz, ityp, iskip,
     +			     interv, itmclr, itmwid, iflgs, iret )
C************************************************************************
C* GG_WSAT								*
C*									*
C* This subroutine sets up the times and attributes for plotting        *
C* WindSAT data.							*
C*									*
C* GG_WSAT ( FILTYP, DATTIM, KWNINC, KCOLRS, KCOLRS2, NUMC, BRBSIZ,	*
C*	     IBWID, HDSZ, ITYP, ISKIP, INTERV, ITMCLR, ITMWID, IFLGS,	*
C*	     IRET )							*
C*                                                                      *
C* Input parameters:							*
C*	FILTYP		CHAR*		File type; i.e., 'WSAT'		*
C*	DATTIM		CHAR*		Ending time for WindSAT data    *
C*	KWNINC(*)	INTEGER		Wind speed increments		*
C*	KCOLRS(*)	INTEGER		Color for each speed increment  *
C*      KCOLRS2(*)      INTEGER         Color for rain-flagged winds  	*
C*	NUMC		INTEGER		Number of wind intervals        *
C*	BRBSIZ		REAL		Wind arrow/barb size		*
C*	IBWID		INTEGER		Wind arrow/barb width		*
C*	HDSIZ		REAL		Wind arrow head size		*
C*	ITYP		INTEGER		Wind vector type		*
C*	ISKIP		INTEGER		Skip value			*
C*	INTERV		INTEGER		Time stamp interval		*
C*	ITMCLR		INTEGER		Time stamp color		*
C*	ITMWID		INTEGER		Line width of time stamp	*
C*	IFLGS(*)	INTEGER		Flag values for data 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* G. McFadden/SAIC	 6/06	Adapted from GG_QSCT			*
C* F. J. Yen/NCEP        7/07   Use start&end min in prefs.tbl for range*
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filtyp, dattim
	INTEGER		kwninc (*), kcolrs (*), kcolrs2(*), iflgs (*),
     +			interv, itmclr, itmwid
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20,
     +			tfile*128, stime*20, flstrt*160, dtime*20
	CHARACTER	label*6, cstmin*(20), cval*(20)
	CHARACTER*(MXFLSZ)      filnam, files (MXNMFL), fnull
	INTEGER		itarr(5), jtarr(5), icolrs (LLCLEV),
     +			iwninc (LLCLEV), icolrs2 (LLCLEV), itype
	REAL		fwninc(LLCLEV)
	LOGICAL		done
C-----------------------------------------------------------------------
	iret = 0
C
	numclr = numc
	CALL ST_LCUC ( filtyp, filtyp, ier )
	IF ( numclr .gt. 0 ) THEN
 	    DO ii = 1, numclr
		iwninc ( ii ) = kwninc ( ii )
		fwninc ( ii ) = kwninc ( ii )
		icolrs ( ii ) = kcolrs ( ii )
		icolrs2( ii ) = kcolrs2 ( ii )
	    END DO
	  ELSE
C
C*	    No speeds or colors were specified.  Set defaults.
C
	    numclr = 1
	    iwninc ( 1 ) = 200
	    fwninc ( 1 ) = 200.
	    icolrs ( 1 ) = 3
	    icolrs2 ( 1 ) = 31
	END IF
	fwninc (numclr+1) = fwninc (numclr)
	icolrs (numclr+1) = 0
	icolrs2 (numclr+1) = 0
        nexp   = MXNMFL
	iorder = 1
C
C*	Check for the last file requested by the user.
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
        CALL ST_NULL ( 'SAT_WIND_START', cstmin, lens, ier )
        cval = ' '
        CALL CTB_PFSTR ( cstmin, cval, ier1 )
        CALL ST_NUMB ( cval, mins, ier2 )
        IF ( (ier1 .ne. 0) .or. (ier2 .ne. 0) .or. (mins .lt. 0) ) THEN
            mins = 6 * 60
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
C*	Get the wind and text attributes.
C
	CALL GQCOLR ( jcolr, ier )

C
C*	Set the wind attributes for the WindSAT data.
C
	IF  ( brbsiz .le. 0.0 )  THEN
	    brbsiz =  0.20
	END IF
	IF  ( ibwid .le. 0 )  THEN
	    ibwid = 1
	END IF
	IF ( ityp .eq. 0 ) THEN
	    IF ( hdsiz .le. 0. ) THEN
		ityp = 2
	      ELSE
		ityp = 1
	    END IF
	  ELSE
	    IF ( ityp .lt. 0 .or. ityp .gt. 5 ) ityp = 5
	    IF ( ityp .ne. 2 .and. hdsiz .le. 0. ) hdsiz = .4 
	END IF
	IF ( ityp .eq. 2 .or. ityp .eq. 5 ) THEN
	    CALL GQBARB ( szbarb, jbrwid, jbrtyp, ier )
	  ELSE IF ( ityp .eq. 1 .or. ityp .eq. 3 ) THEN
	    CALL GQDARR ( szdarr, szdrhd, jdrwid, jdrtyp, ier )
	  ELSE
	    CALL GQARRW ( szarrw, szarhd, jarwid, jartyp, ier )
	END IF
        CALL GQSKY ( szsky, jsktyp, jskwid, ier )
	CALL GQTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr,
     +                irrotn, ijust, ier )
	IF ( ityp .eq. 2 .or. ityp .eq. 5 ) THEN
	    CALL GSBARB ( brbsiz, ibwid, 132, ier )
	  ELSE IF ( ityp .eq. 1 .or. ityp .eq. 3 ) THEN
	    CALL GSDARR ( brbsiz, hdsiz, ibwid, 232, ier )
	  ELSE
	    CALL GSARRW ( brbsiz, hdsiz, ibwid, 232, ier )
	END IF
        CALL GSSKY( brbsiz, 2, ibwid, ier )
C
C*	Set the line attributes for the time stamps.
C
	IF  ( itmwid .eq. 0 )  THEN
            itmwid = 1
        END IF
	CALL GSLINE ( 1, 0, itmwid, 0, ier )
	CALL GSTEXT ( 21, 2, 1.0, 1, 111, 1, 1, ier )

	IF ( interv .le. 0 ) interv = 30
C
C*	Scan the directory for all of the WindSAT data files.
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
	IF ( ier .eq. 0 ) THEN
C
C*	    This data source exists.  Make the file name for the
C*	    last file requested.
C
	    CALL FL_MNAM ( dattm2, templ, filnam, ier )
C
C*	    Find the earliest file to start searching.
C
	    CALL FL_MNAM ( stime, templ, flstrt, ier )
C
C*	    Decode each file until the end time is reached.
C
	    done = .false.
	    ifl  = 1
	    DO WHILE ( ( ifl .le. nfile ) .and. ( .not. done ) )
		IF  ( files(ifl) .gt. filnam )  THEN
		    done = .true.
		  ELSE
		    IF  ( files(ifl) .ge. flstrt )  THEN
			tfile = path(:lenp) // '/' // files(ifl)
			CALL FL_MDAT ( files(ifl), templ, dattm2,
     +				       dtime, ier )
C
C*			Read and plot the WindSAT data.
C
			CALL ST_NULL ( tfile,  tfile,  lens, ier )
			CALL ST_NULL ( stime,  stime,  lens, ier )
			CALL ST_NULL ( dattm2, dattm2, lens, ier )
			CALL ST_NULL ( dtime,  dtime,  lens, ier )
C
			CALL GG_WSRD ( filtyp, tfile, stime, dattm2,
     +			       dtime, fwninc, icolrs, icolrs2, numclr,
     +			       iskip, interv, itmclr, iflgs, ityp, ier )
		    END IF
		END IF
		ifl = ifl + 1
	    END DO
C
	END IF
C
C*	Draw the color bars.
C
	IF ( filtyp .eq. 'WSAT' ) THEN
	  CALL GG_CBAR ( '1/H/UR/.95;.95/.35;.01/1|1/21//111///hw',
     +			numclr, fwninc, icolrs, ier )
	ELSE
C
C*       Get plot bounds in View coordinates to determine ambiguity
C*       label positions.  Save and set attributes for the labels.
C
	  CALL GQBND ( 'V', xl, yb, xr, yt, ier )
          xx = .743
          xx = xx * ( xr - xl ) + xl
          CALL GSCOLR ( 1, ier )        
          CALL GQTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr,
     +                irrotn, ijust, iret )
          CALL GSTEXT (21, 2, .85, 1, 111, 1, 1, ier)
          IF ( filtyp .eq. 'WAMBG1' ) THEN
            label = 'WAMBG1'
            yy = .97 * ( yt - yb ) + yb
            CALL GTEXT ( 'N', xx, yy, label, 0., 0, 0, ier )
            CALL GG_CBAR ( '1/H/UR/.98;.97/.2;.007/1|.714/21//111///hw',
     +                   numclr, fwninc, icolrs, ier )
          ELSE IF ( filtyp .eq. 'WAMBG2' ) THEN
            label = 'WAMBG2'
            yy = .935 * ( yt - yb ) + yb
            CALL GTEXT ( 'N', xx, yy, label, 0., 0, 0, ier )
            CALL GG_CBAR ( '1/H/UR/.98;.935/.2;.007/1|.71/21//111///hw',
     +                  numclr, fwninc, icolrs, ier )
          ELSE IF ( filtyp .eq. 'WAMBG3' ) THEN
            label = 'WAMBG3'
            yy = .9 * ( yt - yb ) + yb 
            CALL GTEXT ( 'N', xx, yy, label, 0., 0, 0, ier )
            CALL GG_CBAR ( '1/H/UR/.98;.90/.2;.007/1|.714/21//111///hw',
     +                  numclr, fwninc, icolrs, ier )
          ELSE IF ( filtyp .eq. 'WAMBG4' ) THEN 
            label = 'WAMBG4'
            yy = .865 * ( yt - yb ) + yb
            CALL GTEXT ( 'N', xx, yy, label, 0., 0, 0, ier )
            CALL GG_CBAR ( '1/H/UR/.98;.865/.2;.007/1|.72/21//111///hw',
     +                  numclr, fwninc, icolrs, ier ) 
	  ENDIF
          CALL GSTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr,
     +                irrotn, ijust, iret )
        ENDIF
	IF ( iflgs(1) .eq. 1 .and. iflgs(2) .eq. 1 ) THEN
          IF ( filtyp .eq. 'WSAT' ) THEN
            CALL GG_CBAR ('1/H/UR/.40;.95/.35;.01/1|1/21//111///hw',
     +                   numclr, fwninc, icolrs2, ier )
          ELSE IF ( filtyp .eq. 'WAMBG1' ) THEN
            CALL GG_CBAR ('1/H/UR/.22;.97/.2;.007/1|.714/21//111///hw',
     +                  numclr, fwninc, icolrs2, ier )
          ELSE IF ( filtyp .eq. 'WAMBG2') THEN
            CALL GG_CBAR ('1/H/UR/.22;.935/.2;.007/1|.714/21//111///hw',
     +                  numclr, fwninc, icolrs2, ier )
          ELSE IF ( filtyp .eq. 'WAMBG3' ) THEN
            CALL GG_CBAR ('1/H/UR/.22;.90/.2;.007/1|.714/21//111///hw',
     +                  numclr, fwninc, icolrs2, ier )
          ELSE IF ( filtyp .eq. 'WAMBG4' ) THEN
            CALL GG_CBAR ('1/H/UR/.22;.865/.2;.007/1|.714/21//111///hw',
     +                  numclr, fwninc, icolrs2, ier )
          END IF 
	END IF
C
C*	Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	IF ( ityp .eq. 2 .or. ityp .eq. 5 ) THEN
	    CALL GSBARB ( szbarb, jbrwid, jbrtyp, ier )
	  ELSE IF ( ityp .eq. 1 .or. ityp .eq. 3) THEN
	    CALL GSDARR ( szdarr, szdrhd, jdrwid, jdrtyp, ier )
	  ELSE
	    CALL GSARRW ( szarrw, szarhd, jarwid, jartyp, ier )
	END IF
        CALL GSSKY( szsky, jsktyp, jskwid, ier )
	CALL GSTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr,
     +                irrotn, ijust, ier )
C*
	RETURN
	END
