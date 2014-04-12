	SUBROUTINE GG_WSPD ( filtyp, dattim, kwninc, kcolrs,
     +			     numc, iskip, interv, itmclr, iret )
C************************************************************************
C* GG_WSPD								*
C*									*
C* This subroutine sets up the times and attributes for plotting	*
C* altimeter-derived windspeed data.		                        *
C*									*
C* GG_WSPD ( FILTYP, DATTIM, KWNINC, KCOLRS, NUMC, ISKIP, INTERV,       * 
C*	     ITMCLR, IRET )                              		*
C*									*
C* Input parameters:							*
C*	FILTYP		CHAR*		File type; e.g., 'WSPDA'        *
C*	DATTIM		CHAR*		Ending time for WINDSPEED	*
C*	KWNINC(*)	INTEGER		WINDSPEED increments		*
C*	KCOLRS(*)	INTEGER		Color for each WINDSPEED increm	*
C*	NUMC		INTEGER		Number of WINDSPEED intervals	*
C*	ISKIP		INTEGER		Skip value			*
C*	INTERV		INTEGER		Time stamp interval		*
C*	ITMCLR		INTEGER		Time stamp color		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* G. McFadden/IMSG	11/13	Modeled after gg_wave           	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filtyp, dattim
	INTEGER		kwninc (*), kcolrs (*)
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20,
     +			dattm2*20, tfile*128,
     +			flstrt*160
	CHARACTER*(MXFLSZ)      filnam, files(MXNMFL), fnull
	CHARACTER cstmin*(20), cval*(20), stime*(20)
	INTEGER		icolrs(LLCLEV), itarr(5), jtarr(5)
	REAL		shainc(LLCLEV)
        LOGICAL         done
C-----------------------------------------------------------------------
	IF ( filtyp .ne. "WSPDA" .and. filtyp .ne. "WSPD2" .and.
     +       filtyp .ne. "WSPDC" ) THEN
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
C*  Set the color attributes.
C
	CALL GQCOLR ( jcolr, ier )
	CALL GSCOLR(itmclr, ier)
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
C*  Display altimeter windspeed data.
C
	IF ( ier .eq. 0 ) THEN
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
C
C*          Plot this file''s windspeed data.
C
                    IF  ( files(ifl) .ge. flstrt )  THEN
                        tfile = path(:lenp) // '/' // files(ifl)
                        CALL ST_NULL ( tfile,  tfile,  lens, ier )
                        CALL GG_RWSP ( fnull, tfile, shainc,
     +                                 icolrs, numclr, iskip,
     +                                 interv, itmclr, ier )    
		    END IF
		END IF
                ifl = ifl + 1
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
	CALL GSTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr,
     +			irrotn, ijust, ier )
C
	RETURN
	END
