	SUBROUTINE GG_TRAK ( filtyp, dattim, tcolor, iskip, iret ) 
C************************************************************************
C* GG_TRAK								*
C*									*
C* This subroutine sets up the times and attributes for plotting	*
C* Altimeter Ground Track Prediction data.				*
C*									*
C* GG_TRAK ( FILTYP, DATTIM, TCOLOR, ISKIP, IRET )			*
C*									*
C* Input parameters:							*
C*	FILTYP		CHAR*		File type; e.g., 'TRAK1',	*
C*					'TRAK2', 'TRAKE'		*
C*	DATTIM		CHAR*		Ending time for TRAKX		*
C*	TCOLOR		INTEGER		Ground track location color	*
C*	ISKIP		INTEGER		Skip value			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* G. McFadden/SAIC	12/08	Modeled after gg_qsct.f			*
C* G. McFadden/IMSG	01/14	Added TRAKS (SARAL) and TRAKC (Cryosat2)*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filtyp, dattim
C*
	CHARACTER	path*25, templ*(MXTMPL), cdttm*20, 
     +			dattm2*20, stime*20,
     +			tfile*128, flstrt*160
	CHARACTER*(MXFLSZ)      files(MXNMFL), fnull, filnam
	INTEGER		tcolor, itarr(5), jtarr(5)
	LOGICAL		done
C-----------------------------------------------------------------------
	IF ( filtyp .ne. "TRAK1" .and. filtyp .ne. "TRAKE" .and.
     +       filtyp .ne. "TRAK2" .and. filtyp .ne. "TRAKS" .and.
     +       filtyp .ne. "TRAKC" ) THEN
	    print *, "filtyp '", filtyp, "' unknown!"
	    iret = -1
	    return
	END IF
	iret = 0
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
C*      Get the start time for significant wave height.
C*      The user will see 6 hours worth of data at a time. 
C
        mins = 6 * 60
        CALL TI_CTOI ( dattm2, itarr, ier )
        CALL TI_SUBM ( itarr, mins, jtarr, ier )
        CALL TI_ITOC ( jtarr, stime, ier )

C
C*  Set the color attributes.
C
 	CALL GQCOLR ( jcolr, ier )
 	CALL GSCOLR ( tcolor, ier )
C
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
	IF ( ier .eq. 0 ) THEN

            IF ( filtyp .eq. "TRAK1" .or. filtyp .eq. "TRAKE" .or.
     +           filtyp .eq. "TRAK2" .or. filtyp .eq. "TRAKS" .or. 
     +           filtyp .eq. "TRAKC" ) THEN

                CALL FL_MNAM ( dattm2, templ, filnam, ier )

                CALL FL_MNAM ( stime, templ, flstrt, ier )

                done = .false.

                ifl = 1
                DO WHILE ( ( ifl .le. nfile ) .and. ( .not. done ) )
                    IF  ( files(ifl) .gt. filnam )  THEN
                        done = .true.
                    ELSE
                        IF ( files(ifl) .ge. flstrt )  THEN
                            tfile = path(:lenp) // '/' // files(ifl)

		            CALL ST_NULL ( tfile,  tfile,  lens, ier )
		            CALL ST_NULL ( stime,  stime,  lens, ier )
		            CALL ST_NULL ( dattm2, dattm2, lens, ier )
		            CALL GG_RTRK ( fnull, tfile, stime, dattm2,
     +                                     tcolor, iskip, ier ) 
                        END IF
                    END IF
                    ifl = ifl + 1
                END DO
            END IF
	END IF
 
C
C*  Reset the saved attributes.
C
	CALL GSCOLR ( jcolr, ier )
	CALL GSTEXT ( itxfn, itxhw, sztext, itxwid, ibrdr,
     +			irrotn, ijust, ier )
C
	RETURN
	END
