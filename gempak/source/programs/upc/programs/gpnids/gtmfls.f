	 SUBROUTINE GTMFLS  (  filtyp, dattim, maxfls, 
     +				filnms, nfiles, templt, iret )
C************************************************************************
C* GTMFLS								*
C* 									*
C* This subroutine builds and searches for a path and filename(s) from  *
C* a type of data file (filtyp) such as surface and upper air 		*
C* observations and numerical model grids.  If a single file is to be	*
C* found, the file name is queried in the following order:		*
C*									*
C*		1. Locally  for all files matching the pattern		*
C*		2. Remotely for all files matching the pattern		*
C*									*
C* If the type of data file is not found, the input for filtyp is 	*
C* assumed to be an actual file name which is returned in filnms(1) 	*
C* with nfiles = 1.							*
C*									*
C* FL_MFLS  ( FILTYP, DATTIM, MAXFLS, FILNMS, NFILES, TEMPLT, IRET )	*
C* 									*
C* Input parameters:							*
C*	FILTYP		CHAR*		File name type			*
C*	DATTIM		CHAR*		Full GEMPAK date/time		*
C*	MAXFLS		INTEGER		Maximum number of files allowed	*
C*									*
C* Output parameters:							*
C*	FILNMS(MAXFLS)	CHAR*		File name(s)			*
C*	NFILES		INTEGER		Number of files returned	*
C*	TEMPLT		CHAR*		File name template, if applcble	*
C*	IRET		INTEGER		Return code			*
C*					    0 = normal return		*
C*					  -11 = no file for time range	*
C*					  -13 = no file for given type	*
C**									*
C* Log:									*
C* S. Chiswell/Unidata	 7/03	modified from fl_mfls to remove cycle,	*
C*				and improve time matching of files.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	filtyp, filnms(maxfls), dattim, templt
	CHARACTER	carr(3)*(LLMXLN), ctmpl*(LLMXLN)
	CHARACTER       filnam*(MXNMFL*MXFLSZ), sep*1
	CHARACTER       tmplt*(MXTMPL), gtime*20, ndttm*40
	CHARACTER	strtim*20, endtim*20, tmpfil*160
	CHARACTER	path*(MXTMPL), rtemplt*(MXTMPL), 
     +			lpath*(MXTMPL), tmpfil2*160, savfil*160
	LOGICAL		found
C------------------------------------------------------------------------
	iret = 0
C
C*	Check filename length
C
	CALL ST_LSTR ( filtyp, lf, ier )
	IF ( lf .eq. 0 )  THEN
	    iret = -13
	    RETURN
	END IF
C
C*	Get template information.
C
	CALL ST_CLST ( filtyp, '|', ' ', 3, carr, num, ier)

	ctmpl = carr(1)
	CALL ST_NULL ( ctmpl, ctmpl, lens, ier )
        tmplt = ' '
        path  = ' '
        CALL CTB_DTGET ( ctmpl, path, tmplt, ic, is, if, ir, ii, ion, 
     +			ihb, mnb, iha, mna, strct, idtmch, ier )
C
	IF  ( ier .ne. 0 )  THEN
C
C*	    If a match was not found, set input file type to filnms.
C
	    filnms(1) = carr(1)
	    nfiles = 1
	    templt = ' '
C
	ELSE
C
	    CALL ST_RNUL ( path, path, lens, ier )
            CALL ST_RNUL ( tmplt, tmplt, lens, ier )
	    templt = tmplt
	    rtemplt = tmplt
	    CALL ST_RPST ( tmplt, 'YYYY', '????',
     +			ipos, tmplt, iret )

	    CALL ST_RPST ( tmplt, 'YY', '??',
     +			ipos, tmplt, iret )

	    CALL ST_RPST ( tmplt, 'MM', '??',
     +			ipos, tmplt, iret )

	    CALL ST_RPST ( tmplt, 'DD', '??',
     +			ipos, tmplt, iret )
C
C * This will break product HHC, need to fix as in nex2gini
C
	    CALL ST_RPST ( tmplt, 'HH', '??',
     +			ipos, tmplt, iret )

	    CALL ST_RPST ( tmplt, 'NN', '??',
     +			ipos, tmplt, iret )
C
C * If blank then do not append
C
	    IF ( carr(2) .ne. ' ' ) THEN
	        CALL ST_RPST ( templt, '%SITE%', carr(2), ipos, templt, 
     +			ier )
	        CALL ST_RPST ( tmplt, '%SITE%', carr(2), ipos, tmplt, 
     +			ier )
	        CALL ST_RPST ( path, '%SITE%', carr(2), ipos, path, 
     +			ier )
	    END IF
	    IF ( carr(3) .ne. ' ' ) THEN
	        CALL ST_RPST ( templt, '%PROD%', carr(3), ipos, templt, 
     +			ier )
	        CALL ST_RPST ( tmplt, '%PROD%', carr(3), ipos, tmplt, 
     +			ier )
	        CALL ST_RPST ( path, '%PROD%', carr(3), ipos, path, 
     +			ier )
	    END IF
C
C*	    Get list of files in the current directory that matches the
C*	    template. If none are found, look in the path directory.
C
	    CALL ST_LSTR ( path, ilenp, ier )
	    CALL ST_LSTR ( tmplt, ilent, ier )
	    ilenf = 0
	    sep = ';'
	    maxlen = MXNMFL * MXFLSZ 
	    isort = -1
	    lpath = '.'
	    ilenl = 1
	    nf = 0
	    
	    CALL CFL_SCND ( lpath(:ilenl), ilenl, tmplt(:ilent), 
     +			ilent, sep, maxlen, isort, filnam,
     +			ilenf, nf, iret )
	    IF ( nf .eq. 0 ) THEN
		lpath = path
		ilenl = ilenp
	        CALL CFL_SCND ( lpath(:ilenl), ilenl, tmplt(:ilent), 
     +			ilent, sep, maxlen, isort, filnam,
     +			ilenf, nf, iret )
	    END IF

            IF (nf .eq. 0) THEN
		templt = ' '
		nfiles = 0
		iret = 0
		RETURN
	    END IF
C
C*	    Determine the type of template and DATTIM.
C
	    CALL ST_LCUC ( dattim, ndttm, ier )
	    CALL TI_RANG ( ndttm, strtim, endtim, mrange, ier )
C
C*	    Single file retrieval.
C
	    IF  ( mrange .eq. 0 )   THEN
C
C*		Normal time or simple forecast sequence.
C
		itype = 1
		CALL CSS_GTIM ( itype, gtime, ier )
		CALL TI_STAN ( ndttm, gtime, gtime, ier )
C
		IF  ( INDEX ( templt, 'NN' ) .eq. 0 )  THEN
		    gtime (10:11) = '00'
		END IF
C
C*		Find the most recent cycle file.
C
		CALL FL_MNAM ( gtime, rtemplt, filnms(1), iret)
	        IF ( carr(2) .ne. ' ' ) THEN
	           CALL ST_RPST ( filnms(1), '%SITE%', carr(2), ipos, 
     +			filnms(1), ier )
		END IF
	        IF ( carr(3) .ne. ' ' ) THEN
	           CALL ST_RPST ( filnms(1), '%PROD%', carr(3), ipos, 
     +			filnms(1), ier )
		END IF
C
		IF  ( iret .ne. 0 )  THEN
		    filnms (1) = ' '
		    nfiles = 0
		    iret   = -11
		ELSE
		    nfiles  = 1
		    found = .false.
		    istrt = 1
		    istop = ilenf
		    CALL ST_LSTR ( filnms(1), ilen1, ier )
		    DO WHILE ( ( .not. found) .and. ( istop .ne. 0 ) )
			istop = INDEX ( filnam( istrt:ilenf ), sep)
			ioff = istrt + istop - 1
			IF ( istop .eq. 0 ) THEN
			    tmpfil = filnam(istrt:ilenf) 
			ELSE
			    tmpfil = filnam(istrt:ioff-1) 
			END IF
		        CALL ST_LSTR ( tmpfil, ilen2, ier )
			CALL CMPFIL ( filnms(1), ilen1, tmpfil, ilen2,
     +				icmp, ier )
			IF ( icmp .ge. 0 ) THEN
			    found = .true.
			END IF
			istrt = ioff + 1
		    END DO
		    filnms(1) = lpath(:ilenl) // '/' //
     +                              tmpfil(:ilen2)
		END IF
C
	    ELSE
C
	        CALL FL_MNAM ( strtim, rtemplt, tmpfil, iret)
	        CALL FL_MNAM ( endtim, rtemplt, tmpfil2, iret)
	        IF ( carr(2) .ne. ' ' ) THEN
	            CALL ST_RPST ( tmpfil, '%SITE%', carr(2), ipos, 
     +			tmpfil, ier )
	            CALL ST_RPST ( tmpfil2, '%SITE%', carr(2), ipos, 
     +			tmpfil2, ier )
		END IF
	        IF ( carr(3) .ne. ' ' ) THEN
	            CALL ST_RPST ( tmpfil, '%PROD%', carr(3), ipos, 
     +			tmpfil, ier )
	            CALL ST_RPST ( tmpfil2, '%PROD%', carr(3), ipos, 
     +			tmpfil2, ier )
		END IF

		nfiles = 0
		istrt = 1
		istop = ilenf
		CALL ST_LSTR ( tmpfil, ilen1, ier )
		CALL ST_LSTR ( tmpfil2, ilen2, ier )
		DO WHILE ( ( nfiles .lt. maxfls ) .and. 
     +				( istop .ne. 0 ) )
                    istop = INDEX ( filnam( istrt:ilenf ), sep)
                    ioff = istrt + istop - 1
                    IF ( istop .eq. 0 ) THEN
                        savfil = filnam(istrt:ilenf)
                    ELSE
                        savfil = filnam(istrt:ioff-1)
                    END IF
                    CALL ST_LSTR ( savfil, ilenx, ier )
                    CALL CMPFIL ( tmpfil, ilen1, savfil, ilenx,
     +                          icmp1, ier )
                    CALL CMPFIL ( tmpfil2, ilen2, savfil, ilenx,
     +                          icmp2, ier )
                    IF ( ( icmp1 .eq. 0 ) .or. 
     +				( icmp2 .eq. 0 ) ) THEN
                        found = .true.
		    ELSE IF ( ( icmp1 .lt. 0 ) .and. 
     +				  ( icmp2 .gt. 0 ) ) THEN
			found = .true.
		    ELSE
		        found = .false.
                    END IF
			
		    IF ( found ) THEN
		        nfiles = nfiles + 1
		           filnms(nfiles) = lpath(:ilenl) // '/' //
     +                              savfil(:ilenx)
		    END IF
                    istrt = ioff + 1
                END DO
		IF ( nfiles .eq. 0 ) THEN
		    filnms(1) = ' '
		    iret = -11
		ELSE
		    DO i=1,nfiles/2
			savfil = filnms(i)
			filnms(i) = filnms(nfiles - i + 1)
			filnms(nfiles - i + 1) = savfil
		    END DO
		END IF
C
	    END IF
C
	END IF
C*
	RETURN
	END

