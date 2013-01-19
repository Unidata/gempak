	SUBROUTINE NSF_DSPL ( panel, dattim, alias, isbcat, cycle,
     +			      parms, color, filter, txtatt, endtim,
     +			      mrange, intrvl, match, minute, ititl, 
     +			      ibfr, mbfr, iaftr, maftr, mstrct, iret )
C************************************************************************
C* NSF_DSPL								*
C*									*
C* This routine plots the requested SFC data to the current display	*
C* device.								*
C*									*
C* NSF_DSPL ( PANEL, DATTIM, ALIAS, ISBCAT, CYCLE, PARMS, COLOR,	*
C*	      FILTER, TXTATT, ENDTIM, MRANGE, INTRVL, MATCH, MINUTE, 	*
C*	      ITITL, IBFR, MBFR, IAFTR, MAFTR, MSTRCT, IRET )		*
C*									*
C* Input parameters:							*
C*	PANEL		CHAR*		GEMPAK panel			*
C*	DATTIM		CHAR*		Full GEMPAK date/time		*
C*	ALIAS		CHAR*		Alias for SFC data		*
C*	ISBCAT		INTEGER		Data subcategory number		*
C*	CYCLE		CHAR*		Cycle time for the data		*
C*	PARMS		CHAR*		Parameter list			*
C*	COLOR		CHAR*		Color list			*
C*	FILTER		CHAR*		Filter value 			*
C*	TXTATT		CHAR*		Text attributes			*
C*	ENDTIM		CHAR*		End time of range		*
C*	MRANGE		INTEGER		Minutes in time range		*
C*	INTRVL		INTEGER		Minutes in time interval	*
C*	MATCH		INTEGER		Flag for time match scheme	*
C*	MINUTE		INTEGER		Number of minutes diff for match*
C*	ITITL		INTEGER		Title line			*
C*      IBFR            INTEGER         Bin hours before current time   *
C*      MBFR            INTEGER         Bin minutes before current time *
C*      IAFTR           INTEGER         Bin hours after current time    *
C*      MAFTR           INTEGER         Bin minutes after current time  *
C*      MSTRCT          INTEGER         Only most recent flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  +1 = no match for time	*
C*					  +2 = max no. of stations saved*
C*					  -3 = no files found		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 6/99	Created					*
C* R. Curtis/EAI	10/00   Changed MXTIME to MXNMFL		*
C* T. Piper/GSC		 7/01	Initialized iflno			*
C* T. Lee/SAIC		 8/03	Added time interval to calling sequence	*
C* T. Lee/SAIC		 2/04	Added reference time flag to nsf_tlst	*
C* T. Lee/SAIC		 4/04	Added delta reference time		*
C* T. Lee/SAIC		10/04	Added bin hours				*
C* A. Hardy/NCEP	02/05	Changed 'ibfr .lt. 0' to '.gt.'		*
C* R. Jones/NCEP	05/06	Changed calling sequence for SFMPRM	*
C*				and SFMPLT to allow variable text sizes	*
C* S. Jacobs/NCEP	 7/06	Allow filter to use biggest text size	*
C* F. J. Yen/NCEP	 4/08	Added bin minutes and most recent flag	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
    	PARAMETER	( MXTARR = 20 * MXNMFL, MXS = 56000 )
C*
	CHARACTER*(*)	panel, dattim, alias, cycle, parms,
     +			color, filter, txtatt, endtim
C*
	CHARACTER	times(MXNMFL)*20, ttlstr*80, filnam*80,
     +			tstn*8, sta*8, defttl*80, sffcur*80, arecur*80,
     +			prcons(MMPARM)*16, pmdset(MMPARM)*4,
     +			prmlst(MMPARM)*4, chd(MMPARM)*12, endflg*1,
     +			timstr*(MXTARR), dtarea*12, binttl*80,
     +			tmptim*20, strtim*20, lastim*20, stalst(MXS)*20
	INTEGER		iflno/0/, iscale(MMPARM), icolor(MMPARM), 
     +			numccc(MMPARM), icrprm(MMPARM), 
     +			icclrs(MMPARM*LLCLEV), jwide(MMPARM)
	INTEGER		idtarr(5), jdtarr(5), idlst(MXS)
	REAL		offset(4), sxplt(LLSTFL), syplt(LLSTFL),
     +			data(MMPARM), outd(MMPARM),
     +			ccvals(MMPARM*LLCLEV), tsize(MMPARM)
	LOGICAL		chrflg(MMPARM), wndflg, newfil
	LOGICAL		done, contin, binplt, findst

C------------------------------------------------------------------------
	iret   = 0
	sffcur = ' '
	arecur = ' '
	iflag  = -1
	idelrt = -1
	nstn = 0
C
C*	Get all times in the range.
C
	CALL NSF_TLST ( alias, cycle, isbcat, endtim, mrange, intrvl,
     +			iflag, idelrt, timstr, lenstr, numt, iret )
	IF  ( iret .ne. 0 )  RETURN
	CALL ST_CLSL ( timstr, ';', ' ', numt, times, ntime, ier )
C
C*	Set the text attributes.
C
	CALL IN_TEXT ( txtatt, ier )
C
C*	Parse the filter value.
C
	CALL IN_FILT ( filter, filtfc, ier )
C
C*	Initialize the filter arrays.
C
	IF  ( filtfc .ne. 0. )  THEN
	    DO  m = 1, LLSTFL
		sxplt (m) = RMISSD
		syplt (m) = RMISSD
	    END DO
	END IF
C
C*	Set the panel.
C
	CALL GG_PANL ( panel, ier )
C
C*	Find the appropriate match for the input time.
C
	CALL TI_MTCH ( match, dattim, times, ntime, minute,
     +		       ipos, ierr )
	IF  ( ( ierr .ne. 0 ) .or. ( ipos .eq. 0 ) )  THEN
	    iret = -3
	    RETURN
	END IF
C
C*      Get bin hours.
C
        ihhbfr = ABS ( ibfr )
        ihhaft = iaftr
        immbfr = ihhbfr * 60 + mbfr
        immaft = ihhaft * 60 + maftr

        IF  ( iaftr .gt. 0 .or. maftr .gt. 0 )  THEN
            CALL TI_CTOI ( times ( ipos ), idtarr, ier )
            CALL TI_ADDM ( idtarr, immaft, jdtarr, ier  )
            CALL TI_ITOC ( jdtarr, tmptim, ier )
            CALL TI_DIFF ( tmptim, times ( numt ), nmin, ier )
            IF  ( nmin .ge. 0 )  THEN
                lastim = times ( numt )
              ELSE
                lastim = tmptim
            END IF
          ELSE
            lastim = times ( ipos )
        END IF

        IF  ( ibfr .gt. 0 .or. mbfr .gt. 0 )  THEN
            CALL TI_CTOI ( times ( ipos ), idtarr, ier )
            CALL TI_SUBM ( idtarr, immbfr, jdtarr, ier  )
            CALL TI_ITOC ( jdtarr, tmptim, ier )
            CALL TI_DIFF ( tmptim, times ( 1 ), nmin, ier )
            IF  ( nmin .le. 0 )  THEN
                strtim = times ( 1 )
              ELSE
                strtim = tmptim
            END IF
          ELSE 
            strtim = times ( ipos )
        END IF
C
C*	Create the proper file name from the alias and
C*	the cycle or time, depending on the data subcategory.
C
	done   = .false.
        contin = .true.
        IF ( strtim .eq. lastim )  THEN
            binplt = .false.
          ELSE
            binplt = .true.
        END IF
C
C*	Reverse order of plotting if most recent flag is set
C
	IF ( mstrct .eq. 1 .and. binplt ) THEN
	    jpos = ntime
	  ELSE
            jpos = 1
	END IF
        DO WHILE ( .not. done )
            IF  ( binplt )  THEN
                CALL TI_DIFF ( times ( jpos ), strtim, nmin, ier )
                IF  ( nmin .lt. 0 )  THEN
                    contin = .false.
                  ELSE 
                    CALL TI_DIFF ( times ( jpos ), lastim, nmin, ier )
                    IF  ( nmin .gt. 0 )  THEN
                        contin = .false.
                      ELSE
                        contin = .true.
                    END IF
                END IF 
              ELSE
                times (jpos) = lastim
            END IF
C
C*          Continue when times fall into the bin hours.
C
            IF  ( contin )  THEN 
		IF  ( isbcat .eq. SCAT_SFF )  THEN
		    CALL FL_MFIL ( alias, cycle, filnam, ierr )
		  ELSE
		    CALL FL_MFIL ( alias, times(jpos), filnam, ierr )
		END IF
		IF  ( ierr .ne. 0 )  THEN
		    iret = -3
		    RETURN
		END IF
C
C*		Open the SFC data file.
C
		sffcur = ' '
		CALL SFMFIL ( filnam, sffcur, iflno, newfil,
     +			      pmdset, npmdst, ier )
C
C*		Compute the parameters. If there is an error, close the 
C*		file and return.
C
		CALL SFMPRM ( parms, pmdset, npmdst, color,
     +			      prmlst, chrflg, ncprm, prcons, wndflg,
     +			      icolor, ccvals, icclrs, numccc, icrprm,
     +			      tsize, jwide, iaddcl, endflg, ier )
		IF  ( ncprm .eq. 0 )  THEN
		    CALL SF_CLOS ( iflno, ier )
		    iret = -6
		    RETURN
		END IF
C
C*              Get offsets for filtering.
C
		IF  ( filtfc .ne. 0. )  THEN
		    CALL GQTEXT ( j1, j2, szt, j3, j4, j5, j6, ier )
		    CALL GSTEXT ( j1, j2, tsize(26), j3, j4, j5, j6,
     +				  ier )
		    CALL SFMCOF ( ncprm - iaddcl, prmlst, wndflg,
     +			    	 filtfc, offset, ier )
		    CALL GSTEXT ( j1, j2, szt, j3, j4, j5, j6, ier )
     		END IF
C
C*		Set the data area to the GAREA.
C
		dtarea = 'GAREA'
	    	CALL LC_UARE ( dtarea, newfil, iflno, arecur, tstn, ier)
C
C*		Set the date/time.
C
		CALL SF_STIM ( iflno, times(jpos), ier )
C
C*		Plot the data for each station.
C
		nplot = 0
		iout  = 0
		DO WHILE  ( iout .eq. 0 )
C
C*		    Set the next station in the file.
C
		    CALL SF_SNXT  ( iflno, sta, id, slat, slon, selv, 
     +				    ispri, iout )
		    IF  ( iout .eq. 0 )  THEN
C
C*			Read the data for this station, and compute the
C*			output parameters from the data parameters.
C
			CALL SF_RDAT ( iflno, data, ihhmm, ier )
			IF  ( ier .eq. 0 )  THEN
			    CALL PC_SSTN ( sta, id, slat, slon, selv, 
     +					   ispri, ihhmm, 1, ier )
			    CALL PC_CMVS ( 0., 0, data, outd, chd, ier )
			END IF
C
C*			Transform the coordinates from Map to Plot and 
C*			apply the filter.
C
			IF  ( ier .eq. 0 )  THEN
			    CALL GTRANS   ( 'M', 'P', 1, slat, slon, 
     +					    sx, sy, ier )
			    IF  ( filtfc .ne. 0. )  THEN
				CALL SFMOVR ( sx, sy, sxplt, syplt, 
     +					      nplot, offset, ier )
				IF  ( ier .eq. 0 )  THEN
				    nplot = nplot + 1
				    sxplt (nplot) = sx
				    syplt (nplot) = sy
				END IF
			    END IF
			END IF
C
C*			Set the group type.
C
			IF  ( ier .eq. 0 )  THEN
			    CALL ST_FIND ( 'TPFC', prmlst, ncprm,
     +				 	    kpos, ier )
			    IF  ( kpos .eq. 0 )  THEN
				igroup = 10
			      ELSE
				igroup = 11
			    END IF
C
C*			    Start the group, plot the data and end the 
C*			    group.
C
			    CALL GSGRP  ( igroup, ierr )
			    IF ( mstrct .eq. 1 .and. binplt) THEN
C*
C*				Since mstrct ("most recent flag") is set,
C*				then save station to be plotted in arrays.
C*				Test against the arrays to determine
C*				if that station has already been plotted
C*				before plotting it.  Both the station
C*				name and station ID must be tested since
C*				some stations do not have station names
C*				and some have 999999 for the station IDs.
C*				
				findst = .false.
				IF ( nstn .lt. MXS ) THEN
      				    IF ( nstn .ge. 1 ) THEN
				        i = 1
				        DO WHILE ( .not. findst .and.
     +					           i .le. nstn )
				          IF ( sta .eq. stalst (i) .and.
     +					       id .eq. idlst(i) ) THEN
					    findst = .true.
				          END IF
				          i = i + 1
				        END DO
				    END IF
				    IF ( .not. findst ) THEN 
				        nstn = nstn + 1
				        stalst (nstn) = sta
				        idlst (nstn) = id
			                CALL SFMPLT ( icolor, sx, sy,
     +					  slat, slon, chrflg, prmlst,
     +					  ncprm, outd, chd,
     +					  ccvals, icclrs, numccc,
     +					  icrprm, tsize, jwide,
     +                                    endflg, ier )
			               CALL GEGRP  ( ierr )
				    END IF
				  ELSE
				    iret = 2
				END IF
			      ELSE
			        CALL SFMPLT ( icolor, sx, sy, slat,
     +					slon, chrflg, prmlst,
     +					ncprm, outd, chd,
     +					ccvals, icclrs, numccc,
     +					icrprm, tsize, jwide,
     +                                  endflg, ier )
			        CALL GEGRP  ( ierr )
			    END IF
			END IF
		    END IF
		END DO
	    END IF
	    IF  ( .not. binplt ) done = .TRUE.	
C
C*	    Close the data file.
C
	    CALL SF_CLOS ( iflno, ier )
C
	    IF ( mstrct .eq. 1 .and. binplt ) THEN
	        IF ( jpos .le. 1 )  THEN
		    done = .true.
		  ELSE
		    jpos = jpos - 1
		END IF
	      ELSE
	        IF ( jpos .ge. ntime )  THEN
		    done = .true.
		  ELSE
	            jpos = jpos + 1
		END IF
	    END IF
	END DO
C
C*	Draw color bar for first color-coded parameter.
C
	ip = 1
	DO WHILE ( ip .le. ncprm )
	    IF  ( icolor(ip) .eq. -1 )  THEN
		CALL GG_CBAR ('1', numccc(1)-1, ccvals, icclrs, ier)
		ip = ncprm + 1
	      ELSE
		ip = ip + 1
	    END IF
	END DO
C
C*	Save the current settings for color and text.
C
	CALL GQCOLR  ( jclr, ier )
	CALL GQTEXT  ( jtxfn, jtxhw, sztxt, jtxwid, jbrdr,
     +		       jrrotn, jjust, ier )
C
C*	Set constant values for color and text for the title.
C
	CALL GSCOLR  ( 31, ier )
	CALL GSTEXT  ( 21, 2, 1.0, 1, 111, 1, 1, ier )
C
C*	Create and plot the title string.
C
	CALL ST_LSTR ( alias, lena, ier )
	defttl = alias(1:lena) // ' ^ _'
	ncttl = ncprm - iaddcl
	DO  ii = 1, ncttl
	    iscale(ii) = 0
	END DO
	IF  ( binplt )  THEN
	    CALL ST_LSTR ( strtim, lens, ier )
	    CALL ST_LSTR ( lastim, lenl, ier )
	    binttl = strtim (:lens) // ' - ' // lastim (:lenl)
	    CALL GR_MTTL ( ' ', defttl, .false., binttl, ' ',
     +			   .false., 0, -1, 0, ncttl, prcons, iscale, 
     +			   ' ', ttlstr, ier )
	  ELSE
	    CALL GR_MTTL ( ' ', defttl, .false., times(ipos), ' ',
     +			   .false., 0, -1, 0, ncttl, prcons, iscale, 
     +			   ' ', ttlstr, ier )
	END IF
	CALL GG_WSTR ( ttlstr, ititl, ier )
C
C*	Reset the saved settings for color and text.
C
	CALL GSCOLR  ( jclr, ier )
	CALL GSTEXT  ( jtxfn, jtxhw, sztxt, jtxwid, jbrdr,
     +		       jrrotn, jjust, ier )
C*
	RETURN
	END
