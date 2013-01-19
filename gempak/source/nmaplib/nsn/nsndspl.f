	SUBROUTINE NSN_DSPL ( panel, dattim, alias, isbcat, cycle,
     +			      parms, color, level, vcord, filter,
     +			      txtatt, endtim, mrange, intrvl, match, 
     +			      minute, ititl, ibfr, mbfr, iaftr,
     +			      maftr, mstrct, iret )
C************************************************************************
C* NSN_DSPL								*
C*									*
C* This routine plots the requested SND data to the current display	*
C* device.								*
C*									*
C* NSN_DSPL ( PANEL, DATTIM, ALIAS, ISBCAT, CYCLE, PARMS, COLOR,	*
C*	      LEVEL, VCORD, FILTER, TXTATT, ENDTIM, MRANGE, INTRVL,	*
C*	      MATCH, MINUTE, ITITL, IBFR, MBFR, IAFTR, MAFTR, 		*
C*	      MSTRCT, IRET )						*
C*									*
C* Input parameters:							*
C*	PANEL		CHAR*		GEMPAK panel			*
C*	DATTIM		CHAR*		Full GEMPAK date/time		*
C*	ALIAS		CHAR*		Alias for SND data		*
C*	ISBCAT		INTEGER		Data subcategory number		*
C*	CYCLE		CHAR*		Cycle time for the data		*
C*	PARMS		CHAR*		Parameter list			*
C*	COLOR		CHAR*		Color list			*
C*	LEVEL		CHAR*		Data level			*
C*	VCORD		CHAR*		Vertical coordinate		*
C*	FILTER		CHAR*		Filter value 			*
C*	TXTATT		CHAR*		Text attributes			*
C*	ENDTIM		CHAR*		End time of range		*
C*	MRANGE		INTEGER		Minutes in time range		*
C*	INTRVL		INTEGER		Minutes in time interval	*
C*	MATCH		INTEGER		Flag for time match scheme	*
C*	MINUTE		INTEGER		Number of minutes diff for match*
C*	ITITL		INTEGER		Title line			*
C*	IBFR		INTEGER		Bin hours before current time	*
C*	MBFR		INTEGER		Bin minutes before current time	*
C*	IAFTR		INTEGER		Bin hours after current time	*
C*	MAFTR		INTEGER		Bin minutes after current time	*
C*	MSTRCT		INTEGER		Most recent only flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  +1 = no match for time	*
C*					  +2 = max no. of stations saved*
C*					  -3 = no files found		*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 6/99	Created					*
C* S. Jacobs/NCEP	11/99	Added the level to the default title	*
C* R. Curtis/EAI	10/00   Changed MXTIME to MXNMFL		*
C* A. Hardy/GSC          3/01   Added check for mandatory data level    *
C* T. Piper/GSC		 7/01	Initialized iflno			*
C* S. Jacobs/NCEP	12/02	Changed MAN check for only SCAT_SND	*
C* T. Lee/SAIC		08/03	Added time interval to calling sequence	*
C* T. Lee/SAIC		02/04	Added reference time flag to NSN_TLST	*
C* T. Lee/SAIC		04/04	Added delta reference time		*
C* T. Lee/SAIC          10/04   Added bin hours                         *
C* A. Hardy/NCEP        02/05   Changed 'ibfr .lt. 0' to '.gt.'         *
C* F. J. Yen/NCEP	 4/08	Added bin minutes & most recent flag-CSC* 
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
    	PARAMETER	( MXTARR = 20 * MXNMFL, MXS = 56000 )
C*
	CHARACTER*(*)	panel, dattim, alias, cycle, parms, color,
     +			level, vcord, filter, txtatt, endtim
C*
	CHARACTER	times(MXNMFL)*20, ttlstr*80, filnam*80,
     +			tstn*8, sta*8, defttl*80, snfcur*80, arecur*80,
     +			prcons(MMPARM)*16, pmdset(MMPARM)*4,
     +			prmlst(MMPARM)*4, chd(MMPARM)*12, manlev(16)*8,
     +			timstr*(MXTARR), voutc*4, dtarea*12, tmplev*72,
     +			binttl*80, tmptim*20, strtim*20,
     +			lastim*20, stalst(MXS)*20
	INTEGER		iflno/0/, iscale(MMPARM), icolor(MMPARM)
	INTEGER		idtarr(5), jdtarr(5), idlst(MXS)
	REAL		offset(4), sxplt(LLSTFL), syplt(LLSTFL), 
     +			data(LLMXDT), outd(MMPARM), rlevel(LLMXLV)
	LOGICAL		binplt, chrflg(MMPARM), contin, done, donedo 
	LOGICAL		wndflg, newfil, findst

	DATA            manlev / '1000', '925', '850', '700', '500',
     +                            '400', '300', '250', '200', '150',
     +                            '100', '70', '50', '30', '20', '10' /
C------------------------------------------------------------------------
	iret   = 0
	arecur = ' '
	iflag  = -1
	idelta = -1
	nstn = 0
C
C*	Get all times in the range.
C
	CALL NSN_TLST ( alias, cycle, isbcat, endtim, mrange, intrvl,
     +			iflag, idelta, timstr, lenstr, numt, iret )
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
C
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
C*      Reverse order of plotting if most recent flag is set
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
C*	    Continue when times fall into the bin hours.
C
	    IF  ( contin )  THEN 

		IF  ( isbcat .eq. SCAT_SNF )  THEN
		    CALL FL_MFIL ( alias, cycle, filnam, ierr )
		  ELSE
		    CALL FL_MFIL ( alias, times (jpos), filnam, ierr )
		END IF
		IF  ( ierr .ne. 0 )  THEN
		    iret = -3
		    RETURN
	        END IF
C
C*	Check for mandatory level data.  If the data is at a 
C*	mandatory level, add the /MAN flag to the level string.
C*	This avoids interpolating the data when the mandatory 
C*	data is missing.  Only perform the check for upper air 
C*	observations (SCAT_SND).
C
		CALL ST_LSTR ( vcord, lenvc, ier )
		CALL ST_LSTR ( level, lenlv, ier )
		tmplev = level(:lenlv) 
C
		IF  ( isbcat .eq. SCAT_SND )  THEN
		    IF  ( vcord(:lenvc) .eq. 'PRES' )  THEN
			ij = 1
			donedo = .false.
			DO WHILE ( .not. donedo )
			    IF  ( level(:lenlv) .eq. manlev(ij) )  THEN
				tmplev = level (:lenlv) // '/MAN'
				CALL ST_LSTR ( tmplev, lenlv, ier )
				donedo = .true.
			    ELSE IF ( ij .eq. 16 )  THEN
				donedo = .true.
			    END IF
			    ij = ij + 1
			END DO
		    END IF
                END IF
C
C*	        Open the SND data file.
C
		snfcur = ' '
	    	CALL SNMFIL ( filnam, snfcur, iflno, newfil,
     +			      pmdset, npmdst, ivert, ier )
C
C*	        Get the levels.
C
	        CALL SNMLEV ( iflno, tmplev(:lenlv), vcord, ivert,
     +			      nlev, rlevel, voutc, lvert, ier )
C
C*	        Compute the parameters. If there is an error, close 
C*		the file and return.
C
	        CALL SNMPRM ( newfil, parms, pmdset, npmdst, prmlst,
     +			      chrflg, ncprm, prcons, wndflg, ier )
	        IF  ( ncprm .eq. 0 )  THEN
		    CALL SN_CLOS ( iflno, ier )
		    iret = -6
		    RETURN
	        END IF
C
C*	        Set the colors.
C
	        CALL SNMCLR ( ncprm, prmlst, color, icolor, ier )
C
C*	        Set the filtering coefficients.
C
	        IF  ( filtfc .ne. 0. )  THEN
		    CALL SNMCOF ( ncprm, prmlst, wndflg,
     +			          filtfc, offset, ier )
	        END IF
C
C*	        Set the data area to the GAREA.
C
	        dtarea = 'GAREA'
	        CALL LC_UARE ( dtarea, newfil, iflno, arecur, tstn, ier)
C
C*	    	Set the date/time.
C
	    	CALL SN_STIM ( iflno, times (jpos), ier )
C
C*	        Plot the data for each station.
C
	        DO  ilv = 1, nlev
		    nplot = 0
		    iout  = 0
		    CALL SN_BEGS ( iflno, ier )
C
		    DO WHILE  ( iout .eq. 0 )
C
C*		        Set the next station in the file.
C
		        CALL SN_SNXT ( iflno, sta, id, slat, slon,
     +				       selv, iout )
		        IF  ( iout .eq. 0 )  THEN
C
C*			    Read the data for this station, and compute
C*			    the output parameters from the data 
C*			    parameters.
C
			    CALL SN_RDAT ( iflno, numlev, data, ihhmm, 
     +					   ier )
			    IF  ( ier .eq. 0 )  THEN
			        ispri = 0
			        CALL PC_SSTN ( sta, id, slat, slon, 
     +					       selv, ispri, ihhmm, 
     +					       numlev, ier )
			        CALL PC_CMVS ( rlevel(ilv), lvert, data,
     +				               outd, chd, ier )
			    END IF
C
C*			    Transform the coordinates from Map to Plot
C*			    and apply the filter.
C
			    IF  ( ier .eq. 0 )  THEN
			        CALL GTRANS ( 'M', 'P', 1, slat, slon,
     +				              sx, sy, ier )
			        IF  ( filtfc .ne. 0. )  THEN
				    CALL SNMOVR ( sx, sy, sxplt, syplt,
     +					          nplot, offset, ier )
			 	    IF  ( ier .eq. 0 )  THEN
				        nplot = nplot + 1
				        sxplt (nplot) = sx
				        syplt (nplot) = sy
				    END IF
			        END IF
			    END IF
C
			    IF  ( ier .eq. 0 )  THEN
C
C*			        Set the group type.
C
			        igroup = 10
C
C*		   	        Start the group, plot the data and end 
C*			        the group.
C
			        CALL GSGRP  ( igroup, ierr )
			 	IF ( mstrct .eq. 1 .and. binplt) THEN
C*
C*                                  Since mstrct ("most recent flag") is
C*                                  set, then save station to be plotted
C*				    in arrays.  Test against the arrays
C*				    to determine if that station has
C*				    already been plotted before plotting
C*				    it.  Both the station name and
C*				    station ID must be tested since
C*                                  some stations do not have station
C*				    names and some have 999999 for the
C*				    station IDs.
C*
                                    findst = .false.
                            	    IF ( nstn .lt. MXS ) THEN
      					IF ( nstn .ge. 1 ) THEN
                                            i = 1
                                            DO WHILE ( .not. findst
     +					           .and. i .le. nstn )
                                              IF ( sta .eq. stalst (i)
     +							 .and.
     +                                           id .eq. idlst(i) ) THEN
                                                  findst = .true.
                                              END IF
                                              i = i + 1
                                            END DO
				        END IF
                                        IF ( .not. findst ) THEN
                                           nstn = nstn + 1
                                           stalst (nstn) = sta
                                           idlst (nstn) = id
			                   CALL SNMPLT ( icolor, prmlst,
     +					      sx, sy, slat, slon,
     +					      chrflg, ncprm, outd,
     +					      chd, ier )
                                           CALL GEGRP  ( ierr )
                                        END IF
				      ELSE
				        iret = 2
				    END IF
                                  ELSE
			            CALL SNMPLT ( icolor, prmlst, sx,
     +					  sy, slat, slon, chrflg,
     +					  ncprm, outd, chd, ier )
                                    CALL GEGRP  ( ierr )
                                END IF
                            END IF
		        END IF
		    END DO
	        END DO
		IF  ( .not. binplt )  done = .TRUE.
	    END IF
C
C*	    Close the data file.
C
	    CALL SN_CLOS ( iflno, ier )
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
	defttl = alias(1:lena) // ' ^ @ _'
	ncttl = ncprm
	DO  ii = 1, ncttl
	    iscale(ii) = 0
	END DO
	ilvl = rlevel(1)

	IF  ( binplt )  THEN
	    CALL ST_LSTR ( strtim, lens, ier )
	    CALL ST_LSTR ( lastim, lenl, ier )
	    binttl = strtim (:lens) // ' - ' // lastim (:lenl)
	    CALL GR_MTTL ( ' ', defttl, .false., binttl, ' ', .true., 
     +			   ilvl, -1, lvert, ncttl, prcons, iscale, 
     + 			   ' ', ttlstr, ier )
	  ELSE
	    CALL GR_MTTL ( ' ', defttl, .false., times(ipos), ' ',
     +			   .true., ilvl, -1, lvert, ncttl, prcons,
     +			   iscale, ' ', ttlstr, ier )
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
