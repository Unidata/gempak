	SUBROUTINE VG_LVPR  ( iret )
C************************************************************************
C* VG_LVPR								*
C*									*
C* This subroutine gets the vertical levels for both the input and	*
C* output vertical coordinates.  This subroutine also sets the		*
C* parameter names by scanning the input file.  GEMPAK files are used.	*
C*									*
C* VG_LVPR ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C*					 -9 = no parms			*
C*					-14 = invalid levels		*
C**									*
C* Log:									*
C* K. Brill/NMC		06/92						*
C* K. Brill/NMC		08/92	Move HGHT to third position		*
C* K. Brill/NMC		08/92	Check if any parms where found		*
C* K. Brill/NMC		08/92	Set pointer to moisture parm		*
C* K. Brill/NMC		02/93	Set NLP, reset np when too big		*
C* S. Jacobs/NMC	 3/95	Updated call to LV_INPT			*
C* K. Brill/EMC		11/95	nli-1 if > MAXLVL; sort Eta like SIGMA	*	
C* R. Tian/SAIC		 4/05	Added GD_OPEN to get input file number	*
C************************************************************************
	INCLUDE		'vicmn.cmn'
C*
	REAL		rlvl (MAXLVL)
	CHARACTER	time (2)*20, ppp*12, vparm*8
	INTEGER		iprmct (MAXPRM), lev (2)
	LOGICAL		done, have, mandat
C-----------------------------------------------------------------------
	iret = 0
	CALL GD_OPEN ( gdcuri, wrtflg, 0, 0, igdfli, adum1,
     +                 adum2, mxgrd, iret )
C
C*	Get the output levels from the user input.
C
	CALL LV_INPT ( glevel, MAXLVL, vcordo, nlo, rlvl,
     +		       levtyp, vparm, igvco, mandat, ier )
	DO il = 1, nlo
	    levout (il) = INT ( rlvl (il) )
	END DO
	IF ( ier .ne. 0 .or. levtyp .ne. 1 ) THEN
	    iret = -14
	    RETURN
	END IF
C
C*	Initialize the input parameter names and levels.
C
	DO ip = 1, MAXPRM
	    parms (ip) = ' '
	END DO
	DO il = 1, MAXLVL
	    levin ( il ) = -999999
	END DO
C
C*	Set GEMPAK input vertical coordinate number.
C
	CALL LV_CORD ( vcordi, vparm, igvci, ier )
C
C*	Get the input levels and the parameter names.  The first
C*	three parameter positions are reserved for pressure,
C*	temperature and height.
C
	parms (1) = 'PRES'
	parms (2) = 'TMPK'
	parms (3) = 'HGHT'
	DO i = 1, MAXPRM
	    iprmct (i) = 0
	END DO
	mstprm = 0
	nli = 1
	np = 3
	done = .false.
	igrd = 1
	DO WHILE ( .not. done )
C
C*	    Get next grid header.
C
	    CALL GD_GIDN ( igdfli, igrd, time, lev, jvc, ppp, ier )
	    IF ( ier .eq. 0 ) THEN	
		IF ( time (1) .eq. gdttm (1) .and.
     +		     time (2) .eq. gdttm (2) .and.
     +		     jvc .eq. igvci .and. lev (1) .ne. 0 ) THEN
		    have = .false.
		    i = 1
		    DO WHILE ( .not. have .and. i .le. nli )
			IF ( lev (1) .eq. levin (i) ) have = .true.
			i = i + 1
		    END DO
		    IF ( .not. have ) THEN
			nli = nli + 1
			IF ( nli .gt. MAXLVL ) THEN
			    WRITE (6,*) ' Too many input levels,',
     +					' top level reset to ',
     +					levin ( MAXLVL ), '.'
			    nli = nli - 1
			    done = .true.
			ELSE
			    levin (nli) = lev (1)
			END IF
		    END IF
		    have = .false.
		    i = 1
		    DO WHILE ( .not. done .and.
     +				.not. have .and. i .le. np )
			IF ( ppp .eq. vcordo ) have = .true.
			IF ( ppp .eq. parms (i) ) THEN
			    iprmct (i) = iprmct (i) + 1
			    have = .true.
			END IF
C
C*			Check for a temperature or potential
C*			temperature that can be converted to a
C*			Kelvin temperature.
C
     			IF ( ppp  .eq. 'THTA' .or.
     +			     ppp  .eq. 'TMPK' .or.
     +			     ppp  .eq. 'TMPC' ) THEN
			    have = .true.
			    iprmct (2) = iprmct (2) + 1
			END IF
C
C*			Check for moisture parameter.
C
			IF ( (	ppp .eq. 'MIXR' .or.
     +			     	ppp .eq. 'RMIX' .or.
     +			     	ppp .eq. 'SPFH' .or.
     +			     	ppp .eq. 'DWPT' .or.
     +			     	ppp .eq. 'DWPK' .or.
     +			     	ppp .eq. 'DWPC' .or.
     +			     	ppp .eq. 'RELH' ) .and.
     +				mstprm .ne. 0 ) THEN
			    have = .true.
			    iprmct (mstprm) = iprmct (mstprm) + 1
			END IF	
			i = i + 1
		    END DO
		    IF ( .not. have ) THEN
			np = np + 1
			IF ( np .gt. MAXPRM ) THEN
			    np = np - 1
			    WRITE (6,*)
     +				' Excessive number of parameters.',
     +			        '  Parameter ', ppp, ' skipped.'
			ELSE 
			    parms (np) = ppp
			    iprmct (np) = 1
C
C*			    Set pointer to moisture parm.
C
			    IF ( ppp .eq. 'MIXR' .or.
     +				 ppp .eq. 'RMIX' .or.
     +				 ppp .eq. 'SPFH' .or.
     +				 ppp .eq. 'DWPT' .or.
     +				 ppp .eq. 'DWPK' .or.
     +				 ppp .eq. 'DWPC' .or.
     +				 ppp .eq. 'RELH' ) mstprm = np
			END IF
		    END IF
		END IF
	    ELSE
		done = .true.
	    END IF
	    igrd = igrd + 1
	END DO
C
C*	Check to see if any parameters were found.
C
	itot = 0
	WRITE(6,*) np, ' Parameters found.'
	DO i = 1, np
	    WRITE (6,*) parms (i), ' found. '
	    itot = itot + iprmct (i)
	END DO
	IF ( itot .eq. 0 ) THEN
	    iret = -9
	    RETURN
	END IF
C
C*	Check to see that total number of levels is ok.
C       (nli won't be >MAXLVL here and nlo can't be allowed > MAXLVL
C       so change the check here from nli+nlo>2*MAXLVL)
C
	IF ( nlo .gt. MAXLVL ) THEN
	    WRITE (6,*) ' Too many output levels -- specify fewer.'
	    iret = -14
	    RETURN
	END IF
C
C*	Sort the input levels.
C
	levin (1) = 0
	DO il = 1, nli
	  rlvl (il) = FLOAT ( levin (il) )
	END DO
	IF ( vcordi .eq. NETA ) THEN
C
C*	    Treat eta coordinate like sigma.
C
	    CALL LV_CORD ( 'SGMA', vparm, igvcix, ier )
	ELSE
	    igvcix = igvci
	END IF
	CALL LV_SORT ( igvcix, nli, rlvl, ier )
	DO il = 1, nli
	  levin (il) = INT ( rlvl (il) )
	END DO
C
C*	Go through and remove parameters that exist on fewer than
C*	four levels.  Also remove Montgomery stream function from
C*      the list.
C
	i = 4
	DO WHILE ( i .le. np )
	    IF ( iprmct (i) .lt. 4 .or.
     +		 parms (i) .eq. 'PSYM' ) THEN
	        WRITE (6,*) parms (i), ' Removed since there are only ',
     +                      iprmct(i), ' levels.'
		DO ii = i, np-1
		    parms (ii) = parms (ii+1)
		    IF ( ii+1 .eq. mstprm ) mstprm = mstprm - 1
		    iprmct (ii) = iprmct (ii+1)
		END DO

		np = np -1
		i = i-1
	    END IF
	    i = i + 1
	END DO

	WRITE(6,*) np, ' Parameters used.'
C*
	RETURN
	END
