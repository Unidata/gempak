	SUBROUTINE SFMPRM  ( sfparm, parms, nparm, colors, 
     +			     prmlst, chrflg, ncprm, prcons, wndflg, 
     +			     icolor, ccvals, icclrs, numccc, icrprm, 
     +			     tsize, jwide, iaddcl, endflg, iret )
C************************************************************************
C* SFMPRM								*
C*									*
C* This subroutine extracts and sets up parameters to process.		*
C*									*
C* SFMPRM ( SFPARM, PARMS, NPARM, COLORS, PRMLST, CHRFLG, NCPRM,        *
C*          PRCONS, WNDFLG, ICOLOR, CCVALS, ICCLRS, NUMCCC, ICRPRM,     *
C*          TSIZE, JWIDE, IADDCL, ENDFLG, IRET )	                *
C*									*
C* Input parameters:							*
C*	SFPARM		CHAR*		Input parameter string		*
C*	PARMS (*)	CHAR*		Data set parameters		*
C*	NPARM		INTEGER		Number of parameters in dataset	*
C*	COLORS		CHAR*		COLORS input			*
C*									*
C* Output parameters:							*
C*	PRMLST (NCPRM)	CHAR*		Output parameters 		*
C*	CHRFLG (NCPRM)	LOGICAL		Character flags			*
C*	NCPRM		INTEGER		Number of valid parameters	*
C*	PRCONS (NCPRM)	CHAR*		Parameters and conditions	*
C*	WNDFLG 		LOGICAL		Flag for existence of wind parm *
C*      ICOLOR (*)      INTEGER         Colors array                    *
C*      CCVALS (*)      REAL            Values for color coding         *
C*      ICCLRS (*)      INTEGER         Colors for color coding         *
C*      NUMCCC (*)      INTEGER         Number of colors for coding     *
C*      ICRPRM (*)      INTEGER         Pointers to reference parms     *
C*	IADDCL		INTEGER		Number of added reference parms *
C*      TSIZE  (26)     REAL            Text size for each numerical    *
C*	JWIDE  (26)     INTEGER         Text width for each numeriacal  *
C*	ENDFLG		CHAR*		Value range end point flag	*
C*					  L = Lower data range		*
C*					  U = Upper data range		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	 6/88	Reorganized; eliminated check for 	*
C*				same parm string since it didn't work	*
C* M. desJardins/GSFC	11/89	Added conditions on parameters		*
C* S. Schotz/GSC	 4/90   Added weather/cloud symbols , plot at	*
C*				station option, and cleaned up 		*
C* S. Schotz/GSC	 5/90   Removed calls to SFMCOF and SFMCLR	*
C* S. Schotz/GSC	 8/90	Add parameter-condition string output	*
C* K. Brill/NMC		10/91	Changes for the cloud symbols		*
C* M. desJardins/NMC	11/91	Add symbol plotting & change winds	*
C* K. Brill/NMC		11/91	Added flag for winds			*
C* M. desJardins/NMC	11/91	Remove DSET; eliminate "ONE"; clean up	*
C* K. Brill/NMC		03/92	Added DARR				*
C* P. Bruehl/Unidata	 1/94	Added SKYM or SKYK as SKYC+BRBM/K	*
C* S. Maxwell/GSC   	 3/97	Added call to IN_MRKR            	*
C* D. Kidwell/NCEP  	 2/98	Added processing of color coding info,  *
C*				removed NEWFIL from calling sequence    *
C* D. Kidwell/NCEP  	 5/98	Added DAWV                              *
C* A. Hardy/GSC          2/99   Increased MXPRPL 11 -> 25               *
C* S. Jacobs/NCEP	 3/99	Added endflg; Changed call to SNMCLR	*
C* A. Hardy/GSC         10/99   Added ICSY and TBSY                     *
C* G. Grosshans/NCEP	10/99	Added BRBS				*
C* D. Kidwell/NCEP  	 3/02	Added DASH                              *
C* D. Kidwell/NCEP  	 9/02	Added TWSY, TSKC and BRGK               *
C* D. Kidwell/NCEP  	 5/03	Added TPWS, AWSY, VWSY and WSKC         *
C* D. Kidwell/NCEP  	10/04	Added TCSL                              *
C* D. Kidwell/NCEP  	 4/05	Added BRPK                              *
C* R. Jones/NCEP	 5/06	Added option for variable text sizes	*
C* R. Jones/NCEP	 7/06	Changed call to IN_NUMB			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		ccvals (*)
	REAL		hght, hdef, tsize (26)
	CHARACTER*(*)	sfparm, parms (*), colors, prmlst (*), 
     + 			prcons(*), endflg
        INTEGER		iwide, iwdef, jwide (26) 
	INTEGER		icolor (*), icclrs (*), numccc (*), icrprm (*)
	LOGICAL		chrflg (*), wndflg
C*
	CHARACTER	prmcnd (MMPARM)*24, p*1, condtn*12,
     +			prmnew (MMPARM)*4
	LOGICAL		cmpflg (MMPARM), levflg (MMPARM), skyflg,
     +			skywnd
C
C*      MXPRPL is the maximum number of parameters to be plotted
C
	INTEGER         MXPRPL
	PARAMETER	( MXPRPL = 25 )
C------------------------------------------------------------------------
	iret = 0
C
	DO  i = 1, MXPRPL
	    prcons (i) = ' '
	END DO
	wndflg = .false.
	iaddcl = 0
C
C*	Break the input string into a list of parameters.
C
	CALL IN_PRMC  ( MXPRPL, sfparm, prmlst, prmcnd, ncprm, ier )
C
C*      Find the general text size (default values)
C
        CALL GQTEXT ( i1, i2, hdef, iwdef, i4, i5, i6, ier )
C
C
C*      Initialize text size vectors to the default values.  
C
	DO  i = 1, 25
            tsize (i) = hdef
            jwide (i) = iwdef
        END DO
C
        tsmax = RMISSD
        jwmax = IMISSD
C
C
C*	Exit if there are no parameters to compute.
C
	IF  ( ncprm .eq. 0 )  RETURN
C
C*	Process SKYK or SKYM as SKYC + BRBK/BRBM
C*	SKYK or SKYM must be in position 1 in order to plot
C*	sky cover symbol with wind barbs.  Otherwise, numerical
C*	value is plotted.
C
	skywnd = .true.
	IF ( prmlst(1) .eq. 'SKYK' ) THEN
	    ncprm = ncprm + 1
	    prmlst(ncprm) = 'BRBK'
	    prmcnd(ncprm) = prmcnd(1)
	    prmlst(1) = 'SKYC'
	  ELSE IF ( prmlst(1) .eq. 'SKYM' ) THEN
	    ncprm = ncprm + 1
	    prmlst(ncprm) = 'BRBM'
	    prmcnd(ncprm) = prmcnd(1)
	    prmlst(1) = 'SKYC'
	  ELSE
	    skywnd = .false.
	END IF
C
C*	Translate input colors.
C
	CALL SFMCLR ( ncprm, prmlst, colors, icolor, ccvals, icclrs, 
     +		      numccc, icrprm, inew, prmnew, endflg, ier )
C
C*	Make sure sky condition and barb plot in same color if SKYK
C*	or SKYM was specified.
C
	IF (skywnd) icolor (ncprm) = icolor (1)
C
C*	Add new reference parameters to the parm list.
C
	iaddcl = inew - ncprm
	DO i = ncprm + 1, inew
	    prmlst (i) = prmnew (i)
	    prmcnd (i) = ' '
	    icolor (i) = 0
	END DO
	ncprm  = inew
C
C*	Determine calculable parameters.
C
	CALL PC_DFLS  ( ncprm, prmlst, chrflg, cmpflg, levflg, np, ier )
C
C*	Check list of calculable parameters.
C
	knt = 0
	DO  i = 1, ncprm
	    IF  ( cmpflg (i) .or. ( prmlst (i) .eq. 'MARK' ) )  THEN
		knt = knt + 1
	      ELSE IF  ( ( prmlst (i) .eq. 'BLNK' ) .or.
     +			 ( prmlst (i) .eq. 'SPAC' ) )  THEN
	      ELSE
		CALL ER_WMSG  ( 'SFMAP', -4, prmlst (i), ier )
		prmlst (i) = 'BLNK'
C
C*              If this is a reference parameter for color coding,   
C*		ignore color coding request and set color to 1.
C
		DO j = 1, ncprm - iaddcl
		    IF ( icolor (j) .lt. 0 ) THEN
			IF ( icrprm ( IABS ( icolor (j) ) ) .eq. i )
     +			     icolor (j) = 1
		    END IF
		END DO	
	    END IF
	END DO
C
C*	Return if there are no real parameters to compute.
C
	IF  ( knt .eq. 0 )  THEN
	    ncprm = 0
	    RETURN
	END IF
C
C*      Check for weather/cloud symbol parameters and decode their 
C*      inputs.
C
	DO  i = 1, ncprm
	    IF ( prmlst (i) .eq. 'WSYM' .or.
     +		 prmlst (i) .eq. 'TWSY' .or.
     +		 prmlst (i) .eq. 'TPWS' .or.
     +		 prmlst (i) .eq. 'AWSY' .or.
     +		 prmlst (i) .eq. 'VWSY' ) THEN
                CALL IN_WSYM  ( prmcnd (i), condtn, ier )
                prmcnd (i) = condtn 
C*
              ELSE IF ( prmlst (i) .eq. 'SKYC' .or.
     +		        prmlst (i) .eq. 'TSKC' .or.
     +			prmlst (i) .eq. 'WSKC' ) THEN
                CALL IN_SKYC  ( prmcnd (i), condtn, ier )
                prmcnd (i) = condtn
C*
	      ELSE IF ( prmlst (i) .eq. 'CSYL' .or.
     +		        prmlst (i) .eq. 'CSYM' .or.
     +		        prmlst (i) .eq. 'CSYH' .or.
     +		        prmlst (i) .eq. 'CSYT' .or.
     +		        prmlst (i) .eq. 'TCSL' ) THEN
	        CALL IN_CLDT  ( prmcnd (i), condtn, ier )
	        prmcnd (i) = condtn
C*
	      ELSE IF ( prmlst (i) .eq. 'PWTH' ) THEN
		CALL IN_PWTH  ( prmcnd (i), condtn, ier )
		prmcnd (i) = condtn
C*
	      ELSE IF ( prmlst (i) .eq. 'PTND' ) THEN
		CALL IN_PTND  ( prmcnd (i), condtn, ier )
		prmcnd (i) = condtn
C*
	      ELSE IF ( prmlst (i) .eq. 'MARK' ) THEN
		CALL IN_MRKR  ( prmcnd (i), condtn, ier )
		prmcnd (i) = condtn
C*
              ELSE IF ( prmlst (i) .eq. 'ICSY' ) THEN
                CALL IN_ICNG  ( prmcnd (i), condtn, ier )
                prmcnd (i) = condtn
C*
              ELSE IF ( prmlst (i) .eq. 'TBSY' ) THEN
                CALL IN_TURB  ( prmcnd (i), condtn, ier )
                prmcnd (i) = condtn
C*
	      ELSE IF ( ( prmlst (i) .eq. 'ARRW' ) .or.
     +			( prmlst (i) .eq. 'ARRM' ) .or.
     +			( prmlst (i) .eq. 'ARRK' ) .or.
     +			( prmlst (i) .eq. 'DARR' ) .or.
     +			( prmlst (i) .eq. 'DAWV' ) .or.
     +			( prmlst (i) .eq. 'DASH' ) .or.
     +			( prmlst (i) .eq. 'BARB' ) .or.
     +			( prmlst (i) .eq. 'BRBS' ) .or.
     +			( prmlst (i) .eq. 'BRBM' ) .or.
     +			( prmlst (i) .eq. 'BRBK' ) .or.
     +			( prmlst (i) .eq. 'BRGK' ) .or.
     +			( prmlst (i) .eq. 'BRPK' ) )  THEN
		wndflg = .true.
		p = prmlst (i) (1:1)
		IF  ( prmlst (1) .eq. 'SKYC' )  THEN
		    skyflg = .true.
		  ELSE
		    skyflg = .false.
		END IF
		CALL IN_PWND  ( prmcnd (i), p, skyflg, condtn, ier )
		prmcnd (i) = condtn
		IF ( prmlst (i) .eq. 'BARB' ) prmlst (i) = 'BRBM'
              ELSE
C
C*		Plot all other parameters.
C
                IF ( prmlst (i) .ne. 'BLNK' ) THEN
                  CALL IN_NUMB ( prmcnd (i), hdef, iwdef,
     +                           condtn, hght, iwide, ier )
C
                  tsize (i) = hght
                  jwide (i) = iwide
                  tsmax = AMAX1 ( tsmax, ABS ( hght ))
                  jwmax = MAX0 ( jwmax, iwide )
                END IF
            END IF
        END DO
C
C*	Save the maximun values in word 26 of tsize and jwide
C
        tsize (26) = tsmax
        jwide (26) = jwmax
C
        DO  i = 1, ncprm
            IF ( tsize (i) .eq. 0 ) tsize (i) = tsmax
            IF ( jwide (i) .eq. 0 ) jwide (i) = jwmax 
        END DO
C
C*	Set conditions on parameters.
C
	CALL PC_SLCD  ( ncprm, prmcnd, ier )
C
C*	Build parameter-condition string
C
	DO  i = 1, ncprm
	    IF  ( prmcnd (i) .ne. ' ' ) THEN
		CALL ST_LSTR (prmlst (i), len, ier )
		prcons (i) = prmlst (i) (:len) // prmcnd (i)
              ELSE
		prcons (i) = prmlst (i)
            END IF
	END DO
C*
	RETURN
	END
