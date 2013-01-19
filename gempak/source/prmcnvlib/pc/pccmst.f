	SUBROUTINE PC_CMST  ( datain, outdat, chrdat, iret )
C************************************************************************
C* PC_CMST								*
C*									*
C* This subroutine computes station parameters.  PC_DFST must be	*
C* called to define the output parameters before this subroutine	*
C* is called.								*
C*									*
C* PC_CMST ( DATAIN, OUTDAT, CHRDAT, IRET )				*
C*									*
C* Input parameters:							*
C*	DATAIN		REAL		Station data			*
C*	 (NPARM,NLEV)							*
C*									*
C* Output parameters:							*
C*	OUTDAT (NSPRM)	REAL		Computed real data		*
C*	CHRDAT (NSPRM)	CHAR*		Computed character data		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = PC_INIT not called	*
C*					 -6 = PC_SSTN not called	*
C*					-16 = no stn parameters set	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	Rewrote for GEMPAK4			*
C* M. desJardins/GSFC	11/89	Added check for conditions		*
C* M. desJardins/GSFC	 7/90	Reorganization				*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* D. Kidwell/NCEP	 7/98	Added computed parameter FELV           *
C* A. Hardy/GSC		 3/99	Added priority parameter SPRI           *
C* A. Hardy/GSC		 3/99	Removed jspri = 0			*
C* S. Jacobs/NCEP	 3/99	Changed buf from 8 char to 12 char	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datain (*), outdat (*)
	CHARACTER*(*)	chrdat (*)
C*
	CHARACTER	buf*12, p*4
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
C*	Check for errors
C
	IF  ( .not. dsflg )  THEN
	    iret = -4
	  ELSE IF  ( .not. tstnfl )  THEN
	    iret = -6
	  ELSE IF  ( ksprm .eq. 0 )  THEN
	    iret = -16
	  ELSE
	    iret = 0
	END IF
	IF  ( iret .ne. 0 )  RETURN
C
C*	Get station elevation from height field if not already in
C*	common.
C
	IF  ( ( ( telv  .eq. 0. ) .or. ERMISS ( telv ) ) .and.
     +		( jhght .ne. 0 ) )  telv = datain ( jhght )
C
C*	Initialize data.
C
	CALL PC_INID  ( outdat, chrdat, ksprm, ier )
C
C*	Loop through and compute parameters.
C
	icndtp = 2
	DO  i = 1, ksprm
	    istprm = i
	    IF  ( scmflg (i) )  THEN
		p = stnprm (i)
		IF  ( p .eq. 'STID' )  THEN
		    outdat (i) = 0.
	            chrdat (i) = ' '
	            CALL ST_LSTR  ( tstid, isiz, ier )
		    chrdat (i) ( 9-isiz:8) = tstid (1:isiz)
		  ELSE IF  ( p .eq. 'STNM' )  THEN
		    outdat (i) = jsnum
		    CALL ST_INCH  ( jsnum, buf, ier )
	            chrdat (i) = ' '
	            CALL ST_LSTR  ( buf, isiz, ier )
		    chrdat (i)( 9-isiz:8) = buf (1:isiz)
		  ELSE IF  ( p .eq. 'SELV' )  THEN
		    outdat (i) = telv
		  ELSE IF  ( p .eq. 'FELV' )  THEN
		    fl         = PR_HGMF ( telv )
		    outdat (i) = ANINT ( PR_D100 ( fl ) )
		  ELSE IF  ( p .eq. 'SLAT' )  THEN
		    outdat (i) = tlat
		  ELSE IF  ( p .eq. 'SLON' )  THEN
		    outdat (i) = tlon
		  ELSE IF  ( p .eq. 'STIM' )  THEN
		    outdat (i) = FLOAT ( ithhmm )
		  ELSE IF  ( p .eq. 'SPRI' )  THEN
		    outdat (i) = FLOAT ( jspri )
		  ELSE
		    CALL PC_CSTB  ( datain, p, data, ier )
		    outdat (i) = data
		END IF
	    END IF
	END DO
C
C*	Check for station conditions.  If not, set data to missing.
C
	IF  ( stncnd )  CALL PC_CSCD  ( ksprm, outdat, iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL PC_INID  ( outdat, chrdat, koutpm (1), ier )
	END IF
C*
	RETURN
	END
