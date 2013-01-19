	SUBROUTINE PC_CMVS  ( vlev, ivcord, datain, outdat, chrdat,
     +			      iret )
C************************************************************************
C* PC_CMVS								*
C*									*
C* This subroutine computes level and station data at a single		*
C* vertical level specified by the level value and vertical		*
C* coordinate.  If the vertical level is not in the data set, the	*
C* data will be interpolated.  PC_DFLS should be called to define	*
C* the output parameters before this subroutine is called.		*
C*									*
C* PC_CMVS ( VLEV, IVCORD, DATAIN, OUTDAT, CHRDAT, IRET )		*
C*									*
C* Input parameters:							*
C*	VLEV		REAL		Vertical level			*
C*	IVCORD		INTEGER		Vertical coordinate of VLEV	*
C*					  0 = NONE			*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*	DATAIN		REAL		Station data			*
C*	 (NPARM,NLEV)							*
C*									*
C* Output parameters:							*
C*	OUTDAT (NOUTPM)	REAL		Computed real data		*
C*	CHRDAT (NOUTPM)	CHAR*		Computed character data		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = PC_INIT must be called	*
C*					 -6 = PC_SSTN must be called	*
C*					 -7 = output must be defined	*
C*					-10 = ivert = 0; vlev <> 0	*
C*					-13 = no comp for ivcord	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/84						*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* M. desJardins/GSFC	11/89	Added conditions			*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* S. Jacobs/NCEP	 3/99	Changed tchar from 8 char to 12 char	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datain (*), outdat (*)
	CHARACTER*(*)	chrdat (*)
C*
	REAL		tdata (MAXPRM)
	CHARACTER	tchar (MAXPRM)*12
	LOGICAL		cndsav
C-------------------------------------------------------------------------
	iret = 0
C
C*	Get station parameters into temporary arrays.
C
	IF  ( ksprm .ne. 0 )  THEN
	    CALL PC_CMST  ( datain, tdata, tchar, iret )
	    IF  ( iret .ne. 0 )  RETURN
	END IF
C
C*	Get level parameters.  First save condition flags so that they
C*	will not be applied until later.
C
	cndsav = levcnd
	levcnd = .false.
	IF  ( tabflg (1) )  THEN
	    CALL PC_CMVR  ( vlev, ivcord, datain, outdat, chrdat, iret )
	END IF
	levcnd = cndsav
C
C*	Add station parameters to output arrays.
C
	DO  i = 1, ksprm
	    IF  ( scmflg (i) )  THEN
		outdat (i) = tdata (i)
		chrdat (i) = tchar (i)
	    END IF
	END DO
C
C*	Check for valid parameters.  If not valid, set data to missing.
C
	IF  ( levcnd )  CALL PC_CLCD ( koutpm (1), outdat, chrdat,
     +				       iret )
	IF  ( iret .ne. 0 )  THEN
	    CALL PC_INID  ( outdat, chrdat, koutpm (1), ier )
	END IF
C*
	RETURN
	END
