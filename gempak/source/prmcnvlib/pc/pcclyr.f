	SUBROUTINE PC_CLYR  ( datain, level1, level2, pres, clev,
     +			      ivcord, outdat, iret )
C************************************************************************
C* PC_CLYR								*
C*									*
C* This subroutine computes requested layer parameters.			*
C*									*
C* PC_CLYR  ( DATAIN, LEVEL1, LEVEL2, PRES, CLEV, IVCORD, OUTDAT,	*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	DATAIN (*)	REAL		Station data			*
C*	LEVEL1		INTEGER		Level below or at req. pres	*
C*	LEVEL2		INTEGER		Level above req. pres		*
C*	PRES		REAL		Requested pressure		*
C*	CLEV		REAL		Input vertical level		*
C*	IVCORD		INTEGER		Requested vertical coordinate	*
C*									*
C* Output parameters:							*
C*	OUTDAT (*)	REAL		Output data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/90						*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	REAL		datain (*), outdat (*)
C*
	CHARACTER	parm*4
	PARAMETER	( IDX1=5, IDX2=6, IDX3=7 )
C------------------------------------------------------------------------
	iret = 0
	nout = koutpm (1)
C
C*	Loop through all the output parameters.
C
	icndtp = 1
	DO  i = 1, nout
	    ilvprm = i
C
C*	    Check if the parameter is already computed.
C
	    IF  ( qlayr (i) )  THEN
C*
		parm = prmlyr (i)
C
C*		Compute the various parameters.
C
		IF  ( parm .eq. 'RICH' )  THEN
		    outdat (i) = PS_RICH  ( datain, jdsprm, clev,
     +					    ivcord )
C------------------------------------------------------------------------
		  ELSE IF  ( parm .eq. 'BVFQ' )  THEN
		    outdat (i) = PS_BVFQ  ( datain, jdsprm, clev, 
     +					    ivcord )
C------------------------------------------------------------------------
		  ELSE IF  ( parm .eq. 'BVSQ' )  THEN
		    outdat (i) = PS_BVSQ  ( datain, jdsprm, clev, 
     +					    ivcord )
C------------------------------------------------------------------------
		  ELSE IF  ( parm .eq. 'BVPD' )  THEN
		    outdat (i) = PS_BVPD  ( datain, jdsprm, clev, 
     +					    ivcord )
C------------------------------------------------------------------------
		  ELSE IF  ( parm .eq. 'LAPS' )  THEN
		    outdat (i) = PS_LAPS  ( datain, jdsprm, clev, 
     +					    ivcord )
C------------------------------------------------------------------------
		  ELSE IF  ( parm .eq. 'STAB' )  THEN
		    outdat (i) = PS_STAB  ( datain, jdsprm, clev, 
     +					    ivcord )
C------------------------------------------------------------------------
		  ELSE IF  ( parm .eq. 'STAP' )  THEN
		    outdat (i) = PS_STAP  ( datain, jdsprm, clev,
     +					    ivcord )
C------------------------------------------------------------------------
		  ELSE IF  ( parm .eq. 'SHRM' )  THEN
		    outdat (i) = PS_SHRM  ( datain, jdsprm, clev, 
     +					    ivcord )
C------------------------------------------------------------------------
		  ELSE IF  ( parm .eq. 'SHRD' )  THEN
		    outdat (i) = PS_SHRD  ( datain, jdsprm, clev, 
     +					    ivcord )
C------------------------------------------------------------------------
		  ELSE IF  ( parm .eq. 'SEPA' )  THEN
		    outdat (i) = PS_SEPA  ( datain, jdsprm, clev, 
     +					    ivcord )
C------------------------------------------------------------------------
		END IF
	    END IF
	END DO
C*
	RETURN
	END
