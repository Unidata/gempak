	SUBROUTINE SN_CRFP ( filnam, prmfil, iflsrc, maxstn, maxtim, 
     +	                     stmflg, isnfln, nparm,  parms,  pkflg, 
     +			     iret )
C************************************************************************
C* SN_CRFP								*
C*									*
C* This subroutine creates a new sounding data file.  The file will	*
C* have times stored as rows of the DM file and stations stored as 	*
C* columns.  If the station time flag is set, a single word is 		*
C* allocated with each data report to store the report time (HHMM).  	*
C* This time should be sent to SN_WDAT.  The parameter packing flag 	*
C* is set if the data will be packed internally according to the 	*
C* format in PRMFIL.  Note that SN_CREF includes the packing 		*
C* information in its arguments.					*
C*									*
C* Parameter packing file (PRMFIL):                                     *
C*                                                                      *
C*      This file specifies the parameters and packing information      *
C*      for a sounding file.  Each line must contain the following      *
C*      information separated by blanks or tabs:                        *
C*                                                                      *
C*          parameter name            CHAR*4                            *
C*          minimum data value        REAL                              *
C*          maximum data value        REAL                              *
C*          resolution                REAL                              *
C*                                                                      *
C*      The resolution should be an integral value of 10; otherwise,    *
C*      the next smaller resolution will be used ( e.g., res = .5 will  *
C*      become .1).  If the data are not to be packed, the minimum      *
C*      and maximum data values and the resolution should not be        *
C*      included.  Note that either all of the parameters or none of    *
C*      the parameters must have packing information.                   *
C*									*
C* The data source values are parameters in GEMPRM.PRM.		*
C*									*
C* SN_CRFP  ( FILNAM, PRMFIL, IFLSRC, MAXSTN, MAXTIM, STMFLG, ISNFLN, 	*
C*            NPARM,  PARMS,  PKFLG,  IRET )				*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Sounding file name		*
C*	PRMFIL		CHAR*		Parameter packing file name 	*
C*	IFLSRC		INTEGER		Data source 			*
C*	MAXSTN		INTEGER	 	Maximum number of stations 	*
C*	MAXTIM		INTEGER  	Maximum number of times 	*
C*	STMFLG		LOGICAL		Station time flag		*
C*									*
C* Output parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	NPARM		INTEGER		Number of parameters 		*
C*	PARMS (NPARM)	CHAR*4		Parameter names 		*
C*	PKFLG		LOGICAL		Parameter packing flag		*
C*	IRET		INTEGER		Return code			*
C*				    	  0 = normal return		*
C*				   	 -1 = file not created		*
C*									*
C**									*
C* Log:									*
C* I. Graffman/RDS	 4/87						*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		 8/93	Cleanup					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'sncmn.cmn'
C*
	CHARACTER* (*) 	filnam, prmfil, parms (*)
	LOGICAL 	pkflg, stmflg
	INTEGER		iscale (MMPARM), iofset (MMPARM), ibits (MMPARM)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get parameter information
C
	CALL DP_FILE ( prmfil, nparm, parms, iscale, iofset, ibits, 
     +                 pkflg,  iret )
C
C*	Write error messages.
C
	IF  ( iret .ne. 0 ) THEN
	    CALL ER_WMSG  ( 'DP', iret, prmfil, ier )
	    iret = -1
	    CALL ER_WMSG  ( 'SN', iret, filnam, ier )
	    RETURN
	END IF
C
C*	Create the file.
C
	CALL SN_CREF ( filnam, iflsrc, nparm, parms, maxstn, maxtim, 
     +                 pkflg, iscale, iofset, ibits, stmflg, isnfln, 
     +	               iret )
C*
	RETURN
	END
