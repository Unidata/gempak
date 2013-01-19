	SUBROUTINE SF_CCLP  ( filnam, prmfil, iflsrc, maxstn, maxtim, 
     +			      stmflg, isffln, nparm,  parms,  pkflg, 
     +			      iret )
C************************************************************************
C* SF_CCLP								*
C*									*
C* This subroutine creates a new climate surface data file.  The	*
C* file will have times stored as columns of the DM file and stations	*
C* as rows.								*
C*									*
C* The contents of the file named in PRMFIL will determine the 		*
C* parameters to be contained in the data set and the packing, if any,	*
C* to be used.  PKFLG is set on output if the data are to be packed.	*
C* All data packing and unpacking will be done internally.  This	*
C* subroutine is identical to SF_CCLF except that the packing 		*
C* information is read from a file in this subroutine.  The data	*
C* packing file is described in the introduction to this chapter.	*
C*									*
C* If the station time flag is set, a single word is allocated with 	*
C* each data report to store the report time (HHMM).  This time 	*
C* should be sent to SF_WDAT.  						*
C*									*
C* If the file cannot be created, error messages will be written.	*
C*									*
C* The data source values are parameters in GEMPRM.PRM .	*
C* These are not currently used by any GEMPAK program.  Current		*
C* definitions include:							*
C*      MFUNKN        unknown						*
C*      MFAIRW        airways surface observation			*
C*      MFMETR        Metar report					*
C*      MFSHIP        ship report					*
C*      MFBUOY        buoy report					*
C*      MFSYNP        synoptic report					*
C*									*
C* SF_CCLP  ( FILNAM, PRMFIL, IFLSRC, MAXSTN, MAXTIM, STMFLG, ISFFLN, 	*
C*            NPARM,  PARMS,  PKFLG,  IRET )				*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Surface file name		*
C*	PRMFIL		CHAR*		Parameter packing file name 	*
C*	IFLSRC		INTEGER		Data source 			*
C*	MAXSTN		INTEGER	 	Maximum number of stations 	*
C*	MAXTIM		INTEGER  	Maximum number of times 	*
C*	STMFLG		LOGICAL		Station time flag		*
C*									*
C* Output parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	NPARM		INTEGER		Number of parameters 		*
C*	PARMS (NPARM)	CHAR*4		Parameter names 		*
C*	PKFLG		LOGICAL		Parameter packing flag		*
C*	IRET		INTEGER		Return code			*
C*				    	  0 = normal return		*
C*				   	 -1 = file not created		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/90						*
C* S. Jacobs/NCEP	 7/96	Updated documentation			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'sfcmn.cmn'
C*
	CHARACTER*(*) 	filnam, prmfil, parms (*)
	LOGICAL 	pkflg, stmflg
C*
	INTEGER		iscale (MMPARM), iofset (MMPARM), ibits (MMPARM)
C------------------------------------------------------------------------
	iret = 0
C
C*	Get parameter information.
C
	CALL DP_FILE  ( prmfil, nparm, parms, iscale, iofset, ibits, 
     +			pkflg, iret)
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DP', iret, prmfil, ier )
	    iret = -1
	    CALL ER_WMSG  ( 'SF', iret, filnam, ier )
	    RETURN
	END IF
C
C*	Create the file.
C
	CALL SF_CCLF  ( filnam, iflsrc, nparm,  parms, maxstn, maxtim, 
     +			pkflg,  iscale, iofset, ibits, stmflg, isffln, 
     +			iret )
C*
	RETURN
	END
