 	SUBROUTINE SF_CSDP  ( filnam, prmfil, iflsrc, maxrpt, stmflg,
     +			      isffln, nparm,  parms,  pkflg,  iret )
C************************************************************************
C* SF_CSDP								*
C*									*
C* This subroutine creates a new ship surface data file.  The		*
C* file will store times and stations together as columns in row	*
C* 1.  This type of file may be used to store data if the station	*
C* locations vary, such as for ship reports.				*
C*									*
C* The parameter packing file named in PRMFIL will determine the 	*
C* parameters to be contained in the data set and the packing, if any,	*
C* to be used.  PKFLG is set on output if the data are to be packed.	*
C* All data packing and unpacking will be done internally.  This	*
C* subroutine is identical to SF_CSDF except that the packing 		*
C* information is read from a packing file in this subroutine.		*
C* The data packing file is described in the introduction to this	*
C* chapter.								*
C*									*
C* If the station time flag is set, a single word is allocated with 	*
C* each data report to store the report time (HHMM).  This time 	*
C* should be sent with the report.					*
C*									*
C* The subroutine SF_WSDD will write data to this file.  The		*
C* data can be read using SF_RDAT, so all GEMPAK programs will be	*
C* able to read this file.						*
C*									*
C* The data source values are parameters in GEMPRM.PRM .		*
C* These are not currently used by any GEMPAK program.  Current		*
C* definitions include:							*
C*      MFUNKN        unknown						*
C*      MFAIRW        airways surface observation			*
C*      MFMETR        Metar report					*
C*      MFSHIP        ship report					*
C*      MFBUOY        buoy report					*
C*      MFSYNP        synoptic report					*
C*									*
C* SF_CSDP  ( FILNAM, PRMFIL, IFLSRC, MAXRPT, STMFLG, ISFFLN, NPARM,	*
C*            PARMS,  PKFLG,  IRET )					*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Surface file name		*
C*	PRMFIL		CHAR*		Parameter packing file name 	*
C*	IFLSRC		INTEGER		Data source 			*
C*	MAXRPT		INTEGER	 	Maximum number of reports	*
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
C* I. Graffman/RDS	 4/87	SF_CRFP					*
C* M. desJardins/GSFC	 2/88	Single dimension version		*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* S. Jacobs/NCEP	 7/96	Updated documentation			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE 	'sfcmn.cmn'
C*
	CHARACTER*(*) 	filnam, prmfil, parms (*)
	INTEGER		iscale (MMPARM), iofset (MMPARM), ibits (MMPARM)
	LOGICAL 	pkflg, stmflg
C------------------------------------------------------------------------
	iret = 0
C
C*	Get parameter information
C
	CALL DP_FILE  ( prmfil, nparm, parms, iscale, iofset, ibits, 
     +			pkflg,  iret )
C
C*	Write error messages.
C
	IF  ( iret .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'DP', iret, prmfil, ier )
	    iret = -1
	    CALL ER_WMSG  ( 'SF', iret, filnam, ier )
	    RETURN
	END IF
C
C*	Create the file.
C
	CALL SF_CSDF  ( filnam, iflsrc, nparm, parms,  maxrpt, pkflg, 
     +			iscale, iofset, ibits, stmflg, isffln, iret )
C*
	RETURN
	END
