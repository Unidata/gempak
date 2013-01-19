	SUBROUTINE TB_RSTN  ( lun, stid, stnnam, istnm, stat, coun,
     +			      slat, slon, selv, ispri, tbchrs, iret )
C************************************************************************
C* TB_RSTN								*
C*									*
C* This subroutine reads the next record from the GEMPAK station table.	*
C*									*
C* TB_RSTN  ( LUN, STID, STNNAM, ISTNM, STAT, COUN, SLAT, SLON,		*
C*            SELV, ISPRI,TBCHRS, IRET )				*
C*									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number		*
C*									*
C* Output parameters:							*
C*	STID		CHAR* 		Station identifier		*
C*	STNNAM		CHAR*32		Station name			*
C*	ISTNM 		INTEGER		Station number			*
C*	STAT		CHAR*2		State				*
C*	COUN		CHAR*2  	Country				*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	ISPRI		INTEGER		Station priority		*
C*	TBCHRS		CHAR*20		Additional parameters		*
C*	IRET		INTEGER		Return code			*
C*				  	   0 = normal return		*
C*				 	  -1 = end of file 		*
C*				 	  -2 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/84						*
C* M. desJardins/GSFC	10/88	GEMPAK4					*
C* K. Brill/NMC		 8/93	Change for 8-char IDs and ISPRI		*
C* L. Sager/NCEP	 6/96	Added the additional parameter variable	*
C* S. Jacobs/NCEP	11/96	Rewrote to call TB_ASTN			*
C* S. Jacobs/NCEP	12/99	Changed size of chrx and tbchrs 14->20	*
C************************************************************************
	CHARACTER*(*) 	stid, stnnam, stat, coun, tbchrs
C*
	CHARACTER  	sid*8, sname*32, st*2, cn*2, chrx*20
C------------------------------------------------------------------------
	iret = 0
C
C*	Read one station from the table.
C
	CALL TB_ASTN  ( lun, 1, nstn, sid, sname, isn, st,
     +			cn, rlat, rlon, relv, ispr, chrx, iret )
C
C*	Check for error.
C
	IF  ( iret .ne. 0 )  THEN
	    IF  ( iret .ne. -1 )  iret   = -2
	    stid   = ' '
	    stnnam = ' '
	    istnm  = 0
	    stat   = ' '
	    coun   = ' '
	    slat   = 0.0
	    slon   = 0.0
	    selv   = 0.0
	    ispri  = 0
	    tbchrs = ' '
	  ELSE
C
C*	    Set output variables.
C
	    iret   = 0
	    stid   = sid
	    istnm  = isn
	    stnnam = sname
	    stat   = st
	    coun   = cn
	    slat   = rlat
	    slon   = rlon
	    selv   = relv 
	    ispri  = ispr
	    tbchrs = chrx
	END IF
C*
	RETURN
	END
