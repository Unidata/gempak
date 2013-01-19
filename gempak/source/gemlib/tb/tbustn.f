	SUBROUTINE TB_USTN  ( lun, stid, stnnam, istnm, stat, coun,
     +			      slat, slon, selv, ispri, iregn, iinst,
     +			      iret )
C************************************************************************
C* TB_USTN								*
C*									*
C* This subroutine reads the next record from the GEMPAK station table.	*
C*									*
C* TB_USTN  ( LUN, STID, STNNAM, ISTNM, STAT, COUN, SLAT, SLON,		*
C*            SELV, ISPRI, IREGN, IINST, IRET )				*
C*									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number		*
C*									*
C* Output parameters:							*
C*	STID		CHAR*8		Station identifier		*
C*	STNNAM		CHAR*32		Station name			*
C*	ISTNM 		INTEGER		Station number			*
C*	STAT		CHAR*2		State				*
C*	COUN		CHAR*2  	Country				*
C*	SLAT		REAL		Station latitude		*
C*	SLON		REAL		Station longitude		*
C*	SELV		REAL		Station elevation		*
C*	ISPRI		INTEGER		Station priority		*
C*	IREGN		INTEGER		Station region number		*
C*	IINST		INTEGER		Station instrument type		*
C*	IRET		INTEGER		Return code			*
C*				  	   0 = normal return		*
C*				 	  -1 = end of file 		*
C*				 	  -2 = read error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/84						*
C* M. desJardins/GSFC	10/88	GEMPAK4					*
C* K. Brill/NMC		 8/93	Change for 8-char IDs and ISPRI		*
C* J. Ator/NP12		04/96	Copy from TB_RSTN, remove 4-char ID's,	*
C*				add IREGN and IINST.			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*) 	stid, stnnam, stat, coun
C*
	CHARACTER  	sid*8, sname*32, st*2, cn*2
	CHARACTER*132	buffer
C------------------------------------------------------------------------
	iret = 0
C
C*	Read the record into a buffer.
C
	READ ( lun, 500, IOSTAT = iostat ) buffer
500	FORMAT ( A )
C
C*	Read in next record.
C
	IF ( iostat .eq. 0 ) THEN
	    READ ( buffer, 1000, IOSTAT = iostat ) sid, isn, sname,
     +		   st, cn, lat, lon, ihght, ispr, irgn, iist 
1000	    FORMAT (A, 1X, I6, 1X, A, 1X, A, 1X, A, 1X,
     +              I5, 1X, I6, 1X, I5, 1X, I2, I1, I2 )
	END IF
C
C*	Check for error.
C
	IF  ( iostat .ne. 0 )  THEN
	    IF  ( iostat .lt. 0 )  THEN
		iret = -1
	      ELSE
		iret = -2
	    END IF
	    stid   = ' '
	    stnnam = ' '
	    istnm  = 0
	    stat   = ' '
	    coun   = ' '
	    slat   = 0.0
	    slon   = 0.0
	    selv   = 0.0
	    ispri  = 0
	    iregn  = IMISSD
	    iinst  = IMISSD
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
	    slat   = lat / 100.
	    slon   = lon / 100.
	    selv   = ihght 
	    ispri  = ispr
	    iregn  = irgn
	    iinst  = iist
	END IF
C*
	RETURN
	END
