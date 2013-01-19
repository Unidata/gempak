	SUBROUTINE SN_FSTN  ( isnfln, stid, iret )
C************************************************************************
C* SN_FSTN								*
C*									*
C* This subroutine finds the location of the specified station in	*
C* a DM file.  The first row or column containing the station is set 	*
C* in the common area.  This subroutine may only be used when		*
C* times and stations are not mixed in row or column headers in the 	*
C* file.  It will execute faster than the SN_Sxxx or SN_Txxx 		*
C* subroutines, but is intended to be used only for real-time ingest 	*
C* programs where the structure of the file is known by the programmer.	*
C* The time may be set using SN_FTIM.  These subroutines may be 	*
C* called in either order.						*
C*									*
C*									*
C* SN_FSTN  ( ISNFLN, STID, IRET )					*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER	 	Sounding file number		*
C*	STID 		CHAR*		Station number or id		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				    	   0 = normal return		*
C*				   	  -4 = file not open		*
C*				  	 -11 = station not found	*
C*				  	 -21 = non-standard file	*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 8/87						*
C* K. Brill/NMC		 8/93	Changes for 8-char station ID		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	CHARACTER* (*)	stid
C*
	CHARACTER	keynam*4, stn*8
	INTEGER		iheadr (MMKEY), istid (2), keyloc (2)
C------------------------------------------------------------------------
	CALL SN_CHKF ( isnfln, iret )
	IF ( iret .ne. 0 )  RETURN
C
C*	Check for distinct date/time and station types.
C
	IF  ( dttype ( isnfln ) .eq. sttype ( isnfln ) ) THEN
	    iret = -21
	    RETURN
	END IF
C
C*	Check search type and reset row and column if necessary.
C
	IF ( .not. ftmset ( isnfln ) )  THEN
	    timset ( isnfln ) = .false.
	    stnset ( isnfln ) = .false.
	    ftmset ( isnfln ) = .true.
	    krow   ( isnfln ) = 0
	    kcol   ( isnfln ) = 0
	    curstn ( isnfln ) = ' '
	    icrstn ( isnfln, 1 ) = 0
	    icrstn ( isnfln, 2 ) = 0
	END IF
C
C*	Convert the station id to integer.
C
	CALL ST_LCUC  ( stid, stn, ier )
	CALL SN_STID  ( stn,  istid, keynam, ier )
	nkey = 1
	IF  ( keynam .eq. 'STID'  ) THEN
	    keyloc (1) = kstid ( isnfln )
	    IF ( kstd2 (isnfln) .gt. 0 ) THEN
		keyloc (2) = kstd2 ( isnfln )
		nkey = 2
	    END IF
	  ELSE
	    keyloc (1) = kstnm ( isnfln )
	END IF
C
C*	Find the row or column.
C
	CALL DM_SRCH ( isnfln, sttype (isnfln), nkey, keyloc, istid, 
     +		       irwcl,  iret )
C
C*	Save row or column in common.
C
	IF  ( iret .eq. 0 )  THEN
	    IF  ( sttype ( isnfln ) .eq. 'ROW' )  THEN
		krow ( isnfln ) = irwcl
		CALL DM_RRWH  ( isnfln, irwcl, iheadr, ier )
	      ELSE
		kcol ( isnfln ) = irwcl 
		CALL DM_RCLH  ( isnfln, irwcl, iheadr, ier )
	    END IF
	    IF  ( kselv ( isnfln ) .gt. 0 )  THEN
		ielv = iheadr ( kselv ( isnfln ) )
		IF  ( ielv .eq. IMISSD )  THEN
		    shght ( isnfln ) = RMISSD
		  ELSE
		    shght ( isnfln ) = ielv
		END IF
	      ELSE
		shght ( isnfln ) = 0.
	    END IF
	    curstn ( isnfln ) = stn
	    icrstn ( isnfln, 1 ) = istid (1)
	    icrstn ( isnfln, 2 ) = istid (2)
	  ELSE
	    iret = -11
	END IF
C*
	RETURN
	END
