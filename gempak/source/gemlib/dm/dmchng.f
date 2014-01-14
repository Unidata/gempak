	SUBROUTINE DM_CHNG  ( iflno, wrtflg, shrflg, iret )
C************************************************************************
C* DM_CHNG								*
C*									*
C* This subroutine changes the access permissions for a DM file.	*
C* If necessary, the file is closed and reopened with the requested	*
C* access.								*
C*									*
C* DM_CHNG  ( IFLNO, WRTFLG, SHRFLG, IRET )				*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	WRTFLG          LOGICAL         Write flag			*
C*	SHRFLG		LOGICAL         Shared access flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-30 = close & open failed	*
C**									*
C* Log:									*
C* K. Brill/NMC		08/90						*
C* J. Whistler/SSAI	05/91		Changed .eq. to .eqv. for 	*
C*					logical comparison		*
C* J. Whistler/SSAI	07/91		Changed irecsz to MBLKSZ	*
C* S. Jacobs/NCEP	 8/13		Added error for non-gempak file	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'dmcmn.cmn'
C*
	LOGICAL 	wrtflg, shrflg
C*
	CHARACTER	filnam*72
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for a standard GEMPAK file.
C
	IF  ( .not. stdgem(iflno) )  THEN
	    iret = -36
	    RETURN
	END IF
C
C*	Check that the file is open.
C
	CALL DM_CHKF  ( iflno, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Get the logical unit number.
C
	lun  = lundm ( iflno )
C
C*	Check for read only access.
C
	IF ( .not. wrtflg ) THEN
	    wflag ( iflno ) = .false.
            kshare ( iflno ) = .false.
	    RETURN
	END IF
C
C*	Check for write and/or shared access.
C
	IF (  wflag (iflno) .eqv. wrtflg  .and.
     +       kshare (iflno) .eqv. shrflg  )  THEN
	    RETURN
  	  ELSE
C
C*	    Get the file name.
C
	    INQUIRE  ( UNIT = lun, NAME = filnam, IOSTAT = iostat )
	    IF  ( iostat .ne. 0 )  THEN
	        iret = -30
		RETURN
	    END IF
C
C*	    Close the file.
C
	    CLOSE ( UNIT = lun, IOSTAT = iostat )
	    IF  ( iostat .ne. 0 )  THEN
	        iret = -30
		RETURN
	    END IF
C*
	    IF ( shrflg ) THEN
C
C*	        Open the file for shared access.
C
	        OPEN
     +          ( UNIT   = lun,      FILE   = filnam,   STATUS = 'OLD',
     +		  ACCESS = 'DIRECT', IOSTAT = iostat,   
     +		  RECL   = MBLKSZ * MMRECL )
C
C*	        Set return code.  If open failed, we're in big trouble.
C
	        IF  ( iostat .ne. 0 )  THEN
	            iret = -30
	        END IF
C*
	      ELSE
C
C*		Open the file for write access.
C
	        OPEN
     +          ( UNIT   = lun,      FILE   = filnam,   STATUS = 'OLD',
     +		  ACCESS = 'DIRECT', IOSTAT = iostat,
     +		  RECL   = MBLKSZ * MMRECL )
C
C*	        Set return code.  If open failed, we're in big trouble.
C
	        IF  ( iostat .ne. 0 )  THEN
	            iret = -30
	        END IF
            END IF
C
C*	    Reset the internal flags.
C
	    kshare ( iflno ) = shrflg
	    wflag  ( iflno ) = wrtflg
C*
	END IF
	RETURN
	END
