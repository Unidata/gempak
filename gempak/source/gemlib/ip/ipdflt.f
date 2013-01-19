	SUBROUTINE IP_DFLT  ( iret )
C************************************************************************
C* IP_DFLT	                                    			*
C*									*
C* This subroutine gets default values for the interface parameters.	*
C* If the local GEMGLB.NTS file cannot be read, $GEMNTS/gemglb.nts is	*
C* read.								*
C*									*
C* IP_DFLT  ( IRET )					                *
C*									*
C* Output parameters:                                                   *
C*	IRET		INTEGER 	Return code			*
C*				   	  0 = normal return		*
C*					 -2 = unable to open GEMGLB	*
C*					 -3 = too many variables	*
C**	                                                                *
C* Log:									*
C* M. desJardins/GSFC	12/88						*
C* D. Moore/OU-GCN	 6/89	Rewrote                                 *
C* M. desJardins/GSFC	 7/89	Read GEMGLB.PDF to get global values	*
C* M. desJardins/GSFC	 4/90	Only open file to write			*
C* M. desJardins/GSFC	10/90	Added DISPLAY function			*
C* K. Tyle/GSC		 7/96	Renamed from NT_DFLT			*
C* D.W.Plummer/NCEP	 6/97	Increased cp2 length to 128		*
C* T. Lee/GSC		11/98	Removed IP_DFTS call			*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER	cp1*8, cp2*128, fff*72, newfil*132
	LOGICAL		exist
C-----------------------------------------------------------------------
	iret   = 0
C
C*	Open GEMGLB.NTS if it exists in local account. If not found, 
C*	open glocal file.
C
	fff = IPFIL
	CALL FL_INQR  ( fff, exist, newfil, ier )
	IF  ( .not. exist )  THEN
	    fff = '$GEMNTS/' // fff 
	    CALL FL_INQR  ( fff, exist, newfil, ier )
	END IF
C
	IF  ( exist )  THEN
	    CALL FL_SOPN  ( fff, iplun, ier )
	  ELSE
	    ierr  = -2
	    CALL ER_WMSG  ( 'IP', ierr, ' ', ier )
	    RETURN
	END IF
C
C*	Read in all the records in the file.
C
	iostat = 0
	DO WHILE  ( iostat .eq. 0 )
	    READ   ( iplun, 1000, IOSTAT = iostat ) cp1, cp2
1000	    FORMAT ( A, A )
	    IF  ( iostat .eq. 0 )  THEN
		CALL ST_LCUC  ( cp1, cp1, ier )
		IF  ( ncparm .ge. MXIPPM )  THEN
		    iret = -3
		    RETURN
		END IF
		ncparm = ncparm + 1
		cparmn ( ncparm ) = cp1
		cparmv ( ncparm ) = cp2
		iplink ( ncparm ) = 0
		chelp1 ( ncparm ) = ' '
	    END IF
	END DO
	CALL FL_CLOS  ( iplun, ier )
C*
	RETURN
	END
