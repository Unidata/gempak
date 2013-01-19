	SUBROUTINE SF_STST  ( isffln, maxstn, stcn, nstn, stid,
     +			      istnm, iret )
C************************************************************************
C* SF_STST								*
C*									*
C* This subroutine returns a list of stations in a state or country.	*
C* SF_STIM must be called before this subroutine is called.  This	*
C* subroutine is included for compatibility with earlier versions of	*
C* GEMPAK.  A search for stations should be set using SF_UARE.		*
C*									*
C* SF_STST  ( ISFFLN, MAXSTN, STCN, NSTN, STID, ISTNM, IRET )		*
C*									*
C* Input parameters:							*
C*	ISFFLN		INTEGER		Surface file number		*
C*	MAXSTN		INTEGER		Maximum number of stations	*
C*	STCN		CHAR*2		State/country name		*
C*									*
C* Output parameters:							*
C*	NSTN		INTEGER		Number of stations		*
C*	STID  (NSTN)	CHAR*4		Station identifiers		*
C*	ISTNM (NSTN)	INTEGER		Station numbers			*
C*	IRET		INTEGER		Return code			*
C*					   2 = too many stations	*
C*					   0 = normal return		*
C*					  -3 = file not open		*
C*					 -15 = invalid search		*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/87						*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sfcmn.cmn'
C*
	CHARACTER*(*) 	stcn, stid (*)
	INTEGER		istnm (*)
C*
	INTEGER		iheadr (MMKEY)
	LOGICAL		done
C-------------------------------------------------------------------------
	CALL SF_CHKF  ( isffln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Set state/country search.
C
	CALL SF_STAT  ( isffln, stcn, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Loop and get stations.
C
	nstn = 0
	done = .false.
	DO WHILE  ( .not. done )
	    CALL DM_NEXT  ( isffln, irow, icol, ier )
	    IF  ( ier .ne. 0 )  THEN
	        done = .true.
	      ELSE IF  ( nstn .ge. maxstn )  THEN
	        iret = 2
	        done = .true.
	      ELSE
	        nstn = nstn + 1
	        IF  ( sttype (isffln) .eq. 'ROW' )  THEN
	            CALL DM_RRWH  ( isffln, irow, iheadr, ier1 )
	          ELSE
	            CALL DM_RCLH  ( isffln, icol, iheadr, ier1 )
	        END IF
	        IF  ( kstid (isffln) .gt. 0 )  CALL ST_ITOC ( 
     +			iheadr ( kstid (isffln) ), 1, stid (nstn), ier )
		IF  ( kstnm (isffln) .gt. 0 )  istnm (nstn) = 
     +			iheadr ( kstnm (isffln) )
	    END IF
	END DO
C*
	RETURN 
	END
