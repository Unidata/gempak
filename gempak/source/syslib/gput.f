	SUBROUTINE GPUT  ( idata, nw, iret ) 
C************************************************************************
C* GPUT									*
C*									*
C* This subroutine sends all or part of a message to a message queue.	*
C*									*
C* GPUT  ( IDATA, NW, IRET )						*
C*									*
C* Input parameters:							*
C*	IDATA (NW)	INTEGER		Message part			*
C*	NW		INTEGER		Length of message part		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85	GEMPLT 3.1				*
C* M. desJardins/NMC	 7/91	UNIX version; synchronize large buffers	*
C* M. desJardins/NMC	 1/92	Check that mailbox has been initialized	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'ADBUFF.CMN'
C*
	INTEGER		idata (*)
C*
	PARAMETER	( IWAIT = 0 )
	INTEGER		jdata (128)
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check that mailbox has been initialized.
C
	IF  ( mbchan .eq. -999 )  THEN
	    iret = NOINIT
	    RETURN
	END IF
C
C*	Check to see if this is a new buffer being sent.
C
	IF  ( ntypsr .ne. 1 )  THEN
	    nwexp  = idata (1)
	    npoint = 0
	    nwout  = 0
	    ntypsr = 1
	END IF
C
C*	Move data into mailbox buffer.  Send data when 128 words are
C*	in buffer or at end of data to be sent.
C
	iknt = 0
	DO WHILE  ( iknt .lt. nw )
C
C*	    Check to see if a full buffer can be sent.
C
	    IF  ( ( npoint .eq. 0 ) .and. ( nw - iknt .ge. 128 ) ) THEN
	        CALL GSEND (iwtype, mbchan, idata ( iknt+1 ), 128, iret)
	        IF  ( iret .ne. 0 ) THEN
	            iknt = nw
	          ELSE
	            nwout = nwout + 128
		    iknt  = iknt  + 128
		    IF  ( nwout .gt. 128 )  THEN
			CALL GRECV ( irtype, IWAIT, mbchan, jdata, ier )
		    END IF
	         END IF
C
C*	         Check if this is the end of the buffer.
C
	       ELSE IF ( ( npoint .eq. 0 ) .and. 
     +		         ( (nw - iknt + nwout ) .eq. nwexp ) ) THEN
	    	  np = nw - iknt
		  CALL GSEND (iwtype, mbchan, idata ( iknt+1 ), np, iret)
		  iknt = nw
		  nwout = nwout + np
		  IF  ( nwout .gt. 128 )  THEN
		      CALL GRECV ( irtype, IWAIT, mbchan, jdata, ier )
		  END IF
C
C*		  Move a single word at a time.
C
	       ELSE
		  iknt = iknt + 1
		  npoint = npoint + 1
		  nwout  = nwout + 1
		  ibuff ( npoint ) = idata ( iknt )
 		  IF  ( ( npoint .eq. 128 ) .or. ( nwout .eq. nwexp ) )
     +                       THEN
		      CALL GSEND (iwtype, mbchan, ibuff, npoint, iret)
		      IF  ( iret .ne. 0 )  THEN
			  iknt = nw
		        ELSE IF  ( nwout .gt. 128 )  THEN
			  CALL GRECV  ( irtype, IWAIT, mbchan, jdata,
     +					ier )
		      END IF
		      npoint = 0
		  END IF
	      END IF
	END DO
C
C*	If current buffer has been completed, reset flag.
C
	IF  (( iret .ne. NORMAL ) .or. ( nwout .ge. nwexp )) ntypsr = -1
C*
	RETURN
	END
