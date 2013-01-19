	SUBROUTINE GGET  ( idata, nw, iret )
C************************************************************************
C* GGET									*
C*									*
C* This subroutine gets part or all of a message.  This subroutine	*
C* waits for a message to be received.					*
C*									*
C* NOTE: NW is an INPUT parameter and IDATA is an OUTPUT parameter.	*
C* Most GEMPAK subroutines have the input parameters before the		*
C* output parameters.							*
C*									*
C* GGET  ( IDATA, NW, IRET )						*
C*									*
C* Output parameters:							*
C*	IDATA (NW)	INTEGER		Message part			*
C*									*
C* Input parameters:							*
C*	NW		INTEGER		Length of message part		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 4/85	GEMPLT 3.1				*
C* M. desJardins/NMC	 7/91	UNIX version; synchronize large buffers	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'ADBUFF.CMN'
C*
	INTEGER		idata (*)
C*
	PARAMETER	( IWAIT = 0 )
	INTEGER		jdata (128)
	DATA		jdata (1) / -888 /
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check to see if this is a new buffer being received.
C
	IF  ( ntypsr .ne. 2 )  THEN
C
C*	    Get the first buffer and initialize for receive.
C
	    CALL GRECV  ( irtype, IWAIT, mbchan, ibuff, iret )
	    nwexp  = ibuff (1) - 1
	    npoint = 1
	    nwout  = 0
	    ntypsr = 2
	END IF
C
C*	Move data from the internal buffer into the data buffer.
C*	Receive data when internal buffer has been sent.
C
	iknt = 0
	DO WHILE  ( ( iknt .lt. nw ) .and. ( nwout .lt. nwexp ) )
C
C*		Check to see if a new buffer must be read and
C*		if the data can be read directly into the data buffer.
C
	    IF  ( ( npoint .eq. 128 ) .and. ( nw - iknt .ge. 128 ) )
     +                THEN
		CALL GRECV ( irtype, IWAIT, mbchan, idata ( iknt+1 ), 
     +			     iret )
	        IF  ( iret .ne. 0 ) THEN
	            iknt = nw
	          ELSE
		    CALL GSEND  ( iwtype, mbchan, jdata, 1, ier )
	            nwout = nwout + 128
		    iknt  = iknt  + 128
	         END IF
C
C*		  Check if a new buffer must be received into internal buffer.
C
	      ELSE IF  ( npoint .eq. 128 ) THEN
		  CALL GRECV ( irtype, IWAIT, mbchan, ibuff, iret )
		  npoint = 0
		  IF  ( iret .ne. 0 )  THEN
		      iknt = nw
		    ELSE
		      CALL GSEND  ( iwtype, mbchan, jdata, 1, ier )
		  END IF
C
C*		  Move a single word at a time.
C
	       ELSE
		  iknt = iknt + 1
		  npoint = npoint + 1
		  nwout  = nwout + 1
		  idata ( iknt ) = ibuff ( npoint )
	      END IF
	END DO
C
C*	If current buffer has been completed, reset flag.
C
	IF  (( iret .ne. NORMAL ) .or. ( nwout .ge. nwexp )) ntypsr = -1
C*
	RETURN
	END
