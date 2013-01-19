	SUBROUTINE DC_WLOG ( loglev, errgrp, numerr, errstr, iret )
C************************************************************************
C* DC_WLOG								*
C*									*
C* This routine constructs a message to be written to the decoder	*
C* log file. Different levels of verbosity and debug may be specified	*
C* to allow the user to turn on and off certain messages.		*
C*									*
C* If the ERRGRP is blank, then only the string contained in ERRSTR is	*
C* written to the log file. If the string is longer than the maximum	*
C* allowable length (44), then the string is broken into smaller	*
C* strings for output to the log file.					*
C*									*
C* DC_WLOG ( LOGLEV, ERRGRP, NUMERR, ERRSTR, IRET )			*
C*									*
C* Input parameters:							*
C*	LOGLEV		INTEGER		Logging level			*
C*					   0 = normal log message	*
C*					  >0 = level of verbosity	*
C*	ERRGRP		CHAR*		Error group			*
C*	NUMERR		INTEGER		Error number			*
C*	ERRSTR		CHAR*		String to be embedded in a error*
C*					  OR full message to be logged	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 8/95						*
C* J. Ator/NCEP		10/95	Added continuation lines		*
C* S. Jacobs/NCEP	 7/96	Updated documentation			*
C* K. Tyle/GSC           1/97   Added logical timflg to call to ER_MMSG *
C* K. Tyle/GSC		 1/97	Call ER_LMSG and DC_WBUF; eliminate call*
C*				to ER_MMSG				*
C* J. Ator/NCEP		12/97	Reduce lineln from 132 to 44		*
C* S. Jacobs/NCEP	 1/00	Changed check for numerr = 0		*
C* Chiz/Unidata          4/00   Allow numerr=0 for status info          
C************************************************************************
	INCLUDE		'dccmn.cmn'
C*
	CHARACTER*(*)	errgrp, errstr
C*
	CHARACTER	outmsg*44
C------------------------------------------------------------------------
	iret = 0
C
C*	Return if the error number is zero.
C
C	IF  ( numerr .eq. 0 )  RETURN
C
C*	Set the string to be printed to the log file. If the error group
C*	is blank use the entire input string.
C
	IF  (( errgrp .eq. ' ' ) .or. (numerr .eq.0 ))  THEN
C
C*	    Determine the length of the input string. If the length
C*	    is zero, return.
C
	    CALL ST_LSTR  ( errstr, lens, ier )
	    IF  ( lens .eq. 0 )  RETURN
C
C*	    Loop until the remaining substring is greater than the 
C*	    maximum allowable string length.
C
	    lineln = 44
	    ipt1   = 1
	    DO WHILE  ( ( lens - ipt1 + 1 ) .gt. lineln )
C
C*	    	There are more than lineln characters remaining to be
C*	    	written to the decoder log.
C
		ipt2 = ipt1 + lineln - 1
C
C*	    	Find the most recent blank character, up to a limit of
C*	    	one-half of lineln, and make that character the end of
C*		the current line.
C
		DO WHILE  ( ( ( ipt2 - ipt1 + 1 ) .gt. ( lineln / 2 ) )
     +			    	.and.
     +			    ( errstr ( ipt2 : ipt2 ) .ne. ' ' ) )
		    ipt2 = ipt2 - 1
		END DO
		outmsg = errstr ( ipt1 : ipt2 )
		lenws  = ipt2 - ipt1 + 1
		CALL ER_LMSG ( loglev, errgrp, numerr, outmsg, iret )
		CALL DC_WBUF ( iret ) 
		ipt1   = ipt2 + 1
	    END DO
C
C*	    Write the remaining portion of the string (if any) to the
C*	    decoder log.
C
	    IF  ( ipt1 .le. lens )  THEN
		outmsg = errstr ( ipt1 : lens )
		lenws  = lens - ipt1 + 1
		CALL ER_LMSG ( loglev, errgrp, numerr, outmsg, iret )
		CALL DC_WBUF ( iret ) 
	    END IF
	  ELSE
C
C*	    Write message to the log file.
C
	    CALL ER_LMSG ( loglev, errgrp, numerr, errstr, iret )
	    CALL DC_WBUF ( iret ) 
	END IF
C*
	RETURN
	END
