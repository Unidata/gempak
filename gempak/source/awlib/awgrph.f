	SUBROUTINE AW_GRPH ( bdata, irept, isub, lenbyt, iff, iret )
C************************************************************************
C* AW_GRPH                                                              *
C*                                                                      *
C* This subroutine sends the vector graphic block to the proper vector  *
C* graphics block.  This subroutine represents mode 4.                  *
C* 								        *
C* AW_GRPH ( BDATA, IREPT, ISUB, LENBYT, IFF, IRET)                   	*
C*								        *
C* Input parameters: 						        *
C*      BDATA(*) BYTE	        Byte data array                         *
C*      IREPT   INTEGER         Beginning byte postion for the block    *
C*      ISUB    INTEGER         Submode number                          *
C*      LENBYT  INTEGER         Length of the block in bytes            *
C*      IFF     INTEGER         Length and checksum indicator           *
C*								        *
C* Output parameters:						        *
C*	IRET	INTEGER		Return code                             *
C**								        *
C* Log:									*
C* A. Hardy/GSC          7/98                                           *
C* A. Hardy/GSC          3/99	Updated prolog; Added byte declaration  *
C* A. Hardy/GSC         10/00	Activated AW_GCUV;Put file number and   *
C*                              dbug in awcmn.cmn			*
C* T. Piper/SAIC	 2/03	Changed b to bdata 			*
C************************************************************************
 	INCLUDE 	'awcmn.cmn'
C*
	BYTE		bdata(*)
C------------------------------------------------------------------------
	iret=0
C
C*	Check the submode byte to call proper subroutine.
C
        IF ( isub .eq. 16 ) THEN
C
C*          Graphics product definition block.
C
	    CALL AW_GDEF ( bdata, irept, iff, iret )
          ELSE IF ( isub .eq. 17 ) THEN
C
C*          Map background definition block.  Right now, this submode 
C*          is defined as the map definition block prior to the 
C*          1992 manual.  Therefore, the map call is place here.
C
            CALL AW_PMAP ( bdata, irept, iff, iret )
          ELSE IF ( isub .eq. 30 ) THEN
	    write(6,*)'Mode 4 Submode 30 subroutine does not exist.'
C
C*          Define graphics parameters block.
C

C           CALL AW_GPAR ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 1 ) THEN
c	    write(6,*)'Mode 4 Submode 1 subroutine does not exist.'
C
C*          Absolute vectors block.
C
       	    CALL AW_GABV ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 2 ) THEN
	    write(6,*)'Mode 4 Submode 2 subroutine does not exist.'
C
C*          Relative vectors block.
C
C      	    CALL AW_GREL ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 3 ) THEN
	    write(6,*)'Mode 4 Submode 3 subroutine does not exist.'
C
C*          Calcomp pen command vectors block.
C
C      	    CALL AW_GCPC ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 4 ) THEN
	    write(6,*)'Mode 4 Submode 4 subroutine does not exist.'
C
C*          Variable exception vectors block.
C
C      	    CALL AW_GVEV ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 5 ) THEN
C
C*          Long/short vectors block.
C
       	    CALL AW_GLSV ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 6 ) THEN
	    write(6,*)'Mode 4 Submode 6 subroutine does not exist.'
C
C*          Point/slope vectors block.
C
C	    CALL AW_GPSV ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 7 ) THEN
	    write(6,*)'Mode 4 Submode 7 subroutine does not exist.'
C
C*          Wind barbs vectors block.
C
C	    CALL AW_GWBV ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 8 ) THEN
C
C*          Vector arrow plot block. 
C	    CALL AW_GVAR ( bdata, irept, lenbyt, iff, iret )
	    write(6,*)'Mode 4 Submode 8 subroutine does not exist.'
          ELSE IF ( isub .eq. 9 ) THEN
	    write(6,*)'Mode 4 Submode 9 subroutine does not exist.'
C
C*          Center radius arc vectors block.
C
C	    CALL AW_GCRA ( bdata, irept, lenbyt, iff, iret )
          ELSE IF ( isub .eq. 10 ) THEN
C
C*          Curve vectors block.
C
 	    CALL AW_GCUV ( bdata, irept, lenbyt, iff, iret )
	  ELSE
	    IF ( dbug .eq. 'y' ) THEN
	        write(flun,*)'Error in reading block submode.'
	        write(flun,*)'Submode was read as: ',isub
	    END IF
        END IF
C*
	RETURN
	END
