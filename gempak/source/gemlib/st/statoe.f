 	SUBROUTINE ST_ATOE ( instr, nchar, barray, iret) 
C************************************************************************
C* ST_ATOE								*
C*									*
C* This subroutine converts a character string in ASCII to EBCDIC       *
C* numerical values.                                                    *
C*									*
C* ST_ATOE  ( INSTR, NCHAR, BARRAY, IRET )				*
C*									*
C* Input parameters:							*
C*	INSTR (*)	CHAR		ASCII characters		*
C*	NCHAR		INTEGER		Number of characters		*
C*									*
C* Output parameters:							*
C*	BARRAY (NCHAR)	BYTE		EBCDIC values                   *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* A. Hardy/GSC          8/98     	Modified from st_etoa           *
C* A. Hardy/GSC          9/98     	Changed array size of instr     *
C* T. Piper/GSC		 3/99		Updated prolog			*
C************************************************************************
	CHARACTER       instr(*)
C*
        CHARACTER       string
	INTEGER         ITABLE (128)
	BYTE            barray(nchar)
        INTEGER         nchar, number 
C*
       DATA   ITABLE  /
     +	  000, 001, 002, 003, 055, 045, 046, 047, 022, 005,
     +	  037, 011, 012, 013, 014, 015, 016, 017, 018, 019, 
     +	  060, 061, 050, 038, 024, 025, 063, 039, 028, 029, 
     +	  030, 031, 064, 090, 127, 123, 091, 108, 080, 125,
     +	  077, 093, 092, 078, 107, 096, 075, 097, 240, 241,
     +	  242, 243, 244, 245, 246, 247, 248, 249, 122, 094, 
     +	  076, 126, 110, 111, 124, 193, 194, 195, 196, 197,
     +	  198, 199, 200, 201, 209, 210, 211, 212, 213, 214,
     +	  215, 216, 217, 226, 227, 228, 229, 230, 231, 232,
     +	  233, 173, 224, 189, 095, 109, 121, 129, 130, 131, 
     +	  132, 133, 134, 135, 136, 137, 145, 146, 147, 148,
     +	  149, 150, 151, 152, 153, 162, 163, 164, 165, 166,
     +    167, 168, 169, 192, 250, 208, 161, 007  /
C------------------------------------------------------------------------
C
C*	Convert one character at a time.
C
	DO  ii = 1, nchar
            string = instr (ii)   
            number = ICHAR ( string )
            barray ( ii ) = ITABLE (number + 1) 
	END DO
	iret = 0
C*
        RETURN
	END
