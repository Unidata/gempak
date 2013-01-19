C  =====================================================================
C  pgm: SHERRM .. Output err or warng message for given error number
C
C  use:     CALL SHERRM(CMD,NUM)
C
C   in: CMD ....... control char for command to be performed - CHAR*1
C   in:               'I' .. initialize vars (automatic for first entry)
C   in:               'W' .. output warning message
C   in:               'E' .. output error message
C   in:               'M' .. output message without warng or error stmt
C   in: NUM ....... number of the error message - INT
C   in: (subrtn) .. enter logical unit number outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFERROR',<number>)
C
C  rqd: SHSAVU
C  =====================================================================
      SUBROUTINE SHERRM(CMD,NUM)

      EXTERNAL       SHSAVU

      INTEGER        NUM,JJ,NM,INITZ,LUNE,LSTAT
      CHARACTER*1    CMD
      CHARACTER*62   MSG(90)
      CHARACTER*62   MM1(15),MM2(15),MM3(15),MM4(15),MM5(15),MM6(15)

      CHARACTER*80   LMSG

      EQUIVALENCE (MSG(1),MM1),(MSG(16),MM2),(MSG(31),MM3),(MSG(46),MM4)
      EQUIVALENCE (MSG(61),MM5),(MSG(76),MM6)

      SAVE           INITZ,LUNE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob5/rfc/ofs/src/shefpars_driv/RCS/sherrm.f,v $
     . $',                                                             '
     .$Id: sherrm.f,v 1.10 2000/03/14 14:21:25 page Exp $
     . $' /
C    ===================================================================
C

      DATA           INITZ / 0 /

      DATA MM1
     ./'                                                              '
     .,'Two digits are required in date or time group                 '
     .,'An expected parameter code is missing                         '
     .,'File read error while accessing data file                     '
     .,'No dot in column 1 when looking for new message               '
     .,'Dot found but not in column 1 of new message                  '
     .,'Unknown message type, looking for .A, .B, or .E               '
     .,'Bad char in message type format (or missing blank delimiter)  '
     .,'Last message format was different from this continuation messg'
     .,'Last message was NOT a revision unlike this continuation messg'
     .,'Last message had an error so cannot continue                  '
     .,'No positional data or no blank before it                      '
     .,'Bad character in station id                                   '
     .,'Station id has more than 8 characters                         '
     .,'Bad number in positional data date group                      '/
      DATA MM2
     ./'Incorrect number in date group                                '
     .,'Incorrect number in time group                                '
     .,'Missing blank char in positional data                         '
     .,'Bad creation date                                             '
     .,'Bad date code letter after the character "D"                  '
     .,'Unknown data qualifier, data value is lost                    '
     .,'Unknown data units code (need S or E)                         '
     .,'Unknown duration code                                         '
     .,'Bad 2-digit number following duration code                    '
     .,'Unknown time interval code (need Y,M,D,H,N,S,E)               '
     .,'Bad 2-digit number following time interval code               '
     .,'Bad character after "DR" (relative date code)                 '
     .,'Bad 1- or 2-digit number in relative date code                '
     .,'Bad character in parameter code                               '
     .,'Bad parameter code calls for send code                        '/
      DATA MM3
     ./'Trace for code other than PP, PC, PY, SD, SF, SW              '
     .,'Variable duration not defined                                 '
     .,'Bad character where delimiter is expected                     '
     .,'Non-existent value for given type and source parameter code   '
     .,'ZULU, DR, or DI has send code QY, PY, or HY                   '
     .,'Forecast data given without creation date                     '
     .,'No value given after parameter code and before slash or eol   '
     .,'Explicit date for codes DRE or DIE is not the end-of-month    '
     .,'Year not in good range (1753-2199)                            '
     .,'Exceeded limit of data items                                  '
     .,'Too many data items for given .B format                       '
     .,'Not enough data items for given .B format                     '
     .,'Cannot adjust forecast date to Zulu time                      '
     .,'Time between 0201 & 0259 on day changing from stnd to daylight'
     .,'No time increment specified (use DI code)                     '/
      DATA MM4
     ./'No ".END" message for previous ".B" format                    '
     .,'ID requires 3 to 8 characters                                 '
     .,'For dayl savgs time, check Apr or Oct for 1976 thru 2040 only '
     .,'Bad character in the message                                  '
     .,'Missing parameter code                                        '
     .,'Bad value chars (or missing delimiter), data may be lost      '
     .,'Bad character in data field                                   '
     .,'"?" not accepted for missing, use "M" or "+"                  '
     .,'Parameter code is too long or too short                       '
     .,'Missing delimiter between data type fields                    '
     .,'Missing delimiter after data type field                       '
     .,'Should use "/" after date, time, or other D-code; before data '
     .,'Parm codes PP and PC require decimal value                    '
     .,'Abort, cannot read "shefparm" file correctly                  '
     .,'Non-existent value for given duration parameter code          '/
      DATA MM5
     ./'Non-existent value for given extremum parameter code          '
     .,'Non-existent value for given conversion factor parameter code '
     .,'Non-existent value for given probability parameter code       '
     .,'Parameter code too short or field misinterpreted as param-code'
     .,'Comma not allowed in data field, data value is lost           '
     .,'Date check for yr-mo-da shows bad date                        '
     .,'No data on line identified with a message type format         '
     .,'An unexpected ".END" message was encountered                  '
     .,' BUMMER!!!  Maximum number of errors reached, abort message   '
     .,'Cannot output to binary shefpars file                         '
     .,'Cannot access "PE conversion factors" from the "shefparm" file'
     .,'Cannot access "send codes" from the "shefparm" file           '
     .,'Cannot access "duration codes" from the "shefparm" file       '
     .,'Cannot access "type/source codes" from the "shefparm" file    '
     .,'Cannot access "extremum codes" from the "shefparm" file       '/
      DATA MM6
     ./'Cannot access "probability codes" from the "shefparm" file    '
     .,'Cannot read "SHEFPARM" file!!!!!                              '
     .,'Bad character in data value, data value is lost               '
     .,'Julian day should be written with 3 digits                    '
     .,'Too many digits in date group!                                '
     .,'Too many characters in quotes                                 '
     .,'Data line found before completing .B format line(s)           '
     .,'Missing slash delimiter or bad time zone code                 '
     .,'Too many chars in qualifier code, data value is lost          '
     .,'Bad data qualifier, rest of format is lost                    '
     .,'Retained comment found without a data value, comment is lost  '
     .,'Unexpected slash found after parameter code, before data value'
     .,'Cannot access "qualifier codes" from the "shefparm" file      '
     .,'                                                              '
     .,'Unknown error number given                                    '/

        IF (CMD.EQ.'I' .OR. INITZ.EQ.0) THEN
          INITZ = 1
          CALL SHSAVU('G_SHEFERROR ',LUNE)
        ENDIF

        IF (CMD.EQ.'W' .OR. CMD.EQ.'E' .OR. CMD.EQ.'M') THEN
         IF (LUNE .GE. 0) THEN
          NM = NUM
          IF (NM.LT.1 .OR. NM.GT.90) NM = 90

          JJ = 62
   20     IF (MSG(NM)(JJ:JJ).NE.' ' .OR. JJ.LE.1) GOTO 30
            JJ = JJ-1
            GOTO 20
   30     CONTINUE

          IF (CMD .EQ. 'W') THEN
C          WRITE(LUNE,'(''  ** WARNG'',I3,'' ** '',A)',IOSTAT=LSTAT)
C    $           NM,MSG(NM)(1:JJ)
           WRITE(LMSG,'(''  ** WARNG'',I3,'' ** '',A)',IOSTAT=LSTAT)
     $           NM,MSG(NM)(1:JJ)
          ELSEIF (CMD .EQ. 'E') THEN
C          WRITE(LUNE,'(''  ** ERROR'',I3,'' ** '',A)',IOSTAT=LSTAT)
C    $           NM,MSG(NM)(1:JJ)
           WRITE(LMSG,'(''  ** ERROR'',I3,'' ** '',A)',IOSTAT=LSTAT)
     $           NM,MSG(NM)(1:JJ)
          ELSEIF (CMD .EQ. 'M') THEN
C           WRITE(LUNE,'(1X,A)',IOSTAT=LSTAT) MSG(NM)(1:JJ)
            WRITE(LMSG,'(1X,A)',IOSTAT=LSTAT) MSG(NM)(1:JJ)
          ENDIF
          CALL DC_WLOG ( 2, 'DC', 2, LMSG, IERWLG )
         ENDIF

        ENDIF

      RETURN
      END
