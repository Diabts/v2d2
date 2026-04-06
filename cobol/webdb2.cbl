       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEBDB2.
      *==============================================================*
      * CICS WEB PROGRAM - MUESTRA TABLAS DB2 EN HTML                *
      * URL: /cics/cwba/webdb2                                       *
      * Compatible: CICS TS 2.2 + COBOL v3.2 + DB2 v7               *
      *==============================================================*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL INCLUDE SQLCA END-EXEC.
       01  WS-RESPONSE        PIC X(8000) VALUE SPACES.
       01  WS-RESP-LEN        PIC S9(8) COMP VALUE 0.
       01  WS-RESP-PTR        PIC S9(8) COMP VALUE 1.
       01  WS-SQLCODE         PIC S9(9) COMP.
       01  WS-COUNT           PIC 9(5) VALUE 0.
       01  WS-COUNT-DISP      PIC Z(4)9.
       01  WS-ROW.
           05 WS-TNAME        PIC X(18).
           05 WS-CREATOR      PIC X(8).
           05 WS-TYPE         PIC X(1).
           05 WS-DBNAME       PIC X(8).
           05 WS-COLCOUNT     PIC S9(4) COMP.
       01  WS-COLCOUNT-DISP   PIC Z(3)9.
       01  WS-TYPE-DESC       PIC X(10).
       01  WS-DOCTOKEN        PIC X(16).
       01  WS-TRAIL-CNT       PIC S9(8) COMP VALUE 0.
           EXEC SQL DECLARE C1 CURSOR FOR
             SELECT NAME, CREATOR, TYPE, DBNAME,
                    COLCOUNT
               FROM SYSIBM.SYSTABLES
              WHERE CREATOR NOT IN
                    ('SYSIBM', 'SYSPROC')
              ORDER BY CREATOR, NAME
              FETCH FIRST 50 ROWS ONLY
           END-EXEC.
       PROCEDURE DIVISION.
       MAIN-PARA.
      * BUILD HTML HEADER
           MOVE 1 TO WS-RESP-PTR.
           STRING
             '<html><head>'
             '<title>DB2 Tables</title>'
             '</head><body>'
             '<h1>z/OS DB2 v7</h1>'
             '<table border=1>'
             '<tr><th>TABLE</th>'
             '<th>CREATOR</th>'
             '<th>TYPE</th>'
             '<th>DB</th>'
             '<th>COLS</th></tr>'
             DELIMITED BY SIZE
             INTO WS-RESPONSE
             WITH POINTER WS-RESP-PTR
           END-STRING.
           SUBTRACT 1 FROM WS-RESP-PTR
             GIVING WS-RESP-LEN.

      * OPEN CURSOR AND FETCH ROWS
           EXEC SQL OPEN C1 END-EXEC.
           MOVE SQLCODE TO WS-SQLCODE.
           IF WS-SQLCODE NOT = 0
             GO TO SEND-RESPONSE
           END-IF.

           PERFORM FETCH-LOOP
             UNTIL WS-SQLCODE NOT = 0.

           EXEC SQL CLOSE C1 END-EXEC.

      * ADD FOOTER
           MOVE WS-COUNT TO WS-COUNT-DISP.
           ADD 1 TO WS-RESP-LEN
             GIVING WS-RESP-PTR.
           STRING
             '</table><p>'
             WS-COUNT-DISP
             ' tables</p>'
             '</body></html>'
             DELIMITED BY SIZE
             INTO WS-RESPONSE
             WITH POINTER WS-RESP-PTR
           END-STRING.
           SUBTRACT 1 FROM WS-RESP-PTR
             GIVING WS-RESP-LEN.

       SEND-RESPONSE.
           EXEC CICS DOCUMENT CREATE
             DOCTOKEN(WS-DOCTOKEN)
             TEXT(WS-RESPONSE)
             LENGTH(WS-RESP-LEN)
           END-EXEC.

           EXEC CICS WEB SEND
             DOCTOKEN(WS-DOCTOKEN)
             CLNTCODEPAGE('iso-8859-1')
           END-EXEC.

           EXEC CICS RETURN END-EXEC.
           STOP RUN.

       FETCH-LOOP.
           EXEC SQL FETCH C1
             INTO :WS-TNAME, :WS-CREATOR,
                  :WS-TYPE, :WS-DBNAME,
                  :WS-COLCOUNT
           END-EXEC.
           MOVE SQLCODE TO WS-SQLCODE.
           IF WS-SQLCODE = 0
             ADD 1 TO WS-COUNT
             MOVE WS-COLCOUNT
               TO WS-COLCOUNT-DISP
             EVALUATE WS-TYPE
               WHEN 'T'
                 MOVE 'TABLE' TO WS-TYPE-DESC
               WHEN 'V'
                 MOVE 'VIEW' TO WS-TYPE-DESC
               WHEN 'G'
                 MOVE 'GTEMP' TO WS-TYPE-DESC
               WHEN 'X'
                 MOVE 'AUX' TO WS-TYPE-DESC
               WHEN OTHER
                 MOVE WS-TYPE TO WS-TYPE-DESC
             END-EVALUATE
             ADD 1 TO WS-RESP-LEN
               GIVING WS-RESP-PTR
             STRING
               '<tr><td>' WS-TNAME '</td>'
               '<td>' WS-CREATOR '</td>'
               '<td>' WS-TYPE-DESC '</td>'
               '<td>' WS-DBNAME '</td>'
               '<td>' WS-COLCOUNT-DISP
               '</td></tr>'
               DELIMITED BY SIZE
               INTO WS-RESPONSE
               WITH POINTER WS-RESP-PTR
             END-STRING
             SUBTRACT 1 FROM WS-RESP-PTR
               GIVING WS-RESP-LEN
           END-IF.
