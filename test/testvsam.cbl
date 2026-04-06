       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTVSAM.
      *==============================================================*
      * PROGRAMA DE EJEMPLO CON VSAM PARA PROBAR V2D2               *
      * TIENE: SELECT/ASSIGN, FD, REDEFINES, READ/WRITE/REWRITE     *
      *==============================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTFILE ASSIGN TO CUSTDD
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CUST-ID
               FILE STATUS IS WS-FS.
           SELECT LOGFILE ASSIGN TO LOGDD
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FS2.
       DATA DIVISION.
       FILE SECTION.
       FD  CUSTFILE.
       01  CUST-RECORD.
           05 CUST-ID          PIC X(10).
           05 CUST-NAME        PIC X(30).
           05 CUST-TYPE        PIC X(1).
           05 CUST-DETAIL REDEFINES CUST-TYPE.
              10 CUST-DET-TYPE PIC X(1).
           05 CUST-DATA.
              10 CUST-ADDR     PIC X(50).
              10 CUST-PHONE    PIC X(15).
              10 CUST-BAL      PIC S9(7)V99 COMP-3.
           05 CUST-CORP REDEFINES CUST-DATA.
              10 CORP-NAME     PIC X(40).
              10 CORP-CONTACT  PIC X(20).
              10 CORP-BAL      PIC S9(9)V99 COMP-3.
       FD  LOGFILE.
       01  LOG-RECORD.
           05 LOG-DATE         PIC X(10).
           05 LOG-TIME         PIC X(8).
           05 LOG-MSG          PIC X(62).
       WORKING-STORAGE SECTION.
       01  WS-FS              PIC XX.
       01  WS-FS2             PIC XX.
       01  WS-EOF             PIC X VALUE 'N'.
       01  WS-COUNT           PIC 9(5) VALUE 0.
       01  WS-KEY             PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN INPUT CUSTFILE.
           OPEN OUTPUT LOGFILE.
           PERFORM READ-ALL-CUSTS.
           PERFORM UPDATE-CUST.
           PERFORM ADD-CUST.
           PERFORM DELETE-CUST.
           CLOSE CUSTFILE.
           CLOSE LOGFILE.
           STOP RUN.
       READ-ALL-CUSTS.
           MOVE LOW-VALUES TO CUST-ID.
           START CUSTFILE KEY >= CUST-ID.
           PERFORM UNTIL WS-EOF = 'Y'
             READ CUSTFILE NEXT
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END ADD 1 TO WS-COUNT
             END-READ
           END-PERFORM.
       UPDATE-CUST.
           MOVE 'CUST000001' TO CUST-ID.
           READ CUSTFILE KEY IS CUST-ID.
           IF WS-FS = '00'
             MOVE 'UPDATED NAME' TO CUST-NAME
             REWRITE CUST-RECORD
           END-IF.
       ADD-CUST.
           MOVE 'CUST999999' TO CUST-ID.
           MOVE 'NEW CUSTOMER' TO CUST-NAME.
           MOVE 'P' TO CUST-TYPE.
           WRITE CUST-RECORD.
       DELETE-CUST.
           MOVE 'CUST999999' TO CUST-ID.
           DELETE CUSTFILE.
       LOG-MESSAGE.
           MOVE 'PROCESS COMPLETE' TO LOG-MSG.
           WRITE LOG-RECORD.
