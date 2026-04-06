       IDENTIFICATION DIVISION.
       PROGRAM-ID. V2D2MAIN.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RESP         PIC S9(8) COMP.
       01  WS-MSG          PIC X(79) VALUE SPACES.
       01  WS-OPT-IN       PIC X(1)  VALUE SPACES.
       01  WS-ABSTIME      PIC S9(15) COMP-3.
       01  WS-DATE-FORM    PIC X(10).
       01  WS-NEW-JOB      PIC S9(9) COMP VALUE 0.
       01  WS-DSN-IN       PIC X(44) VALUE SPACES.
       01  WS-MBR-IN       PIC X(8)  VALUE SPACES.
       01  WS-SQLCD        PIC -(8)9.
       01  WS-LEN60        PIC S9(4) COMP VALUE +60.
       01  WS-LAST-JOB     PIC S9(9) COMP VALUE 0.
       01  WS-LAST-DSN     PIC X(44).
       01  WS-LAST-MBR     PIC X(8).
       01  WS-LAST-STS     PIC X(1).
       01  WS-LAST-VCNT    PIC S9(4) COMP VALUE 0.
       01  WS-FILE-CNT     PIC S9(9) COMP VALUE 0.
       01  WS-FIELD-CNT    PIC S9(9) COMP VALUE 0.
       01  WS-OP-CNT       PIC S9(9) COMP VALUE 0.
       01  WS-JOB-DISP     PIC ZZ9.
       01  WS-CNT-DISP     PIC ZZ9.
       01  WS-STS-TEXT      PIC X(12).
           EXEC SQL INCLUDE SQLCA END-EXEC.
       COPY V2D2MAP.
       PROCEDURE DIVISION.
       MAIN-PARA.
           EXEC CICS HANDLE AID
               PF3(EXIT-PARA)
           END-EXEC.
           EXEC CICS ASKTIME
               ABSTIME(WS-ABSTIME)
           END-EXEC.
           EXEC CICS FORMATTIME
               ABSTIME(WS-ABSTIME)
               DDMMYYYY(WS-DATE-FORM)
               DATESEP('/')
           END-EXEC.
           MOVE WS-DATE-FORM TO M01DATEO.
           MOVE 'Bienvenido al sistema V2D2'
             TO M01MSGO.
           EXEC CICS SEND MAP('V2D2M01')
               MAPSET('V2D2MAP') ERASE
           END-EXEC.
       RECEIVE-PARA.
           EXEC CICS RECEIVE MAP('V2D2M01')
               MAPSET('V2D2MAP')
           END-EXEC.
           MOVE M01OPTI TO WS-OPT-IN.
           EVALUATE WS-OPT-IN
             WHEN '1'
               GO TO ANALYZE-PARA
             WHEN '2'
               GO TO DDL-PARA
             WHEN '3'
               GO TO CONVERT-PARA
             WHEN '4'
               GO TO COMPILE-PARA
             WHEN '5'
               MOVE 'Migracion datos - pendiente'
                 TO M01MSGO
               EXEC CICS SEND MAP('V2D2M01')
                   MAPSET('V2D2MAP') ERASE
               END-EXEC
             WHEN '6'
               GO TO STATUS-PARA
             WHEN 'X'
               GO TO EXIT-PARA
             WHEN 'x'
               GO TO EXIT-PARA
             WHEN OTHER
               MOVE 'Opcion invalida'
                 TO M01MSGO
               EXEC CICS SEND MAP('V2D2M01')
                   MAPSET('V2D2MAP') ERASE
               END-EXEC
           END-EVALUATE.
           GO TO RECEIVE-PARA.
      *==========================================================
      * OPTION 1: ANALYZE PROGRAM
      *==========================================================
       ANALYZE-PARA.
           EXEC CICS HANDLE AID
               PF3(MAIN-PARA)
           END-EXEC.
           MOVE 'Ingrese dataset y member'
             TO M02MSGO.
           EXEC CICS SEND MAP('V2D2M02')
               MAPSET('V2D2MAP') ERASE
           END-EXEC.
       ANAL-RECEIVE.
           EXEC CICS RECEIVE MAP('V2D2M02')
               MAPSET('V2D2MAP')
           END-EXEC.
           MOVE M02DSNI TO WS-DSN-IN.
           MOVE M02MBRI TO WS-MBR-IN.
           IF WS-MBR-IN = SPACES
             MOVE 'Ingrese un member'
               TO M02MSGO
             EXEC CICS SEND MAP('V2D2M02')
                 MAPSET('V2D2MAP') ERASE
             END-EXEC
             GO TO ANAL-RECEIVE
           END-IF.
           EXEC SQL
             SELECT COALESCE(MAX(JOB_ID),0) + 1
             INTO :WS-NEW-JOB
             FROM IBMUSER.V2D2_JOBS
           END-EXEC.
           EXEC SQL
             INSERT INTO IBMUSER.V2D2_JOBS
             (JOB_ID, SOURCE_DSN, MEMBER,
              STATUS, VSAM_COUNT, CREATED_TS)
             VALUES (:WS-NEW-JOB, :WS-DSN-IN,
              :WS-MBR-IN, 'A', 0,
              CURRENT TIMESTAMP)
           END-EXEC.
           IF SQLCODE NOT = 0
             MOVE SQLCODE TO WS-SQLCD
             MOVE SPACES TO M02MSGO
             STRING 'Error DB2: ' WS-SQLCD
               DELIMITED BY SIZE
               INTO M02MSGO
             EXEC CICS SEND MAP('V2D2M02')
                 MAPSET('V2D2MAP') ERASE
             END-EXEC
             GO TO ANAL-RECEIVE
           END-IF.
           EXEC SQL COMMIT END-EXEC.
           MOVE WS-NEW-JOB TO WS-JOB-DISP.
           MOVE SPACES TO M02MSGO.
           STRING 'Job ' WS-JOB-DISP
             ' creado. Ejecute V2D2RUNA.'
             DELIMITED BY SIZE
             INTO M02MSGO.
           EXEC CICS SEND MAP('V2D2M02')
               MAPSET('V2D2MAP') ERASE
           END-EXEC.
           GO TO ANAL-RECEIVE.
      *==========================================================
      * OPTION 2: GENERATE DDL
      *==========================================================
       DDL-PARA.
           PERFORM GET-LAST-JOB.
           IF WS-LAST-JOB = 0
             MOVE 'No hay jobs. Analice primero.'
               TO M01MSGO
             EXEC CICS SEND MAP('V2D2M01')
                 MAPSET('V2D2MAP') ERASE
             END-EXEC
             GO TO RECEIVE-PARA
           END-IF.
           PERFORM DECODE-STATUS.
           PERFORM GET-COUNTS.
           MOVE WS-LAST-JOB TO WS-JOB-DISP.
           MOVE WS-FILE-CNT TO WS-CNT-DISP.
           MOVE SPACES TO M01MSGO.
           STRING 'Job' WS-JOB-DISP ' '
             WS-LAST-MBR ' '
             WS-CNT-DISP 'arch. Ejecute V2D2RUND'
             DELIMITED BY SIZE
             INTO M01MSGO.
           EXEC CICS SEND MAP('V2D2M01')
               MAPSET('V2D2MAP') ERASE
           END-EXEC.
           GO TO RECEIVE-PARA.
      *==========================================================
      * OPTION 3: CONVERT PROGRAM
      *==========================================================
       CONVERT-PARA.
           PERFORM GET-LAST-JOB.
           IF WS-LAST-JOB = 0
             MOVE 'No hay jobs. Analice primero.'
               TO M01MSGO
             EXEC CICS SEND MAP('V2D2M01')
                 MAPSET('V2D2MAP') ERASE
             END-EXEC
             GO TO RECEIVE-PARA
           END-IF.
           PERFORM DECODE-STATUS.
           MOVE WS-LAST-JOB TO WS-JOB-DISP.
           MOVE SPACES TO M01MSGO.
           STRING 'Job' WS-JOB-DISP ' '
             WS-LAST-MBR ' '
             WS-STS-TEXT ' Ejecute V2D2RUNX'
             DELIMITED BY SIZE
             INTO M01MSGO.
           EXEC CICS SEND MAP('V2D2M01')
               MAPSET('V2D2MAP') ERASE
           END-EXEC.
           GO TO RECEIVE-PARA.
      *==========================================================
      * OPTION 4: COMPILE PROGRAM
      *==========================================================
       COMPILE-PARA.
           PERFORM GET-LAST-JOB.
           IF WS-LAST-JOB = 0
             MOVE 'No hay jobs. Analice primero.'
               TO M01MSGO
             EXEC CICS SEND MAP('V2D2M01')
                 MAPSET('V2D2MAP') ERASE
             END-EXEC
             GO TO RECEIVE-PARA
           END-IF.
           PERFORM DECODE-STATUS.
           MOVE WS-LAST-JOB TO WS-JOB-DISP.
           MOVE SPACES TO M01MSGO.
           STRING 'Job' WS-JOB-DISP ' '
             WS-LAST-MBR ' '
             WS-STS-TEXT ' Ejecute V2D2COMP'
             DELIMITED BY SIZE
             INTO M01MSGO.
           EXEC CICS SEND MAP('V2D2M01')
               MAPSET('V2D2MAP') ERASE
           END-EXEC.
           GO TO RECEIVE-PARA.
      *==========================================================
      * OPTION 6: VIEW STATUS
      *==========================================================
       STATUS-PARA.
           EXEC CICS HANDLE AID
               PF3(MAIN-PARA)
           END-EXEC.
           PERFORM GET-LAST-JOB.
           IF WS-LAST-JOB = 0
             MOVE 'No hay jobs registrados'
               TO M02MSGO
             EXEC CICS SEND MAP('V2D2M02')
                 MAPSET('V2D2MAP') ERASE
             END-EXEC
             GO TO STATUS-WAIT
           END-IF.
           PERFORM DECODE-STATUS.
           PERFORM GET-COUNTS.
           MOVE WS-LAST-DSN TO M02DSNO.
           MOVE WS-LAST-MBR TO M02MBRO.
           MOVE WS-LAST-JOB TO WS-JOB-DISP.
           MOVE WS-FILE-CNT TO WS-CNT-DISP.
           MOVE SPACES TO M02MSGO.
           STRING 'Job' WS-JOB-DISP ' '
             WS-STS-TEXT ' Archivos=' WS-CNT-DISP
             DELIMITED BY SIZE
             INTO M02MSGO.
           EXEC CICS SEND MAP('V2D2M02')
               MAPSET('V2D2MAP') ERASE
           END-EXEC.
       STATUS-WAIT.
           EXEC CICS RECEIVE MAP('V2D2M02')
               MAPSET('V2D2MAP')
           END-EXEC.
           GO TO MAIN-PARA.
      *==========================================================
      * COMMON PARAGRAPHS
      *==========================================================
       GET-LAST-JOB.
           EXEC SQL
             SELECT JOB_ID, SOURCE_DSN, MEMBER,
               STATUS, VSAM_COUNT
             INTO :WS-LAST-JOB, :WS-LAST-DSN,
               :WS-LAST-MBR, :WS-LAST-STS,
               :WS-LAST-VCNT
             FROM IBMUSER.V2D2_JOBS
             WHERE JOB_ID = (SELECT MAX(JOB_ID)
               FROM IBMUSER.V2D2_JOBS)
           END-EXEC.
           IF SQLCODE NOT = 0
             MOVE 0 TO WS-LAST-JOB
           END-IF.
       GET-COUNTS.
           EXEC SQL
             SELECT COUNT(*) INTO :WS-FILE-CNT
             FROM IBMUSER.V2D2_FILES
             WHERE JOB_ID = :WS-LAST-JOB
           END-EXEC.
           EXEC SQL
             SELECT COUNT(*) INTO :WS-OP-CNT
             FROM IBMUSER.V2D2_OPS
             WHERE JOB_ID = :WS-LAST-JOB
           END-EXEC.
       DECODE-STATUS.
           EVALUATE WS-LAST-STS
             WHEN 'A' MOVE 'Analizado  '
               TO WS-STS-TEXT
             WHEN 'P' MOVE 'Pendiente  '
               TO WS-STS-TEXT
             WHEN 'C' MOVE 'Convertido '
               TO WS-STS-TEXT
             WHEN 'X' MOVE 'Compilado  '
               TO WS-STS-TEXT
             WHEN 'E' MOVE 'Error      '
               TO WS-STS-TEXT
             WHEN OTHER MOVE 'Desconocido'
               TO WS-STS-TEXT
           END-EVALUATE.
       EXIT-PARA.
           MOVE 'V2D2 Finalizado' TO WS-MSG.
           EXEC CICS SEND TEXT
               FROM(WS-MSG)
               LENGTH(WS-LEN60) ERASE
           END-EXEC.
           EXEC CICS RETURN END-EXEC.
           STOP RUN.
