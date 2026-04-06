/* REXX - V2D2SCAN - COBOL SOURCE ANALYZER V2                 */
/* V2: Fix field parsing scope (FILE SECTION only),            */
/*     proper OFFSET_POS sequencing, record name tracking      */
/*                                                              */
/* Parametros: JOB_ID DSN MEMBER                               */
/*============================================================*/
PARSE ARG P_JOBID P_DSN P_MEMBER
IF P_JOBID = '' THEN DO
  SAY 'USO: V2D2SCAN job_id dsn member'
  EXIT 8
END

/* Connect to DB2 */
S_RC = RXSUBCOM('ADD','DSNREXX','DSNREXX')
ADDRESS DSNREXX "CONNECT DSN1"
IF RC <> 0 THEN DO
  SAY '*** DB2 CONNECT FAILED RC='RC
  EXIT 8
END
SAY 'V2D2SCAN: Conectado a DB2'

/* Read source file */
FULLDSN = "'"STRIP(P_DSN)"("STRIP(P_MEMBER)")'"
ADDRESS TSO "ALLOC F(SRCIN) DA("FULLDSN") SHR"
IF RC <> 0 THEN DO
  SAY '*** ERROR ALLOCATING' FULLDSN 'RC='RC
  CALL LOG_MSG P_JOBID, 'E', 'No se puede abrir' FULLDSN
  CALL UPD_STATUS P_JOBID, 'E'
  EXIT 8
END
"EXECIO * DISKR SRCIN (STEM SRC. FINIS"
ADDRESS TSO "FREE F(SRCIN)"
SAY 'V2D2SCAN: Leidas' SRC.0 'lineas de' FULLDSN

/* Initialize counters */
FILE_COUNT = 0
OP_COUNT = 0
FIELD_COUNT = 0
IN_FD = 0
IN_SELECT = 0
IN_FILE_SECTION = 0
CURRENT_FILE = ''
CURRENT_LEVEL = 0
SELECT_BUF = ''
FD_FILE = ''
REDEF_GROUP = ' '
LINE_NUM = 0
FIELD_SEQ = 0
CURRENT_REC_NAME = ''

/* Main parsing loop */
DO I = 1 TO SRC.0
  LINE = SRC.I
  IF LENGTH(LINE) < 7 THEN ITERATE
  COL7 = SUBSTR(LINE,7,1)
  IF COL7 = '*' THEN ITERATE
  IF LENGTH(LINE) >= 72 THEN
    CODE = SUBSTR(LINE,8,65)
  ELSE
    CODE = SUBSTR(LINE,8)
  CODE = STRIP(CODE)
  IF CODE = '' THEN ITERATE
  LINE_NUM = LINE_NUM + 1
  UCODE = TRANSLATE(CODE)

  /*----- TRACK SECTIONS -----*/
  IF POS('FILE SECTION',UCODE) > 0 THEN DO
    IN_FILE_SECTION = 1
    ITERATE
  END
  IF POS('WORKING-STORAGE',UCODE) > 0 &,
     POS('SECTION',UCODE) > 0 THEN DO
    IN_FILE_SECTION = 0
    IN_FD = 0
    ITERATE
  END
  IF POS('LINKAGE SECTION',UCODE) > 0 THEN DO
    IN_FILE_SECTION = 0
    IN_FD = 0
    ITERATE
  END
  IF POS('PROCEDURE',UCODE) > 0 &,
     POS('DIVISION',UCODE) > 0 THEN DO
    IN_FILE_SECTION = 0
    IN_FD = 0
  END

  /*----- DETECT SELECT/ASSIGN -----*/
  IF WORD(UCODE,1) = 'SELECT' THEN DO
    IN_SELECT = 1
    SELECT_BUF = UCODE
    ITERATE
  END
  IF IN_SELECT = 1 THEN DO
    SELECT_BUF = SELECT_BUF UCODE
    IF POS('.',SELECT_BUF) > 0 THEN DO
      IN_SELECT = 0
      CALL PARSE_SELECT SELECT_BUF
    END
    ITERATE
  END

  /*----- DETECT FD (only in FILE SECTION) -----*/
  IF IN_FILE_SECTION & WORD(UCODE,1) = 'FD' THEN DO
    FD_FILE = WORD(UCODE,2)
    /* Remove trailing period */
    IF RIGHT(FD_FILE,1) = '.' THEN
      FD_FILE = LEFT(FD_FILE, LENGTH(FD_FILE)-1)
    IN_FD = 1
    CURRENT_LEVEL = 0
    FIELD_SEQ = 0
    CURRENT_REC_NAME = ''
    ITERATE
  END

  /*----- DETECT FIELD DEFINITIONS (in FD only) -----*/
  IF IN_FD = 1 & IN_FILE_SECTION = 1 THEN DO
    FIRSTW = WORD(UCODE,1)
    /* Another FD means end of current FD */
    IF FIRSTW = 'FD' THEN DO
      FD_FILE = WORD(UCODE,2)
      IF RIGHT(FD_FILE,1) = '.' THEN
        FD_FILE = LEFT(FD_FILE, LENGTH(FD_FILE)-1)
      CURRENT_LEVEL = 0
      FIELD_SEQ = 0
      CURRENT_REC_NAME = ''
      ITERATE
    END
    IF DATATYPE(FIRSTW,'W') THEN DO
      LEVEL = FIRSTW + 0
      IF LEVEL >= 1 & LEVEL <= 49 THEN DO
        /* Track 01-level record name */
        IF LEVEL = 1 THEN DO
          RNAME = WORD(UCODE,2)
          IF RIGHT(RNAME,1) = '.' THEN
            RNAME = LEFT(RNAME, LENGTH(RNAME)-1)
          CURRENT_REC_NAME = RNAME
          /* Update file record name in DB2 */
          CALL UPDATE_REC_NAME FD_FILE, RNAME
        END
        CURRENT_LEVEL = LEVEL
        FIELD_SEQ = FIELD_SEQ + 1
        CALL PARSE_FIELD FD_FILE, UCODE, LEVEL, FIELD_SEQ
      END
    END
  END

  /*----- DETECT BATCH I/O OPERATIONS -----*/
  IF WORD(UCODE,1) = 'READ' THEN
    CALL LOG_OP P_JOBID, I, 'READ', WORD(UCODE,2), 'N'
  IF WORD(UCODE,1) = 'WRITE' THEN
    CALL LOG_OP P_JOBID, I, 'WRITE', WORD(UCODE,2), 'N'
  IF WORD(UCODE,1) = 'REWRITE' THEN
    CALL LOG_OP P_JOBID, I, 'REWRITE', WORD(UCODE,2), 'N'
  IF WORD(UCODE,1) = 'DELETE' &,
     POS('EXEC',UCODE) = 0 THEN
    CALL LOG_OP P_JOBID, I, 'DELETE', WORD(UCODE,2), 'N'
  IF WORD(UCODE,1) = 'START' &,
     POS('EXEC',UCODE) = 0 THEN
    CALL LOG_OP P_JOBID, I, 'START', WORD(UCODE,2), 'N'
  IF WORD(UCODE,1) = 'OPEN' THEN
    CALL LOG_OP P_JOBID, I, 'OPEN', WORD(UCODE,3), 'N'
  IF WORD(UCODE,1) = 'CLOSE' THEN
    CALL LOG_OP P_JOBID, I, 'CLOSE', WORD(UCODE,2), 'N'

  /*----- DETECT CICS I/O OPERATIONS -----*/
  IF POS('EXEC CICS',UCODE) > 0 THEN DO
    IF POS('READ FILE',UCODE) > 0 |,
       POS('READ  FILE',UCODE) > 0 THEN
      CALL LOG_CICS_OP P_JOBID, I, 'READ', UCODE
    IF POS('WRITE FILE',UCODE) > 0 |,
       POS('WRITE  FILE',UCODE) > 0 THEN
      CALL LOG_CICS_OP P_JOBID, I, 'WRITE', UCODE
    IF POS('REWRITE',UCODE) > 0 THEN
      CALL LOG_CICS_OP P_JOBID, I, 'REWRITE', UCODE
    IF POS('DELETE FILE',UCODE) > 0 THEN
      CALL LOG_CICS_OP P_JOBID, I, 'DELETE', UCODE
    IF POS('STARTBR',UCODE) > 0 THEN
      CALL LOG_CICS_OP P_JOBID, I, 'STARTBR', UCODE
    IF POS('READNEXT',UCODE) > 0 THEN
      CALL LOG_CICS_OP P_JOBID, I, 'READNEXT', UCODE
    IF POS('READPREV',UCODE) > 0 THEN
      CALL LOG_CICS_OP P_JOBID, I, 'READPREV', UCODE
    IF POS('ENDBR',UCODE) > 0 THEN
      CALL LOG_CICS_OP P_JOBID, I, 'ENDBR', UCODE
    IF POS('RESETBR',UCODE) > 0 THEN
      CALL LOG_CICS_OP P_JOBID, I, 'RESETBR', UCODE
  END

END /* main loop */

/* Update job status */
CALL UPD_STATUS P_JOBID, 'P'
ADDRESS DSNREXX "EXECSQL COMMIT"
SAY 'V2D2SCAN: Analisis completado.'
SAY '  Archivos VSAM:' FILE_COUNT
SAY '  Campos:' FIELD_COUNT
SAY '  Operaciones I/O:' OP_COUNT
ADDRESS DSNREXX "DISCONNECT"
EXIT 0

/*============================================================*/
/* PARSE_SELECT: Extract file info from SELECT statement       */
/*============================================================*/
PARSE_SELECT:
  PARSE ARG SELBUF
  FNAME = WORD(SELBUF,2)
  FORG = 'SEQ'
  FKEY = ''
  IF POS('INDEXED',SELBUF) > 0 THEN FORG = 'KSDS'
  IF POS('SEQUENTIAL',SELBUF) > 0 THEN FORG = 'ESDS'
  IF POS('RELATIVE',SELBUF) > 0 THEN FORG = 'RRDS'
  KP = POS('RECORD KEY',SELBUF)
  IF KP > 0 THEN DO
    REST = SUBSTR(SELBUF, KP)
    ISP = POS('IS',REST)
    IF ISP > 0 THEN DO
      AFTER = SUBSTR(REST, ISP+2)
      FKEY = WORD(AFTER,1)
      IF RIGHT(FKEY,1) = '.' THEN
        FKEY = LEFT(FKEY, LENGTH(FKEY)-1)
    END
  END
  FILE_COUNT = FILE_COUNT + 1
  SAY '  VSAM FILE:' FNAME 'ORG='FORG 'KEY='FKEY
  S1 = "INSERT INTO IBMUSER.V2D2_FILES",
    "(JOB_ID, FILE_NAME, FILE_ORG, KEY_FIELD,",
    "REC_LEN, REDEF_COUNT, TABLE_NAME)",
    "VALUES("P_JOBID",'"LEFT(FNAME,30)"',",
    "'"FORG"','"LEFT(FKEY,30)"',",
    "0, 0, 'IBMUSER."LEFT(FNAME,30)"')"
  ADDRESS DSNREXX "EXECSQL EXECUTE IMMEDIATE :S1"
  IF SQLCODE <> 0 THEN
    SAY '  WARNING: INSERT V2D2_FILES SQLCODE='SQLCODE
  RETURN

/*============================================================*/
/* UPDATE_REC_NAME: Store 01-level record name for a file      */
/* Updates REC_LEN field to store record name (repurposed)     */
/*============================================================*/
UPDATE_REC_NAME:
  PARSE ARG UR_FILE, UR_REC
  SAY '  REC NAME:' UR_FILE '->' UR_REC
  /* Store in a temp stem for V2D2XFRM to query later */
  /* We'll store it as a log message for now */
  S1 = "INSERT INTO IBMUSER.V2D2_LOG",
    "(JOB_ID, SEQ_NUM, MSG_TYPE, MESSAGE)",
    "VALUES("P_JOBID",",
    "(SELECT COALESCE(MAX(SEQ_NUM),0)+1",
    " FROM IBMUSER.V2D2_LOG WHERE JOB_ID="P_JOBID"),",
    "'R','"LEFT(UR_FILE,30) || ':' || LEFT(UR_REC,30)"')"
  ADDRESS DSNREXX "EXECSQL EXECUTE IMMEDIATE :S1"
  RETURN

/*============================================================*/
/* PARSE_FIELD: Extract field definition from COBOL            */
/*============================================================*/
PARSE_FIELD:
  PARSE ARG PFILE, PCODE, PLEVEL, PSEQ
  PFNAME = WORD(PCODE,2)
  IF PFNAME = '' | PFNAME = 'FILLER' THEN RETURN
  IF RIGHT(PFNAME,1) = '.' THEN
    PFNAME = LEFT(PFNAME, LENGTH(PFNAME)-1)
  PPIC = ''
  PUSAGE = 'DISPLAY'
  PP = POS('PIC',PCODE)
  IF PP > 0 THEN DO
    REST = SUBSTR(PCODE, PP+3)
    REST = STRIP(REST)
    IF LEFT(REST,4) = 'TURE' THEN
      REST = STRIP(SUBSTR(REST,5))
    IF LEFT(REST,2) = 'IS' THEN
      REST = STRIP(SUBSTR(REST,3))
    PPIC = WORD(REST,1)
    IF RIGHT(PPIC,1) = '.' THEN
      PPIC = LEFT(PPIC, LENGTH(PPIC)-1)
  END
  IF POS('COMP-3',PCODE) > 0 THEN PUSAGE = 'COMP-3'
  ELSE IF POS('COMP',PCODE) > 0 THEN PUSAGE = 'COMP'
  ELSE IF POS('BINARY',PCODE) > 0 THEN PUSAGE = 'COMP'
  ELSE IF POS('PACKED',PCODE) > 0 THEN PUSAGE = 'COMP-3'
  PREDEF = ''
  IF POS('REDEFINES',PCODE) > 0 THEN DO
    RP = POS('REDEFINES',PCODE)
    REST = SUBSTR(PCODE, RP+9)
    PREDEF = WORD(STRIP(REST),1)
    IF RIGHT(PREDEF,1) = '.' THEN
      PREDEF = LEFT(PREDEF, LENGTH(PREDEF)-1)
    REDEF_GROUP = D2C(C2D(REDEF_GROUP)+1)
    IF REDEF_GROUP < 'A' THEN REDEF_GROUP = 'A'
  END
  PFLEN = CALC_LEN(PPIC, PUSAGE)
  PDB2 = MAP_DB2(PPIC, PUSAGE)
  PCOLNAME = TRANSLATE(PFNAME,' ','-')
  PCOLNAME = TRANSLATE(PCOLNAME,'_',' ')

  FIELD_COUNT = FIELD_COUNT + 1
  S1 = "INSERT INTO IBMUSER.V2D2_FIELDS",
    "(JOB_ID, FILE_NAME, FIELD_NAME, LEVEL_NUM,",
    "PIC_CLAUSE, USAGE_TYPE, FIELD_LEN,",
    "REDEF_OF, REDEF_GROUP, DB2_TYPE,",
    "DB2_COLNAME, OFFSET_POS)",
    "VALUES("P_JOBID",'"LEFT(PFILE,30)"',",
    "'"LEFT(PFNAME,30)"',"PLEVEL",",
    "'"LEFT(PPIC,30)"','"LEFT(PUSAGE,10)"',",
    PFLEN",'"LEFT(PREDEF,30)"',",
    "'"REDEF_GROUP"','"LEFT(PDB2,30)"',",
    "'"LEFT(PCOLNAME,30)"',"PSEQ")"
  ADDRESS DSNREXX "EXECSQL EXECUTE IMMEDIATE :S1"
  IF SQLCODE <> 0 THEN
    SAY '  WARNING: INSERT V2D2_FIELDS SQLCODE='SQLCODE
  RETURN

/*============================================================*/
/* CALC_LEN: Calculate field length from PIC clause            */
/*============================================================*/
CALC_LEN:
  PARSE ARG CPIC, CUSAGE
  IF CPIC = '' THEN RETURN 0
  LEN = 0
  J = 1
  DO WHILE J <= LENGTH(CPIC)
    CH = SUBSTR(CPIC,J,1)
    IF J < LENGTH(CPIC) & SUBSTR(CPIC,J+1,1) = '(' THEN DO
      EP = POS(')',CPIC,J+1)
      IF EP > 0 THEN DO
        NUM = SUBSTR(CPIC,J+2,EP-J-2) + 0
        LEN = LEN + NUM
        J = EP + 1
      END
      ELSE DO
        LEN = LEN + 1
        J = J + 1
      END
    END
    ELSE DO
      IF CH = 'X' | CH = 'A' | CH = '9' |,
         CH = 'Z' | CH = '-' | CH = '+'  THEN
        LEN = LEN + 1
      IF CH = 'V' THEN NOP
      IF CH = 'S' THEN NOP
      J = J + 1
    END
  END
  IF CUSAGE = 'COMP-3' THEN
    LEN = (LEN + 2) % 2
  IF CUSAGE = 'COMP' THEN DO
    IF LEN <= 4 THEN LEN = 2
    ELSE IF LEN <= 9 THEN LEN = 4
    ELSE LEN = 8
  END
  RETURN LEN

/*============================================================*/
/* MAP_DB2: Map COBOL PIC to DB2 data type                    */
/*============================================================*/
MAP_DB2:
  PARSE ARG MPIC, MUSAGE
  IF MPIC = '' THEN RETURN 'CHAR(1)'
  UPIC = TRANSLATE(MPIC)
  IF LEFT(UPIC,1) = 'X' THEN DO
    IF POS('(',UPIC) > 0 THEN DO
      EP = POS(')',UPIC)
      MLEN = SUBSTR(UPIC, 3, EP-3) + 0
    END
    ELSE MLEN = LENGTH(UPIC)
    RETURN 'CHAR('MLEN')'
  END
  IF POS('9',UPIC) > 0 THEN DO
    INTDIG = 0
    DECDIG = 0
    INDEC = 0
    K = 1
    DO WHILE K <= LENGTH(UPIC)
      CH = SUBSTR(UPIC,K,1)
      IF CH = 'V' THEN INDEC = 1
      IF CH = '9' THEN DO
        IF K < LENGTH(UPIC) &,
           SUBSTR(UPIC,K+1,1) = '(' THEN DO
          EP = POS(')',UPIC,K+1)
          IF EP > 0 THEN DO
            NUM = SUBSTR(UPIC,K+2,EP-K-2) + 0
            IF INDEC THEN DECDIG = DECDIG + NUM
            ELSE INTDIG = INTDIG + NUM
            K = EP + 1
            ITERATE
          END
        END
        IF INDEC THEN DECDIG = DECDIG + 1
        ELSE INTDIG = INTDIG + 1
      END
      K = K + 1
    END
    TOTDIG = INTDIG + DECDIG
    IF MUSAGE = 'COMP' & DECDIG = 0 THEN DO
      IF TOTDIG <= 4 THEN RETURN 'SMALLINT'
      IF TOTDIG <= 9 THEN RETURN 'INTEGER'
      RETURN 'DECIMAL('TOTDIG',0)'
    END
    RETURN 'DECIMAL('TOTDIG','DECDIG')'
  END
  RETURN 'CHAR(1)'

/*============================================================*/
/* LOG_OP: Log a batch VSAM operation                          */
/*============================================================*/
LOG_OP:
  PARSE ARG LJ, LL, LTYPE, LFILE, LCICS
  OP_COUNT = OP_COUNT + 1
  LSRC = LEFT(SRC.LL,72)
  LSRC = TRANSLATE(LSRC,"''","'")
  S1 = "INSERT INTO IBMUSER.V2D2_OPS",
    "(JOB_ID, LINE_NUM, OP_TYPE, FILE_NAME,",
    "IS_CICS, SOURCE_LINE)",
    "VALUES("LJ","LL",'"LEFT(LTYPE,10)"',",
    "'"LEFT(LFILE,30)"','"LCICS"',",
    "'"LEFT(LSRC,72)"')"
  ADDRESS DSNREXX "EXECSQL EXECUTE IMMEDIATE :S1"
  RETURN

/*============================================================*/
/* LOG_CICS_OP: Log a CICS VSAM operation                     */
/*============================================================*/
LOG_CICS_OP:
  PARSE ARG LJ, LL, LTYPE, LCODE
  LFILE = ''
  FP = POS('FILE(',LCODE)
  IF FP > 0 THEN DO
    REST = SUBSTR(LCODE, FP+5)
    EP = POS(')',REST)
    IF EP > 0 THEN DO
      LFILE = SUBSTR(REST,1,EP-1)
      LFILE = STRIP(LFILE,"B","'")
    END
  END
  CALL LOG_OP LJ, LL, LTYPE, LFILE, 'Y'
  RETURN

/*============================================================*/
/* LOG_MSG: Insert message into V2D2_LOG                       */
/*============================================================*/
LOG_MSG:
  PARSE ARG MJ, MTYPE, MMSG
  S1 = "INSERT INTO IBMUSER.V2D2_LOG",
    "(JOB_ID, SEQ_NUM, MSG_TYPE, MESSAGE)",
    "VALUES("MJ",",
    "(SELECT COALESCE(MAX(SEQ_NUM),0)+1",
    " FROM IBMUSER.V2D2_LOG WHERE JOB_ID="MJ"),",
    "'"MTYPE"','"LEFT(MMSG,80)"')"
  ADDRESS DSNREXX "EXECSQL EXECUTE IMMEDIATE :S1"
  RETURN

/*============================================================*/
/* UPD_STATUS: Update job status                               */
/*============================================================*/
UPD_STATUS:
  PARSE ARG UJ, USTAT
  S1 = "UPDATE IBMUSER.V2D2_JOBS",
    "SET STATUS = '"USTAT"',",
    "VSAM_COUNT ="FILE_COUNT,
    "WHERE JOB_ID ="UJ
  ADDRESS DSNREXX "EXECSQL EXECUTE IMMEDIATE :S1"
  RETURN
