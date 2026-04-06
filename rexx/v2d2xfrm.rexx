/* REXX - V2D2XFRM - COBOL VSAM TO DB2 CONVERTER              */
/* V3: Full REDEFINES support, column-level SQL, proper        */
/*     host variable generation from V2D2_FIELDS metadata      */
/*                                                              */
/* Parametros: JOB_ID DSN MEMBER OUTPUT_MEMBER                 */
/*============================================================*/
PARSE ARG P_JOBID P_DSN P_MEMBER P_OUTMBR
IF P_JOBID = '' THEN DO
  SAY 'USO: V2D2XFRM job_id dsn member [out_member]'
  EXIT 8
END
IF P_OUTMBR = '' THEN
  P_OUTMBR = LEFT(P_MEMBER,6) || 'D2'

S_RC = RXSUBCOM('ADD','DSNREXX','DSNREXX')
ADDRESS DSNREXX "CONNECT DSN1"
IF RC <> 0 THEN DO
  SAY '*** DB2 CONNECT FAILED'
  EXIT 8
END

/* Load file info from DB2 */
CALL LOAD_FILE_INFO P_JOBID
/* Load field info from DB2 - the key new feature */
CALL LOAD_FIELD_INFO P_JOBID
/* Build record-name to file-name mapping from source */
/* (needed because WRITE uses record name, not file name) */
REC2FILE. = ''

/* Read source */
FULLDSN = "'"STRIP(P_DSN)"("STRIP(P_MEMBER)")'"
ADDRESS TSO "ALLOC F(SRCIN) DA("FULLDSN") SHR"
"EXECIO * DISKR SRCIN (STEM SRC. FINIS"
ADDRESS TSO "FREE F(SRCIN)"
SAY 'V2D2XFRM: Leidas' SRC.0 'lineas'

/* Initialize output */
OUT. = ''
OCNT = 0
CHANGES = 0
IN_SELECT = 0
IN_FD = 0
IN_EXEC_CICS = 0
EXEC_BUF = ''
SKIP_TO_PERIOD = 0
ADDED_SQLCA = 0
ADDED_CURSORS = 0
WS_FOUND = 0
PROC_FOUND = 0
IN_READ_BLK = 0
FD_CNT = 0
FD_LINES. = ''
/* Track which FD we're inside for field mapping */
CURRENT_FD_FILE = ''

/* Main transformation loop */
DO I = 1 TO SRC.0
  LINE = SRC.I
  IF LENGTH(LINE) < 7 THEN DO
    CALL ADD_OUT LINE
    ITERATE
  END
  COL7 = SUBSTR(LINE,7,1)
  IF COL7 = '*' THEN DO
    CALL ADD_OUT LINE
    ITERATE
  END

  IF LENGTH(LINE) >= 72 THEN
    CODE = SUBSTR(LINE,8,65)
  ELSE
    CODE = SUBSTR(LINE,8)
  UCODE = TRANSLATE(STRIP(CODE))

  /*----- HANDLE READ BLOCK CONTINUATIONS -----*/
  IF IN_READ_BLK > 0 THEN DO
    IF WORD(UCODE,1) = 'INTO' & IN_READ_BLK = 1 THEN DO
      CALL ADD_COMMENT LINE, 'V2D2: (READ cont.)'
      ITERATE
    END
    IF WORD(UCODE,1) = 'AT' & WORD(UCODE,2) = 'END' THEN DO
      CALL ADD_COMMENT LINE,,
        'V2D2: AT END -> IF SQLCODE=100'
      CALL ADD_CODE,
        '           IF SQLCODE = 100'
      IN_READ_BLK = 2
      ITERATE
    END
    IF WORD(UCODE,1) = 'NOT' THEN DO
      IF POS('AT END',UCODE) > 0 |,
         POS('AT  END',UCODE) > 0 THEN DO
        CALL ADD_COMMENT LINE,,
          'V2D2: NOT AT END -> ELSE'
        CALL ADD_CODE '           ELSE'
        IN_READ_BLK = 3
        ITERATE
      END
    END
    IF LEFT(STRIP(UCODE),8) = 'END-READ' THEN DO
      CALL ADD_COMMENT LINE,,
        'V2D2: END-READ -> END-IF'
      IF IN_READ_BLK > 1 THEN
        CALL ADD_CODE '           END-IF.'
      IN_READ_BLK = 0
      ITERATE
    END
    /* Still inside AT END body - pass through */
    IF IN_READ_BLK > 1 THEN DO
      CALL ADD_OUT LINE
      ITERATE
    END
    /* IN_READ_BLK=1 but no AT END found = simple READ */
    IN_READ_BLK = 0
    /* Fall through to normal processing */
  END

  /*----- SKIP SELECT/ASSIGN STATEMENTS -----*/
  IF WORD(UCODE,1) = 'SELECT' &,
     POS('ASSIGN',UCODE) > 0 THEN DO
    CALL ADD_COMMENT LINE, 'V2D2: SELECT/ASSIGN REMOVED'
    IN_SELECT = 1
    CHANGES = CHANGES + 1
    ITERATE
  END
  IF IN_SELECT = 1 THEN DO
    CALL ADD_COMMENT LINE, 'V2D2: (continuation)'
    IF POS('.',UCODE) > 0 THEN IN_SELECT = 0
    ITERATE
  END

  /*----- SKIP FD ENTRIES, TRACK WHICH FD -----*/
  IF WORD(UCODE,1) = 'FD' THEN DO
    CURRENT_FD_FILE = WORD(UCODE,2)
    IF RIGHT(CURRENT_FD_FILE,1) = '.' THEN
      CURRENT_FD_FILE = LEFT(CURRENT_FD_FILE,,
        LENGTH(CURRENT_FD_FILE)-1)
    CALL ADD_COMMENT LINE, 'V2D2: FD REMOVED'
    IN_FD = 1
    SKIP_TO_PERIOD = 1
    CHANGES = CHANGES + 1
    ITERATE
  END
  /* Skip all lines under FD (record layouts) */
  IF IN_FD = 1 THEN DO
    IF POS('PROCEDURE',UCODE) > 0 &,
       POS('DIVISION',UCODE) > 0 THEN DO
      IN_FD = 0
      /* Fall through to output this line */
    END
    ELSE IF POS('WORKING-STORAGE',UCODE) > 0 THEN DO
      IN_FD = 0
      /* Fall through */
    END
    ELSE IF WORD(UCODE,1) = 'FD' THEN DO
      CURRENT_FD_FILE = WORD(UCODE,2)
      IF RIGHT(CURRENT_FD_FILE,1) = '.' THEN
        CURRENT_FD_FILE = LEFT(CURRENT_FD_FILE,,
          LENGTH(CURRENT_FD_FILE)-1)
      CALL ADD_COMMENT LINE, 'V2D2: FD REMOVED'
      CHANGES = CHANGES + 1
      ITERATE
    END
    ELSE DO
      /* Capture 01-level record name -> file mapping */
      FW = WORD(UCODE,1)
      IF DATATYPE(FW,'W') & FW + 0 = 1 THEN DO
        RECN = WORD(UCODE,2)
        IF RIGHT(RECN,1) = '.' THEN
          RECN = LEFT(RECN, LENGTH(RECN)-1)
        REC2FILE.RECN = CURRENT_FD_FILE
        SAY '  REC2FILE:' RECN '->' CURRENT_FD_FILE
      END
      CALL ADD_COMMENT LINE, 'V2D2: FD record layout'
      ITERATE
    END
  END

  /*----- ADD SQLCA + HOST VARS AFTER WS SECTION -----*/
  IF POS('WORKING-STORAGE',UCODE) > 0 &,
     POS('SECTION',UCODE) > 0 THEN DO
    WS_FOUND = 1
    CALL ADD_OUT LINE
    IF ADDED_SQLCA = 0 THEN DO
      CALL ADD_CODE,
        '           EXEC SQL INCLUDE SQLCA END-EXEC.'
      /* V4: Use DCLGEN INCLUDEs instead of inline host vars */
      CALL GENERATE_DCLGEN_INCLUDES
      CALL ADD_CODE,
        '       01  V2D2-SQLCODE PIC S9(9) COMP.'
      ADDED_SQLCA = 1
    END
    ITERATE
  END

  /*----- ADD CURSOR DECL BEFORE PROCEDURE DIV -----*/
  IF POS('PROCEDURE',UCODE) > 0 &,
     POS('DIVISION',UCODE) > 0 THEN DO
    PROC_FOUND = 1
    IF ADDED_CURSORS = 0 THEN DO
      CALL ADD_CURSOR_DECLARATIONS
      ADDED_CURSORS = 1
    END
    CALL ADD_OUT LINE
    ITERATE
  END

  /*----- CONVERT BATCH VSAM OPERATIONS -----*/
  IF WORD(UCODE,1) = 'OPEN' THEN DO
    CALL CONVERT_OPEN LINE, UCODE
    ITERATE
  END
  IF WORD(UCODE,1) = 'CLOSE' THEN DO
    CALL CONVERT_CLOSE LINE, UCODE
    ITERATE
  END
  IF WORD(UCODE,1) = 'READ' &,
     POS('EXEC',UCODE) = 0 THEN DO
    CALL CONVERT_READ LINE, UCODE
    ITERATE
  END
  IF WORD(UCODE,1) = 'WRITE' &,
     POS('EXEC',UCODE) = 0 THEN DO
    CALL CONVERT_WRITE LINE, UCODE
    ITERATE
  END
  IF WORD(UCODE,1) = 'REWRITE' &,
     POS('EXEC',UCODE) = 0 THEN DO
    CALL CONVERT_REWRITE LINE, UCODE
    ITERATE
  END
  IF WORD(UCODE,1) = 'DELETE' &,
     POS('EXEC',UCODE) = 0 THEN DO
    CALL CONVERT_DELETE LINE, UCODE
    ITERATE
  END
  IF WORD(UCODE,1) = 'START' &,
     POS('EXEC',UCODE) = 0 THEN DO
    CALL CONVERT_START LINE, UCODE
    ITERATE
  END

  /*----- CONVERT CICS VSAM OPERATIONS -----*/
  IF POS('EXEC CICS',UCODE) > 0 THEN DO
    IF POS('READ FILE',UCODE) > 0 |,
       POS('READ  FILE',UCODE) > 0 THEN DO
      CALL CONVERT_CICS_READ LINE, UCODE
      ITERATE
    END
    IF POS('WRITE FILE',UCODE) > 0 |,
       POS('WRITE  FILE',UCODE) > 0 THEN DO
      CALL CONVERT_CICS_WRITE LINE, UCODE
      ITERATE
    END
    IF POS('REWRITE',UCODE) > 0 &,
       POS('FILE',UCODE) > 0 THEN DO
      CALL CONVERT_CICS_REWRITE LINE, UCODE
      ITERATE
    END
    IF POS('DELETE FILE',UCODE) > 0 THEN DO
      CALL CONVERT_CICS_DELETE LINE, UCODE
      ITERATE
    END
    IF POS('STARTBR',UCODE) > 0 THEN DO
      CALL CONVERT_CICS_STARTBR LINE, UCODE
      ITERATE
    END
    IF POS('READNEXT',UCODE) > 0 THEN DO
      CALL CONVERT_CICS_READNEXT LINE, UCODE
      ITERATE
    END
    IF POS('READPREV',UCODE) > 0 THEN DO
      CALL CONVERT_CICS_READNEXT LINE, UCODE
      ITERATE
    END
    IF POS('ENDBR',UCODE) > 0 THEN DO
      CALL CONVERT_CICS_ENDBR LINE, UCODE
      ITERATE
    END
    IF POS('HANDLE CONDITION',UCODE) > 0 &,
       POS('NOTFND',UCODE) > 0 THEN DO
      CALL ADD_COMMENT LINE,,
        'V2D2: HANDLE NOTFND -> check SQLCODE=100'
      CHANGES = CHANGES + 1
      ITERATE
    END
  END

  /* Default: pass through unchanged */
  CALL ADD_OUT LINE
END /* main loop */

/* Write output */
OUTDSN = "'"STRIP(P_DSN)"("STRIP(P_OUTMBR)")'"
ADDRESS TSO "ALLOC F(SRCOUT) DA("OUTDSN") SHR"
"EXECIO" OCNT "DISKW SRCOUT (STEM OUT. FINIS"
ADDRESS TSO "FREE F(SRCOUT)"

SAY 'V2D2XFRM: Conversion completada.'
SAY '  Entrada:' P_MEMBER
SAY '  Salida: ' P_OUTMBR
SAY '  Lineas originales:' SRC.0
SAY '  Lineas generadas: ' OCNT
SAY '  Cambios realizados:' CHANGES

/* Show field stats per file */
DO SF = 1 TO FILE.0
  FN = FILE.SF.NAME
  SAY '  File' FN': total='FLD.FN.0,
    'primary='FLD.FN.PCOUNT 'redef_skipped='FLD.FN.RCOUNT
END

/* Update job status */
S1 = "UPDATE IBMUSER.V2D2_JOBS",
  "SET STATUS = 'C'",
  "WHERE JOB_ID = "P_JOBID
ADDRESS DSNREXX "EXECSQL EXECUTE IMMEDIATE :S1"
ADDRESS DSNREXX "EXECSQL COMMIT"
ADDRESS DSNREXX "DISCONNECT"
EXIT 0

/*============================================================*/
/* HELPER ROUTINES                                             */
/*============================================================*/

ADD_OUT:
  PARSE ARG ALINE
  OCNT = OCNT + 1
  OUT.OCNT = ALINE
  RETURN

ADD_CODE:
  PARSE ARG ACODE
  OCNT = OCNT + 1
  OUT.OCNT = LEFT(ACODE,80)
  RETURN

ADD_COMMENT:
  PARSE ARG CLINE, CMSG
  OCNT = OCNT + 1
  OUT.OCNT = LEFT('      *' CMSG,80)
  RETURN

/*============================================================*/
/* LOAD_FILE_INFO: Get VSAM file info from DB2                 */
/*============================================================*/
LOAD_FILE_INFO:
  PARSE ARG LJ
  S9 = "SELECT FILE_NAME, FILE_ORG, KEY_FIELD,",
    "TABLE_NAME",
    "FROM IBMUSER.V2D2_FILES",
    "WHERE JOB_ID =" LJ,
    "ORDER BY FILE_NAME"
  ADDRESS DSNREXX "EXECSQL DECLARE C9 CURSOR FOR S9"
  ADDRESS DSNREXX "EXECSQL PREPARE S9 FROM :S9"
  ADDRESS DSNREXX "EXECSQL OPEN C9"
  SAY 'LOAD_FILE_INFO: OPEN SQLCODE=' SQLCODE
  FCOUNT = 0
  DO FOREVER
    ADDRESS DSNREXX,
      "EXECSQL FETCH C9 INTO :LF,:LO,:LK,:LT"
    IF SQLCODE <> 0 THEN LEAVE
    FCOUNT = FCOUNT + 1
    FILE.FCOUNT.NAME = STRIP(LF)
    FILE.FCOUNT.ORG = STRIP(LO)
    FILE.FCOUNT.KEY = STRIP(LK)
    FILE.FCOUNT.TABLE = STRIP(LT)
    SAY '  FILE.' || FCOUNT || ':' FILE.FCOUNT.NAME,
        FILE.FCOUNT.ORG FILE.FCOUNT.KEY
  END
  ADDRESS DSNREXX "EXECSQL CLOSE C9"
  FILE.0 = FCOUNT
  SAY 'LOAD_FILE_INFO: Loaded' FCOUNT 'files'
  RETURN

/*============================================================*/
/* LOAD_FIELD_INFO: Load field metadata from V2D2_FIELDS       */
/* This is the core of REDEFINES handling.                     */
/* For each file, loads all fields, identifies which are       */
/* REDEFINES (skip) vs primary (keep), and builds:             */
/*   FLD.fname.n.xxx  - all fields indexed by n                */
/*   FLD.fname.0      - total field count                      */
/*   FLD.fname.PCOUNT - primary (non-redefines) field count    */
/*   FLD.fname.RCOUNT - skipped redefines field count          */
/*   FLD.fname.P.n.xxx - primary fields indexed by n           */
/*   FLD.fname.P.0     - count of primary fields               */
/*   FLD.fname.COLS    - comma-separated DB2 column list       */
/*   FLD.fname.HOSTS   - comma-separated host var list         */
/*   FLD.fname.SETS    - col=:host pairs for UPDATE SET        */
/*============================================================*/
LOAD_FIELD_INFO:
  PARSE ARG LJ
  DO LF = 1 TO FILE.0
    FNAME = FILE.LF.NAME
    FLD.FNAME.0 = 0
    FLD.FNAME.PCOUNT = 0
    FLD.FNAME.RCOUNT = 0
    FLD.FNAME.P.0 = 0
    FLD.FNAME.COLS = ''
    FLD.FNAME.HOSTS = ''
    FLD.FNAME.SETS = ''
    /* Track which field names have been REDEFINED */
    /* We keep the ORIGINAL definition, skip the REDEFINES */
    REDEF_TARGETS. = ''
  END

  S2 = "SELECT FILE_NAME, FIELD_NAME, LEVEL_NUM,",
    "PIC_CLAUSE, USAGE_TYPE, FIELD_LEN,",
    "REDEF_OF, REDEF_GROUP, DB2_TYPE,",
    "DB2_COLNAME",
    "FROM IBMUSER.V2D2_FIELDS",
    "WHERE JOB_ID =" LJ,
    "ORDER BY FILE_NAME, OFFSET_POS"
  ADDRESS DSNREXX "EXECSQL DECLARE C2 CURSOR FOR S2"
  ADDRESS DSNREXX "EXECSQL PREPARE S2 FROM :S2"
  SAY 'LOAD_FIELD_INFO: PREPARE SQLCODE=' SQLCODE
  IF SQLCODE <> 0 THEN RETURN
  ADDRESS DSNREXX "EXECSQL OPEN C2"
  SAY 'LOAD_FIELD_INFO: OPEN SQLCODE=' SQLCODE

  DO FOREVER
    ADDRESS DSNREXX,
      "EXECSQL FETCH C2 INTO",
      ":V_FN,:V_FLDN,:V_LVL,:V_PIC,:V_USG,",
      ":V_LEN,:V_RDF,:V_RDG,:V_DB2,:V_COL"
    IF SQLCODE <> 0 THEN LEAVE

    V_FN = STRIP(STRIP(V_FN),'T','.')
    V_FLDN = STRIP(STRIP(V_FLDN),'T','.')
    V_LVL = STRIP(V_LVL) + 0
    V_PIC = STRIP(V_PIC)
    V_USG = STRIP(V_USG)
    V_LEN = STRIP(V_LEN) + 0
    V_RDF = STRIP(V_RDF)
    V_RDG = STRIP(V_RDG)
    V_DB2 = STRIP(V_DB2)
    V_COL = STRIP(V_COL)

    /* Initialize if first time seeing this file */
    IF DATATYPE(FLD.V_FN.0, 'W') = 0 THEN DO
      FLD.V_FN.0 = 0
      FLD.V_FN.PCOUNT = 0
      FLD.V_FN.RCOUNT = 0
      FLD.V_FN.P.0 = 0
      FLD.V_FN.COLS = ''
      FLD.V_FN.HOSTS = ''
      FLD.V_FN.SETS = ''
    END
    /* Store in all-fields array */
    N = FLD.V_FN.0 + 1
    FLD.V_FN.0 = N
    FLD.V_FN.N.NAME = V_FLDN
    FLD.V_FN.N.LEVEL = V_LVL
    FLD.V_FN.N.PIC = V_PIC
    FLD.V_FN.N.USAGE = V_USG
    FLD.V_FN.N.LEN = V_LEN
    FLD.V_FN.N.REDEF = V_RDF
    FLD.V_FN.N.RGROUP = V_RDG
    FLD.V_FN.N.DB2TYPE = V_DB2
    FLD.V_FN.N.COLNAME = V_COL
    FLD.V_FN.N.IS_PRIMARY = 1   /* assume primary */

    /* Determine if this is a REDEFINES or child of one */
    IS_REDEF = 0

    /* Case 1: This field directly REDEFINES another */
    IF V_RDF <> '' THEN DO
      IS_REDEF = 1
      /* Mark this field name as a redefine target group */
      REDEF_TARGETS.V_FN.V_FLDN = 'R'
      /* Also mark the level so children get skipped */
      REDEF_TARGETS.V_FN.ACTIVE = V_LVL
      REDEF_TARGETS.V_FN.RNAME = V_FLDN
    END

    /* Case 2: Child of a REDEFINES group field */
    /* If we're inside a REDEFINES block (same or deeper level) */
    IF REDEF_TARGETS.V_FN.ACTIVE <> '' & IS_REDEF = 0 THEN DO
      ACTIVE_LVL = REDEF_TARGETS.V_FN.ACTIVE + 0
      IF V_LVL > ACTIVE_LVL THEN DO
        /* Deeper level = child of the REDEFINES group */
        IS_REDEF = 1
      END
      ELSE DO
        /* Same or shallower level = we left the REDEFINES */
        REDEF_TARGETS.V_FN.ACTIVE = ''
        REDEF_TARGETS.V_FN.RNAME = ''
      END
    END

    IF IS_REDEF THEN DO
      FLD.V_FN.N.IS_PRIMARY = 0
      FLD.V_FN.RCOUNT = FLD.V_FN.RCOUNT + 1
      SAY '  FIELD (skip redef):' V_FN'.'V_FLDN,
        'L'V_LVL V_PIC 'REDEFINES='V_RDF
    END
    ELSE DO
      /* Primary field - add to primary array */
      /* Only include elementary items (have a PIC) */
      IF V_PIC <> '' THEN DO
        PN = FLD.V_FN.P.0 + 1
        FLD.V_FN.P.0 = PN
        FLD.V_FN.P.PN.NAME = V_FLDN
        FLD.V_FN.P.PN.LEVEL = V_LVL
        FLD.V_FN.P.PN.PIC = V_PIC
        FLD.V_FN.P.PN.USAGE = V_USG
        FLD.V_FN.P.PN.LEN = V_LEN
        FLD.V_FN.P.PN.DB2TYPE = V_DB2
        FLD.V_FN.P.PN.COLNAME = V_COL
        FLD.V_FN.PCOUNT = PN
        /* Build column/host lists */
        IF FLD.V_FN.COLS <> '' THEN DO
          FLD.V_FN.COLS = FLD.V_FN.COLS || ', '
          FLD.V_FN.HOSTS = FLD.V_FN.HOSTS || ', '
          FLD.V_FN.SETS = FLD.V_FN.SETS || ', '
        END
        /* Host var name: cobol name with colons */
        HNAME = ':' || V_FLDN
        FLD.V_FN.COLS = FLD.V_FN.COLS || V_COL
        FLD.V_FN.HOSTS = FLD.V_FN.HOSTS || HNAME
        FLD.V_FN.SETS = FLD.V_FN.SETS || V_COL,
          || ' = ' || HNAME
        SAY '  FIELD (primary) :' V_FN'.'V_FLDN,
          'L'V_LVL V_PIC '->' V_DB2
      END
      ELSE DO
        /* Group item (no PIC) - track for structure */
        SAY '  FIELD (group)   :' V_FN'.'V_FLDN,
          'L'V_LVL '(group)'
        FLD.V_FN.PCOUNT = FLD.V_FN.PCOUNT + 0
      END
    END
  END
  ADDRESS DSNREXX "EXECSQL CLOSE C2"

  /* Summary */
  DO LF = 1 TO FILE.0
    FN = FILE.LF.NAME
    SAY 'LOAD_FIELD_INFO:' FN,
      'total='FLD.FN.0 'primary_elem='FLD.FN.P.0,
      'redef_skipped='FLD.FN.RCOUNT
  END
  RETURN

/*============================================================*/
/* GENERATE_DCLGEN_INCLUDES: Emit EXEC SQL INCLUDE for each   */
/* file's DCLGEN copybook instead of inline host variables.   */
/* DCLGEN members are generated by V2D2DCLG (run before this) */
/* Member naming: first 6 chars of filename + 'DC'            */
/*============================================================*/
GENERATE_DCLGEN_INCLUDES:
  DO GF = 1 TO FILE.0
    FN = FILE.GF.NAME
    IF FLD.FN.P.0 = 0 THEN ITERATE
    /* Build DCLGEN member name: same logic as V2D2DCLG */
    MBRNAME = LEFT(TRANSLATE(FN,' ','-'),6)
    MBRNAME = STRIP(TRANSLATE(MBRNAME,'_',' '))
    MBRNAME = MBRNAME || 'DC'
    MBRNAME = LEFT(MBRNAME,8)
    CALL ADD_CODE '      * V2D2: DCLGEN INCLUDE FOR' FN
    CALL ADD_CODE '      * V2D2: Primary fields:',
      FLD.FN.P.0 '- REDEFINES skipped:' FLD.FN.RCOUNT
    CALL ADD_CODE,
      '           EXEC SQL INCLUDE' MBRNAME 'END-EXEC.'
  END
  RETURN

/*============================================================*/
/* GENERATE_HOST_VARIABLES: (legacy - kept for compatibility)  */
/* Use GENERATE_DCLGEN_INCLUDES instead                        */
/*============================================================*/
GENERATE_HOST_VARIABLES:
  CALL GENERATE_DCLGEN_INCLUDES
  RETURN

/*============================================================*/
/* FIND_FILE: Find file info by name                           */
/*============================================================*/
FIND_FILE:
  PARSE ARG FFNAME
  FFNAME = TRANSLATE(STRIP(FFNAME))
  IF RIGHT(FFNAME,1) = '.' THEN
    FFNAME = LEFT(FFNAME, LENGTH(FFNAME)-1)
  IF RIGHT(FFNAME,1) = ',' THEN
    FFNAME = LEFT(FFNAME, LENGTH(FFNAME)-1)
  /* Try direct file name match */
  DO FF = 1 TO FILE.0
    IF TRANSLATE(FILE.FF.NAME) = FFNAME THEN
      RETURN FF
  END
  /* Try record-name -> file-name mapping */
  MAPPED = REC2FILE.FFNAME
  IF MAPPED <> '' & MAPPED <> 'REC2FILE.'FFNAME THEN DO
    MAPPED = TRANSLATE(STRIP(MAPPED))
    DO FF = 1 TO FILE.0
      IF TRANSLATE(FILE.FF.NAME) = MAPPED THEN DO
        SAY '  FIND_FILE:' FFNAME '-> (rec2file) ->' MAPPED
        RETURN FF
      END
    END
  END
  SAY '  FIND_FILE: NOT FOUND:' FFNAME
  RETURN 0

/*============================================================*/
/* BUILD_SQL_COLS: Build column list for SQL, splitting into   */
/* multiple lines if needed (72-col COBOL limit)               */
/* Returns number of lines written                             */
/*============================================================*/
BUILD_SQL_SELECT:
  PARSE ARG BS_FN, BS_INDENT
  /* Build SELECT col1, col2, ... FROM table */
  /* INTO :host1, :host2, ... */
  IF FLD.BS_FN.P.0 = 0 THEN DO
    CALL ADD_CODE BS_INDENT || 'SELECT *'
    CALL ADD_CODE BS_INDENT || '  INTO :WS-RECORD'
    RETURN
  END
  /* SELECT with columns, wrap at ~60 chars */
  SLINE = 'SELECT '
  DO BSI = 1 TO FLD.BS_FN.P.0
    COL = FLD.BS_FN.P.BSI.COLNAME
    IF BSI < FLD.BS_FN.P.0 THEN COL = COL || ','
    IF LENGTH(SLINE || COL) > 58 THEN DO
      CALL ADD_CODE BS_INDENT || SLINE
      SLINE = '  ' || COL
    END
    ELSE SLINE = SLINE || ' ' || COL
  END
  IF SLINE <> '' THEN
    CALL ADD_CODE BS_INDENT || SLINE
  /* INTO with host vars */
  HLINE = 'INTO '
  DO BSI = 1 TO FLD.BS_FN.P.0
    HVAR = ':' || FLD.BS_FN.P.BSI.NAME
    IF BSI < FLD.BS_FN.P.0 THEN HVAR = HVAR || ','
    IF LENGTH(HLINE || HVAR) > 58 THEN DO
      CALL ADD_CODE BS_INDENT || HLINE
      HLINE = '  ' || HVAR
    END
    ELSE HLINE = HLINE || ' ' || HVAR
  END
  IF HLINE <> '' THEN
    CALL ADD_CODE BS_INDENT || HLINE
  RETURN

BUILD_SQL_INSERT:
  PARSE ARG BI_FN, BI_INDENT
  IF FLD.BI_FN.P.0 = 0 THEN DO
    CALL ADD_CODE BI_INDENT || '  VALUES (:WS-RECORD)'
    RETURN
  END
  /* Column list */
  CLINE = '('
  DO BII = 1 TO FLD.BI_FN.P.0
    COL = FLD.BI_FN.P.BII.COLNAME
    IF BII < FLD.BI_FN.P.0 THEN COL = COL || ','
    ELSE COL = COL || ')'
    IF LENGTH(CLINE || COL) > 58 THEN DO
      CALL ADD_CODE BI_INDENT || CLINE
      CLINE = ' ' || COL
    END
    ELSE CLINE = CLINE || ' ' || COL
  END
  IF CLINE <> '' THEN
    CALL ADD_CODE BI_INDENT || CLINE
  /* VALUES list */
  VLINE = 'VALUES ('
  DO BII = 1 TO FLD.BI_FN.P.0
    HVAR = ':' || FLD.BI_FN.P.BII.NAME
    IF BII < FLD.BI_FN.P.0 THEN HVAR = HVAR || ','
    ELSE HVAR = HVAR || ')'
    IF LENGTH(VLINE || HVAR) > 58 THEN DO
      CALL ADD_CODE BI_INDENT || VLINE
      VLINE = ' ' || HVAR
    END
    ELSE VLINE = VLINE || ' ' || HVAR
  END
  IF VLINE <> '' THEN
    CALL ADD_CODE BI_INDENT || VLINE
  RETURN

BUILD_SQL_UPDATE_SET:
  PARSE ARG BU_FN, BU_INDENT
  IF FLD.BU_FN.P.0 = 0 THEN DO
    CALL ADD_CODE BU_INDENT || 'SET ROW = :WS-RECORD'
    RETURN
  END
  CALL ADD_CODE BU_INDENT || 'SET'
  DO BUI = 1 TO FLD.BU_FN.P.0
    COL = FLD.BU_FN.P.BUI.COLNAME
    HVAR = ':' || FLD.BU_FN.P.BUI.NAME
    SVAL = '  ' || COL || ' = ' || HVAR
    IF BUI < FLD.BU_FN.P.0 THEN SVAL = SVAL || ','
    CALL ADD_CODE BU_INDENT || SVAL
  END
  RETURN

/*============================================================*/
/* ADD_CURSOR_DECLARATIONS: Add cursor declares with columns   */
/*============================================================*/
ADD_CURSOR_DECLARATIONS:
  DO CF = 1 TO FILE.0
    IF FILE.CF.TABLE <> '' THEN DO
      CNAME = 'C-' || FILE.CF.NAME
      TNAME = FILE.CF.TABLE
      FN = FILE.CF.NAME
      CALL ADD_CODE '      * V2D2: CURSOR FOR' FN
      CALL ADD_CODE '           EXEC SQL'
      CALL ADD_CODE '             DECLARE' CNAME 'CURSOR FOR'
      /* Use column list if available */
      IF FLD.FN.P.0 > 0 THEN DO
        SLINE = '             SELECT '
        DO CI = 1 TO FLD.FN.P.0
          COL = FLD.FN.P.CI.COLNAME
          IF CI < FLD.FN.P.0 THEN COL = COL || ','
          IF LENGTH(SLINE || COL) > 58 THEN DO
            CALL ADD_CODE SLINE
            SLINE = '               ' || COL
          END
          ELSE SLINE = SLINE || ' ' || COL
        END
        IF SLINE <> '' THEN CALL ADD_CODE SLINE
        CALL ADD_CODE '             FROM' TNAME
      END
      ELSE DO
        CALL ADD_CODE '             SELECT * FROM' TNAME
      END
      IF FILE.CF.KEY <> '' THEN DO
        KCOL = TRANSLATE(FILE.CF.KEY,' ','-')
        KCOL = TRANSLATE(KCOL,'_',' ')
        CALL ADD_CODE '             ORDER BY' KCOL
      END
      CALL ADD_CODE '           END-EXEC.'
    END
  END
  RETURN

/*============================================================*/
/* CONVERSION ROUTINES - BATCH                                 */
/*============================================================*/

CONVERT_OPEN:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE, 'V2D2: OPEN -> SQL OPEN CURSOR'
  FNAME = WORD(CCODE,3)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    CNAME = 'C-' || FILE.FI.NAME
    CALL ADD_CODE,
      '           EXEC SQL OPEN' CNAME 'END-EXEC.'
  END
  CHANGES = CHANGES + 1
  RETURN

CONVERT_CLOSE:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE, 'V2D2: CLOSE -> SQL CLOSE CURSOR'
  FNAME = WORD(CCODE,2)
  IF RIGHT(FNAME,1) = '.' THEN
    FNAME = LEFT(FNAME, LENGTH(FNAME)-1)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    CNAME = 'C-' || FILE.FI.NAME
    CALL ADD_CODE,
      '           EXEC SQL CLOSE' CNAME 'END-EXEC.'
  END
  CHANGES = CHANGES + 1
  RETURN

CONVERT_READ:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE, 'V2D2: READ -> SQL FETCH/SELECT'
  FNAME = WORD(CCODE,2)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    FN = FILE.FI.NAME
    IF POS('KEY',CCODE) > 0 THEN DO
      /* Keyed read -> SELECT with columns */
      CALL ADD_CODE '           EXEC SQL'
      CALL BUILD_SQL_SELECT FN, '             '
      CALL ADD_CODE '             FROM' FILE.FI.TABLE
      KCOL = TRANSLATE(FILE.FI.KEY,' ','-')
      KCOL = TRANSLATE(KCOL,'_',' ')
      CALL ADD_CODE '             WHERE' KCOL,
        '= :' || FILE.FI.KEY
      CALL ADD_CODE '           END-EXEC.'
    END
    ELSE DO
      /* Sequential read -> FETCH with columns */
      CNAME = 'C-' || FN
      CALL ADD_CODE '           EXEC SQL'
      CALL ADD_CODE '             FETCH' CNAME
      /* INTO with host var list */
      IF FLD.FN.P.0 > 0 THEN DO
        HLINE = '             INTO '
        DO RI = 1 TO FLD.FN.P.0
          HVAR = ':' || FLD.FN.P.RI.NAME
          IF RI < FLD.FN.P.0 THEN HVAR = HVAR || ','
          IF LENGTH(HLINE || HVAR) > 58 THEN DO
            CALL ADD_CODE HLINE
            HLINE = '               ' || HVAR
          END
          ELSE HLINE = HLINE || ' ' || HVAR
        END
        IF HLINE <> '' THEN CALL ADD_CODE HLINE
      END
      ELSE
        CALL ADD_CODE '             INTO :WS-RECORD'
      CALL ADD_CODE '           END-EXEC.'
    END
  END
  CHANGES = CHANGES + 1
  IN_READ_BLK = 1
  RETURN

CONVERT_WRITE:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE, 'V2D2: WRITE -> SQL INSERT'
  FNAME = WORD(CCODE,2)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    FN = FILE.FI.NAME
    CALL ADD_CODE '           EXEC SQL'
    CALL ADD_CODE '             INSERT INTO' FILE.FI.TABLE
    CALL BUILD_SQL_INSERT FN, '             '
    CALL ADD_CODE '           END-EXEC.'
  END
  CHANGES = CHANGES + 1
  RETURN

CONVERT_REWRITE:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE, 'V2D2: REWRITE -> SQL UPDATE'
  FNAME = WORD(CCODE,2)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    FN = FILE.FI.NAME
    KCOL = TRANSLATE(FILE.FI.KEY,' ','-')
    KCOL = TRANSLATE(KCOL,'_',' ')
    CALL ADD_CODE '           EXEC SQL'
    CALL ADD_CODE '             UPDATE' FILE.FI.TABLE
    CALL BUILD_SQL_UPDATE_SET FN, '             '
    CALL ADD_CODE '             WHERE' KCOL,
      '= :' || FILE.FI.KEY
    CALL ADD_CODE '           END-EXEC.'
  END
  CHANGES = CHANGES + 1
  RETURN

CONVERT_DELETE:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE, 'V2D2: DELETE -> SQL DELETE'
  FNAME = WORD(CCODE,2)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    KCOL = TRANSLATE(FILE.FI.KEY,' ','-')
    KCOL = TRANSLATE(KCOL,'_',' ')
    CALL ADD_CODE '           EXEC SQL'
    CALL ADD_CODE '             DELETE FROM' FILE.FI.TABLE
    CALL ADD_CODE '             WHERE' KCOL,
      '= :' || FILE.FI.KEY
    CALL ADD_CODE '           END-EXEC.'
  END
  CHANGES = CHANGES + 1
  RETURN

CONVERT_START:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE, 'V2D2: START -> SQL OPEN CURSOR'
  FNAME = WORD(CCODE,2)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    CNAME = 'C-' || FILE.FI.NAME
    CALL ADD_CODE,
      '           EXEC SQL OPEN' CNAME 'END-EXEC.'
  END
  CHANGES = CHANGES + 1
  RETURN

/*============================================================*/
/* CONVERSION ROUTINES - CICS                                  */
/*============================================================*/

CONVERT_CICS_READ:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE, 'V2D2: CICS READ -> SQL SELECT'
  FNAME = EXTRACT_FILE(CCODE)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    FN = FILE.FI.NAME
    KCOL = TRANSLATE(FILE.FI.KEY,' ','-')
    KCOL = TRANSLATE(KCOL,'_',' ')
    CALL ADD_CODE '           EXEC SQL'
    CALL BUILD_SQL_SELECT FN, '             '
    CALL ADD_CODE '             FROM' FILE.FI.TABLE
    CALL ADD_CODE '             WHERE' KCOL,
      '= :' || FILE.FI.KEY
    CALL ADD_CODE '           END-EXEC.'
  END
  CALL SKIP_CICS_CONT I
  CHANGES = CHANGES + 1
  RETURN

CONVERT_CICS_WRITE:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE, 'V2D2: CICS WRITE -> SQL INSERT'
  FNAME = EXTRACT_FILE(CCODE)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    FN = FILE.FI.NAME
    CALL ADD_CODE '           EXEC SQL'
    CALL ADD_CODE '             INSERT INTO' FILE.FI.TABLE
    CALL BUILD_SQL_INSERT FN, '             '
    CALL ADD_CODE '           END-EXEC.'
  END
  CALL SKIP_CICS_CONT I
  CHANGES = CHANGES + 1
  RETURN

CONVERT_CICS_REWRITE:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE, 'V2D2: CICS REWRITE -> SQL UPDATE'
  FNAME = EXTRACT_FILE(CCODE)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    FN = FILE.FI.NAME
    KCOL = TRANSLATE(FILE.FI.KEY,' ','-')
    KCOL = TRANSLATE(KCOL,'_',' ')
    CALL ADD_CODE '           EXEC SQL'
    CALL ADD_CODE '             UPDATE' FILE.FI.TABLE
    CALL BUILD_SQL_UPDATE_SET FN, '             '
    CALL ADD_CODE '             WHERE' KCOL,
      '= :' || FILE.FI.KEY
    CALL ADD_CODE '           END-EXEC.'
  END
  CALL SKIP_CICS_CONT I
  CHANGES = CHANGES + 1
  RETURN

CONVERT_CICS_DELETE:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE, 'V2D2: CICS DELETE -> SQL DELETE'
  FNAME = EXTRACT_FILE(CCODE)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    KCOL = TRANSLATE(FILE.FI.KEY,' ','-')
    KCOL = TRANSLATE(KCOL,'_',' ')
    CALL ADD_CODE '           EXEC SQL'
    CALL ADD_CODE '             DELETE FROM' FILE.FI.TABLE
    CALL ADD_CODE '             WHERE' KCOL,
      '= :' || FILE.FI.KEY
    CALL ADD_CODE '           END-EXEC.'
  END
  CALL SKIP_CICS_CONT I
  CHANGES = CHANGES + 1
  RETURN

CONVERT_CICS_STARTBR:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE,,
    'V2D2: CICS STARTBR -> SQL OPEN CURSOR'
  FNAME = EXTRACT_FILE(CCODE)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    CNAME = 'C-' || FILE.FI.NAME
    CALL ADD_CODE,
      '           EXEC SQL OPEN' CNAME 'END-EXEC.'
  END
  CALL SKIP_CICS_CONT I
  CHANGES = CHANGES + 1
  RETURN

CONVERT_CICS_READNEXT:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE,,
    'V2D2: CICS READNEXT -> SQL FETCH'
  FNAME = EXTRACT_FILE(CCODE)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    FN = FILE.FI.NAME
    CNAME = 'C-' || FN
    CALL ADD_CODE '           EXEC SQL'
    CALL ADD_CODE '             FETCH' CNAME
    IF FLD.FN.P.0 > 0 THEN DO
      HLINE = '             INTO '
      DO RI = 1 TO FLD.FN.P.0
        HVAR = ':' || FLD.FN.P.RI.NAME
        IF RI < FLD.FN.P.0 THEN HVAR = HVAR || ','
        IF LENGTH(HLINE || HVAR) > 58 THEN DO
          CALL ADD_CODE HLINE
          HLINE = '               ' || HVAR
        END
        ELSE HLINE = HLINE || ' ' || HVAR
      END
      IF HLINE <> '' THEN CALL ADD_CODE HLINE
    END
    ELSE
      CALL ADD_CODE '             INTO :WS-RECORD'
    CALL ADD_CODE '           END-EXEC.'
  END
  CALL SKIP_CICS_CONT I
  CHANGES = CHANGES + 1
  RETURN

CONVERT_CICS_ENDBR:
  PARSE ARG CLINE, CCODE
  CALL ADD_COMMENT CLINE,,
    'V2D2: CICS ENDBR -> SQL CLOSE CURSOR'
  FNAME = EXTRACT_FILE(CCODE)
  FI = FIND_FILE(FNAME)
  IF FI > 0 THEN DO
    CNAME = 'C-' || FILE.FI.NAME
    CALL ADD_CODE,
      '           EXEC SQL CLOSE' CNAME 'END-EXEC.'
  END
  CALL SKIP_CICS_CONT I
  CHANGES = CHANGES + 1
  RETURN

/*============================================================*/
/* EXTRACT_FILE: Extract FILE(name) from CICS command          */
/*============================================================*/
EXTRACT_FILE:
  PARSE ARG ECODE
  FP = POS('FILE(',ECODE)
  IF FP = 0 THEN RETURN ''
  REST = SUBSTR(ECODE, FP+5)
  EP = POS(')',REST)
  IF EP = 0 THEN RETURN ''
  EFNAME = SUBSTR(REST,1,EP-1)
  EFNAME = STRIP(EFNAME,"B","'")
  RETURN EFNAME

/*============================================================*/
/* SKIP_CICS_CONT: Skip EXEC CICS continuation lines          */
/*============================================================*/
SKIP_CICS_CONT:
  PARSE ARG SI
  DO SI = I+1 TO SRC.0
    SLINE = SRC.SI
    IF LENGTH(SLINE) < 7 THEN ITERATE
    IF SUBSTR(SLINE,7,1) = '*' THEN ITERATE
    SCODE = TRANSLATE(STRIP(SUBSTR(SLINE,8)))
    CALL ADD_COMMENT SLINE, 'V2D2: (CICS cont.)'
    IF POS('END-EXEC',SCODE) > 0 THEN DO
      I = SI
      LEAVE
    END
  END
  RETURN
