       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTCOMP.
      *==============================================================*
      * PROGRAMA COMPLEJO CON MULTIPLES REDEFINES ANIDADOS           *
      * - REGISTRO POLIZA CON REDEFINES POR TIPO (AUTO/VIDA/HOGAR)  *
      * - REDEFINES ANIDADOS EN SUBESTRUCTURAS                       *
      * - OCCURS, COMP-3, COMP, FILLER                              *
      * - MULTIPLES ARCHIVOS VSAM (KSDS + ESDS)                     *
      *==============================================================*
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POLFILE ASSIGN TO POLDD
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS POL-NUMERO
               FILE STATUS IS WS-FS1.
           SELECT MOVFILE ASSIGN TO MOVDD
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-FS2.
       DATA DIVISION.
       FILE SECTION.
       FD  POLFILE.
       01  POL-REGISTRO.
           05 POL-NUMERO           PIC X(12).
           05 POL-SUCURSAL         PIC 9(4)    COMP.
           05 POL-FECHA-EMISION    PIC X(10).
           05 POL-FECHA-VENC       PIC X(10).
           05 POL-TIPO             PIC X(1).
               88 POL-ES-AUTO      VALUE 'A'.
               88 POL-ES-VIDA      VALUE 'V'.
               88 POL-ES-HOGAR     VALUE 'H'.
           05 POL-ESTADO           PIC X(1).
               88 POL-ACTIVA       VALUE 'A'.
               88 POL-CANCELADA    VALUE 'C'.
               88 POL-SUSPENDIDA   VALUE 'S'.
           05 POL-PRIMA-TOTAL      PIC S9(9)V99 COMP-3.
           05 POL-CLIENTE.
              10 CLI-TIPO          PIC X(1).
              10 CLI-DOCUMENTO     PIC X(15).
              10 CLI-NOMBRE        PIC X(40).
              10 CLI-DIRECCION     PIC X(60).
              10 CLI-TELEFONO      PIC X(20).
              10 CLI-EMAIL         PIC X(50).
           05 POL-DETALLE-AUTO REDEFINES POL-CLIENTE.
              10 AUTO-MARCA        PIC X(20).
              10 AUTO-MODELO       PIC X(20).
              10 AUTO-ANIO         PIC 9(4).
              10 AUTO-PATENTE      PIC X(10).
              10 AUTO-CHASIS       PIC X(20).
              10 AUTO-MOTOR        PIC X(20).
              10 AUTO-COLOR        PIC X(15).
              10 AUTO-VALOR        PIC S9(9)V99 COMP-3.
              10 FILLER            PIC X(53).
           05 POL-DETALLE-VIDA REDEFINES POL-CLIENTE.
              10 VIDA-BENEFICIARIO PIC X(40).
              10 VIDA-PARENTESCO   PIC X(15).
              10 VIDA-CAPITAL      PIC S9(11)V99 COMP-3.
              10 VIDA-EDAD-INGR   PIC 9(3).
              10 VIDA-FUMADOR      PIC X(1).
              10 VIDA-GRUPO-SANG   PIC X(3).
              10 FILLER            PIC X(117).
           05 POL-DETALLE-HOGAR REDEFINES POL-CLIENTE.
              10 HOG-DIRECCION     PIC X(60).
              10 HOG-LOCALIDAD     PIC X(30).
              10 HOG-PROVINCIA     PIC X(20).
              10 HOG-CP            PIC X(8).
              10 HOG-METROS-CUAD   PIC 9(5)    COMP.
              10 HOG-VALOR-INMUEBLE PIC S9(11)V99 COMP-3.
              10 HOG-TIPO-CONST    PIC X(1).
                 88 HOG-MATERIAL   VALUE 'M'.
                 88 HOG-MADERA     VALUE 'W'.
                 88 HOG-MIXTO      VALUE 'X'.
              10 FILLER            PIC X(50).
           05 POL-COBERTURAS.
              10 POL-NUM-COBERT    PIC 9(2)    COMP.
              10 POL-COBERT OCCURS 5 TIMES.
                 15 COB-CODIGO     PIC X(6).
                 15 COB-DESCRIPCION PIC X(30).
                 15 COB-MONTO      PIC S9(9)V99 COMP-3.
                 15 COB-DEDUCIBLE  PIC S9(7)V99 COMP-3.
           05 POL-PAGOS.
              10 PAG-FORMA         PIC X(1).
                 88 PAG-DEBITO     VALUE 'D'.
                 88 PAG-TARJETA    VALUE 'T'.
                 88 PAG-EFECTIVO   VALUE 'E'.
              10 PAG-DETALLE-DEB REDEFINES PAG-FORMA.
                 15 PAG-DEB-TIPO   PIC X(1).
              10 PAG-CUOTAS        PIC 9(2)    COMP.
              10 PAG-MONTO-CUOTA   PIC S9(7)V99 COMP-3.
              10 PAG-DIA-VENC      PIC 9(2).
           05 POL-OBSERVACIONES    PIC X(200).
           05 POL-AUDIT.
              10 AUD-USUARIO       PIC X(8).
              10 AUD-FECHA-ALTA    PIC X(10).
              10 AUD-FECHA-MODIF   PIC X(10).
              10 AUD-TERMINAL      PIC X(8).
       FD  MOVFILE.
       01  MOV-REGISTRO.
           05 MOV-FECHA            PIC X(10).
           05 MOV-HORA             PIC X(8).
           05 MOV-POLIZA           PIC X(12).
           05 MOV-TIPO-MOV         PIC X(3).
           05 MOV-MONTO            PIC S9(9)V99 COMP-3.
           05 MOV-DESCRIPCION      PIC X(50).
           05 MOV-USUARIO          PIC X(8).
       WORKING-STORAGE SECTION.
       01  WS-FS1                  PIC XX.
       01  WS-FS2                  PIC XX.
       01  WS-EOF                  PIC X VALUE 'N'.
       01  WS-COUNT                PIC 9(7) VALUE 0.
       01  WS-TOTAL-PRIMAS         PIC S9(11)V99 COMP-3
                                   VALUE 0.
       01  WS-KEY                  PIC X(12).
       01  WS-TIPO-BUSQ            PIC X(1).
       PROCEDURE DIVISION.
       MAIN-PARA.
           OPEN I-O POLFILE.
           OPEN OUTPUT MOVFILE.
           PERFORM LIST-ALL-POLIZAS.
           PERFORM BUSCAR-POLIZA.
           PERFORM ALTA-POLIZA.
           PERFORM MODIF-POLIZA.
           PERFORM BAJA-POLIZA.
           CLOSE POLFILE.
           CLOSE MOVFILE.
           STOP RUN.
       LIST-ALL-POLIZAS.
           MOVE LOW-VALUES TO POL-NUMERO.
           START POLFILE KEY >= POL-NUMERO.
           PERFORM UNTIL WS-EOF = 'Y'
             READ POLFILE NEXT
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END
                 ADD 1 TO WS-COUNT
                 ADD POL-PRIMA-TOTAL TO WS-TOTAL-PRIMAS
             END-READ
           END-PERFORM.
       BUSCAR-POLIZA.
           MOVE 'POL-00000001' TO POL-NUMERO.
           READ POLFILE KEY IS POL-NUMERO.
           IF WS-FS1 = '00'
             DISPLAY 'ENCONTRADA: ' POL-NUMERO
                     ' TIPO=' POL-TIPO
                     ' PRIMA=' POL-PRIMA-TOTAL
           END-IF.
       ALTA-POLIZA.
           MOVE 'POL-99999999' TO POL-NUMERO.
           MOVE 1001 TO POL-SUCURSAL.
           MOVE '2026-04-05' TO POL-FECHA-EMISION.
           MOVE '2027-04-05' TO POL-FECHA-VENC.
           MOVE 'A' TO POL-TIPO.
           MOVE 'A' TO POL-ESTADO.
           MOVE 50000.00 TO POL-PRIMA-TOTAL.
           MOVE 'D' TO CLI-TIPO.
           MOVE '20-12345678-9' TO CLI-DOCUMENTO.
           MOVE 'JUAN PEREZ' TO CLI-NOMBRE.
           MOVE 'AV CORRIENTES 1234' TO CLI-DIRECCION.
           MOVE '011-4555-1234' TO CLI-TELEFONO.
           MOVE 'jperez@mail.com' TO CLI-EMAIL.
           WRITE POL-REGISTRO.
           IF WS-FS1 = '00'
             MOVE '2026-04-05' TO MOV-FECHA
             MOVE '10:30:00' TO MOV-HORA
             MOVE POL-NUMERO TO MOV-POLIZA
             MOVE 'ALT' TO MOV-TIPO-MOV
             MOVE POL-PRIMA-TOTAL TO MOV-MONTO
             MOVE 'ALTA DE POLIZA AUTO' TO MOV-DESCRIPCION
             MOVE 'SYSTEM' TO MOV-USUARIO
             WRITE MOV-REGISTRO
           END-IF.
       MODIF-POLIZA.
           MOVE 'POL-00000001' TO POL-NUMERO.
           READ POLFILE KEY IS POL-NUMERO.
           IF WS-FS1 = '00'
             MOVE 75000.00 TO POL-PRIMA-TOTAL
             MOVE '2026-04-05' TO AUD-FECHA-MODIF
             MOVE 'ADMIN' TO AUD-USUARIO
             REWRITE POL-REGISTRO
             IF WS-FS1 = '00'
               MOVE '2026-04-05' TO MOV-FECHA
               MOVE '10:35:00' TO MOV-HORA
               MOVE POL-NUMERO TO MOV-POLIZA
               MOVE 'MOD' TO MOV-TIPO-MOV
               MOVE POL-PRIMA-TOTAL TO MOV-MONTO
               MOVE 'MODIFICACION PRIMA' TO MOV-DESCRIPCION
               MOVE 'ADMIN' TO MOV-USUARIO
               WRITE MOV-REGISTRO
             END-IF
           END-IF.
       BAJA-POLIZA.
           MOVE 'POL-99999999' TO POL-NUMERO.
           DELETE POLFILE.
           IF WS-FS1 = '00'
             MOVE '2026-04-05' TO MOV-FECHA
             MOVE '10:40:00' TO MOV-HORA
             MOVE 'POL-99999999' TO MOV-POLIZA
             MOVE 'BAJ' TO MOV-TIPO-MOV
             MOVE 0 TO MOV-MONTO
             MOVE 'BAJA DE POLIZA' TO MOV-DESCRIPCION
             MOVE 'ADMIN' TO MOV-USUARIO
             WRITE MOV-REGISTRO
           END-IF.
