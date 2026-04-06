# V2D2 - Sistema de Conversion VSAM a DB2

Sistema que automatiza la conversion de programas COBOL que usan VSAM a DB2, corriendo 100% en z/OS 1.4 con CICS/DB2.

## Que hace

Dado un programa COBOL que lee/escribe archivos VSAM, V2D2:

1. **Analiza** el source COBOL - detecta archivos VSAM, FDs, REDEFINES, operaciones I/O
2. **Genera DDL** - crea `CREATE TABLE` desde las definiciones de registro (FD)
3. **Convierte** el programa - reemplaza operaciones VSAM por SQL embebido
4. **Compila** el programa convertido (DFHECP1$ + DSNHPC + IGYCRCTL + IEWL + BIND)
5. **Migra datos** - lee VSAM, INSERT en DB2, maneja REDEFINES

## Arquitectura

- **REXX** - parsing de COBOL, generacion de DDL, transformacion de codigo (COBOL es malo para manipular strings)
- **COBOL/CICS** - interfaz interactiva con BMS maps (transaccion V2D2)
- **DB2** - almacena resultados del analisis y metadatos
- **JCL batch** - compilacion y migracion de datos

## Estructura

```
rexx/
  v2d2scan.rexx   - Parser de COBOL (detecta VSAM, FDs, REDEFINES, I/O)
  v2d2gddl.rexx   - Generador de DDL (PIC -> DB2 types)
  v2d2dclg.rexx   - Generador de DCLGEN (copybooks con DECLARE TABLE + host vars)
  v2d2xfrm.rexx   - Conversor de programa (VSAM ops -> SQL, usa DCLGEN INCLUDEs)

cobol/
  v2d2main.cbl    - Menu principal CICS

bms/
  v2d2map.bms     - BMS mapset (pantallas CICS)

jcl/
  v2d2comp.jcl    - JCL de compilacion

sql/
  v2d2_schema.sql - DDL de tablas del sistema

test/
  testcomp.cbl    - Programa de prueba complejo (seguros, REDEFINES multiples)
  testvsam.cbl    - Programa de prueba simple
```

## Conversion VSAM -> DB2

### Batch

| VSAM | DB2 |
|------|-----|
| `READ file INTO rec` | `EXEC SQL FETCH cursor INTO :vars END-EXEC` |
| `READ file KEY IS k` | `EXEC SQL SELECT ... WHERE key = :k END-EXEC` |
| `WRITE rec` | `EXEC SQL INSERT INTO tabla VALUES(...) END-EXEC` |
| `REWRITE rec` | `EXEC SQL UPDATE tabla SET ... WHERE key = :k END-EXEC` |
| `DELETE file` | `EXEC SQL DELETE FROM tabla WHERE key = :k END-EXEC` |

### CICS

| CICS VSAM | DB2 |
|-----------|-----|
| `EXEC CICS READ FILE(f) INTO(r) RIDFLD(k)` | `EXEC SQL SELECT ... WHERE key = :k END-EXEC` |
| `EXEC CICS WRITE FILE(f) FROM(r)` | `EXEC SQL INSERT INTO tabla VALUES(:r) END-EXEC` |
| `EXEC CICS STARTBR FILE(f) RIDFLD(k)` | `EXEC SQL OPEN cursor WHERE key >= :k END-EXEC` |
| `EXEC CICS READNEXT FILE(f) INTO(r)` | `EXEC SQL FETCH cursor INTO :r END-EXEC` |
| `EXEC CICS ENDBR FILE(f)` | `EXEC SQL CLOSE cursor END-EXEC` |

## DCLGEN

V2D2 genera **DCLGEN copybooks** estandar IBM para cada tabla DB2 generada. Cada copybook contiene:

1. `EXEC SQL DECLARE tabla TABLE (...)` - declaracion de la estructura de la tabla
2. Host variables COBOL que matchean exactamente las columnas DB2

Los DCLGEN se guardan en `IBMUSER.V2D2.DCLGEN` y el programa convertido los incluye con `EXEC SQL INCLUDE member END-EXEC.` en WORKING-STORAGE.

Pipeline: `V2D2SCAN` -> `V2D2GDDL` -> **`V2D2DCLG`** -> `V2D2XFRM`

## Manejo de REDEFINES

V2D2 detecta REDEFINES en el FD y genera una **tabla ancha** con todas las columnas de todas las variantes. Los campos REDEFINES se omiten de las host variables/DCLGEN y el SQL generado usa solo los campos primarios.

## Requisitos

- z/OS 1.4+
- CICS TS 2.2+
- DB2 v7+
- COBOL (IGYCRCTL)
- Hercules (o mainframe real)

## Datasets en z/OS

- `IBMUSER.V2D2.REXX` - Scripts REXX
- `IBMUSER.V2D2.COBOL` - Fuentes COBOL CICS
- `IBMUSER.V2D2.BMS` - BMS mapset source
- `IBMUSER.V2D2.LOAD` - Load modules
- `IBMUSER.V2D2.DBRM` - DBRMs
- `IBMUSER.V2D2.DCLGEN` - DCLGEN copybooks generados
- `IBMUSER.V2D2.JCL` - JCL procedures
