       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SMOV-AMP.
       AUTHOR.        DROGUERIA SUR.
       INSTALLATION.  VILLARINO 52. BAHIA BLANCA.
       DATE-WRITTEN.  SEPTIEMBRE 1999.

010704* (me) control para que no se pueda dar de baja productos si ya se
010704*      generó un vencimeinto para ese comprobante

260503* (me) modificado para incorporar datos necesarios para el nuevo sistema
      *** Modificado el 09/05/01 por EE para eliminar el pedido de impuestos
      *** internos en la carga de perfumer¡a y reemplazarlo por la marca de
      *** nacional o importado para Aduana.


      *** 28/02/03: Tratamiento para productos que vienen en consignacion
      *** Se agrego un nuevo archivo AMPIO.

      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                             *
      *         ***  OBJETIVOS DEL PROGRAMA "MOV-AMP"  ***          *
      *              """"""""""""""""""""""""""""""""               *
      *                                                             *
      *    ACTUALIZAR EL ARCHIVO MAESTRO DE PRODUCTOS "AMP" A TRA-  *
      *    VES DE LAS TRANSACCIONES VARIAS QUE SE PRODUCEN CON AL-  *
      *    MACENAMIENTO DE LAS MISMAS PARA SU POSTERIOR LISTADO.-   *
      *                                                             *
      *    LISTADO DE LAS TRANSACCIONES: TOTAL O POR MOVIMIENTO.-   *
      *                                                             *
      *    TRANSFERENCIAS DE TODO TIPO ENTRE DISTINTAS DIVISIONES.- *
      *                                                             *
      *    CONSULTAS DEL "AMP" A TRAVES DEL SUBPROGRAMA "CONS-AMP". *
      *                                                             *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *



      /    ***  D I V I S I O N   D E   M E D I O S  ***
      *         """""""""""""""""""""""""""""""""""

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SOURCE-COMPUTER. DOS-UNIX-XENIX.
       OBJECT-COMPUTER. DOS-UNIX-XENIX.

       SPECIAL-NAMES.
           SWITCH-1 ON   IS XENIX,
           SWITCH-1 OFF  IS DOS,
           SWITCH-2 ON   IS UNIX,
           SWITCH-8 ON   IS SPOOLING,
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT AMP ASSIGN TO RANDOM,
                  "/USER/MAESTROS/STCK/AMP-N-01",
                  ORGANIZATION IS RELATIVE,
                  ACCESS MODE  IS DYNAMIC,
                  RELATIVE KEY IS CLAVE-AMP,
                  FILE STATUS  IS STATUS-AMP.

           SELECT AID ASSIGN TO RANDOM,
                  "/USER/MAESTROS/STCK/AID-N-01",
                  ORGANIZATION IS INDEXED,
                  ACCESS MODE  IS DYNAMIC,
                  RECORD           KEY CLAVE-AID-TROQUEL,
                  ALTERNATE RECORD KEY CLAVE-AID-LABORATORIO DUPLICATES,
                  ALTERNATE RECORD KEY CLAVE-AID-ALFANUMERICA,
                  FILE STATUS  IS STATUS-AID.

           SELECT AIL ASSIGN TO RANDOM,
                  "/USER/MAESTROS/STCK/AIL-N-01",
                  ORGANIZATION IS INDEXED,
                  ACCESS MODE  IS DYNAMIC,
                  RECORD           KEY IS CLAVE-AIL-CATEGORIA,
                  ALTERNATE RECORD KEY IS CLAVE-AIL-LAB-DESC,
                  ALTERNATE RECORD KEY IS CLAVE-AIL-DESCRIPCION,
                  FILE STATUS  IS STATUS-AIL.

           SELECT PROV ASSIGN TO RANDOM,
                  "/USER/MAESTROS/PROV/PROVE-01",
                  ORGANIZATION IS INDEXED,
                  ACCESS MODE  IS RANDOM,
                  RECORD KEY   IS CLAVE-PROV,
                  ALTERNATE RECORD KEY IS CLBSQ-PROV   WITH DUPLICATES,
                  ALTERNATE RECORD KEY IS RAZON-SOCIAL WITH DUPLICATES,
                  FILE STATUS  IS STATUS-PROV.

           SELECT TRNAMP ASSIGN TO RANDOM,
                  "/USER/MAESTROS/STCK/TRNAMP01",
                  ORGANIZATION IS INDEXED,
                  ACCESS MODE  IS DYNAMIC,
                  RECORD KEY   IS CLAVE-TRNAMP,
                  FILE STATUS  IS STATUS-TRNAMP.

           SELECT PEDIDO ASSIGN TO RANDOM,
                  "/USER/MAESTROS/STCK/PEDIDO01",
                  ORGANIZATION IS INDEXED,
                  ACCESS MODE  IS DYNAMIC,
                  RECORD           KEY IS CLAVE-NUMERO-PEDIDO,
                  ALTERNATE RECORD KEY IS CLAVE-PROV-PEDIDO DUPLICATES,
                  FILE STATUS  IS STATUS-PEDIDO.

221092     SELECT DIF-PROV ASSIGN TO RANDOM,
                  "/USER/MAESTROS/SHRE/DIFPRV01",
                  ORGANIZATION IS INDEXED,
                  ACCESS MODE  IS DYNAMIC,
                  RECORD           KEY IS CLAVE-DIF-PROV,
                  ALTERNATE RECORD KEY IS CLAVE-DIF-FECHA   DUPLICATES,
                  FILE STATUS  IS STATUS-DIF-PROV.

270594     SELECT TRN-ESP ASSIGN TO RANDOM,
                  "/USER/MAESTROS/STCK/TRNESP01",
                  ORGANIZATION IS INDEXED,
                  ACCESS MODE  IS DYNAMIC,
                  RECORD           KEY IS CLAVE-TRN-ESP,
                  FILE STATUS  IS STATUS-TRN-ESP.

060697     SELECT AMP-P ASSIGN TO RANDOM,
                  "/USER/MAESTROS/STCK/AMP-P-01",
                  ORGANIZATION IS RELATIVE,
                  ACCESS MODE  IS DYNAMIC,
                  RELATIVE KEY IS CLAVE-AMP,
                  FILE STATUS  IS STATUS-AMP-P.

180797     SELECT FACPRV01 ASSIGN TO RANDOM,
                  "/USER/MAESTROS/PROV/FACPRV01",
180797            ORGANIZATION IS INDEXED,
180797            ACCESS MODE IS DYNAMIC,
180797            RECORD KEY IS CLAVE-FACPRV,
180797            FILE STATUS   IS STATUS-FACPRV01.

020997     SELECT FCISPR ASSIGN TO RANDOM,
                  "/USER/MAESTROS/STCK/FCISPR01",
020997            ORGANIZATION IS RELATIVE,
020997            ACCESS MODE IS DYNAMIC,
020997            RELATIVE KEY IS CLAVE-AMP,
180797            FILE STATUS   IS STATUS-FCISPR.

           SELECT AMPIO ASSIGN TO RANDOM,
                  "/USER/MAESTROS/STCK/AMP-IO-01",
                  ORGANIZATION IS RELATIVE,
                  ACCESS MODE  IS DYNAMIC,
                  RELATIVE KEY IS CLAVE-AMP,
                  FILE STATUS  IS STATUS-AMPIO.


010704     SELECT VENCIM ASSIGN TO RANDOM,
                  "/USER/MAESTROS/PROV/VENCIM01",
                  ORGANIZATION IS INDEXED,
                  ACCESS MODE IS DYNAMIC,
                  RECORD KEY IS CLAVE-VENCIM,
                  ALTERNATE RECORD KEY IS CLAVE-SEC1-VENCIM
                                   WITH DUPLICATES,
                  ALTERNATE RECORD KEY IS CLAVE-SEC2-VENCIM
                                   WITH DUPLICATES,
                  FILE STATUS   IS STATUS-VENCIM.



           SELECT LO ASSIGN TO PRINT, FILE-LO,
                  ORGANIZATION IS SEQUENTIAL,
                  ACCESS MODE  IS SEQUENTIAL,
                  FILE STATUS  IS STATUS-LO.


      /    ***  D I V I S I O N   D E   D A T O S  ***
      *         """""""""""""""""""""""""""""""""

       DATA DIVISION.
       FILE SECTION.

      *    ***  DESCRIPCION DEL ARCHIVO "AMP"  ***
      *         """""""""""""""""""""""""""""

       FD  AMP LABEL RECORD IS STANDARD.
       01  REG-AMP.
         03 DATOS-FIJOS-AMP.
           05 ANAGRAFICOS-AMP.
              10 DESCRIPCION-AMP.
                 15 RDESCRIPCION-AMP     PIC X(23).
                 15 FILLER               PIC X(7).
              10 CATEGORIA-AMP           PIC 9.
OJO           88 MEDICAMENTO-AMP         VALUES ARE 1, 6, 7.
              88 ACCESORIO-AMP           VALUES ARE 2, 3, 4, 8, 9.
      *---> a pedido de Enrique 05/01/04 categoria 2 se trata = que la categoria 5        
      * SAQUE 2 210121
              88 PERFUMERIA-AMP          VALUE  IS  5.
              10 CLASIFICACION-AMP       PIC X.
              88 VENTA-LIBRE             VALUES ARE "9" "V".
              10 IMPUESTOS-AMP.
                 15 IVA-AMP              PIC 9.
                 15 INT-AMP              PIC 9.
              10 CLAVES-AMP.
                 15 CLAVE-ALFAN-AMP      PIC X(12).
                 15 TROQUEL-AMP          PIC X(08).
                 15 CLAVE-LABOR-AMP.
                    20 COD-LAB-AMP       PIC 9(4)     COMP-6.
                    20 LIN-LAB-AMP       PIC X(6).
                 15 SINONIMIAS-AMP.
                    20 DROGA-BASE-AMP    PIC 9(5)     COMP-1.
                    20 BULTO-AMP         PIC 9(4)     COMP-6.
              10 CONTROLES-AMP.
                 15 CONTROL-MUTUALES.
                    20 TABLA-PROHIBICION PIC 9(6)     COMP-6.
                    20 TABLA-DESC-ESPECL PIC 9(6)     COMP-6.
                    20 IND-VENTA-AMP     PIC X.
                    20 CUANTOS-TAMANOS   PIC 99       COMP-6.
                    20 QUE-TAMANO        PIC 99       COMP-6.
                 15 FILLER               PIC X(1).
                 15 CANTIDAD-OFERTA-AMP  PIC 9(4)V99  COMP-6.
                 15 IND-CONTROL-AMP      PIC X(1).
                 15 CODIGO-DESCUENTO-AMP PIC 9(2)     COMP-6.
                 15 FECHA-ULT-ACTUALIZ   PIC 9(6)     COMP-6.
         03 DATOS-VARIABLES-AMP.
           05 IMPORTES-AMP.
              10 COSTO-PONDERADO-AMP     PIC 9(8)V99  COMP-6.
090894        10 TIPO-COSTO-AMP          PIC X.
090894        10 COSTO-ULTIMO-AMP        PIC 9(6)V99  COMP-6.
              10 FECHA-COSTO-AMP         PIC 9(6)     COMP-6.
              10 PRECIO-PUBLICO-AMP      PIC 9(8)V99  COMP-6.
              10 PRECIO-FAR-SOC-AMP      PIC 9(8)V99  COMP-6.
              10 FECHA-PRECIO-AMP        PIC 9(6)     COMP-6.
              10 COEF-PRECIO-REPOSICION  PIC 9V9(5)   COMP-6.
           05 COMPRAS-AMP.
131198        10 MES-PLAZO-AMP           PIC 99       COMP-6.
131198        10 FALTA-PROV-TMP-AMP      PIC X.
080198*** En este acmpo que ahora es filler estaba la condicion 1 del labor.
080198        10 PLAZO-AMP               PIC 9999     COMP-6.
161297        10 FECHA-ULT-COND-AMP      PIC 9(6)     COMP-6.
090501        10 ADUANA-AMP              PIC X.

**************10 PUNTO-PEDIDO-AMP        PIC 9(6)V99  COMP-6.
**************10 PUNTO-SEGURIDAD-AMP     PIC 9(6)V99  COMP-6.
              10 ABCS-STOCK.
                 15 CAT-STOCK-AMP        PIC X.
                 15 ABC-STOCK-AMP        PIC X.
                 15 ABC-VENTAS-AMP       PIC X.
                 15 ABC-MARGEN-AMP       PIC X.
              10 ULTIMO-ABC              PIC X.
              10 FECHA-ULT-ABC           PIC 9(6)     COMP-6.
           05 PARTIDAS-AMP.
              10 PARTIDA-AMP             OCCURS 5 TIMES.
                 15 ORIGEN-PARTIDA       PIC X.
                 15 COD-PROV-PARTIDA     PIC 9(4)     COMP-6.
                 15 FECHA-PARTIDA        PIC 9(6)     COMP-6.
                 15 COMPRA-PARTIDA       PIC 9(6)     COMP-6.
                 15 EXISTENCIA-PARTIDA   PIC 9(6)     COMP-6.
                 15 VENCIMIENTO-PARTIDA  PIC 9(4)     COMP-6.
           05 MOVIMIENTOS-AMP.
              10 TOTAL-ACUM-HISTORICO    PIC 9(8)     COMP-6.
              10 TOTALES-SUCURSAL        OCCURS 08 TIMES.
                  15 EXISTENCIA-SUC      PIC 9(6)     COMP-6.
                  15 TOTAL-ACUM-SUC      PIC 9(6)     COMP-6.
              10 DESCUEN-PTM-TL-AMP      PIC 9(02)V99 COMP-6.
              10 CANT-PTM-TL-AMP         PIC 9(04)    COMP-6.
              10 VIGENCIA-PTM-TL-AMP     PIC 9(06)    COMP-6.
              10 FILLER                  PIC X(5).
              10 ACUMULADOS-AMP          OCCURS 12 TIMES.
                 15 CANT-COMPRADA-MES    PIC 9(5)     COMP-1.
                 15 CANT-VENDIDA-MES     PIC 9(5)     COMP-1.
              10 TOTAL-INV-FISICO        PIC 9(6)     COMP-6.
           05 PEDIDOS-AMP.
              10 CANTIDAD-PEDIDA         PIC 9(6)     COMP-6.
              10 ULTIMO-PEDIDO           PIC 9(6)     COMP-6.
           05 VENTAS-AMP.
              10 VENTA-SUC               OCCURS 05 TIMES.
                 15 PENDIENTE-SUC        PIC S9(5)    COMP-1.
                 15 VENDIDA-SUC          PIC S9(5)    COMP-1.
                 15 FECHA-ULT-LIST       PIC 9(6)     COMP-6.
           05 FECHA-SUSPEND              PIC 9(6)     COMP-6.
           05 FILLER                     PIC X(32).
           05 FALTAS-AMP.
              10 COMIENZO-FALTA          PIC 9(6)     COMP-6.
              10 DIAS-EN-FALTA           PIC 9(5)     COMP-1.
              10 ACUM-FALTAS-DIA-AMP     PIC 9(5)     COMP-1.
              10 ACUM-FALTAS-TOT-AMP     PIC 9(5)     COMP-1.
           05 PROXIMO-DESCUENTO-AMP      PIC 9(2)     COMP-6.

      *    ***  DESCRIPCION DEL ARCHIVO "AID"  ***
      *         """""""""""""""""""""""""""""

       FD  AID LABEL RECORD IS STANDARD.
       01  REG-AID.
           05 CLAVE-AID-TROQUEL.
              10 TROQUEL-AID.
                 15 09-X3 PIC X OCCURS 8 TIMES INDEXED BY 09-IND-X3.
           05 CLAVE-AID-LABORATORIO.
              10 COD-LAB-AID             PIC 9(4) COMP-6.
              10 LIN-LAB-AID             PIC X(6).
           05 CLAVE-AID-ALFANUMERICA.
              10 CLAVE-ALFAN-AID         PIC X(12).
              10 POS-REL-AID             PIC 9(5) COMP-1.

      *    ***  DESCRIPCION DEL ARCHIVO "AIL"  ***
      *         """""""""""""""""""""""""""""

       FD  AIL LABEL RECORD IS STANDARD.
       01  REG-AIL.
           05 CLAVE-AIL-CATEGORIA.
              10 CATEGORIA-AIL           PIC 9.
              10 CLASIFICACION-AIL       PIC X.
              10 CLAVE-AIL-LAB-DESC.
                 15 COD-LAB-AIL          PIC 9(4) COMP-6.
                 15 CLAVE-AIL-DESCRIPCION.
                    20 DESCRIPCION-AIL   PIC X(23).
                    20 POS-REL-AIL       PIC 9(5) COMP-1.

      *    ***  DESCRIPCION DEL ARCHIVO "PROV"  ***
      *         """"""""""""""""""""""""""""""
           COPY "F:\FUENTES\FD\PROVE.FD".


      *    ***  DESCRIPCION DEL ARCHIVO "TRNAMP"  ***
      *         """"""""""""""""""""""""""""""""

       FD  TRNAMP LABEL RECORD IS STANDARD.
       01  REG-TRNAMP.
           05 CLAVE-TRNAMP.
              10 SUCURSAL-TRN  PIC 9(02)    COMP-6.
              10 CATEGORIA-TRN PIC 9(01).
              10 CODIGO-TRN    PIC X(02).
211092        88 TRN-NEGATIVO  VALUES ARE "S0" THRU "S9" "AF" "AR" "TR",
081093                                    "-1" THRU "-5" "RF".
              10 NUMERO-TRN    PIC 9(06)    COMP-6.
              10 REGISTRO-TRN  PIC 9(04)    COMP-6.
           05 A-SUCURSAL-TRN   PIC 9(02)    COMP-6.
           05 FECHA-TRN        PIC 9(06)    COMP-6.
           05 TROQUEL-TRN      PIC X(08).
           05 DESC-PROD-TRN    PIC X(30).
           05 CANTIDAD-TRN     PIC S9(5)V99 COMP-3.
           05 COSTO-PRECIO-TRN PIC S9(9)V99 COMP-3.
           05 VENCIMIENTO-TRN  PIC 9(04)    COMP-6.
           05 QUIEN-TRN.
              10 ARTEFACTO-TRN PIC X(07).
              10 ARTEFACTO1TRN REDEFINES ARTEFACTO-TRN.
              15 FILLER        PIC X(04).
              15 ARTEFACTODTRN PIC X(03).
              10 ARTEFACTO2TRN REDEFINES ARTEFACTO-TRN.
              15 FILLER        PIC X(03).
              15 ARTEFACTOXTRN PIC X(04).
              10 OPERADORA-TRN PIC X(02).
              10 HORA-MINU-TRN PIC 9(04)    COMP-6.
           05 IMPUTACION       OCCURS 5 TIMES.
              10 ORIG-IMP-TRN  PIC X(01).
              10 PROV-IMP-TRN  PIC 9(04)    COMP-6.
              10 FECHA-IMP-TRN PIC 9(06)    COMP-6.
              10 COMPR-IMP-TRN PIC 9(06)    COMP-6.
              10 CANT-IMP-TRN  PIC 9(06)    COMP-6.
              10 VTO-IMP-TRN   PIC 9(04)    COMP-6.

      *    ***  DESCRIPCION DEL ARCHIVO "PEDIDO"  ***
      *         """"""""""""""""""""""""""""""""
           COPY "F:\FUENTES\FD\PEDIDO.FD".


      *    ***  DESCRIPCION DEL ARCHIVO "DIF-PROV"  ***
      *         """""""""""""""""""""""""""""""""
           COPY "F:\FUENTES\FD\DIFPROV.FD".

270594*    ***  DESCRIPCION DEL ARCHIVO "TRN-ESP"  ***
      *         """"""""""""""""""""""""""""""""
           COPY "F:\FUENTES\FD\TRN-ESP.FD".

      *
060697*    ***  DESCRIPCION DEL ARCHIVO "AMP-P"  ***
      *         """""""""""""""""""""""""""""""

       FD  AMP-P LABEL RECORD IS STANDARD.
       01  REG-AMP-P.
           05 ITEM-AMP-P          OCCURS 20 TIMES.
              10 COD-PROV-AMP-P   PIC 9(4) COMP-6.
              10 FEC-PART-AMP-P   PIC 9(6) COMP-6.
              10 NRO-COMP-AMP-P   PIC 9(6) COMP-6.
130697        10 NRO-PART-AMP-P   PIC X(8).
              10 CNT-PART-AMP-P   PIC 9(6) COMP-6.
              10 FILLER           PIC X(1).

180797*    ***  DESCRIPCION DEL ARCHIVO "FACPRV01"  ***
      *         """"""""""""""""""""""""""""""""""

           COPY "F:\FUENTES\FD\FACPRV.FD".

180797*    ***  DESCRIPCION DEL ARCHIVO "FCISPR"  ***
      *         """"""""""""""""""""""""""""""""""

       FD  FCISPR LABEL RECORD IS STANDARD.
       01  REG-FCISPR.
           05 ITEM-FCISPR            OCCURS 20 TIMES.
              10 COSTO-FCISPR        PIC 9(6)V99  COMP-6.
              10 CONIVA-FCISPR       PIC X.
              10 DESC-FCISPR         PIC 9(6)V99  COMP-6.


280203*    ***  DESCRIPCION DEL ARCHIVO "AMPIO"  ***
      *         """""""""""""""""""""""""""""""

           COPY "F:\FUENTES\FD\AMPIO.FD".



      *      ***  DESCRIPCION DEL ARCHIVO "VENCIM"  ***
      *           """"""""""""""""""""""""""""
           COPY "F:\FUENTES\FD\VENCIM.FD".



      *    ***  DESCRIPCION DEL ARCHIVO "LO"  ***
      *         """"""""""""""""""""""""""""

       FD  LO LABEL RECORD IS OMITTED.
       01  REG-132             PIC X(132).
       01  REG-80              PIC X(080).
       01  REG-1               PIC X(001).
       01  LINEA-1.
           05 LSUC-CAT-1.
              10 LSUCURSAL-1   PIC 99/ BLANK WHEN ZERO.
              10 LCATEGORIA-1.
              15 NCATEGORIA-1  PIC Z-.
           05 LTIPO-COMP-1     PIC X(2).
020394     05 LNUMERO-COMP-1   PIC 9(6)-.
020394     05 TNUMERO-COMP-1   REDEFINES LNUMERO-COMP-1,
                               PIC Z(6)-.
           05 LFECHA-COMP-1    PIC 999999/.
           05 LHORA-MINUTO-1   PIC 9999/.
           05 LARTEFACTO-1     PIC X(3)/.
           05 LOPERADORA-1     PIC X(2).
           05 LTROQUEL-1       PIC BX(9).
           05 NTROQUEL-1       REDEFINES LTROQUEL-1,
                               PIC BZ(8)-.
           05 LDESC-PROD-1     PIC X(26).
           05 LCANT-MOVIM-1    PIC ZZZZ9,99-.
           05 LCOD-PROV-1      PIC Z(5)-.
           05 LINEA-2.
            10 LORIG-IMP-2     PIC X.
            10 LFECHA-COMPRA-2 PIC 999999-.
            10 LCANT-COMPRA-2  PIC ZZZZ9,99-.
            10 LCANT-EXIST-2   PIC ZZZZ9,99.
            10 LCOSTO-COMPRA-2 PIC ZZZ.ZZ9,99.
            10 LCOSTO-TOTAL-2  PIC ZZ.ZZZ.ZZ9,99-.
       01  VLINEA-1.
           05 VTROQUEL-1       PIC X(9).
           05 VDESC-PROD-1     PIC X(30).
           05 VEXIST-REM-1     PIC Z(3)9,99-.
           05 VEXIST-FAC-1     PIC Z(5)9,99-.
090894     05 VCOSTO-ULT-1     PIC ZZZ.ZZ9,99.
090894     05 VTIPO-COSTO-1    PIC X.
           05 VPRECIO-PUBL-1   PIC Z.ZZZ.ZZ9,99.
       01  LINEA-4.
211092     05 LDETALLE-4.
              10 FILLER        PIC X(17).
              10 LNUMERO-4     PIC 9-.
           05 LSECC-4          PIC X(6).
300993     05 LCOMP-4          PIC ZZ.ZZZ-.
           05 LPROD-4          PIC ZZZZ.ZZZ-.
           05 LCANT-4          PIC ZZZZ.ZZ9,99-.
           05 LEXIST-4         PIC ZZZZ.ZZ9,99-.
300993     05 LCOSTO-4         PIC ZZZ.ZZZ.ZZ9,99-.
300594 01  LINEA-LE.
           05 FILLER           PIC X.
           05 XFECHA-LE.
              10 LFECHA-LE     PIC B99/99/99B.
           05 LTOTAL-LE        REDEFINES XFECHA-LE,
                               PIC Z.ZZZ.ZZ9-.
           05 FILLER           PIC X.
           05 LNOMBRE-LE       PIC BXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXB.
           05 FILLER           PIC X.
           05 LTIPO-LE         PIC BXB.
           05 FILLER           PIC X.
           05 LCOMPROBANTE-LE.
              10 LCOD-COMP-LE  PIC BXXB.
              10 LNRO-COMP-LE  PIC 999999B.
           05 FILLER           PIC X.
           05 LIMPORTE-LE      PIC Z.ZZZ.ZZZ.ZZ9,99CR.
           05 FILLER           PIC X.
250794 01  VPARTIDA-B.
           05 VORIGEN-B        PIC BX/.
           05 VPROV-B          PIC 9999-.
           05 VFECHA-B         PIC 99/99/99-.
           05 VVTO-B           PIC 99/99-.
           05 VCOMPRADO-B      PIC ZZZ.ZZ9,99-.
           05 VEXISTENCIA-B    PIC ZZZ.ZZ9,99-.
060697 01  VPARTIDA.
           05 VNRO-PART-AMP-P  PIC X(8).
           05 FILLER           PIC X.
           05 VCNT-PART-AMP-P  PIC ****9.
           05 FILLER           PIC X.
           05 VCOD-PROV-AMP-P  PIC 9999.
           05 FILLER           PIC X.
           05 VFEC-PART-AMP-P  PIC 99/99/99.
           05 FILLER           PIC X.
           05 VNRO-COMP-AMP-P  PIC 999999.


      /    ***  S E C C I O N   A R E A S   D E   T R A B A J O  ***
      *         """""""""""""""""""""""""""""""""""""""""""""""

       WORKING-STORAGE SECTION.

       01  LINEA-STATUS.
           05 FILLER             PIC X(4) VALUE "AMP:".
           05 STATUS-AMP         PIC X(2) VALUE SPACES.
           05 FILLER             PIC X(5) VALUE ",AID:".
           05 STATUS-AID         PIC X(2) VALUE SPACES.
           05 FILLER             PIC X(5) VALUE ",AIL:".
           05 STATUS-AIL         PIC X(2) VALUE SPACES.
           05 FILLER             PIC X(6) VALUE ",PROV:".
           05 STATUS-PROV        PIC X(2) VALUE SPACES.
           05 FILLER             PIC X(5) VALUE ",TRN:".
           05 STATUS-TRNAMP      PIC X(2) VALUE SPACES.
           05 FILLER             PIC X(5) VALUE ",PED:".
           05 STATUS-PEDIDO      PIC X(2) VALUE SPACES.
221092     05 FILLER             PIC X(5) VALUE ",DIF:".
221092     05 STATUS-DIF-PROV    PIC X(2) VALUE SPACES.
270594     05 FILLER             PIC X(5) VALUE ",ESP:".
270594     05 STATUS-TRN-ESP     PIC X(2) VALUE SPACES.
060697     05 FILLER             PIC X(6) VALUE ",AMPP:".
060697     05 STATUS-AMP-P       PIC X(2) VALUE SPACES.
150797     05 FILLER             PIC X(6) VALUE ",FACP:".
180797     05 STATUS-FACPRV01    PIC X(2) VALUE SPACES.
KKKKKK     05 FILLER             PIC X(6) VALUE ",FCIS:".
KKKKKK     05 STATUS-FCISPR      PIC X(2) VALUE SPACES.
KKKKKK     05 FILLER             PIC X(6) VALUE ",AMIO:".
KKKKKK     05 STATUS-AMPIO       PIC X(2) VALUE SPACES.
           
           05 STATUS-VENCIM     PIC X(2) VALUE SPACES.
              88 EOF-VENCIM     VALUE "FF".
180797     05 FILLER             PIC X(4) VALUE ",LO:".
           05 STATUS-LO          PIC X(2) VALUE SPACES.

       01  LINK-CONT-LO.
           05 IMPRESION        PIC X.
           05 LINEASMAX        PIC 9(2) COMP-6.
           05 PROCESO-LINK.
              10 FILE-LO-LINK  PIC X(80).
           05 PERMISO-LINK     PIC X.
           05 IMPRESO-LINK     PIC 9(2).
           05 AUTOMAT-LINK     PIC X VALUE "N".

       01  CAMPOS-ANT.
           05 SUCURSAL-ANT     PIC 9(2)  VALUE 1.
           05 TRN-ANT          PIC X(2)  VALUE SPACES.
           05 OPERADORA-ANT    PIC X(3)  VALUE SPACES.
           05 COD-PROV-ANT     PIC 9(4)  VALUE ZEROES.
281092     05 COD-PROV-DIST    PIC 9(4)  VALUE ZEROES.
281092     05 RAZON-SOCIAL-D   PIC X(30) VALUE SPACES.
270797     05 FE-COMP-ANT      PIC 9(6).
           05 FECHA-COMP-ANT   PIC 9(6).
           05 RFECHA-COMP-ANT  REDEFINES FECHA-COMP-ANT.
              10 ANO-ANT       PIC 9(2).
              10 MES-ANT       PIC 9(2).
              10 DIA-ANT       PIC 9(2).
           05 FEC-NUE.
              10 ANO-NUE       PIC 9(2).
              10 MES-NUE       PIC 9(2).
              10 DIA-NUE       PIC 9(2).
           05 NRO-COMP-ANT     PIC 9(6)  VALUE ZEROES.
           05 DESC-PROD-ANT    PIC 9(6)V99 VALUE ZEROES.
           05 DESC-GLOB-ANT    PIC 9(6)V99 VALUE ZEROES.
           05 DESC-REC         PIC 9(6)V99 VALUE ZEROES.
250797     05 IVA-RECEP-ANT    PIC X       VALUE "N".

       01  XNUMERO             PIC X(12) JUSTIFIED RIGHT.
       01  NUMERO              REDEFINES XNUMERO,
                               PIC 9(12).
       01  NUMERO-DEC          REDEFINES XNUMERO,
                               PIC 9(10)V99.
       01  NUMERO-COEF         REDEFINES XNUMERO,
                               PIC 9(8)V9(4).
       01  FECHA-EDIT          REDEFINES XNUMERO,
                               PIC 99/99/99.
       01  FECHA-VTO-EDIT      REDEFINES XNUMERO,
                               PIC 99/99.
       01  NUM-EDIT-1          REDEFINES XNUMERO,
                               PIC Z(4).
250194 01  ZNUM-PORC           REDEFINES XNUMERO,
                               PIC Z99,99.
271198 01  ZNUM-PORC-2         REDEFINES XNUMERO,
                               PIC Z99,99-.

       01  NUM-HORA-MINUTO     REDEFINES XNUMERO.
           05 FILLER           PIC X(4).
           05 HORA-MINUTO      PIC 9(4).
           05 FILLER           PIC X(4).
300393 01  NUM-TRANSF          REDEFINES XNUMERO,
                               PIC 9(5).

       01  FECHAUX.
           05 ANOX       PIC 9(02).
           05 MESX       PIC 9(02).
           05 FILLER     PIC 9(02).
       01 FECHAUXR   REDEFINES FECHAUX PIC 9(06).


       01 WK-VCTO-AUX.
          05 WK-ESTA-VTO           PIC X VALUE SPACES.
          05 WK-ENCUENTRO-VTO      PIC X VALUE SPACES.
          05 WK-DISTRIBUIDOR       PIC 9(06) VALUE ZEROES.
          05 WK-FACTURA            PIC 9(08) VALUE ZEROES.

       77  WK-CLAVE            PIC X(04) VALUE SPACES.
       77  KARLA               PIC 9(08).
       77  IVA-WK              PIC V99.
       77  LIVA                PIC X(03).

       77  MESES               PIC 9(04).
       77  AUXANIO             PIC S9(04).
       77  AUXMES              PIC S9(04).


       77  NUM-EDIT-2          PIC ZZZ.ZZ9,99.
       77  NUM-EDIT-3          PIC ZZZZ.ZZ9,99.
       77  IVA-RECEP           PIC X.
       77  RESPUESTA           PIC X.
       88  AFIRMATIVO          VALUES ARE "S" "0".
       88  NEGATIVO            VALUE  IS  "N".
110293 88  SEGURISIMO          VALUE  IS  "!".
       77  RESP-REMITO         PIC X(1).
       77  TRN                 PIC X(2).
081093 88  TRN-SIN-COSTO       VALUE  IS            "TR".
211092 88  NE-NS               VALUES ARE "E0" THRU "E9" "S0" THRU "S9",
081093                                    "+1" THRU "+5" "-1" THRU "-5".
170693 88  ESMMX               VALUES ARE "E0" "S0" "E5" "S5" "E8" "S8",
021293                                    "+1" THRU "+5" "-1" THRU "-5".
120794 88  TRNVESP             VALUES ARE "E4" "S4" "+1" "-1".
       77  OPERADORA           PIC X(3).
       77  A-SUCURSAL          PIC 9(2).
       77  BAND                PIC 9(4)     COMP-1.
221092 88  BAND-ENTER          VALUES ARE 00, 10, 13.
       77  WK-BAND               PIC 9(4)     COMP-1.
       88  WK-BAND-ENTER         VALUES ARE 00, 10, 13.
       77  EXC                 PIC 9(4)     COMP-1.
       77  AUXILIAR            PIC S9(7)V99 COMP-3,
                                            VALUE IS ZERO.
011194 77  AUXILIAR1           PIC 9(8)     COMP-6.
       77  INTERMEDIO2         PIC S9(7)V99 COMP-3,
                                            VALUE IS ZERO.
       77  INTERMEDIO          PIC S9(7)V99 COMP-3
                                            VALUE IS ZERO.
130995 77  NUMERAL             REDEFINES INTERMEDIO,
                               PIC 9(10)    COMP-6.
       77  TOTAL-SBT           PIC S9(7)V99 COMP-3,
                                            VALUE IS ZERO.
       77  TOTAL-DES           PIC S9(7)V99 COMP-3,
                                            VALUE IS ZERO.
       77  TOTAL-IVA           PIC S9(7)V99 COMP-3,
                                            VALUE IS ZERO.
       77  TOTAL-FAC           PIC S9(7)V99 COMP-3,
                                            VALUE IS ZERO.
       77  TOTAL-EXN           PIC S9(7)V99 COMP-3,
                                            VALUE IS ZERO.
       77  COSTO-TOTAL         PIC S9(7)V99 COMP-3.
       77  AUX                 PIC 9(4)     COMP-1.
       77  IND                 PIC 9(4)     COMP-1.
       77  IND-1               PIC 9(4)     COMP-1.
       77  IND-P               PIC 9(4)     COMP-1.
       77  IND-V               PIC 9(2).
       77  NRO-LINEA           PIC 9(2)     VALUE IS 99.
       77  NRO-PAGINA          PIC 9(4)     VALUE IS 1.
       77  FECHA-DIA           PIC 9(6).
       77  CLAVE-AMP           PIC 9(6)     VALUE IS 1.
       77  TIPO-LISTADO        PIC X.
       77  ACCESO              PIC X.
300594 77  QUE-LE              REDEFINES ACCESO,
                               PIC X.
300594 88  QUEVLE              VALUES ARE "M" "A" "P" "T".
       77  QUE-PRECIO          PIC X        VALUE IS SPACE.
       77  COEF-S-PRECIO       PIC 99V9(4)  COMP-6,
                                            VALUE IS ZERO.
       77  ACT-PRECIO          PIC X.
       77  IVA-REPO            PIC X.
       77  IND-TRN             PIC 9(2).
       77  IND-SIG             PIC X        VALUE IS "N".
       77  CONTADOR-TRN        PIC 9(4)     COMP-6.
260894 77  FECHA-INIC          PIC 9(6).
260894 77  FECHA-FINAL         PIC 9(6).
130193 77  BORRADO             PIC X.
280993 77  DIV                 PIC 99       COMP-6.
130697 77  PARTIDA             PIC X(8).
230797 77  DISCRI-IVA          PIC X.
230797 77  DESC-MED-FAC        PIC S9(7)V99 COMP-3,
                                   VALUE IS ZERO.
       77  DESC-PER-FAC        PIC S9(7)V99 COMP-3,
                                   VALUE IS ZERO.
       77  DESC-ACC-FAC        PIC S9(7)V99 COMP-3,
                                   VALUE IS ZERO.
230797 77  PERCEPCION-IB-FAC   PIC S9(7)V99 COMP-3,
                                   VALUE IS ZERO.
271097 77  PERCEP-P3337        PIC S9(7)V99 COMP-3,
                                   VALUE IS ZERO.
230797 77  DESC-PRODUCTO       PIC 9(6)V99 COMP-6.
270398 77  MODO-CARGA          PIC X  VALUE IS "U".
       88  POR-BULTO           VALUE IS "B".
271198 77  AUX-DIF             PIC S9(6)V99 COMP-3 VALUE IS ZERO.
090501 77  NAC-IMP             PIC X VALUE SPACES.

       01  CLAVE-PEDIDO-AUX.
           05 NUMERO-PEDIDO-AUX       PIC 9(6)    COMP-6.
           05 ORDEN-1-PEDIDO-AUX      PIC 9(5)    COMP-1.



280993 01  COMANDO-SYSTEM.
241194     05 COMANDO          PIC X(60).
           05 09-AREAS-CLAVE-ALFAB REDEFINES COMANDO.
              10 09-INPUT      PIC X(30).
              10 09-ENTRADA    REDEFINES 09-INPUT.
                 15 09-X1      PIC X OCCURS 30 INDEXED BY 09-IND-X1.
              10 09-SALIDA4.
      *...........09-IND-X2 se usa en el Reg-Prov, por eso se cambio a X4.
      *           15 09-X2      PIC X OCCURS 12 INDEXED BY 09-IND-X2.
                 15 09-X4      PIC X OCCURS 12 INDEXED BY 09-IND-X4.
              10 09-CARACTER   PIC X.
              10 09-CUENTA     PIC 9(02) COMP-6.
241194        10 FILLER        PIC X(16).
           05 FILLER           PIC X(01) VALUE LOW-VALUE.


       01  FECHA-COMPLETA.
           05  CENTURIA   PIC 99   COMP-6.
           05  FECHA-8    PIC 9(6) COMP-6.
           05  RFECHA-8   REDEFINES FECHA-8.
               10 ANO-8     PIC 99 COMP-6.
               10 MES-8     PIC 99 COMP-6.
               10 DIA-8     PIC 99 COMP-6.
       01  RFECHA-COMPLETA REDEFINES FECHA-COMPLETA PIC 9(8) COMP-6.
##2000 01  FECHA-COMPLETAC REDEFINES FECHA-COMPLETA.
           05 CENC         PIC 9(2) COMP-6.
           05 ANO-MESC     PIC 9(4) COMP-6.
           05 ANORMESC     REDEFINES ANO-MESC.
              10 ANOC      PIC 9(2) COMP-6.
              10 MESC      PIC 9(2) COMP-6.
           05 DIAC         PIC 9(2) COMP-6.

##2000 01  FECHA-ANT-8         PIC 9(8) COMP-6.

       01  FECHA               PIC 9(6).
       01  XFECHA              REDEFINES FECHA.
           05 ANO              PIC 9(2).
           05 MES              PIC 9(2).
           05 DIA              PIC 9(2).

       01  LIN-POS-SIZ.
           05 LIN              PIC 9(2).
           05 POSI              PIC 9(2).
           05 SIZ              PIC 9(2).

       01  EXISTENCIAS.
           05 EXIST-REMITO     PIC S9(5)    COMP-3.
           05 EXIST-FACTURA    PIC S9(5)    COMP-3.
           05 EXIST-TOTAL      PIC S9(7)    COMP-3.

       01  TABLA-ACUMULADOS.
           05 ITEM-TOTAL       OCCURS 5 TIMES.
              10 TOT-CATEGORIA PIC 9(4)     COMP-1.
              10 TOT-COMP      PIC 9(5)     COMP-1.
              10 TOT-PROD      PIC 9(5)     COMP-1.
              10 TOT-CANT      PIC S9(7)    COMP-3.
              10 TOT-EXIST     PIC S9(7)    COMP-3.
              10 TOT-COSTO     PIC S9(9)V99 COMP-3.

       01  CLAVE-CONTROL.
           05 SUC-CONTROL      PIC 9(2).
           05 CAT-CONTROL      PIC 9(2).
           05 NRO-CONTROL      PIC 9(6).
           05 TRANS-CONTROL.
211092     88 VTRANSCONTROL    VALUES ARE "FC" "TR" "RM" "AF" "AR" "RF",
211092                                    "S0" THRU "S9" "E0" THRU "E9",
081093                                    "-1" THRU "-5" "+1" THRU "+5".
211092        10 TRANS1CONTROL PIC X.
211092        10 TRANS2CONTROL PIC 9.

       01  MENSAJE-ERROR.
           05 MENSAJE          PIC X(26).
           05 FILLER           PIC X(01) VALUE "=".
           05 NUM-MENS         PIC ZZZZ.ZZ9,99-.


      /    ***  TITULOS USADOS EN EL PROGRAMA  ***
      *         """""""""""""""""""""""""""""

       01  TITULO-1.
           05 FILLER PIC X(54) VALUE "Sistema Gesti¢n de Stocks".
           05 FILLER PIC X(51) VALUE "þþùú {MOV-AMP} úùþþ".
           05 FILLER PIC X(7)  VALUE "Fecha:".
           05 TFEC-1 PIC 99/99/99.
           05 FILLER PIC X(8)  VALUE " þ Hoja:".
           05 TPAG-1 PIC ZZZ9.
       01  TITULO-4.
120794     05 TSUC-4.
           10 FILLER PIC X(15) VALUE SPACES.
           10 TESP-4 PIC X(02) VALUE SPACES.
           10 FILLER PIC X(10) VALUE SPACES.
           05 FILLER PIC X(31) VALUE "Movimientos del Stock entre el ".
           05 TFEC41 PIC 99/99/99.
           05 FILLER PIC X(06) VALUE " y el ".
           05 TFEC42 PIC 99/99/99.

       01 TABLA-OBJETOS.
          10 ITEM-OBJETO1     PIC X(22) VALUE "/USER/OBJETOS/XCONT-LO".
          10 ITEM-OBJETO2     PIC X(22) VALUE "/USER/OBJETOS/CUADRADO".
          10 ITEM-OBJETO3     PIC X(22) VALUE "/USER/OBJETOS/CONS-AMP".
          10 ITEM-OBJETO4     PIC X(22) VALUE "/USER/OBJETOS/CONT-ACC".
       01  OTROS-FILES.
           05 FILE-LO             PIC X(07)  VALUE "PRINTER".
300393     05 FILE-TRANSF         PIC X(14)  VALUE SPACES.

       LINKAGE SECTION.

       77  KEY-PARAMETRO            PIC 9(02).
       77  ARTEFACTO                PIC X(07).
       77  CLIENTE                  PIC 9(02).
       01  PARAMETROS-EMPRESA.
           05 NOMBRE-EMPRESA        PIC X(32).
           05 TABLA-SUCURSALES.
              10 ITEM-SUCURSAL      OCCURS 10 TIMES.
                 15 NOMBRE-SUCURSAL PIC X(10).
                 15 NOMBREXSUCURSAL PIC X(30).

      /    ***  D I V I S I O N   D E   P R O C E D I M I E N T O S  ***
      *         """""""""""""""""""""""""""""""""""""""""""""""""""

       PROCEDURE DIVISION USING KEY-PARAMETRO, ARTEFACTO, CLIENTE,
                                PARAMETROS-EMPRESA.

       DECLARATIVES.
       CONTROL-PERIFERICOS SECTION.
           USE AFTER ERROR PROCEDURE ON AMP AID AIL PROV TRNAMP PEDIDO,
280203     AMP-P, DIF-PROV, TRN-ESP, AMPIO,
180797     FACPRV01, FCISPR, VENCIM.
       CONTROL-PER.
           IF STATUS-TRNAMP      = "94" OPEN OUTPUT TRNAMP,
                                        CLOSE       TRNAMP,
                                        OPEN I-O    TRNAMP      ELSE
           IF STATUS-PEDIDO      = "94" OPEN OUTPUT PEDIDO,
                                        CLOSE       PEDIDO,
                                        OPEN I-O    PEDIDO      ELSE
           IF STATUS-DIF-PROV    = "94" OPEN OUTPUT DIF-PROV
                                        CLOSE       DIF-PROV,
                                        OPEN I-O    DIF-PROV    ELSE
270594     IF STATUS-TRN-ESP     = "94" OPEN OUTPUT TRN-ESP,
                                        CLOSE       TRN-ESP,
                                        OPEN I-O    TRN-ESP     ELSE
060697     IF STATUS-AMP-P       = "94" OPEN OUTPUT AMP-P,
                                        CLOSE       AMP-P,
                                        OPEN I-O    AMP-P       ELSE
280203     IF STATUS-AMPIO       = "94" OPEN OUTPUT AMPIO,
                                        CLOSE       AMPIO,
                                        OPEN I-O    AMPIO       ELSE

180797     IF STATUS-FACPRV01    = "94" OPEN OUTPUT FACPRV01,
                                        CLOSE FACPRV01,
                                        OPEN I-O FACPRV01       ELSE
           IF STATUS-VENCIM      = "94" OPEN OUTPUT VENCIM,
                                        CLOSE       VENCIM,
                                        OPEN I-O    VENCIM      ELSE
180797     IF STATUS-FCISPR      = "94" OPEN OUTPUT FCISPR,
                                        CLOSE FCISPR,
                                        OPEN I-O FCISPR,
310393*    IF STATUS-TRANSF      = "94" NEXT SENTENCE           ELSE
           IF STATUS-AMP         = "99" OR
              STATUS-AID         = "99" OR
              STATUS-AIL         = "99" OR
              STATUS-PROV        = "99" OR
              STATUS-TRNAMP      = "99" OR
              STATUS-PEDIDO      = "99" OR
              STATUS-VENCIM      = "99" OR 
              STATUS-DIF-PROV    = "99" OR
270594        STATUS-TRN-ESP     = "99" OR
060697        STATUS-AMP-P       = "99" OR
180797        STATUS-FACPRV01    = "99" OR
280203        STATUS-AMPIO       = "99" OR
KKKKKK        STATUS-FCISPR      = "99",
              DISPLAY "REGISTRO BLOQUEADO POR OTRO PROCESO !"
                       LINE 24 REVERSE BLINK ERASE EOL,
              ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW,
              PERFORM DISP-SPACES                               ELSE
           IF STATUS-TRNAMP      = "93" NEXT SENTENCE           ELSE
           DISPLAY LINEA-STATUS LINE 24 REVERSE ERASE EOL,
           ACCEPT RESPUESTA POSITION 0 PROMPT "¯" BLINK,
           PERFORM DISP-SPACES,
           IF STATUS-AMP         = "93" OR
              STATUS-AID         = "93" OR
              STATUS-AIL         = "93" OR
              STATUS-PROV        = "93" OR
              STATUS-PEDIDO      = "93" OR
              STATUS-DIF-PROV    = "93" OR
270594        STATUS-TRN-ESP     = "93" OR
060697        STATUS-AMP-P       = "93" OR
280203        STATUS-AMPIO       = "93" OR
180797        STATUS-FACPRV01    = "93" OR
KKKKKK        STATUS-FCISPR      = "93",
              DISPLAY "HAY ARCHIVOS BLOQUEADOS POR OTROS PROCESOS !"
                       LINE 24 REVERSE BLINK ERASE EOL,
              ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW BLINK,
              PERFORM DISP-SPACES,
              GO TO EXIT-PROGRAM-DECL.
           GO TO SALIDA-DECLARATIVO-1.

       EXIT-PROGRAM-DECL.
           EXIT PROGRAM.
       STOP-RUN-DECL.
           STOP RUN.

       SALIDA-DECLARATIVO-1.

       DECLARATIVO-LO SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON LO.
       CONTROL-LO.
           DISPLAY "Controle el Estado de la Impresora y Pulse:{Enter} p
      -            "ara Continuar ..." LINE 24 REVERSE BLINK ERASE EOL.
           ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW.

       DISP-SPACES.
           IF SPOOLING DISPLAY "SPOOLING" LINE 24 LOW ERASE EOL ELSE
211092                 DISPLAY "þùú"      LINE 24 LOW ERASE EOL.
       SAL-DISP-SPACES.

       END DECLARATIVES.

       SECCION-1 SECTION 1.

       COMIENZO-PROGRAMA.
           GO TO SECCION-50.

      *    ***  RUTINAS DEL PROGRAMA  ***
      *         """"""""""""""""""""

       CONFIRMA.
           DISPLAY " Confirma? (" POSITION 0 LOW,
                   "S"            POSITION 0 HIGH,
                   "|"            POSITION 0 LOW BLINK,
                   "0"            POSITION 0 HIGH,
                   "/"            POSITION 0 LOW,
                   "N"            POSITION 0 HIGH,
                   ") "           POSITION 0 LOW ERASE EOL.
           ACCEPT RESPUESTA POSITION 0 PROMPT "*" BLINK NO BEEP;
                  ON EXCEPTION EXC MOVE "N" TO RESPUESTA.
           PERFORM DISP-SPACES.
       SAL-CONFIRMA.

110293 CONFIRMACION.
           DISPLAY " Confirma? (" POSITION 0 LOW,
                   "!"            POSITION 0 BLINK,
                   "/"            POSITION 0 LOW,
                   "N"            POSITION 0 HIGH,
                   ") "           POSITION 0 LOW ERASE EOL.
           ACCEPT RESPUESTA POSITION 0 PROMPT "*" BLINK NO BEEP;
                  ON EXCEPTION EXC MOVE "N" TO RESPUESTA.
           PERFORM DISP-SPACES.
       SAL-CONFIRMACION.

       FECHA-8-DIGITOS.
           IF FECHA-8 = ZEROES
              MOVE ZEROES TO RFECHA-COMPLETA
           ELSE
              IF ANO-8 > 50
                MOVE 19       TO CENTURIA
              ELSE
                MOVE 20       TO CENTURIA.
       FIN-FECHA-8-DIGITOS.
           EXIT.


280993 CALL-SYSTEM.
           CALL   "SYSTEM" USING COMANDO-SYSTEM.
           CANCEL "SYSTEM".
       SAL-CALL-SYSTEM.

       RUT-FECHA.
           MOVE ANO TO AUX.
           MOVE DIA TO ANO.
           MOVE AUX TO DIA.
       SAL-RUT-FECHA.

       ENTRA-FECHA.
           DISPLAY SPACES LINE LIN POSITION POSI SIZE 8.
           PERFORM CONTROL-NUMERO.
           IF BAND NOT = ZERO GO TO SAL-ENTRA-FECHA.
           MOVE NUMERO TO FECHA.
           IF FECHA NOT = ZEROES,
           IF (ANO < 1 OR ANO > 31) OR
              (MES < 1 OR MES > 12),
              DISPLAY "FECHA INCORRECTA !" LINE 24 BLINK BEEP,
              GO TO ENTRA-FECHA ELSE
           MOVE FECHA TO FECHA-EDIT,
           DISPLAY FECHA-EDIT LINE LIN POSITION POSI,
           PERFORM RUT-FECHA.
       SAL-ENTRA-FECHA. EXIT.

       CONTROLA-IMPRESORA.
           MOVE "PROGRAMA {MOV-AMP} DEL SISTEMA GESTION DE STOCKS ..."
                                          TO PROCESO-LINK.
           CALL   ITEM-OBJETO1 USING LINK-CONT-LO;
                  ON OVERFLOW MOVE ZEROES TO LINEASMAX,
                              MOVE "N"    TO PERMISO-LINK.
           CANCEL ITEM-OBJETO1.
       SAL-CONTROLA-IMPRESORA.

       EJECUTA-BEEP.
050198     DISPLAY " " LINE 24 POSITION 79 BEEP,
                   "|" LINE 24 POSITION 79 BEEP,
                   "/" LINE 24 POSITION 79 BEEP,
                   "-" LINE 24 POSITION 79 BEEP,
                   "\" LINE 24 POSITION 79 BEEP.
       SAL-EJECUTA-BEEP.


       CONTROL-NUMERO.
           ACCEPT NUMERO LINE LIN POSITION POSI SIZE SIZ PROMPT "þ";
                  NO BEEP CONVERT;
                  ON EXCEPTION BAND MOVE ZEROES TO EXC.
           IF BAND     = 98,
              IF LIN   = 24 DISPLAY "?" LINE LIN POSITION POSI BEEP,
                            GO TO CONTROL-NUMERO             ELSE
                            DISPLAY "DATO NUMERICO !"
                                     LINE 24 BLINK BEEP ERASE EOL,
                            GO TO CONTROL-NUMERO             ELSE
                     IF NOT BAND-ENTER,
               DISPLAY SPACES LINE LIN POSITION POSI SIZE SIZ ELSE
               IF (BAND NOT = ZEROES AND BAND NOT = 10),
                   MOVE ZEROES TO BAND,
                   COMPUTE EXC = SIZ - 12,
                   INSPECT NUMERO TALLYING EXC FOR LEADING ZEROES,
                   IF EXC > ZEROES DISPLAY SPACES POSITION 0 SIZE EXC.
           IF LIN NOT = 24 PERFORM DISP-SPACES.
       SAL-CONTROL-NUMERO.

       09-RUT-CLAVE.
           MOVE SPACES TO 09-SALIDA4.
           INSPECT 09-INPUT REPLACING ALL SPACES BY "*".
           SET 09-IND-X4 TO 1.
           PERFORM 09-CICLO VARYING 09-IND-X1 FROM 1 BY 1
                            UNTIL   09-IND-X4 > 12
                            OR      09-IND-X1 > 30.
       SAL-09-RUT-CLAVE.
       09-CICLO.
           MOVE 09-X1(09-IND-X1) TO 09-CARACTER.
           IF (09-CARACTER NUMERIC) OR
              (09-CARACTER ALPHABETIC AND
              ((09-IND-X1 = 1) OR
              (09-CARACTER NOT = "A" AND
               09-CARACTER NOT = "E" AND
               09-CARACTER NOT = "I" AND
               09-CARACTER NOT = "O" AND
               09-CARACTER NOT = "U")))
           MOVE 09-CARACTER TO 09-X4(09-IND-X4),
           SET 09-IND-X4 UP BY 1.
       SAL-09-CICLO.

       09-RUT-TROQUEL.
           MOVE ZEROES TO TROQUEL-AID, 09-CUENTA.
           SET 09-IND-X3 TO 8.
           PERFORM 09-ANAL-CARACTER THRU SAL-09-ANAL-CARACTER
                   VARYING 09-IND-X1 FROM 8 BY -1
                   UNTIL (09-CUENTA NOT = ZEROES OR
                          09-IND-X1     = ZEROES).
           IF (09-CUENTA NOT = ZEROES OR 09-IND-X3 > 1),
               MOVE 09-INPUT TO TROQUEL-AID.
       SAL-09-RUT-TROQUEL.
       09-ANAL-CARACTER.
           IF 09-IND-X3 = ZERO MOVE 8 TO 09-CUENTA ELSE
           MOVE 09-X1(09-IND-X1) TO 09-CARACTER,
           IF (09-CARACTER = SPACE OR 09-CARACTER = "-")
               NEXT SENTENCE                 ELSE
           IF (09-CARACTER < "0" OR 09-CARACTER > "9"),
               MOVE 9           TO 09-CUENTA ELSE
               MOVE 09-CARACTER TO 09-X3(09-IND-X3),
               SET  09-IND-X3 DOWN BY 1,
               IF 09-IND-X3 = 7 MOVE "-" TO 09-X3(7),
                                SET 09-IND-X3 TO 6.
       SAL-09-ANAL-CARACTER.

KKKKKK LEE-FCISPR.
           READ FCISPR RECORD;
                INVALID KEY
                MOVE LOW-VALUES TO REG-FCISPR.
           IF STATUS-FCISPR = "99" GO TO LEE-FCISPR.
       FIN-LEE-FCISPR.



*********** CALCULOS PARA VISUALIZAR Y GRABAR EN LA IM **************


       CALCULA-TOTAL-FACTURA.
           MOVE ZEROES TO TOTAL-FAC.

           COMPUTE TOTAL-SBT =  TOT-NOM-MED-FACPRV +
                                TOT-NOM-PER-FACPRV +
                                TOT-NOM-ACC-FACPRV.

           COMPUTE TOTAL-IVA = IVA-GRV-MED-FACPRV +
                               IVA-GRV-PER-FACPRV +
                               IVA-GRV-ACC-FACPRV.

           COMPUTE TOTAL-DES = DESCUEN-MED-FACPRV +
                               DESCUEN-PER-FACPRV +
                               DESCUEN-ACC-FACPRV.
           MOVE IVA-EXN-MED-FACPRV TO TOTAL-EXN.

       SUMA-TOTAL-FACTURA.
******* Total Gravado ***************
           IF TRN = "AF" MULTIPLY -1 BY AUXILIAR GIVING AUXILIAR.

           COMPUTE INTERMEDIO ROUNDED = ( AUXILIAR *
                                        ( DESC-PRODUCTO / 100 )) .

******* Si tiene un descuento del 100 % no debe acumular. ******
           IF DESC-PRODUCTO = 100,
                 MOVE ZEROES TO AUXILIAR.

           MOVE TOTAL-SBT TO NUM-EDIT-3.
           DISPLAY NUM-EDIT-3 LINE 24 POSITION 9.
           MOVE AUXILIAR TO NUM-EDIT-3.
           DISPLAY NUM-EDIT-3 LINE 24 POSITION 25.
           COMPUTE TOTAL-SBT = TOTAL-SBT + AUXILIAR.
           MOVE TOTAL-SBT TO NUM-EDIT-3.
           DISPLAY NUM-EDIT-3 LINE 23 POSITION 9 HIGH.

******* Importe del Descuento *******
           COMPUTE INTERMEDIO ROUNDED = ( AUXILIAR *
                                        ( DESC-PRODUCTO / 100 )) .
           COMPUTE TOTAL-DES ROUNDED = (TOTAL-DES + INTERMEDIO).
           MOVE TOTAL-DES TO NUM-EDIT-3.
           DISPLAY NUM-EDIT-3 LINE 23 POSITION 27 HIGH.

******** Importe del IVA a partir del Importe con descuento. ********
******** Tiene el IVA incluido en el Precio              ************
           IF MEDICAMENTO-AMP AND IVA-AMP = 0 AND IVA-RECEPCION = "S"
               MOVE ZEROES TO INTERMEDIO2               ELSE
               IF INTERMEDIO = 0
IVA19              MULTIPLY AUXILIAR BY IVA-WK GIVING
                                INTERMEDIO2 ROUNDED
               ELSE
                   COMPUTE INTERMEDIO2 ROUNDED =
IVA19                    ( AUXILIAR - INTERMEDIO ) * IVA-WK.
           IF PERFUMERIA-AMP OR ACCESORIO-AMP OR
              ( MEDICAMENTO-AMP AND IVA-AMP = 1 )
                 ADD INTERMEDIO2 TO TOTAL-IVA.
           IF ( MEDICAMENTO-AMP AND IVA-AMP = 0 )
                 ADD INTERMEDIO2 TO TOTAL-EXN.
           MOVE TOTAL-IVA TO NUM-EDIT-3.
           DISPLAY NUM-EDIT-3 LINE 23 POSITION 45 HIGH.
                          
******** Total de la Factura = total - descuentos + iva - percepcion ***
           COMPUTE TOTAL-FAC = TOTAL-SBT - TOTAL-DES + TOTAL-IVA +
                               TOTAL-EXN.
           MOVE TOTAL-FAC TO NUM-EDIT-3.
           DISPLAY NUM-EDIT-3 LINE 23 POSITION 65 HIGH.

       SAL-CALCULA-TOTAL-FACTURA.

240797 GRABO-FACPRV.
           IF TRN = "AF"
                PERFORM CALCULOS-PARA-AF.

      *....ELSA
260503     IF IVA-AMP = 0
260503        ADD AUXILIAR      TO EXENTO-FACPRV.
260503     IF IVA-AMP = 1
260503        ADD AUXILIAR      TO GRAVADO1-FACPRV.
260503     IF IVA-AMP = 2
260503        ADD AUXILIAR      TO GRAVADO2-FACPRV.

           IF MEDICAMENTO-AMP,
*******  No incluye IVA el Precio y es Exento
              IF IVA-RECEPCION = "N" AND IVA-AMP = 0
                  ADD INTERMEDIO2 TO IVA-EXN-MED-FACPRV,
                  ADD AUXILIAR    TO TOT-NOM-MED-FACPRV,
                  ADD INTERMEDIO  TO DESCUEN-MED-FACPRV  ELSE
******* No incluye IVA el Precio y es Gravado
              IF IVA-RECEPCION = "N" AND IVA-AMP = 1,
                  ADD INTERMEDIO2 TO IVA-GRV-MED-FACPRV,
                  ADD AUXILIAR    TO TOT-NOM-MED-FACPRV,
                  ADD INTERMEDIO  TO DESCUEN-MED-FACPRV  ELSE
******* Si incluye IVA el Precio y es Gravado
*******         ( le saco el IVA al Precio )
              IF IVA-RECEPCION = "S" AND IVA-AMP = 1,
                  ADD INTERMEDIO2 TO  IVA-GRV-MED-FACPRV,
                  COMPUTE TOT-NOM-MED-FACPRV =
                               ( AUXILIAR - INTERMEDIO2 ),
                  ADD INTERMEDIO  TO DESCUEN-MED-FACPRV  ELSE
             IF IVA-RECEPCION = "S" AND IVA-AMP = 0,
                  ADD AUXILIAR    TO TOT-NOM-MED-FACPRV,
                  ADD INTERMEDIO2 TO IVA-EXN-MED-FACPRV,
                  ADD INTERMEDIO  TO DESCUEN-MED-FACPRV.


           IF PERFUMERIA-AMP
               ADD INTERMEDIO2 TO IVA-GRV-PER-FACPRV,
               ADD AUXILIAR    TO TOT-NOM-PER-FACPRV,
               ADD INTERMEDIO  TO DESCUEN-PER-FACPRV  ELSE
           IF ACCESORIO-AMP
               ADD INTERMEDIO2 TO IVA-GRV-ACC-FACPRV,
               ADD AUXILIAR    TO TOT-NOM-ACC-FACPRV,
               ADD INTERMEDIO  TO DESCUEN-ACC-FACPRV.

           MOVE PERCEPCION-IB-FAC TO TOT-PERCEP-FACPRV.
271097     MOVE PERCEP-P3337      TO TOT-P3337-FACPRV.

           IF TRN = "AF" AND ( TOT-NOM-MED-FACPRV < 1 AND
                               TOT-NOM-ACC-FACPRV < 1 AND
                               TOT-NOM-PER-FACPRV < 1 )
               DELETE FACPRV01 RECORD INVALID KEY GO TO ACTUALIZA-AMP-P
           ELSE
               REWRITE REG-FACPRV INVALID KEY
                       DISPLAY "NO GRABE " LINE 2 POSITION 1.
           COMPUTE AUXILIAR ROUNDED =
                   (CANTIDAD-TRN * COSTO-PRECIO-TRN).

       SAL-GRABO-FACPRV.

       LIMPIA-TOTALES-LIN23.
           DISPLAY "           " LINE 23 POSITION 9  HIGH,
                   "           " LINE 23 POSITION 27 HIGH,
                   "           " LINE 23 POSITION 45 HIGH,
                   "           " LINE 23 POSITION 65 HIGH.
       SAL-LIMPIA-TOTALES-LIN23.

       LIMPIA-RECTANGULO.
           DISPLAY SPACES LINE  8 POSITION 26 SIZE 29,
                   SPACES LINE  9 POSITION 26 SIZE 29,
                   SPACES LINE 10 POSITION 26 SIZE 29,
                   SPACES LINE 11 POSITION 26 SIZE 29,
                   SPACES LINE 10 POSITION 56 SIZE 25,
                   SPACES LINE 11 POSITION 56 SIZE 25,
                   SPACES LINE 12 POSITION 56 SIZE 25,
                   SPACES LINE 13 POSITION 56 SIZE 25.
       SAL-LIMPIA-RECTANGULO.

       CALCULOS-PARA-AF.
*********** Importe del Producto *********
           COMPUTE AUXILIAR = CANTIDAD-TRN *
                              COSTO-PRECIO-TRN * (-1).
*********** Importe del Descuento ********
           COMPUTE INTERMEDIO ROUNDED = ( AUXILIAR *
                                        ( DESC-PRODUCTO / 100 )) .
**********  Importe del IVA *************
           IF MEDICAMENTO-AMP AND IVA-AMP = 0 AND IVA-RECEPCION = "S"
               MOVE ZEROES TO INTERMEDIO2               ELSE
               IF INTERMEDIO = 0
IVA19              MULTIPLY AUXILIAR BY IVA-WK GIVING
                                INTERMEDIO2 ROUNDED
               ELSE
                   COMPUTE INTERMEDIO2 ROUNDED =
IVA19                    ( AUXILIAR - INTERMEDIO ) * IVA-WK.

       SAL-CALCULOS-PARA-AF.

       PIDE-NUE-PERCEP-IB-FAC.
           DISPLAY "Ingrese el Nuevo Importe de la Percepci¢n ..."
                    LINE 24 LOW BLINK ERASE EOL.
           MOVE "116911" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
           IF BAND NOT = ZEROES,
                GO TO SAL-P-NUE-PER-IB-FAC.
           IF NUMERO-DEC = ZEROES,
              MOVE TOT-PERCEP-FACPRV TO NUMERO-DEC.
           MOVE NUMERO-DEC TO PERCEPCION-IB-FAC, NUM-EDIT-2.
           DISPLAY NUM-EDIT-2 LINE 11 POSITION 69.
       PIDE-NUE-PERCEP-P3337.
           DISPLAY "Ingrese el Nuevo Importe de la R.G 3337 ..."
                    LINE 24 LOW BLINK ERASE EOL.
           MOVE "126911" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
           IF BAND NOT = ZEROES,
                GO TO SAL-P-NUE-PER-IB-FAC.
           IF NUMERO-DEC = ZEROES,
              MOVE TOT-P3337-FACPRV TO NUMERO-DEC.
           MOVE NUMERO-DEC TO PERCEP-P3337, NUM-EDIT-2.
           DISPLAY NUM-EDIT-2 LINE 12 POSITION 69.
       SAL-P-NUE-PER-IB-FAC.

       CONTROLAR-VENCIM.

           COMPUTE AUXANIO  = (DIA - ANOX).

           IF AUXANIO <  ZEROES
                MOVE ZEROES  TO MESES.

           COMPUTE MESES = AUXANIO * 12.

           COMPUTE AUXMES    = (MES  - MESX)
           IF AUXMES < ZEROES
               COMPUTE MESES = MESES - 12,
               COMPUTE MESES = (12 - MESX) + MES + MESES
           ELSE
               COMPUTE MESES = AUXMES + MESES.

           IF MESES < 7
               DISPLAY  "Ingres¢ un vencimiento inferior a 7 MESES "
                        LINE 24 POSITION 1 ERASE EOL,
                        " VERIFIQUE !!! " POSITION 0,
               ACCEPT RESPUESTA POSITION 0.

           IF MESES > 48
               DISPLAY  "Ingres¢ un vencimiento superior a 4 A¥OS"
                        LINE 24 POSITION 1 ERASE EOL,
                        " VERIFIQUE !!! " POSITION 0,
               ACCEPT RESPUESTA POSITION 0.
       FIN-CONTROLAR-VENCIM.


*********** FIN CALCULOS PARA VISUALIZAR Y GRABAR EN LA IM **********

       PIDE-PRODUCTO SECTION 20.
       PIDE-CODIGO.
           UNLOCK AMP.
           UNLOCK TRNAMP.
           UNLOCK PEDIDO.
060697     UNLOCK AMP-P.
KKKKKK     UNLOCK FCISPR.
060697     MOVE LOW-VALUES TO REG-TRNAMP, PARTIDA.
           IF ACCESO = "C" GO TO PIDE-TROQUEL.
           IF ACCESO = "L" GO TO PIDE-LINEA.
           IF ACCESO = "P" GO TO PIDE-NOMBRE.
           IF ACCESO = "D" GO TO LEE-PEDIDO.
           IF ACCESO = "N" GO TO PIDE-CLAVE.

       RASTREA-ANTERIOR.
           IF ORDEN-1-PEDIDO = ZEROES     GO TO PIDE-ACCESO.
           SUBTRACT 1 FROM ORDEN-1-PEDIDO.
           START PEDIDO  KEY = CLAVE-NUMERO-PEDIDO;
                 INVALID KEY              GO TO RASTREA-ANTERIOR.
       LEE-PEDIDO.
170599     READ PEDIDO NEXT RECORD WITH NO LOCK
                                          AT END GO TO PIDE-ACCESO.
           IF STATUS-PEDIDO  = "99"       GO TO LEE-PEDIDO.
030894     IF ORDEN-1-PEDIDO > 32760      GO TO LEE-PEDIDO.
           MOVE CLAVE-AMP-PEDIDO TO CLAVE-AMP.
170599     MOVE CLAVE-NUMERO-PEDIDO       TO CLAVE-PEDIDO-AUX.
       LEE-AMP-PEDIDO.
           READ AMP RECORD INVALID KEY    GO TO NO-EXISTE-AMP-PEDIDO.
           IF STATUS-AMP = "99"           GO TO LEE-AMP-PEDIDO.
           GO TO VIS-AMP.
       NO-EXISTE-AMP-PEDIDO.
           DISPLAY "NO EXISTE EL PRODUCTO PEDIDO:"
                                      LINE    24 BLINK REVERSE,
                   DESCRIPCION-PEDIDO POSITION 0 LOW,
                   " (Dado de Baja) " POSITION 0 LOW ERASE EOL,
           ACCEPT RESPUESTA POSITION 0 PROMPT "¯" BLINK;
                  ON EXCEPTION EXC MOVE "N" TO RESPUESTA.
           PERFORM DISP-SPACES.
           IF NEGATIVO        GO TO PIDE-ACCESO.
           IF RESPUESTA = "-" GO TO RASTREA-ANTERIOR.
           GO TO LEE-PEDIDO.

       PIDE-LINEA.
***********IF IND-SIG = "S" GO TO LEE-SIG-AID.
           DISPLAY "L:" LINE 19 POSITION 1 LOW REVERSE,
                   "Ingrese el Codigo de Linea del Proveedor..."
                        LINE 24 POSITION 1 LOW BLINK,
                   "({"         POSITION 0 LOW,
                   "Enter"      POSITION 0 LOW REVERSE,
                   "}:Contin£a con el Siguiente)"
                                POSITION 0 LOW ERASE EOL.
           ACCEPT LIN-LAB-AID LINE 19 POSITION 3 PROMPT "x" ECHO NO BEEP
                  ON EXCEPTION EXC GO TO PIDE-ACCESO.
           PERFORM DISP-SPACES.
           MOVE COD-PROV TO COD-LAB-AID.
           IF LIN-LAB-AID = SPACES GO TO LEE-SIG-AID.
       POSICIONA-CLAVE-AID-LAB.
           START AID KEY IS NOT < CLAVE-AID-LABORATORIO;
                 INVALID KEY
                 DISPLAY "NO EXISTE TAL PRODUCTO ! " LINE 24 BLINK,
                 ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW,
                 GO TO PIDE-ACCESO.
           GO TO LEE-SIG-AID.
       PIDE-TROQUEL.
***********IF IND-SIG = "S" GO TO LEE-SIG-AID.
           DISPLAY "Ingrese el Codigo del Producto ..."
                           LINE    24 LOW BLINK,
                   " ({"   POSITION 0 LOW,
                   "Enter" POSITION 0 LOW REVERSE,
                   "}:Contin£a con el Siguiente)"
                           POSITION 0 LOW ERASE EOL.
           ACCEPT 09-INPUT LINE 19 POSITION 1 SIZE 8 PROMPT "x";
                  ECHO NO BEEP ON EXCEPTION EXC GO TO PIDE-ACCESO.
           PERFORM DISP-SPACES.
           IF 09-INPUT = SPACES GO TO LEE-SIG-AID.
           PERFORM 09-RUT-TROQUEL.
           DISPLAY TROQUEL-AID LINE 19 POSITION 1 REVERSE.
       POSICIONA-TROQUEL-AID.
           START AID KEY IS NOT < CLAVE-AID-TROQUEL;
                 INVALID KEY
                 DISPLAY "NO EXISTE UN CODIGO PARECIDO ! "
                          LINE 24 BLINK,
                 ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW,
                 GO TO PIDE-ACCESO.
           GO TO LEE-SIG-AID.
       PIDE-CLAVE.
***********IF IND-SIG = "S" GO TO LEE-SIG-AID.
           DISPLAY "Ingrese la Descripci¢n del Producto ..."
                           LINE    24 LOW BLINK,
                   " ({"   POSITION 0 LOW,
                   "Enter" POSITION 0 LOW REVERSE,
                   "}:Contin£a con el Siguiente)"
                           POSITION 0 LOW ERASE EOL.
           ACCEPT 09-INPUT LINE 19 POSITION 10 PROMPT "x" ECHO NO BEEP;
                  ON EXCEPTION EXC GO TO PIDE-ACCESO.
           PERFORM DISP-SPACES.
           IF 09-INPUT = SPACES GO TO LEE-SIG-AID.
           PERFORM 09-RUT-CLAVE.
           MOVE 09-SALIDA4 TO CLAVE-ALFAN-AID.
       POSICIONA-CLAVE-ALFAN-AID.
           START AID KEY IS NOT < CLAVE-ALFAN-AID;
                 INVALID KEY
                 DISPLAY "NO EXISTE UN PRODUCTO SIMILAR ! "
                          LINE 24 BLINK ERASE EOL,
                 ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW,
                 GO TO PIDE-ACCESO.
           GO TO LEE-SIG-AID.
       PIDE-NOMBRE.
           MOVE LOW-VALUES TO CLAVE-AIL-DESCRIPCION.
***********IF IND-SIG = "S" GO TO LEE-SIG-AIL.
           DISPLAY SPACES LINE 19 POSITION 33 SIZE 7,
                   "Ingrese la Descripci¢n del Producto ..."
                          LINE 24 POSITION  1 LOW BLINK,
                   " ({"          POSITION  0 LOW,
                   "Enter"        POSITION  0 LOW REVERSE,
                   "}:Contin£a con el Siguiente)"
                                  POSITION  0 LOW ERASE EOL.
           ACCEPT DESCRIPCION-AIL LINE 19 POSITION 10 PROMPT "x" ECHO;
                  NO BEEP ON EXCEPTION EXC GO TO PIDE-ACCESO.
           PERFORM DISP-SPACES.
           IF DESCRIPCION-AIL = SPACES     GO TO LEE-SIG-AIL.
       POSICIONA-CLAVE-AIL-LAB-DESC.
           START AIL KEY IS NOT < CLAVE-AIL-LAB-DESC;
                 INVALID KEY
                 DISPLAY "NO EXISTE TAL PRODUCTO PARA ESE PROVEEDOR ! "
                          LINE 24 BLINK ERASE EOL,
                 ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW,
                 GO TO PIDE-ACCESO.
       LEE-SIG-AIL.
           READ AIL NEXT RECORD WITH NO LOCK;
                AT END
                DISPLAY "NO HAY MAS PRODUCTOS ! "
                         LINE 24 BLINK ERASE EOL,
                ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW,
                GO TO PIDE-ACCESO.
           MOVE POS-REL-AIL TO CLAVE-AMP.
           GO TO LEE-AMP.
       LEE-SIG-AID.
           READ AID NEXT RECORD WITH NO LOCK;
                AT END
                DISPLAY "NO HAY MAS PRODUCTOS ! "
                         LINE 24 BLINK ERASE EOL,
                ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW,
                GO TO PIDE-ACCESO.
           IF STATUS-AID = "99" GO TO LEE-SIG-AID.
           MOVE POS-REL-AID TO CLAVE-AMP.
       LEE-AMP.
           READ AMP RECORD;
                INVALID KEY
                DISPLAY "NO EXISTE ESE PRODUCTO ! "
                         LINE 24 BLINK ERASE EOL,
                ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW,
                GO TO PIDE-ACCESO.
           IF STATUS-AMP = "99" GO TO LEE-AMP.
           
      * A pedido de Enrique se saltean los productos con marca "N".
240504     IF IND-CONTROL-AMP = "N" 
              DISPLAY DESCRIPCION-AMP LINE 19 POSITION 9,
              DISPLAY " PRODUCTO DADO DE BAJA  ! No se permite movimient 
      -               "o de productos ... "
                      LINE 24 POSITION 01 REVERSE BLINK,
              ACCEPT RESPUESTA POSITION 0,
              IF ACCESO = "C" OR ACCESO = "L" OR ACCESO = "N"
                 GO TO LEE-SIG-AID
              ELSE IF ACCESO = "P" 
                 GO TO LEE-SIG-AIL.

       LEE-AMPIO.
           IF SUCURSAL-ANT NOT = 04 GO TO VIS-AMP.
           READ AMPIO RECORD
                    INVALID KEY
                    MOVE LOW-VALUES TO REG-AMPIO,
                    MOVE ZEROES TO FECHA-PARTIDAIO(1),
                    GO TO VIS-AMP.
           IF STATUS-AMPIO = "99" GO TO LEE-AMPIO.


       VIS-AMP.
060697     READ AMP-P RECORD WITH NO LOCK;
                INVALID KEY
                MOVE LOW-VALUES     TO REG-AMP-P.
KKKKKK     READ FCISPR RECORD WITH NO LOCK;
                INVALID KEY
                MOVE LOW-VALUES     TO REG-FCISPR.
           IF STATUS-AMP  = "99" GO TO VIS-AMP.
250693     IF LIN-LAB-AMP > SPACES,
              MOVE LIN-LAB-AMP      TO VTROQUEL-1 ELSE
              MOVE TROQUEL-AMP      TO VTROQUEL-1.
           MOVE DESCRIPCION-AMP     TO VDESC-PROD-1.
           MOVE LOW-VALUES          TO EXISTENCIAS.
           PERFORM DETERMINA-EXISTENCIAS VARYING IND-P FROM 1 BY 1
                   UNTIL FECHA-PARTIDA(IND-P) = ZEROES OR IND-P > 5.
           MOVE EXIST-REMITO        TO VEXIST-REM-1.
           MOVE EXIST-FACTURA       TO VEXIST-FAC-1.
           ADD  EXIST-REMITO, EXIST-FACTURA
                GIVING                 EXIST-TOTAL.
090894     IF TIPO-COSTO-AMP = "L",
              MOVE "*"              TO VTIPO-COSTO-1,
              MOVE COSTO-ULTIMO-AMP TO VCOSTO-ULT-1 ELSE
              MOVE " "              TO VTIPO-COSTO-1,
030292        COMPUTE VCOSTO-ULT-1 ROUNDED =
030292               (COEF-PRECIO-REPOSICION * PRECIO-PUBLICO-AMP).
           MOVE PRECIO-PUBLICO-AMP  TO VPRECIO-PUBL-1.
           DISPLAY VLINEA-1 LINE 19 POSITION 1.
060697 VIS-PARTIDA.
           MOVE 20 TO IND.
       BUSCA-VPARTIDA.
           IF ITEM-AMP-P(IND) = LOW-VALUES,
              IF IND NOT = 6, ADD -1 TO IND, GO TO BUSCA-VPARTIDA.
           SUBTRACT 5 FROM IND.
130697     MOVE "140680"                  TO LIN-POS-SIZ.
       V-PARTIDA.
           MOVE ALL "ú"                   TO VPARTIDA.
           IF ITEM-AMP-P(IND) NOT = LOW-VALUES,
              MOVE COD-PROV-AMP-P(IND)    TO VCOD-PROV-AMP-P,
              MOVE FEC-PART-AMP-P(IND)    TO FECHA, PERFORM RUT-FECHA,
              MOVE FECHA                  TO VFEC-PART-AMP-P,
              MOVE NRO-COMP-AMP-P(IND)    TO VNRO-COMP-AMP-P,
              MOVE NRO-PART-AMP-P(IND)    TO VNRO-PART-AMP-P,
              MOVE CNT-PART-AMP-P(IND)    TO VCNT-PART-AMP-P.
           DISPLAY VPARTIDA LINE LIN POSITION POSI LOW.
           ADD  1                         TO IND.
           IF LIN NOT = 16, ADD  1        TO LIN,
                        GO TO V-PARTIDA.
130697     IF POSI     = 06, MOVE "144280" TO LIN-POS-SIZ,
                        GO TO V-PARTIDA.
060697 SAL-VIS-PARTIDA. EXIT.
***********IF (TRN = "TR" AND PENDIENTE-SUC(A-SUCURSAL) NOT > ZEROES),
***********    MOVE "S" TO IND-SIG,
***********    GO TO PIDE-CODIGO.
***********MOVE "N" TO IND-SIG.
060697 PREG-ES-ESTE.
           DISPLAY "Es ‚ste?   ("               LINE    24 LOW,
                   "S"                           POSITION 0 HIGH,
                   "|"                           POSITION 0 LOW BLINK,
                   "0"                           POSITION 0 HIGH,
                   "/"                           POSITION 0 LOW,
                   "N"                           POSITION 0 HIGH,
                   "/Siguiente:{"                POSITION 0 LOW,
                   "Enter"                       POSITION 0 LOW REVERSE.
           IF ACCESO = "D",
           DISPLAY "}, seg£n Pedido= Anterior:{" POSITION 0 LOW,
                   "-"                           POSITION 0 HIGH BLINK.
           DISPLAY "})"                ERASE EOL POSITION 0 LOW.
           ACCEPT RESPUESTA LINE 24 POSITION 10 PROMPT "*";
                  NO BEEP BLINK ON EXCEPTION EXC MOVE "N" TO RESPUESTA.
***********PERFORM DISP-SPACES.
           IF NEGATIVO        GO TO PIDE-ACCESO.
           IF RESPUESTA = "-",
              IF ACCESO = "D" GO TO RASTREA-ANTERIOR ELSE
                              GO TO VIS-AMP.
           IF NOT AFIRMATIVO  GO TO PIDE-CODIGO.
030595 CONTROLA-IND-CONTROL.
180497     IF IND-CONTROL-AMP = "S",
              DISPLAY "PRODUCTO CON VENTA SUSPENDIDA !"
                                          LINE    24 BLINK REVERSE ELSE
180497     IF IND-CONTROL-AMP = "D",
              DISPLAY "PRODUCTO DISCONTINUADO !"
                                          LINE    24 BLINK REVERSE ELSE
180497     IF IND-CONTROL-AMP = "M",
              DISPLAY "PRODUCTO DISCONTINUADO MOMENTANEAMENTE !"
                                          LINE    24 BLINK REVERSE ELSE
180497     IF IND-CONTROL-AMP = "P",
              MOVE IND-CONTROL-AMP     TO RESPUESTA,
              GO TO ACTUALIZO-REG-AMP                              ELSE
120704     IF IND-CONTROL-AMP = "N" AND 
120704        EXISTENCIA-SUC (01) > ZEROES  AND 
120704      ( CATEGORIA-AMP = 2 OR 
120704        CATEGORIA-AMP = 3 OR 
120704        CATEGORIA-AMP = 5 )
120704            GO TO PIDE-CANTIDAD                              ELSE
270498     IF IND-CONTROL-AMP = "N",
              DISPLAY "PRODUCTO DADO DE BAJA  !"
                                          LINE    24 BLINK REVERSE ELSE

                                          GO TO PIDE-CANTIDAD.
           DISPLAY " Lo Cambia Ahora:{"   POSITION 0 LOW,
                   "!"                    POSITION 0 BLINK,
                   "}úContin£a:"          POSITION 0 LOW,
                   "Enter"                POSITION 0 HIGH,
                   " "                    POSITION 0 LOW ERASE EOL.
           ACCEPT RESPUESTA POSITION 0 PROMPT "?" BLINK;
                  ON EXCEPTION EXC GO TO VIS-AMP.
290595     IF NOT SEGURISIMO       GO TO PIDE-CANTIDAD.


           IF IND-CONTROL-AMP = "S"
                MOVE ZEROES TO FECHA-SUSPEND.
       ACTUALIZO-REG-AMP.
           IF MEDICAMENTO-AMP MOVE SPACE TO IND-CONTROL-AMP ELSE
                              MOVE "-"   TO IND-CONTROL-AMP.
           REWRITE REG-AMP.
           IF STATUS-AMP = "99" GO TO ACTUALIZO-REG-AMP.
050697 LEE-AMP-IND-CONTROL.
           READ AMP RECORD INVALID KEY GO TO PIDE-CODIGO.
           IF STATUS-AMP = "99"        GO TO LEE-AMP-IND-CONTROL.
           IF RESPUESTA NOT = "P"
           DISPLAY "Producto Cambiado ! " LINE    24 BLINK,
                   "Pulse:{"              POSITION 0 LOW,
180497             "Enter"                POSITION 0 LOW REVERSE,
                   "}ú"                   POSITION 0 LOW ERASE EOL.
           IF RESPUESTA NOT = "P"
           ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW BLINK NO BEEP.
       PIDE-CANTIDAD.
           DISPLAY "Ingrese la Cantidad del Producto ..."
                    LINE 24 LOW BLINK ERASE EOL.
           IF (TRN NOT = "TR" OR PENDIENTE-SUC(A-SUCURSAL) NOT > 0),
               GO TO ENTRA-CANTIDAD.
           MOVE PENDIENTE-SUC(A-SUCURSAL) TO NUM-EDIT-2, CANTIDAD-TRN.
           DISPLAY NUM-EDIT-2       LINE 21 POSITION 2,
                   "Transferencia:" LINE 24 POSITION 1 LOW REVERSE,
                   "   (Confirma:{"         POSITION 0 LOW,
                   "C"                      POSITION 0 HIGH,
                   "}úBorra:{"              POSITION 0 LOW,
                   "B"                      POSITION 0 HIGH,
                   "}úModifica:{"           POSITION 0 LOW,
                   "M"                      POSITION 0 HIGH,
                   "}úSiguiente:{"          POSITION 0 LOW,
                   "S"                      POSITION 0 HIGH,
                   "|"                      POSITION 0 LOW BLINK,
                   "0"                      POSITION 0 HIGH,
                   "})"                     POSITION 0 LOW ERASE EOL.
           ACCEPT RESPUESTA LINE 24 POSITION 16 PROMPT "*" NO BEEP BLINK
                  ON EXCEPTION EXC GO TO PIDE-ACCESO.
           PERFORM DISP-SPACES.
           IF RESPUESTA = "C" GO TO SAL-PIDE-PRODUCTO.
           IF RESPUESTA = "B" MOVE ZEROES TO PENDIENTE-SUC(A-SUCURSAL),
                              PERFORM REGRABA-AMP,
                              GO TO RUT-NS.
           IF RESPUESTA = "M" GO TO ENTRA-CANTIDAD.
           IF AFIRMATIVO      GO TO RUT-NS.
           GO TO PIDE-CANTIDAD.
       ENTRA-CANTIDAD.
           DISPLAY "ÄÄÄÄÄÄÄÄÄÄ" LINE 22 POSITION 02 LOW.
270398     IF POR-BULTO
               DISPLAY "Ingrese la Cantidad de Bultos del Producto ..."
                    LINE 24 LOW BLINK ERASE EOL
           ELSE
               DISPLAY "Ingrese la Cantidad del Producto ..."
                    LINE 24 LOW BLINK ERASE EOL.
           MOVE "210210"     TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
221092     IF BAND = 10 PERFORM ENTRA-NUEVO-NUMERO,
                        PERFORM PIDE-NUE-PERCEP-IB-FAC THRU
                                SAL-P-NUE-PER-IB-FAC,
221092                  GO TO   ENTRA-CANTIDAD.
           IF BAND NOT = ZEROES GO TO PIDE-ACCESO.
040504     IF NUMERO = ZEROES  
              DISPLAY "Ingrese Cantidad Mayor a cero ..."
                       LINE 24 LOW BLINK ERASE EOL,
                       ACCEPT RESPUESTA,
              GO TO  ENTRA-CANTIDAD.
              
           MOVE NUMERO       TO CANTIDAD-TRN.
           MOVE CANTIDAD-TRN TO NUM-EDIT-2.
           DISPLAY NUM-EDIT-2 LINE 21 POSITION 02.
      *********************************************************
270398     IF POR-BULTO
               COMPUTE CANTIDAD-TRN = CANTIDAD-TRN * BULTO-AMP
               MOVE CANTIDAD-TRN TO NUM-EDIT-2
               DISPLAY NUM-EDIT-2 LINE 22 POSITION 02
           ELSE
               DISPLAY "ÄÄÄÄÄÄÄÄÄÄ" LINE 22 POSITION 02 LOW.
      *********************************************************
170693     IF (TRN-SIN-COSTO OR (NE-NS AND NOT ESMMX)),
               COMPUTE COSTO-PRECIO-TRN ROUNDED =
                      (COEF-PRECIO-REPOSICION * PRECIO-PUBLICO-AMP),
               MOVE COSTO-PRECIO-TRN TO NUM-EDIT-2,
kkkkkk         DISPLAY NUM-EDIT-2 LINE 21 POSITION 15,
               COMPUTE AUXILIAR ROUNDED =
                      (CANTIDAD-TRN * COSTO-PRECIO-TRN),
               MOVE AUXILIAR TO NUM-EDIT-3,
               DISPLAY NUM-EDIT-3 LINE 21 POSITION 30,
               GO TO PIDE-VENCIMIENTO.
       PIDE-COSTO-PRECIO.
           DISPLAY "ÄÄÄÄÄÄÄÄÄÄ" LINE 22 POSITION 15 LOW.
           DISPLAY "Ingrese el " LINE 24 LOW BLINK.
           IF COEF-S-PRECIO < 1,
               DISPLAY "PRECIO"  LINE 24 POSITION 12 HIGH BLINK ELSE
               DISPLAY "COSTO"   LINE 24 POSITION 12 HIGH BLINK.
           IF POR-BULTO
               DISPLAY " del Bulto ......"   POSITION  0 LOW BLINK
                                         ERASE EOL
           ELSE
               DISPLAY " del Producto ..."   POSITION  0 LOW BLINK
                                         ERASE EOL.
       P-COSTO-PRECIO.
           MOVE "211510" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
221092     IF BAND = 10 PERFORM ENTRA-NUEVO-NUMERO,
221092                  GO TO PIDE-COSTO-PRECIO.
           IF BAND NOT = ZEROES GO TO PIDE-CANTIDAD.
270398     IF NUMERO = ZEROES AND POR-BULTO GO TO PIDE-COSTO-PRECIO.
           IF NUMERO   = ZEROES,
              IF COEF-S-PRECIO < 1,
                 MOVE PRECIO-PUBLICO-AMP TO NUMERO-DEC ELSE
030292           COMPUTE AUXILIAR ROUNDED =
030292           (COEF-PRECIO-REPOSICION * PRECIO-PUBLICO-AMP),
              IF ACCESO = "D",
                 MOVE COSTO-PEDIDO       TO NUMERO-DEC ELSE
                 MOVE AUXILIAR           TO NUMERO-DEC.
           IF NUMERO-DEC NOT > ZEROES GO TO PIDE-COSTO-PRECIO.
           MOVE NUMERO-DEC               TO COSTO-PRECIO-TRN,
                                            NUM-EDIT-2.
           DISPLAY NUM-EDIT-2 LINE 21 POSITION 15.
           IF POR-BULTO
               COMPUTE COSTO-PRECIO-TRN ROUNDED =
                       COSTO-PRECIO-TRN / BULTO-AMP
               MOVE COSTO-PRECIO-TRN TO NUM-EDIT-2
               DISPLAY NUM-EDIT-2 LINE 22 POSITION 15
           ELSE
               DISPLAY "ÄÄÄÄÄÄÄÄÄÄ" LINE 22 POSITION 15 LOW.
270398     IF NOT MEDICAMENTO-AMP AND NOT POR-BULTO,
              IF (COEF-S-PRECIO NOT    < 1 AND
080192            COSTO-PRECIO-TRN NOT = AUXILIAR) OR
                 (COEF-S-PRECIO        < 1 AND
080192            COSTO-PRECIO-TRN NOT = PRECIO-PUBLICO-AMP),
080192            DISPLAY "EL COSTO/PRECIO ES DISTINTO AL CALCULADO !"
030292                     LINE 24 BLINK,
110293            PERFORM CONFIRMACION,
110293            IF NOT SEGURISIMO
                         DISPLAY "ÄÄÄÄÄÄÄÄÄÄ" LINE 22 POSITION 15 LOW
                         GO TO PIDE-COSTO-PRECIO.
271198     IF MEDICAMENTO-AMP
               COMPUTE AUX-DIF ROUNDED =
                 (((PRECIO-PUBLICO-AMP * COEF-PRECIO-REPOSICION)
                 -  COSTO-PRECIO-TRN)
                 /  (PRECIO-PUBLICO-AMP * COEF-PRECIO-REPOSICION)) * 100
               IF AUX-DIF > 1 OR AUX-DIF < -1
                   MOVE AUX-DIF TO ZNUM-PORC-2
                   DISPLAY "El Precio tiene una diferencia de "
                                      LINE 24 POSITION 1 LOW ERASE EOL,
                           ZNUM-PORC-2  POSITION 0,
                           " % ",
                   PERFORM EJECUTA-BEEP 6 TIMES,
                   DISPLAY " " LINE 24 POSITION 45,
                   PERFORM CONFIRMACION,
                   IF NOT SEGURISIMO GO TO PIDE-COSTO-PRECIO.

           IF COSTO-PRECIO-TRN NOT = ZEROES,
           IF PRECIO-PUBLICO-AMP > ZEROES AND
              (COEF-S-PRECIO NOT < 1      AND
               COSTO-PRECIO-TRN  > PRECIO-PUBLICO-AMP),
               DISPLAY "EL COSTO NO PUEDE SER MAYOR QUE EL PRECIO AL PUB
      -                "LICO !" LINE 24 BLINK BEEP ERASE EOL,
110293         PERFORM CONFIRMACION,
110293         IF NOT SEGURISIMO
                         DISPLAY "ÄÄÄÄÄÄÄÄÄÄ" LINE 22 POSITION 15 LOW
                         GO TO P-COSTO-PRECIO ELSE
110293                   NEXT SENTENCE        ELSE
           IF COSTO-ULTIMO-AMP  > ZEROES AND
              (COEF-S-PRECIO    < 1      AND
               COSTO-PRECIO-TRN < COSTO-ULTIMO-AMP),
               DISPLAY "EL PRECIO AL PUBLICO NO PUEDE SER MENOR QUE EL U
      -                "LTIMO COSTO !" LINE 24 BLINK BEEP ERASE EOL,
110293         PERFORM CONFIRMACION,
110293         IF NOT SEGURISIMO GO TO P-COSTO-PRECIO.
270797     IF NOT (TRN = "FC" OR TRN = "AF")
              MOVE    "N" TO IVA-RECEPCION,
              DISPLAY "ú" LINE 21 POSITION 25,
              GO TO PIDE-CONTROL.
230594 PIDE-IVA-REPOSICION.
           IF PERFUMERIA-AMP OR ACCESORIO-AMP,
                 MOVE "N" TO IVA-RECEP-ANT.
           DISPLAY "Este Valor Incluye el IVA ? (S|0/N)"
                    LINE 24 ERASE EOL.
           ACCEPT IVA-RECEPCION LINE 21 POSITION 25 PROMPT "x" ECHO;
                  NO BEEP ON EXCEPTION EXC GO TO PIDE-COSTO-PRECIO.
           IF IVA-RECEPCION = " "
               MOVE IVA-RECEP-ANT TO IVA-RECEPCION.
           IF IVA-RECEPCION = "0"
               MOVE "S" TO IVA-RECEPCION.
           IF IVA-RECEPCION = "S" OR IVA-RECEPCION  = "N",
               MOVE IVA-RECEPCION TO IVA-RECEP-ANT,
               DISPLAY IVA-RECEPCION LINE 21 POSITION 25 BLINK,
           ELSE
               GO TO PIDE-IVA-REPOSICION.
       PIDE-CONTROL.
           DISPLAY "Ingrese el Importe Total (Para Control) ..."
                            LINE 24 POSITION  1 LOW BLINK,
                   " ({"            POSITION  0 LOW,
                   "Enter"          POSITION  0 LOW REVERSE,
                   "}:No Controla)" POSITION  0 LOW ERASE EOL.
       P-CONTROL.
           MOVE "213011" TO LIN-POS-SIZ.
           DISPLAY "ÄÄÄÄÄÄÄÄÄÄ" LINE 22 POSITION 30 LOW.
           PERFORM CONTROL-NUMERO.
221092     IF BAND = 10 PERFORM ENTRA-NUEVO-NUMERO,
221092                  GO TO PIDE-CONTROL.
           IF BAND NOT = ZEROES GO TO PIDE-COSTO-PRECIO.
           COMPUTE AUXILIAR ROUNDED = (CANTIDAD-TRN * COSTO-PRECIO-TRN).
           IF NUMERO-DEC = ZEROES MOVE AUXILIAR TO NUMERO-DEC.
           MOVE NUMERO-DEC TO NUM-EDIT-3.
           DISPLAY NUM-EDIT-3 LINE 21 POSITION 30.
           IF AUXILIAR NOT = NUMERO-DEC,
              MOVE AUXILIAR TO NUM-EDIT-3,
              DISPLAY "ERROR !"        LINE    24 BLINK REVERSE,
                      " Importe Real:" POSITION 0 BEEP BLINK,
                      NUM-EDIT-3       POSITION 0 ERASE EOL,
              GO TO P-CONTROL.
           IF ACT-PRECIO = "S",
              IF COEF-S-PRECIO < 1,
                 MOVE COSTO-PRECIO-TRN TO PRECIO-PUBLICO-AMP,
                 PERFORM ACT-PRECIO-FAR-SOCIAL,
                 MOVE FECHA-COMP-ANT   TO FECHA-PRECIO-AMP ELSE
              IF COEF-S-PRECIO > 1,
                 COMPUTE PRECIO-PUBLICO-AMP ROUNDED =
                        (COSTO-PRECIO-TRN * COEF-S-PRECIO),
                 PERFORM ACT-PRECIO-FAR-SOCIAL,
                 MOVE FECHA-COMP-ANT   TO FECHA-PRECIO-AMP.
           IF COEF-S-PRECIO NOT > 1,
              MULTIPLY COEF-S-PRECIO BY COSTO-PRECIO-TRN ROUNDED.
230797 PIDE-DESCUENTO-PRODUCTO.
           DISPLAY "Ingrese el Descuento del PRODUCTO ... "
                    LINE 24 POSITION 1 LOW BLINK ERASE EOL.
           MOVE "215106" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
           IF BAND NOT = ZEROES GO TO PIDE-CONTROL.
           IF NUMERO = 1
              MOVE ZEROES TO DESC-PROD-ANT, NUMERO.
           IF NUMERO = ZEROES,
                   MOVE DESC-PROD-ANT TO NUMERO-DEC.

      *    IF NUMERO-DEC > 50 GO TO PIDE-DESCUENTO-PRODUCTO.
           MOVE NUMERO-DEC TO DESC-PRODUCTO, DESC-PROD-ANT.
           MOVE NUMERO-DEC TO ZNUM-PORC.
           DISPLAY ZNUM-PORC LINE 21 POSITION 51.

       PIDE-VENCIMIENTO.
           DISPLAY "Ingrese el Mes y A¤o del Vencimiento ..."
                                       LINE    24 LOW BLINK,
                   " ({"               POSITION 0 LOW,
                   "Enter"             POSITION 0 LOW REVERSE,
                   "}:No corresponde)" POSITION 0 LOW ERASE EOL.
       P-VENCIMIENTO.
           DISPLAY SPACES LINE 21 POSITION 60 SIZE 5.
           MOVE "216004" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
221092     IF BAND = 10 PERFORM ENTRA-NUEVO-NUMERO,
221092                  GO TO PIDE-VENCIMIENTO.
           IF BAND NOT = ZEROES
kkkkkk         IF NE-NS
kkkkkk            GO TO PIDE-CANTIDAD
               ELSE
                  GO TO PIDE-DESCUENTO-PRODUCTO.
##2000     IF NUMERO   = ZEROES MOVE 1250 TO NUMERO.
           MOVE NUMERO TO FECHA, FECHA-VTO-EDIT.
           IF (MES < 01 OR MES > 12),
               DISPLAY "VENCIMIENTO INCORRECTO !" LINE 24 BLINK BEEP,
               GO TO P-VENCIMIENTO.

           IF FECHA NOT = 1250 AND FECHA NOT = ZEROES

               PERFORM CONTROLAR-VENCIM THRU FIN-CONTROLAR-VENCIM.

           MOVE MES    TO AUX.
           MOVE DIA    TO MES.
           MOVE AUX    TO DIA.
           MOVE FECHA  TO VENCIMIENTO-TRN.
           DISPLAY FECHA-VTO-EDIT LINE 21 POSITION 60.
060697 PIDE-PARTIDA.
           DISPLAY "Ingrese el Numero de la Partida ..."
                                       LINE    24 LOW BLINK,
                   " ({"               POSITION 0 LOW,
                   "Enter"             POSITION 0 LOW REVERSE,
                   "}:No Corresponde)" POSITION 0 LOW ERASE EOL.
       P-PARTIDA.
130697     ACCEPT PARTIDA LINE 21 POSITION 68 PROMPT "þ" NO BEEP ECHO;
                  ON EXCEPTION EXC GO TO PIDE-VENCIMIENTO.
           PERFORM DISP-SPACES.
      ********** Partida cero y es "S" o "E" o "+1" ....
KKKKKK     IF PARTIDA NOT > SPACES
211092        IF ( TRN NOT < "S0" AND TRN NOT > "S9") OR
081093             (TRN NOT < "-1" AND TRN NOT > "-5") OR
220694             (TRN     = "E8") OR NE-NS
                   MOVE "        " TO PARTIDA ELSE
                   MOVE "Sin Part" TO PARTIDA.
261193     IF NOT (TRN = "FC" AND (PERFUMERIA-AMP OR ACCESORIO-AMP)),
                   GO TO PIDE-DATOS-CORRECTOS.
261193 PIDE-DESCUENTO.
250194     DISPLAY "Ingrese el Porcentaje de Descuento Inclu¡do ..."
                                          LINE 24 LOW BLINK ERASE EOL,
                   "Dto.:ùùù,ùù%ÄÄ"     LINE 22 POSITION 27 LOW,
090501             "Nac/Imp(I): ù " LINE 22 POSITION 43 LOW,
                   "Des.Glob :ùùù,ùùú% " LINE 22 POSITION 60 LOW.
250194     MOVE "223206" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
           IF BAND NOT = ZEROES,
130697        DISPLAY ALL "Ä" LINE 22 POSITION 27 SIZE 52 LOW,
060697        GO TO PIDE-PARTIDA.
180897     IF NUMERO = 1
180897        MOVE ZEROES TO NUMERO, DESC-GLOB-ANT.
180897     IF NUMERO = ZEROES,
180897        MOVE DESC-GLOB-ANT TO NUMERO-DEC.
      *    IF   NUMERO-DEC > 50 GO TO PIDE-DESCUENTO.
210599     MOVE NUMERO-DEC TO DESC-GLOB-ANT, DESC-REC,
                              DESCUENTO-RECEPCION,
                              ZNUM-PORC.
           DISPLAY ZNUM-PORC LINE LIN POSITION POSI.
250797******* Se carga solo el descuento de pronto pago para que
      ******* aparezca en el listado. Pero en DESCUENTO-RECEPCION
      ******* guardo los dos porcentajes juntos.
      ******* Formula: 100 -
      *******          {(100 - DESC-PRODUCTO) *
      *******          ((100 - DESCUENTO-RECEPCION)/100)}
010399     COMPUTE DESCUENTO-RECEPCION ROUNDED =  100 -
                         (( 100 - DESC-PRODUCTO ) *
                         (( 100 - DESCUENTO-RECEPCION ) / 100 )).
           MOVE DESCUENTO-RECEPCION TO ZNUM-PORC.
           DISPLAY ZNUM-PORC LINE 22 POSITION 70.
210599     MOVE DESCUENTO-RECEPCION TO DESC-REC.
      ***********************************************************************
090501**** A pedido de Reggiani se modifica para incorporar la marca de
      **** Nacional o Importado de Aduana para grabar en el AMP. Se elimina
      **** el tratamiento de Imp.Int. y se deja la l¢gica asumiendo que se
      **** tipeo un enter y el numero queda en cero.
      ***********************************************************************
250194 PIDE-IMP-INT.
           MOVE SPACES TO NAC-IMP.
090501     DISPLAY "Ingrese la marca de Nacional (Enter/N) o Importado (
      -            "I)    ..." LINE 24 LOW BLINK ERASE EOL.
           DISPLAY ADUANA-AMP LINE 22 POSITION 55.
           MOVE "225501" TO LIN-POS-SIZ.
           
090501     ACCEPT NAC-IMP  LINE 22 POSITION 55  NO BEEP ECHO;
                  ON EXCEPTION EXC GO TO PIDE-DESCUENTO.
           PERFORM DISP-SPACES.
           IF NAC-IMP NOT = "I" AND NAC-IMP NOT = "N"
              GO TO PIDE-IMP-INT.

      *    PERFORM CONTROL-NUMERO.
      *    IF BAND NOT = ZEROES GO TO PIDE-DESCUENTO.
090501     MOVE ZEROES TO NUMERO.
           IF   NUMERO-DEC = ZEROES,
@@@@@@          IF INT-AMP = 1 MOVE 7,53 TO NUMERO-DEC ELSE
100394*         DISPLAY "+%" LINE 22 POSITION 57 HIGH  ELSE
@@@@@@          IF INT-AMP = 2 MOVE 7,00 TO NUMERO-DEC.
100394*         DISPLAY "-%" LINE 22 POSITION 57 BLINK ELSE
100394*         DISPLAY "+%" LINE 22 POSITION 57 LOW.
      *    IF   NUMERO-DEC > 50 GO TO PIDE-IMP-INT.
           MOVE NUMERO-DEC TO IMP-INT-RECEPCION, ZNUM-PORC.
      *    DISPLAY ZNUM-PORC LINE LIN POSITION POS.
      *
           IF NAC-IMP = SPACES    MOVE "N" TO NAC-IMP.
           IF ADUANA-AMP = NAC-IMP GO TO PIDE-DATOS-CORRECTOS.
           MOVE NAC-IMP TO ADUANA-AMP.
       ACTUALIZO-NAC-IMP.
           REWRITE REG-AMP.
           IF STATUS-AMP = "99" GO TO ACTUALIZO-NAC-IMP.
       LEE-AMP-NAC-IMP.
           READ AMP RECORD INVALID KEY GO TO PIDE-CODIGO.
           IF STATUS-AMP = "99"        GO TO LEE-AMP-NAC-IMP.
      ******************* Fin de la modificaci¢n del 09/05/01 **************
       PIDE-DATOS-CORRECTOS.
           DISPLAY "Datos Correctos ?   (" LINE    24 LOW,
                   "S"                     POSITION 0 HIGH,
                   "|"                     POSITION 0 LOW BLINK,
                   "0"                     POSITION 0 HIGH,
                   "/"                     POSITION 0 LOW,
                   "N"                     POSITION 0 HIGH,
181094             ") þ (No Valoriza esta Imputaci¢n: {"
181094                                     POSITION 0 LOW,
181094             "!"                     POSITION 0 BLINK,
181094             "})"                    POSITION 0 LOW ERASE EOL.
           ACCEPT RESPUESTA LINE 24 POSITION 19 PROMPT "*" BLINK NO BEEP
                  ON EXCEPTION EXC MOVE "N" TO RESPUESTA.
           IF NEGATIVO,
080294        IF (TRN = "FC" AND (PERFUMERIA-AMP OR ACCESORIO-AMP)),
                             GO TO PIDE-IMP-INT ELSE
060697                       GO TO PIDE-PARTIDA.
181094     IF SEGURISIMO,
              MOVE ZEROES TO COSTO-PRECIO-TRN, NUM-EDIT-2, NUM-EDIT-3,
              DISPLAY NUM-EDIT-2 LINE 21 POSITION 31 BLINK,
                      NUM-EDIT-3 LINE 21 POSITION 57 BLINK ELSE
           IF NOT AFIRMATIVO GO TO PIDE-DATOS-CORRECTOS.
           IF TRN = "FC" OR TRN = "AF"
210797         READ FACPRV01 RECORD;
                    INVALID KEY
                    WRITE REG-FACPRV.
            IF TRN = "FC" OR TRN = "AF"
               PERFORM CALCULA-TOTAL-FACTURA THRU
                                     SAL-CALCULA-TOTAL-FACTURA.
           IF TRN NOT = "FC" GO TO SAL-PIDE-PRODUCTO.
           MOVE 5 TO IND.
       CONTROLA-VENCIMIENTO.
           IF VENCIMIENTO-PARTIDA(IND) = ZEROES,
              IF IND = 1 GO TO SAL-PIDE-PRODUCTO ELSE
                         SUBTRACT 1 FROM IND,
                         GO TO CONTROLA-VENCIMIENTO.
##2000     MOVE VENCIMIENTO-PARTIDA(IND) TO ANO-MESC.
##2000     MOVE 01                       TO DIAC.
##2000     PERFORM FECHA-8-DIGITOS THRU FIN-FECHA-8-DIGITOS.
           MOVE RFECHA-COMPLETA          TO FECHA-ANT-8.
##2000     MOVE VENCIMIENTO-TRN          TO ANO-MESC.
##2000     MOVE 01                       TO DIAC.
##2000     PERFORM FECHA-8-DIGITOS THRU FIN-FECHA-8-DIGITOS.
##2000     IF (VENCIMIENTO-PARTIDA(IND) NOT = 5012 AND
##2000         FECHA-ANT-8 > RFECHA-COMPLETA),
      *    IF (VENCIMIENTO-PARTIDA(IND) NOT = 9912 AND
      *        VENCIMIENTO-PARTIDA(IND) > VENCIMIENTO-TRN),
               DISPLAY "EL VENCIMIENTO ES MENOR AL DE LA ULTIMA PARTIDA 
      -                "!" LINE 24 BLINK BEEP ERASE EOL,
110293         PERFORM CONFIRMACION,
060697         IF NOT SEGURISIMO GO TO PIDE-PARTIDA.
       SAL-PIDE-PRODUCTO.
261193     IF (TRN = "FC" AND (PERFUMERIA-AMP OR ACCESORIO-AMP)),
130697         DISPLAY ALL "Ä" LINE 22 POSITION 2 SIZE 77 LOW.
           DISPLAY "Un momento por favor ..." LINE 24 BLINK ERASE EOL.




       ENTRA-NUEVO-NUMERO SECTION 20.
       E-NUEVO-NUMERO.
           DISPLAY "Ingrese el Nuevo Numero del Comprobante ..."
                    LINE 24 LOW BLINK ERASE EOL.
           MOVE "064706" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
           IF BAND NOT = ZEROES,
              DISPLAY NRO-COMP-ANT LINE LIN POSITION POSI REVERSE,
              GO TO SAL-ENTRA-NUEVO-NUMERO.
           IF NUMERO   = ZEROES,
              IF NRO-COMP-ANT NOT > ZEROES GO TO ENTRA-NUEVO-NUMERO ELSE
              MOVE NRO-COMP-ANT TO NUMERO,
              DISPLAY NRO-COMP-ANT LINE 6 POSITION 47 HIGH.
           MOVE NUMERO TO NRO-COMP-ANT, NRO-CPTE-FACPRV.
240797     MOVE ZEROES TO AUXILIAR, INTERMEDIO, INTERMEDIO2,
                          TOTAL-SBT, TOTAL-IVA, TOTAL-DES.
           READ FACPRV01
                INVALID KEY
                MOVE LOW-VALUES TO DATOS-FACPRV.
240797     PERFORM CALCULA-TOTAL-FACTURA THRU SAL-CALCULA-TOTAL-FACTURA.

       SAL-ENTRA-NUEVO-NUMERO.

030994 ACT-PRECIO-FAR-SOCIAL SECTION 20.
       ACT-PRECIO-FAR-SOC.
           COMPUTE PRECIO-FAR-SOC-AMP ROUNDED = (PRECIO-PUBLICO-AMP).
       SAL-ACT-PRECIO-FAR-SOC.

       DETERMINA-EXISTENCIAS SECTION 30.
       DET-EXIST.
           IF ORIGEN-PARTIDA        (IND-P) = "R",
              ADD EXISTENCIA-PARTIDA(IND-P) TO EXIST-REMITO ELSE
              ADD EXISTENCIA-PARTIDA(IND-P) TO EXIST-FACTURA.
       SAL-DETERMINA-EXISTENCIAS.

       DISP-MENSAJE-ERROR SECTION 30.
       DISP-MEN-ERROR.
           IF INTERMEDIO < ZEROES,
              MOVE ZEROES TO INTERMEDIO,
              DISPLAY MENSAJE-ERROR LINE 24 HIGH,
              PERFORM CONFIRMA,
              IF NOT AFIRMATIVO,
                 DISPLAY " Transacci¢n Ignorada !"
                          LINE 24 BLINK BEEP,
                 GO TO DETERMINA-RUTINA ELSE
011194        DISPLAY "Clave:" LINE 24 REVERSE ERASE EOL,
              ACCEPT NUMERO POSITION 0 OFF NO BEEP,
              PERFORM DISP-SPACES,
211294********ACCEPT FECHA FROM DATE, COMPUTE AUXILIAR1 = (ANO * FECHA),
211294********IF NUMERO NOT = AUXILIAR1,
211294        IF NUMERO NOT = 123456,
                 MOVE -1 TO INTERMEDIO,
                 GO TO DISP-MEN-ERROR.
       SAL-DISP-MENSAJE-ERROR.

       ARMA-TRNAMP SECTION 30.
180797 ACTUALIZA-FACPRV.
*********** Actualizo el Archivo FACPRV01 con los Importes *********
           IF TRN = "FC" OR TRN = "AF",
               IF TRN-ANT NOT = "RF",
                   PERFORM GRABO-FACPRV THRU SAL-GRABO-FACPRV.
060697 ACTUALIZA-AMP-P.
060697     IF PARTIDA NOT > SPACES   GO TO A-TRNAMP.
           READ AMP-P RECORD;
                INVALID KEY
                MOVE LOW-VALUES      TO REG-AMP-P.
           IF STATUS-AMP-P = "99"    GO TO ACTUALIZA-AMP-P.
KKKKKK     PERFORM LEE-FCISPR THRU FIN-LEE-FCISPR.
           MOVE TRN-ANT              TO CODIGO-TRN.
           IF TRN-NEGATIVO           GO TO RESTA-PARTIDA.
       SUMA-PARTIDA.
           MOVE 1                    TO IND.
       C-SUMA-PARTIDA.
##2000     MOVE FEC-PART-AMP-P(IND)  TO FECHA-8.
           PERFORM FECHA-8-DIGITOS THRU FIN-FECHA-8-DIGITOS.
           IF ((COD-PROV-AMP-P(IND) = COD-PROV-DIST  AND
                (FEC-PART-AMP-P(IND) = FECHA-COMP-ANT OR
##2000          RFECHA-COMPLETA      = FEC-CPTE-FACPRV) AND
                NRO-COMP-AMP-P(IND) = NRO-COMP-ANT   AND
                NRO-PART-AMP-P(IND) = PARTIDA)       OR
               (ITEM-AMP-P    (IND) = LOW-VALUES)),
                GO TO AGREGA-PARTIDA.
           IF IND NOT = 20, ADD 1 TO IND,  GO TO C-SUMA-PARTIDA.
           MOVE 1                    TO EXC.
           MOVE 2                    TO IND.
       C-DESPLAZA-PARTIDA.
           MOVE ITEM-AMP-P(IND)      TO ITEM-AMP-P(EXC).
KKKKKK     MOVE ITEM-FCISPR(IND)     TO ITEM-FCISPR(EXC).
           IF IND NOT = 20,
              ADD  1                 TO IND,
              ADD  1                 TO EXC,
              GO TO C-DESPLAZA-PARTIDA.
           MOVE LOW-VALUES           TO ITEM-AMP-P(IND).
KKKKKK     MOVE LOW-VALUES           TO ITEM-FCISPR(IND).
       AGREGA-PARTIDA.
           MOVE COD-PROV-DIST        TO COD-PROV-AMP-P(IND).
##2000     MOVE FEC-CPTE-FACPRV      TO RFECHA-COMPLETA.
##2000     MOVE FECHA-8              TO FEC-PART-AMP-P(IND).
           MOVE NRO-COMP-ANT         TO NRO-COMP-AMP-P(IND).
           MOVE PARTIDA              TO NRO-PART-AMP-P(IND).
           ADD  CANTIDAD-TRN         TO CNT-PART-AMP-P(IND).
**********************************************************
           MOVE COSTO-PRECIO-TRN     TO COSTO-FCISPR(IND).
           MOVE IVA-RECEP-ANT        TO CONIVA-FCISPR(IND).
           MOVE DESC-PRODUCTO        TO DESC-FCISPR(IND).
*************************************************************
           GO TO REGRABA-AMP-P.
       RESTA-PARTIDA.
           MOVE 1                    TO IND.
       C-BUSCA-PARTIDA.
##2000     MOVE FEC-PART-AMP-P(IND)  TO FECHA-8.
           PERFORM FECHA-8-DIGITOS THRU FIN-FECHA-8-DIGITOS.
           
           IF (COD-PROV-AMP-P(IND) = COD-PROV-DIST  AND
##2000         (RFECHA-COMPLETA      = FEC-CPTE-FACPRV OR
               FEC-PART-AMP-P(IND) = FECHA-COMP-ANT) AND
               NRO-COMP-AMP-P(IND) = NRO-COMP-ANT   AND
               NRO-PART-AMP-P(IND) = PARTIDA) NEXT SENTENCE         ELSE
               IF IND NOT = 20, ADD 1 TO IND, GO TO C-BUSCA-PARTIDA ELSE
               MOVE " " TO RESPUESTA,
               DISPLAY "NO EXISTE UNA PARTIDA EN {AMP-P} CON ESOS DATOS
      -                "! " LINE 24 BLINK ERASE EOL,
               ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW,
               PERFORM DISP-SPACES
               IF RESPUESTA NOT = "!"
                   GO TO DETERMINA-RUTINA.
           IF CANTIDAD-TRN < CNT-PART-AMP-P(IND),
              SUBTRACT CANTIDAD-TRN FROM CNT-PART-AMP-P(IND),
              GO TO REGRABA-AMP-P.
           MOVE LOW-VALUES TO ITEM-AMP-P(IND).
KKKKKK     MOVE LOW-VALUES TO ITEM-FCISPR(IND).
           MOVE 1 TO IND. MOVE 2 TO EXC.
       ALINEA-REG-AMP-P.
           IF ITEM-AMP-P(IND) = LOW-VALUES,
              MOVE ITEM-AMP-P(EXC)  TO ITEM-AMP-P(IND),
KKKKKK        MOVE ITEM-FCISPR(EXC) TO ITEM-FCISPR(IND),
              MOVE LOW-VALUES       TO ITEM-AMP-P(EXC),
KKKKKK        MOVE LOW-VALUES       TO ITEM-FCISPR(EXC),
              IF EXC NOT = 20,
                 ADD  1 TO EXC,
                 GO TO ALINEA-REG-AMP-P.
           IF IND NOT = 19,
              ADD  1 TO IND, COMPUTE EXC = (IND + 1),
              GO TO ALINEA-REG-AMP-P.
060697 REGRABA-AMP-P.
           IF REG-AMP-P = LOW-VALUES,
              DELETE AMP-P  RECORD INVALID KEY GO TO REGRABA-FCISPR,
           ELSE
              REWRITE REG-AMP-P   INVALID KEY WRITE REG-AMP-P.
           PERFORM VIS-PARTIDA THRU SAL-VIS-PARTIDA.
       REGRABA-FCISPR.
      *----Es el stock de CONSIGNACION
280203     IF SUCURSAL-ANT = 04 GO TO A-TRNAMP.


           IF REG-FCISPR = LOW-VALUES,
              DELETE FCISPR  RECORD INVALID KEY GO TO A-TRNAMP,
           ELSE
              REWRITE REG-FCISPR INVALID KEY WRITE REG-FCISPR.
       A-TRNAMP.
           MOVE SUCURSAL-ANT    TO SUCURSAL-TRN.
           MOVE TRN-ANT         TO CODIGO-TRN.
           IF MEDICAMENTO-AMP MOVE 1 TO CATEGORIA-TRN ELSE
           IF ACCESORIO-AMP   MOVE 2 TO CATEGORIA-TRN ELSE
                              MOVE 3 TO CATEGORIA-TRN.
           MOVE NRO-COMP-ANT    TO NUMERO-TRN.
220793     MOVE TROQUEL-AMP     TO TROQUEL-TRN.
           MOVE FECHA-COMP-ANT  TO FECHA-TRN.
           MOVE DESCRIPCION-AMP TO DESC-PROD-TRN.
170693     IF (TRN-SIN-COSTO OR (NE-NS AND NOT ESMMX)),
               MULTIPLY COEF-PRECIO-REPOSICION BY
               PRECIO-PUBLICO-AMP GIVING COSTO-PRECIO-TRN ROUNDED.
           MOVE ARTEFACTO       TO ARTEFACTO-TRN.
           MOVE OPERADORA       TO OPERADORA-TRN.
           ACCEPT NUMERO      FROM TIME.
           MOVE HORA-MINUTO     TO HORA-MINU-TRN.
       GRABA-REG-TRNAMP.
           ADD  1               TO CONTADOR-TRN.
           MOVE CONTADOR-TRN    TO REGISTRO-TRN.
           WRITE REG-TRNAMP;
                 INVALID KEY
                 GO TO GRABA-REG-TRNAMP.
           COMPUTE COSTO-PONDERADO-AMP ROUNDED =
                   ((EXIST-FACTURA * COSTO-PONDERADO-AMP) +
                    (CANTIDAD-TRN  * COSTO-PRECIO-TRN))   /
                    (CANTIDAD-TRN  + EXIST-FACTURA).
       REGRABA-AMP.
           MOVE FECHA-DIA            TO FECHA-ULT-ACTUALIZ.
KKKKKK     IF FALTA-PROV-TMP-AMP = "P"
KKKKKK          MOVE LOW-VALUES TO FALTA-PROV-TMP-AMP.
           REWRITE REG-AMP.
           IF STATUS-AMP  = "99"     GO TO REGRABA-AMP.

280203     IF SUCURSAL-ANT = 04
               REWRITE REG-AMPIO
                       INVALID KEY WRITE REG-AMPIO.

240693 REGRABA-PEDIDO.

           IF (ACCESO = "D"),
170599         PERFORM RELEE-PEDIDO THRU FIN-RELEE-PEDIDO.
           IF (ACCESO = "D"),
260600         PERFORM GRABO-CANTIDAD THRU FIN-GRABO-CANTIDAD,
030293         MOVE COD-PROV-DIST    TO COD-DIST-PEDIDO,
##2000         MOVE FECHA-COMP-ANT   TO FECHA-8,
##2000         PERFORM FECHA-8-DIGITOS THRU FIN-FECHA-8-DIGITOS,
##2000         MOVE RFECHA-COMPLETA  TO FECHA-RECEPCION,
               MOVE TRN-ANT          TO TIPO-RECEPCION,
               MOVE NRO-COMP-ANT     TO NRO-RECEPCION,
               MOVE COSTO-PRECIO-TRN TO COSTO-RECEPCION,
210599         MOVE DESC-REC         TO DESCUENTO-RECEPCION,
010793         IF (TRN-ANT = "FC"),
260600*............PERFORM GRABO-CANTIDAD THRU FIN-GRABO-CANTIDAD,
260600*............ADD  CANTIDAD-TRN TO CANT-RECEPCION,
                   REWRITE REG-PEDIDO    ELSE
010793         IF (TRN-ANT = "AF"),

                   IF CANTIDAD-TRN > CANT-RECEPCION,
                      MOVE ZEROES    TO CANT-RECEPCION,
                      REWRITE REG-PEDIDO ELSE
                      SUBTRACT CANTIDAD-TRN FROM CANT-RECEPCION,
                      REWRITE REG-PEDIDO.
270594 ACTUALIZA-TRN-ESP.
120794     IF NOT TRNVESP GO TO ACTUALIZA-DIF-PROV.
270594 ABRE-TRN-ESP.
           OPEN I-O TRN-ESP.
           IF STATUS-TRN-ESP = "98",
              IF RESPUESTA = "!" OPEN OUTPUT TRN-ESP,
                                 CLOSE       TRN-ESP,
                                 GO TO ABRE-TRN-ESP ELSE
                                 GO TO ABRE-TRN-ESP.
       ACTUAL-TRN-ESP.
           MOVE LOW-VALUES           TO REG-TRN-ESP.
##2000     MOVE FECHA-COMP-ANT       TO FECHA-8.
##2000     PERFORM FECHA-8-DIGITOS THRU FIN-FECHA-8-DIGITOS.
##2000     MOVE RFECHA-COMPLETA      TO FECHA-TRN-ESP.
           MOVE RAZON-SOCIAL         TO NOMBRE-TRN-ESP.
           MOVE COD-PROV-DIST        TO CODIGO-TRN-ESP.
           MOVE NRO-COMP-ANT         TO NUMERO-TRN-ESP.
           MOVE TRN-ANT              TO COD-TRN-TRN-ESP.
           READ TRN-ESP RECORD;
                INVALID KEY          GO TO ACT-TRN-ESP.
           IF STATUS-TRN-ESP = "99"  GO TO ACTUAL-TRN-ESP.
       ACT-TRN-ESP.
           COMPUTE AUXILIAR ROUNDED = (CANTIDAD-TRN * COSTO-PRECIO-TRN).
           IF (TRN-ANT = "S4" OR TRN-ANT = "-1"),
               MULTIPLY -1 BY AUXILIAR.
           ADD  AUXILIAR             TO IMPORTE-TRN-ESP(CATEGORIA-TRN).
           REWRITE REG-TRN-ESP INVALID KEY WRITE REG-TRN-ESP.
           CLOSE TRN-ESP.
221092 ACTUALIZA-DIF-PROV.
           IF (TRN-ANT NOT = "FC" AND TRN-ANT NOT = "AF"),
               GO TO SAL-ARMA-TRNAMP.
       ACTUAL-DIF-PROV.
           MOVE LOW-VALUES           TO REG-DIF-PROV.
281092     MOVE COD-PROV-DIST        TO COD-PROV-DIF.
           MOVE NRO-COMP-ANT         TO NRO-COMP-DIF.
           READ DIF-PROV RECORD;
                INVALID KEY          GO TO ACT-DIF-PROV.
           IF STATUS-DIF-PROV = "99" GO TO ACTUAL-DIF-PROV.
       ACT-DIF-PROV.
111094     IF FEC-PROV-DIF = ZEROES,
##2000         MOVE FECHA-COMP-ANT    TO  FECHA-8,
               PERFORM FECHA-8-DIGITOS THRU FIN-FECHA-8-DIGITOS,
##2000         MOVE RFECHA-COMPLETA TO FEC-PROV-DIF.

090395     IF FECHA-DIF(1) = ZEROES,
##2000        MOVE FECHA-DIA         TO  FECHA-8,
              PERFORM FECHA-8-DIGITOS THRU FIN-FECHA-8-DIGITOS,
              MOVE RFECHA-COMPLETA    TO  FECHA-DIF(1).
230693     MOVE RAZON-SOCIAL-D       TO NOM-PROV-DIF.
140793     COMPUTE AUXILIAR ROUNDED = (CANTIDAD-TRN * COSTO-PRECIO-TRN).
           IF TRN-ANT = "AF" MULTIPLY -1 BY AUXILIAR.
221093     ADD  AUXILIAR             TO IMP-CAT-DIF(1, CATEGORIA-TRN).
           ADD  AUXILIAR             TO IMPORTE-DIF(1).
           IF (IMPORTE-DIF(1) = ZEROES AND IMPORTE-DIF(2) = ZEROES),
               GO TO ELIMINA-DIF-PROV.
           REWRITE REG-DIF-PROV INVALID KEY WRITE REG-DIF-PROV.
           GO TO SAL-ARMA-TRNAMP.
       ELIMINA-DIF-PROV.
           DELETE DIF-PROV RECORD INVALID KEY GO TO SAL-ARMA-TRNAMP.
       SAL-ARMA-TRNAMP.


170599 RELEE-PEDIDO.
           MOVE CLAVE-PEDIDO-AUX  TO CLAVE-NUMERO-PEDIDO.
           READ PEDIDO RECORD
                       INVALID KEY GO TO FIN-RELEE-PEDIDO.
           IF STATUS-PEDIDO = "99" GO TO RELEE-PEDIDO.
       FIN-RELEE-PEDIDO.


260600 GRABO-CANTIDAD.
           IF TRN = "AF" GO TO FIN-GRABO-CANTIDAD.

           IF NRO-COMP-ANT = NRO-RECEPCION
               ADD  CANTIDAD-TRN        TO CANT-RECEPCION
           ELSE
               IF CANT-RECEPCION > CANTIDAD-PEDIDO
                   MOVE ZEROES       TO CANTIDAD-PEDIDO,
                   MOVE CANTIDAD-TRN TO CANT-RECEPCION,
               ELSE
                    COMPUTE CANTIDAD-PEDIDO = CANTIDAD-PEDIDO -
                                             CANT-RECEPCION
                   MOVE CANTIDAD-TRN        TO CANT-RECEPCION.
       FIN-GRABO-CANTIDAD.

       BUSQUEDA-LUGAR SECTION 30.
       INICIA-BUSQ-LUGAR.
           MOVE 1                     TO IND-P.
       CICLO-BUSQ-LUGAR.
           IF FECHA-PARTIDA(IND-P) = ZEROES GO TO SAL-BUSQUEDA-LUGAR.
           ADD  1                     TO IND-P.
           IF IND-P NOT > 5                 GO TO CICLO-BUSQ-LUGAR.
       CALCULA-PONDERADOS.
           MOVE 5                     TO IND-P.
           MOVE "?"                   TO ORIGEN-PARTIDA     (2).
250794*****MOVE 9999                  TO COD-PROV-PARTIDA   (2).
##2000     MOVE 5012                  TO VENCIMIENTO-PARTIDA(2)
           ADD  EXISTENCIA-PARTIDA(1) TO EXISTENCIA-PARTIDA (2).
           ADD  COMPRA-PARTIDA    (1) TO COMPRA-PARTIDA     (2).
           MOVE PARTIDA-AMP       (2) TO PARTIDA-AMP        (1).
           MOVE PARTIDA-AMP       (3) TO PARTIDA-AMP        (2).
           MOVE PARTIDA-AMP       (4) TO PARTIDA-AMP        (3).
           MOVE PARTIDA-AMP       (5) TO PARTIDA-AMP        (4).
           MOVE LOW-VALUES            TO PARTIDA-AMP        (5).
       SAL-BUSQUEDA-LUGAR.

      /    ***  S E G M E N T O S   I N D E P E N D I E N T E S  ***
      *         """""""""""""""""""""""""""""""""""""""""""""""

       SECCION-50 SECTION 50.
       PREPARA-PARAMETROS.
######     DISPLAY "Versi¢n Programa: 27/10/99" LINE 25 POSITION 28 LOW.
130995 CONTROLA-HORA.
           ACCEPT FECHA     FROM DATE.
##2000     PERFORM FECHA-8-DIGITOS THRU FIN-FECHA-8-DIGITOS.
           ACCEPT NUMERO    FROM TIME.
           IF HORA-MINUTO < 0600,
              PERFORM DETERMINA-NUMERAL,
              SUBTRACT 1    FROM NUMERAL,
              PERFORM DETERMINA-FECHA.
           MOVE FECHA TO FECHA-DIA, FECHA-COMP-ANT. PERFORM RUT-FECHA.
           MOVE FECHA TO TFEC-1, TFEC42.
           IF HORA-MINUTO < 0600,
              DISPLAY "ATENCION:"         LINE    24 BLINK REVERSE,
                      " Dada la Hora de la Maquina, Tomo la Fecha:"
                                          POSITION 0,
                      TFEC-1              POSITION 0 REVERSE,
                      " para Trabajar ! " POSITION 0 ERASE EOL,
              ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW BLINK;
                     ON EXCEPTION EXC GO TO EXIT-PROGRAM.
       ABRE-ARCHIVOS.
190892     OPEN I-O TRNAMP.
190892     IF STATUS-TRNAMP NOT = "00",
190892        DISPLAY "IMPOSIBLE CONTINUAR !"
190892                 LINE    24 BLINK REVERSE,
190892                " Se est n Emitiendo los Listados Finales ..."
190892                 POSITION 0 ERASE EOL,
190892        ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW BLINK,
190892        GO TO EXIT-PROGRAM.
261193 ABRE-PEDIDO.
           OPEN I-O PEDIDO.
           IF STATUS-PEDIDO = "98",
              IF RESPUESTA = "!" OPEN OUTPUT PEDIDO,
                                 CLOSE       PEDIDO,
                                 GO TO ABRE-PEDIDO   ELSE
                                 GO TO ABRE-PEDIDO.
291093 ABRE-DIF-PROV.
           OPEN I-O DIF-PROV.
           IF STATUS-DIF-PROV = "98",
              IF RESPUESTA = "!" OPEN OUTPUT DIF-PROV,
                                 CLOSE       DIF-PROV,
                                 GO TO ABRE-DIF-PROV ELSE
                                 GO TO ABRE-DIF-PROV.
270594 ABRE-VARIOS.
280203     OPEN I-O   AMP, AMP-P, FACPRV01, FCISPR, AMPIO
                INPUT AID, AIL, VENCIM.
       DIAGRAMA-PANTALLA.
           DISPLAY SPACE ERASE.
           CALL ITEM-OBJETO2 USING "101011022".
           DISPLAY "{MOV-AMP}-¯Operaciones"  POSITION  2 LOW REVERSE,
                                             LINE      1,
                   ALL "~"                   POSITION  2 LOW SIZE 22,
                   "["                       POSITION  2 LOW,
                   "TR"                      POSITION  0 HIGH,
                   "]¯ Transf.Productos"     POSITION  0 LOW,
                   "["                       POSITION  2 LOW,
211092             "S#"                      POSITION  0 HIGH,
                   "]¯ Salida (#:"           POSITION  0 LOW,
                   "0"                       POSITION  0 HIGH,
                   " a "                     POSITION  0 LOW,
                   "9"                       POSITION  0 HIGH,
                   ")"                       POSITION  0 LOW,
                   "["                       POSITION  2 LOW,
                   "E#"                      POSITION  0 HIGH,
211092             "]¯ Entrada(#:"           POSITION  0 LOW,
                   "0"                       POSITION  0 HIGH,
                   " a "                     POSITION  0 LOW,
                   "9"                       POSITION  0 HIGH,
                   ")"                       POSITION  0 LOW,
100893             "["                       POSITION  2 LOW,
                   "ñ#"                      POSITION  0 HIGH,
                   "]¯ Ent/Sal(#:"           POSITION  0 LOW,
                   "1"                       POSITION  0 HIGH,
                   " a "                     POSITION  0 LOW,
                   "5"                       POSITION  0 HIGH,
                   ")"                       POSITION  0 LOW,
                   "["                       POSITION  2 LOW,
                   "FC"                      POSITION  0 HIGH,
                   "]¯ Factura Proveed."     POSITION  0 LOW,
                   "["                       POSITION  2 LOW,
                   "RM"                      POSITION  0 HIGH,
                   "]¯ Remito Proveedor"     POSITION  0 LOW,
                   "["                       POSITION  2 LOW,
                   "AF"                      POSITION  0 HIGH,
                   "]¯ Anulaci¢n Factur"     POSITION  0 LOW,
                   "["                       POSITION  2 LOW,
                   "AR"                      POSITION  0 HIGH,
                   "]¯ Anulaci¢n Remito"     POSITION  0 LOW,
                   "["                       POSITION  2 LOW,
                   "FI"                      POSITION  0 HIGH,
                   "]¯ Fin del Programa"     POSITION  0 LOW.
           CALL ITEM-OBJETO2 USING "202250929".
           DISPLAY "Operaci¢n:"       LINE 1 POSITION 34 LOW REVERSE,
                   "þ...þ"                   POSITION 38 LOW,
                   "Fecha  de Carga    :"    POSITION 26 LOW,
                   "Codigo de Proveedor:"    POSITION 26 LOW,
                   "Fecha   Comprobante:"    POSITION 26 LOW,
                   "Numero  Comprobante:"    POSITION 26 LOW,
                   "ÇÄÄ(Acceso  Producto:ùùùùùù)ÄÄ¶"
                                             POSITION 25 LOW,
060697             "Ñ"               LINE 12 POSITION 30 LOW,
060697             "Ñ"               LINE 12 POSITION 50 LOW,
060697             "ÜÜÜÜÜÛ"          LINE 13 POSITION 25 LOW,
060697             "úUltimas  Partidasú"     POSITION 00 LOW REVERSE,
060697             "ÛÜÜÜÜÜ"                  POSITION 00 LOW,
130697             "þÕúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúÑúúúúúúúúúúúúúú
130697-            "úúúúúúúúúúúúúúúúúúúúú¸þ" POSITION 04 LOW,
130697             "þÆúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúØúúúúúúúúúúúúúú
130697-            "úúúúúúúúúúúúúúúúúúúúúµþ" POSITION 04 LOW,
130697             "þÔúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúúÏúúúúúúúúúúúúúú
130697-            "úúúúúúúúúúúúúúúúúúúúú¾þ" POSITION 04 LOW,
                   "Linea/                                þþþúExistencia
      -            "súþþ   þþúI m p o r t e súþþ",
060697                               LINE 17 POSITION 01 LOW REVERSE,
                   "Troquel  Descripci¢n del Producto     p/Remito p/Fac
      -            "tura   Ult.Costo      Precio",
                                     LINE 18 POSITION 01 LOW REVERSE.
230797     CALL ITEM-OBJETO2 USING "101560723".
           DISPLAY "ÄÄÄÄD i v i s i ¢ nÄÄÄÄ"
                                     LINE  1 POSITION 57 LOW REVERSE,
                   ALL "~"           SIZE 23 POSITION 57 LOW.
           MOVE 3 TO LIN.
           PERFORM VIS-SUCURSAL VARYING IND-V FROM 1 BY 1
                                UNTIL   IND-V > 6.
           CALL   ITEM-OBJETO2 USING "120010178".
270594     CANCEL ITEM-OBJETO2.
           DISPLAY " Cantidad "      LINE 20 POSITION  2 LOW REVERSE,
                   "Importe Total"   LINE 20 POSITION 30 LOW REVERSE,
230797             "Descuento"       LINE 20 POSITION 48 LOW REVERSE,
                   "Vto:"            LINE 20 POSITION 60 LOW REVERSE,
130697             "Nro.Partida "    LINE 20 POSITION 66 LOW REVERSE,
                   "Subtot: "        LINE 23 POSITION  1 LOW REVERSE,
                   "Dto: "           LINE 23 POSITION 22 LOW REVERSE,
                   "IVA: "           LINE 23 POSITION 40 LOW REVERSE,
                   "TOTAL "          LINE 23 POSITION 58 LOW REVERSE.
       PIDE-TRN.
           DISPLAY "Ingrese la Operaci¢n que desea efectuar ..."
                             LINE 24 LOW BLINK ERASE EOL,
                   " Costo " LINE 20 POSITION 18 LOW REVERSE.
       P-TRN.
           MOVE "N" TO ACCESO, IND-SIG.
           IF PERMISO-LINK = "S",
              CLOSE LO WITH NO REWIND,
              MOVE "C" TO PERMISO-LINK,
              PERFORM CONTROLA-IMPRESORA.
           ACCEPT TRN LINE 1 POSITION 45 PROMPT "x" ECHO NO BEEP;
                  ON EXCEPTION EXC MOVE "FI" TO TRN.
           PERFORM DISP-SPACES.
290993     IF TRN = "FI" GO TO FIN-PROGRAMA.
           DISPLAY SPACES LINE 3 POSITION 50 SIZE 02,
                   SPACES LINE 8 POSITION 26 SIZE 29,
                   SPACES        POSITION 26 SIZE 29,
                   SPACES        POSITION 26 SIZE 29,
                   SPACES        POSITION 26 SIZE 29.
           IF TRN = SPACES MOVE TRN-ANT TO TRN.
           MOVE TRN TO TRN-ANT.
           DISPLAY TRN       LINE  1 POSITION 45,
                   " Costo " LINE 20 POSITION 18 LOW REVERSE.


      *-------------------
      
040708     IF (TRN NOT < "E0" AND TRN NOT > "E9") OR
040708        (TRN NOT < "+1" AND TRN NOT > "+5")
040708        DISPLAY "clave : " LINE 24 POSITION 65 
040708        ACCEPT WK-CLAVE PROMPT "*" OFF NO BEEP  
040708               POSITION 0 ERASE EOL
040708               ON EXCEPTION EXC MOVE SPACES TO WK-CLAVE.
040708     
040708     IF ( (TRN NOT < "E0" AND TRN NOT > "E9") OR
040708          (TRN NOT < "+1" AND TRN NOT > "+5") ) AND
040708         WK-CLAVE NOT = "ArrI"  GO TO FIN-PROGRAMA.
      
      
      *-------------------



060905     IF (TRN NOT < "S0" AND TRN NOT > "S9") OR
060905        (TRN NOT < "-1" AND TRN NOT > "-5")
060905        DISPLAY "clave : " LINE 24 POSITION 65 
              ACCEPT WK-CLAVE PROMPT "*" OFF NO BEEP  
                     POSITION 0 ERASE EOL
                     ON EXCEPTION EXC MOVE SPACES TO WK-CLAVE.
           
060905     IF ((TRN NOT < "S0" AND TRN NOT > "S9")  OR
060905         (TRN NOT < "-1" AND TRN NOT > "-5")) AND
                TRN NOT = "S8" AND 
               WK-CLAVE NOT = "SWAT"  GO TO FIN-PROGRAMA.


060905     IF  TRN = "S8" AND
               WK-CLAVE NOT = "BBJJ"  GO TO FIN-PROGRAMA.


211092     IF NOT ((TRN NOT < "S0" AND TRN NOT > "S9") OR
081093             (TRN NOT < "-1" AND TRN NOT > "-5") OR
220694             (TRN     = "E8")) GO TO S-P-TRN.
220694     MOVE "7" TO RESPUESTA.
           CALL   ITEM-OBJETO4 USING RESPUESTA;
                  ON OVERFLOW GO TO S-P-TRN.
           IF NOT AFIRMATIVO GO TO PIDE-TRN ELSE
               CANCEL ITEM-OBJETO4.
270594 S-P-TRN.
170693     IF ((ESMMX) OR TRN = "RM" OR TRN = "AR"),
140794         MOVE 1,99 TO COEF-S-PRECIO.
           IF (TRN-SIN-COSTO OR NE-NS OR TRN = "FC" OR TRN = "AF"
081093                                OR TRN = "RM" OR TRN = "AR"),
                               GO TO PIDE-OPERADORA.
           IF  TRN     = "CO"  GO TO LLAMA-CONSULTIVO.
           DISPLAY "OPERACION INCORRECTA !" LINE 24 BLINK BEEP.
           GO TO P-TRN.
       FIN-PROGRAMA.
060697     CLOSE AMP, TRNAMP, AID, AIL, PEDIDO, DIF-PROV, AMP-P,
                 FACPRV01, FCISPR, AMPIO, VENCIM.
           DISPLAY "*****úPROGRAMA {MOV-AMP} FINALIZADOú*****"
                    LINE 12 POSITION 20 ERASE BLINK REVERSE.
       EXIT-PROGRAM.
           EXIT PROGRAM.
       STOP-RUN.
           STOP RUN.

       VIS-SUCURSAL.
           DISPLAY IND-V LINE LIN POSITION 57,
                   NOMBREXSUCURSAL(IND-V)
                         LINE LIN POSITION 60 LOW SIZE 20.
           ADD 1 TO LIN.
       SAL-VIS-SUCURSAL.

       LLAMA-CONSULTIVO.
           DISPLAY "Un momento ..." LINE    24 BLINK,
                   " Cargando el Programa Consultivo de Productos"
                                    POSITION 0 HIGH ERASE EOL.
060697     CLOSE AMP, TRNAMP, AID, AIL, PEDIDO, DIF-PROV, AMP-P.
           CALL   ITEM-OBJETO3 USING KEY-PARAMETRO, ARTEFACTO, CLIENTE,
                                     PARAMETROS-EMPRESA;
                  ON OVERFLOW DISPLAY SPACE BEEP.
           CANCEL ITEM-OBJETO3.
           GO TO ABRE-ARCHIVOS.

130995 DETERMINA-NUMERAL.
           IF MESC > 2 ADD 1 TO MESC ELSE SUBTRACT 1 FROM ANOC,
                                        ADD  13     TO  MESC.
           MULTIPLY 365,25 BY ANOC GIVING AUXILIAR1.
           MOVE AUXILIAR1  TO NUMERAL.
           MULTIPLY  30,60 BY MESC GIVING AUXILIAR1.
           ADD  AUXILIAR1  TO NUMERAL.
           ADD  DIAC       TO NUMERAL.
       SAL-DETERMINA-NUMERAL.

130995 DETERMINA-FECHA.
           SUBTRACT 122,1     FROM NUMERAL GIVING AUXILIAR.
           DIVIDE   AUXILIAR   BY  365,25  GIVING ANOC.
           MULTIPLY 365,25     BY  ANOC    GIVING AUXILIAR1.
           SUBTRACT AUXILIAR1 FROM NUMERAL GIVING AUXILIAR.
           DIVIDE   AUXILIAR   BY  30,61   GIVING MESC.
           MOVE     AUXILIAR1  TO  AUXILIAR.
           MULTIPLY 30,6       BY  MESC    GIVING AUXILIAR1.
           SUBTRACT AUXILIAR, AUXILIAR1 FROM NUMERAL GIVING DIAC.
           IF MESC > 13 SUBTRACT 13 FROM MESC ELSE SUBTRACT 1 FROM MESC.
           IF MESC <  3 ADD 1 TO ANOC.
       SAL-DETERMINA-FECHA.


      /    ***   PEDIDO DATOS COMUNES A TODOS LOS MOVIMIENTOS   ***
      *          """"""""""""""""""""""""""""""""""""""""""""

       SECCION-55 SECTION 55.
       PIDE-OPERADORA.
210797     MOVE LOW-VALUES TO REG-FACPRV.
           DISPLAY "Ingrese el Codigo del Operador (Su Codigo) ..."
                    LINE 24 LOW BLINK ERASE EOL.
           ACCEPT OPERADORA LINE 2 POSITION 39 PROMPT "x" ECHO NO BEEP;
                  ON EXCEPTION EXC GO TO PIDE-TRN. 
           PERFORM DISP-SPACES.
           IF OPERADORA = SPACES,
              IF OPERADORA-ANT = SPACES,
                 GO TO PIDE-OPERADORA ELSE
                 MOVE OPERADORA-ANT TO OPERADORA,
                 DISPLAY OPERADORA LINE 2 POSITION 39.
           MOVE OPERADORA TO OPERADORA-ANT.
       PIDE-FECHA-CARGA.
           ACCEPT FECHAUXR FROM DATE.
           DISPLAY "Ingrese la Fecha de CARGA del Comprobante ..."
                    LINE 24 LOW BLINK ERASE EOL.
           MOVE "034706" TO LIN-POS-SIZ.
           PERFORM ENTRA-FECHA THRU SAL-ENTRA-FECHA.
270594     IF BAND NOT = ZERO,
               DISPLAY "ÍÍÍ" LINE 2 POSITION 50 LOW,
               GO TO PIDE-OPERADORA.
           IF FECHA    = ZERO MOVE FECHA-COMP-ANT TO FECHA.
           MOVE FECHA TO FECHA-COMP-ANT.
           PERFORM RUT-FECHA.
           MOVE FECHA TO FECHA-EDIT.
           DISPLAY FECHA-EDIT LINE 3 POSITION 47.
290595     IF (TRN-ANT = "FC" AND FECHA-COMP-ANT NOT = FECHA-DIA),
               DISPLAY "ATENCION:" LINE 24 BEEP BLINK REVERSE,
                       " La Fecha de Ingreso del Comprobante NO COINC
      -                "IDE con la del Dia !"   POSITION 0 HIGH,
               PERFORM CONFIRMACION,
               IF NOT SEGURISIMO GO TO PIDE-FECHA-CARGA.
       PIDE-COD-SUC.
           DISPLAY "Ingrese el Codigo de Divisi¢n ..."
                                       LINE    24 LOW BLINK,
                   " (Seg£n la Tabla)" POSITION 0 LOW ERASE EOL.
       P-COD-SUC.

280103*----Solo pregunta por la sucursal si es Remito.
           IF TRN NOT = "RM" AND TRN NOT = "AR"
               MOVE 01 TO SUCURSAL-ANT,
               GO TO P-C-S.
           DISPLAY "Dep¢sito: " LINE 3 POSITION 60 REVERSE LOW,
           MOVE "037002" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
           IF BAND NOT = ZEROES,
              DISPLAY SPACES LINE 3 POSITION 70 SIZE 2,
              GO TO PIDE-OPERADORA.
           IF NUMERO = ZERO MOVE 01 TO NUMERO.
           IF NUMERO NOT =  04 AND NUMERO NOT = 01
              DISPLAY "DIVISION INCORRECTA !" LINE 24 BLINK BEEP,
              GO TO P-COD-SUC.

           IF NUMERO = 04 AND (TRN-ANT NOT = "RM"
                               AND TRN-ANT NOT = "AR")
              DISPLAY " SOLAMENTE APLICABLE AL DEPOSITO PRINCIPAL: {01}
      -              "!"     LINE 24 POSITION 1 BLINK ERASE EOL,
              ACCEPT RESPUESTA POSITION 0,
              GO TO PIDE-COD-SUC.


           MOVE NUMERO TO SUCURSAL-ANT.
           DISPLAY SUCURSAL-ANT LINE 3 POSITION 70.
***********IF (NOT NE-NS AND TRN NOT = "TR")
***********    AND SUCURSAL-ANT  NOT = 1,
***********   DISPLAY TRN-ANT LINE    24 LOW REVERSE BEEP,
***********           " SOLAMENTE APLICABLE AL DEPOSITO PRINCIPAL: {01} 
***********           "!"     POSITION 0 BLINK,
***********   GO TO P-COD-SUC.

       P-C-S.
           IF TRN-ANT NOT = "TR",
              MOVE ZEROES TO A-SUCURSAL,
270594        GO TO ABRE-PROV.
       PIDE-A-SUCURSAL.
           DISPLAY "Indique a qu‚ Divisi¢n va a Transferir ..."
                    LINE 24 LOW BLINK ERASE EOL.
       P-A-SUCURSAL.
           MOVE "025002" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
           IF BAND NOT = ZEROES,
               DISPLAY "ÍÍÍ" LINE 2 POSITION 50 LOW,
               GO TO PIDE-FECHA-CARGA.
      *     PIDE-COD-SUC.
290498     IF NUMERO   = ZEROES OR NUMERO > 05 OR NUMERO = SUCURSAL-ANT,
***********OR (NUMERO  = 01 AND SUCURSAL-ANT = 02)
***********OR (NUMERO  = 02 AND SUCURSAL-ANT = 01),
              DISPLAY "TRANSFIERE A UNA DIVISION INCORRECTA !"
                       LINE 24 BLINK BEEP,
              GO TO P-A-SUCURSAL.
           MOVE NUMERO TO A-SUCURSAL.
270594 ABRE-PROV.
           OPEN INPUT PROV.
       PIDE-COD-PROV.
           DISPLAY "Ingrese el Codigo del Proveedor ..."
                          LINE    24 LOW BLINK,
                   " ({"  POSITION 0 LOW,
                   "9999" POSITION 0 HIGH,
                   "}:Proveedor para Casos Especiales)"
                          POSITION 0 LOW ERASE EOL.
       P-COD-PROV.
           MOVE "044704" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
270594     IF BAND NOT = ZEROES,
              CLOSE PROV,
140794        DISPLAY SPACES LINE 4 POSITION 51 SIZE 4,
              GO TO PIDE-FECHA-CARGA.
           IF NUMERO   = ZEROES,
              MOVE COD-PROV-ANT TO NUMERO.
           MOVE NUMERO TO COD-PROV, COD-PROV-ANT.
           IF   NUMERO = ZEROES,
                IF (TRN = "TR" OR NE-NS),
                   DISPLAY "DIGITE EL CODIGO DE PROVEEDOR PARA EL ACCESO
      -                    " AUTOMATICO !"    LINE 24 BLINK BEEP,
                   GO TO P-COD-PROV ELSE
                   DISPLAY "DIGITE CODIGO DE PROVEEDOR PARA AFECTAR LA P
      -            "ARTIDA CORRESPONDIENTE !" LINE 24 BLINK BEEP,
                   GO TO P-COD-PROV.
010704     MOVE NUMERO TO COD-PROV-VENCIM WK-DISTRIBUIDOR.
           DISPLAY COD-PROV-ANT LINE 4 POSITION 47.
       LEE-PROV.
           READ PROV RECORD WITH NO LOCK;
                INVALID KEY
                DISPLAY "NO EXISTE ESE PROVEEDOR !" LINE 24 BLINK BEEP,
                GO TO P-COD-PROV.
           IF STATUS-PROV = "99" GO TO LEE-PROV.
           DISPLAY RAZON-SOCIAL LINE 24 1 REVERSE.
           PERFORM CONFIRMA.
           IF NEGATIVO           GO TO PIDE-COD-PROV.
281092     IF (TRN-ANT NOT = "FC" AND TRN-ANT NOT = "AF") AND
030293        (TRN-ANT NOT = "RM" AND TRN-ANT NOT = "AR"),
               DISPLAY SPACES LINE 4 POSITION 51 SIZE 4,
250794         MOVE COD-PROV-ANT TO COD-PROV-DIST,
270594         GO TO CIERRA-PROV.
           MOVE REG-PROV TO REG-132.
       PIDE-COD-PROV-DIST.
           DISPLAY "Ingrese el Codigo del Distribuidor ..."
                           LINE    24 LOW BLINK,
                   " ({"   POSITION 0 LOW,
                   "Enter" POSITION 0 LOW REVERSE,
                   "}:Idem Proveedor)"
                           POSITION 0 LOW ERASE EOL.
       P-COD-PROV-DIST.
           MOVE "045104" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
           IF BAND NOT = ZEROES GO TO PIDE-COD-PROV.
           IF NUMERO   = ZEROES,
              MOVE COD-PROV-ANT TO NUMERO.
180797     MOVE NUMERO TO COD-PROV, COD-PROV-DIST, COD-PRV-FACPRV.
           DISPLAY COD-PROV-DIST LINE 4 POSITION 51 REVERSE.
       LEE-PROV-DIST.
           READ PROV RECORD WITH NO LOCK;
                INVALID KEY
                DISPLAY "NO EXISTE ESE DISTRIBUIDOR !"
                         LINE 24 BLINK BEEP,
                GO TO P-COD-PROV-DIST.
           IF STATUS-PROV = "99" GO TO LEE-PROV-DIST.
           DISPLAY RAZON-SOCIAL LINE 24.
           PERFORM CONFIRMA.
           IF NEGATIVO           GO TO PIDE-COD-PROV-DIST.
           MOVE RAZON-SOCIAL TO RAZON-SOCIAL-D.
           MOVE REG-132 TO REG-PROV.
270594 CIERRA-PROV. CLOSE PROV.
       PIDE-FECHA-COMP.
           DISPLAY "Ingrese la Fecha del Comprobante ..."
                    LINE 24 LOW BLINK ERASE EOL.
           MOVE "054706" TO LIN-POS-SIZ.
           PERFORM ENTRA-FECHA THRU SAL-ENTRA-FECHA.
270594     IF BAND NOT = ZERO
KKKKKK            MOVE ZEROES TO FECHA
                  GO TO ABRE-PROV.
           IF FECHA    = ZERO GO TO PIDE-FECHA-COMP.
      *       IF FE-COMP-ANT = SPACES
      *           MOVE FECHA-COMP-ANT TO FECHA
      *       ELSE
      *           MOVE FE-COMP-ANT TO FECHA.


180797     MOVE FECHA TO FE-COMP-ANT, FEC-NUE.
##2000     MOVE FECHA TO FECHA-8.
##2000     PERFORM FECHA-8-DIGITOS THRU FIN-FECHA-8-DIGITOS.
           MOVE RFECHA-COMPLETA TO FEC-CPTE-FACPRV.

IVA19      IF RFECHA-COMPLETA > 20021117 AND RFECHA-COMPLETA < 20030118
               MOVE 0,19  TO IVA-WK,
               MOVE "19%" TO LIVA
           ELSE
              MOVE 0,21   TO IVA-WK,
              MOVE "21%"  TO LIVA.

           PERFORM RUT-FECHA.
           MOVE FECHA TO FECHA-EDIT.
           DISPLAY FECHA-EDIT LINE 5 POSITION 47.
       PIDE-NRO-COMP.
           MOVE ZEROES TO AUXILIAR, INTERMEDIO, INTERMEDIO2.
250797     PERFORM LIMPIA-TOTALES-LIN23.
           PERFORM LIMPIA-RECTANGULO.
           DISPLAY "Ingrese el Numero del Comprobante ..."
                    LINE 24 LOW BLINK ERASE EOL.
           MOVE "064706" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
           IF BAND NOT = ZERO GO TO PIDE-FECHA-COMP.
           IF NUMERO   = ZERO,
              IF NRO-COMP-ANT NOT > ZEROES GO TO PIDE-NRO-COMP ELSE
              MOVE NRO-COMP-ANT TO NUMERO,
              DISPLAY NRO-COMP-ANT LINE 6 POSITION 47.
180797     MOVE NUMERO TO NRO-COMP-ANT, NRO-CPTE-FACPRV.

010704     MOVE NUMERO TO NROCOMP1-VENCIM WK-FACTURA.
010704     MOVE SPACES TO WK-ESTA-VTO.
010704     PERFORM CONTROL-VTO THRU F-CONTROL-VTO.
           
           IF WK-ESTA-VTO = "S"
              DISPLAY " Factura con vencimiento asignado en Proveedores 
      -               "-- Confirmar con TESORERIA -- "
                      LINE 24 POSITION 01 REVERSE BLINK,
              ACCEPT RESPUESTA,
              IF RESPUESTA NOT = "*"
                 GO TO FIN-PROGRAMA.

           MOVE ZEROES TO CONTADOR-TRN.
           IF (TRN NOT = "FC" AND TRN NOT = "AF") GO TO PIDE-ACCESO.
           MOVE ZEROES TO AUXILIAR, INTERMEDIO, INTERMEDIO2,
                          TOTAL-SBT, TOTAL-IVA, TOTAL-DES.
           READ FACPRV01
                INVALID KEY
                MOVE LOW-VALUES TO DATOS-FACPRV.
240797     PERFORM CALCULA-TOTAL-FACTURA THRU SAL-CALCULA-TOTAL-FACTURA.
       PIDE-PROCEDENCIA-FACTURA.
           DISPLAY "Ingres¢ el Remito ?   (Si/No)"
                                LINE  9 POSITION 26 LOW REVERSE,
                   "Indique si ha Ingresado un Remito Previo para esta F
      -            "actura ..." LINE 24 POSITION  1 LOW BLINK ERASE EOL.
           ACCEPT RESP-REMITO LINE 9 POSITION 46 PROMPT "*" NO BEEP;
                  ON EXCEPTION EXC GO TO PIDE-NRO-COMP.
           PERFORM DISP-SPACES.
           IF RESP-REMITO = "0",
              MOVE "S" TO RESP-REMITO,
              DISPLAY RESP-REMITO LINE 9 POSITION 46.
           IF (RESP-REMITO NOT = "S" AND RESP-REMITO NOT = "N"),
               GO TO PIDE-PROCEDENCIA-FACTURA.
       PIDE-QUE-PRECIO.
           DISPLAY "Coefic.sobre COSTO :" LINE 10 POSITION 26
                                          LOW REVERSE.
           MOVE "C" TO QUE-PRECIO.
      *    DISPLAY "Coefic.sobre ??????:"
      *                              LINE 10 POSITION 26 LOW REVERSE,
      *            SPACES            LINE 10 POSITION 47 SIZE 6,
      *            "De acuerdo a Factura Proveedor:"
      *                              LINE 24 POSITION  1 LOW REVERSE,
      *            " Carga "                 POSITION  0 LOW,
      *            "P"                       POSITION  0 HIGH,
      *            "recio al Publico ¢ "     POSITION  0 LOW,
      *            "C"                       POSITION  0 HIGH,
      *            "osto del Producto ?"     POSITION  0 LOW ERASE EOL.
      *    ACCEPT RESPUESTA LINE 10 POSITION 39 PROMPT "x" BLINK NO BEEP
      *           ON EXCEPTION EXC GO PIDE-PROCEDENCIA-FACTURA.
      *    IF RESPUESTA = SPACE MOVE    QUE-PRECIO TO RESPUESTA,
      *                         DISPLAY QUE-PRECIO LINE 10 POSITION 39.
      *    MOVE RESPUESTA TO QUE-PRECIO.
      *    IF QUE-PRECIO = "C",
      *       DISPLAY "Costo :"      LINE 10 POSITION 39 LOW REVERSE,
      *               "1,0000"       LINE 10 POSITION 47,
      *               "Costo  "      LINE 20 POSITION 18 LOW REVERSE,
      *       ELSE
      *    IF QUE-PRECIO = "P",
      *       DISPLAY "Precio:"      LINE 10 POSITION 39 LOW REVERSE,
      *               "0,0000"       LINE 10 POSITION 47,
      *               "Precio "      LINE 20 POSITION 18 LOW REVERSE,
      *       ELSE GO TO PIDE-QUE-PRECIO.
      *PIDE-COEF-PRECIO.
      *    IF QUE-PRECIO = "P",
      *    DISPLAY "Costo Aproximado del Producto:"
      *                              LINE 24 POSITION  1 LOW REVERSE,
      *            " ( Coeficiente ) "       POSITION  0 HIGH,
      *            "x"                       POSITION  0 HIGH BLINK,
      *            " ( Precio en Factura )"  POSITION  0 HIGH ERASE EOL,
      *    ELSE
      *    DISPLAY "Precio Aproximado del Producto:"
      *                              LINE 24 POSITION  1 LOW REVERSE,
      *            " ( Coeficiente ) "       POSITION  0 HIGH,
      *            "x"                       POSITION  0 HIGH BLINK,
      *            " ( Costo en Factura )"   POSITION  0 HIGH ERASE EOL.
      *    MOVE "104904" TO LIN-POS-SIZ.
      *    PERFORM CONTROL-NUMERO.
      *    IF BAND NOT = ZEROES GO TO PIDE-QUE-PRECIO.
      *    IF NUMERO   < 1000   GO TO PIDE-COEF-PRECIO.
      *    IF QUE-PRECIO = "C" ADD 16973 TO NUMERO.
           MOVE 16973 TO NUMERO.
           MOVE NUMERO-COEF TO COEF-S-PRECIO.
           DISPLAY "1,6973" LINE 10 POSITION 49.
           MOVE NUMERO-COEF TO COEF-S-PRECIO.
      *    DISPLAY NUMERO-COEF LINE 11 POSITION 59.
           MOVE "N" TO ACT-PRECIO.
      *PIDE-ACT-PRECIO.
      *    DISPLAY "Actualiza  Precio ?   (Si/No)"
      *             LINE 11 POSITION 26 LOW REVERSE,
      *            "Indique si efect£a la Actualizaci¢n inmediata del Pr
      *-           "ecio del Producto ..."
      *             LINE 24 POSITION  1 LOW BLINK ERASE EOL.
      *    ACCEPT ACT-PRECIO LINE 11 POSITION 46 PROMPT "*";
      *           NO BEEP BLINK ON EXCEPTION EXC
      *           DISPLAY SPACES LINE 11 POSITION 26 SIZE 29,
      *           GO TO PIDE-QUE-PRECIO.
      *    PERFORM DISP-SPACES.
      *    IF ACT-PRECIO = "0",
      *       MOVE "S" TO ACT-PRECIO,
      *       DISPLAY ACT-PRECIO LINE 11 POSITION 46.
      *    IF (ACT-PRECIO NOT = "S" AND ACT-PRECIO NOT = "N"),
      *        GO TO PIDE-ACT-PRECIO.
       PIDE-MODO-CARGA.
           DISPLAY "Unidad/Bulto:    (U/B) "
                      LINE 11 POSITION 26 LOW REVERSE.
           ACCEPT MODO-CARGA LINE 11 POSITION 41 PROMPT "*" NO BEEP.
           IF MODO-CARGA = SPACES
                MOVE "U" TO MODO-CARGA.
           DISPLAY MODO-CARGA LINE 11 POSITION 41.
           IF MODO-CARGA NOT = "U" AND MODO-CARGA NOT = "B"
                    GO TO PIDE-NRO-COMP.
           IF POR-BULTO
                   DISPLAY "  Bultos  " LINE 20 POSITION 2 LOW REVERSE
           ELSE
                   DISPLAY " Cantidad " LINE 20 POSITION 2 LOW REVERSE.
       PIDE-DATOS-GENERAL-COMPROB.
230797     CALL   ITEM-OBJETO2 USING "110560223".
           DISPLAY "IVA: " LINE 10 POSITION 60 REVERSE LOW,
                   LIVA POSITION 0 REVERSE LOW.

           DISPLAY "Percep.IB  :"           LINE 11 POSITION 57 LOW.
           DISPLAY "R.G. 3337  :"           LINE 12 POSITION 57 LOW.
           CANCEL ITEM-OBJETO2.
230797 PIDE-PERCEPCION-IB-FACTURA.
           DISPLAY "Ingrese el Importe de la Percepci¢n de Ingreso
      -    "Brutos" LINE 24 POSITION 1 LOW BLINK ERASE EOL.
           MOVE "116911" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
           IF BAND NOT = ZEROES GO TO PIDE-MODO-CARGA.
           IF NUMERO-DEC = 0,
               MOVE TOT-PERCEP-FACPRV TO NUMERO-DEC.
           MOVE NUMERO-DEC TO PERCEPCION-IB-FAC.
           MOVE NUMERO-DEC TO NUM-EDIT-2.
           DISPLAY NUM-EDIT-2 LINE 11 POSITION 69.
271097 PIDE-P3337.
           DISPLAY "Ingrese el Importe de la R.G. 3337"
                    LINE 24 POSITION 1 LOW BLINK ERASE EOL.
           MOVE "126911" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
           IF BAND NOT = ZEROES GO TO PIDE-PERCEPCION-IB-FACTURA.
           IF NUMERO-DEC = 0,
               MOVE TOT-P3337-FACPRV TO NUMERO-DEC.
           MOVE NUMERO-DEC TO PERCEP-P3337.
           MOVE NUMERO-DEC TO NUM-EDIT-2.
           DISPLAY NUM-EDIT-2 LINE 12 POSITION 69.
       PIDE-ACCESO.
           UNLOCK AMP    RECORD.
           UNLOCK TRNAMP RECORD.
           UNLOCK PEDIDO RECORD.
060697     UNLOCK AMP-P  RECORD.
           MOVE LOW-VALUES TO REG-AID, REG-AIL.
           DISPLAY "Acceso:"         LINE    24 LOW REVERSE,
                   "   (p/Codigo:{"  POSITION 0 LOW,
                   "C"               POSITION 0 HIGH,
                   "}úp/Linea:{"     POSITION 0 LOW,
                   "L"               POSITION 0 HIGH,
                   "}úp/Prov:{"      POSITION 0 LOW,
                   "P"               POSITION 0 HIGH,
                   "}úp/Pedido:{"    POSITION 0 LOW,
                   "D"               POSITION 0 HIGH,
                   "}úp/Nombre:{"    POSITION 0 LOW,
                   "N"               POSITION 0 HIGH,
                   "}"               POSITION 0 LOW.
           ACCEPT RESPUESTA LINE 24 POSITION 9 PROMPT "*" NO BEEP BLINK;
                  ON EXCEPTION EXC GO TO PIDE-NRO-COMP.
           PERFORM DISP-SPACES.
           IF RESPUESTA = SPACES MOVE ACCESO    TO RESPUESTA ELSE
                                 MOVE RESPUESTA TO ACCESO.
           IF ACCESO = "C" DISPLAY "Codigo" LINE 7 POSITION 46,
                           GO TO POSICIONA-TROQUEL.
           IF ACCESO = "L" DISPLAY " Linea" LINE 7 POSITION 46,
                           GO TO POSICIONA-LINEA.
           IF ACCESO = "P" DISPLAY "Provd." LINE 7 POSITION 46,
                           GO TO POSICIONA-PROVEEDOR.
           IF ACCESO = "D" DISPLAY "Pedido" LINE 7 POSITION 46,
                           GO TO PIDE-NRO-PEDIDO.
           IF ACCESO = "N" DISPLAY "Nombre" LINE 7 POSITION 46,
                           GO TO POSICIONA-NOMBRE.
           DISPLAY "?" LINE 24 POSITION 9 BEEP.
           GO TO PIDE-ACCESO.

       PIDE-NRO-PEDIDO.
           DISPLAY "Seg£n Nota de Pedido:"
                    LINE  8 POSITION 26 LOW REVERSE,
                   "Ingrese el Numero del Pedido Efectuado ..."
                    LINE 24 POSITION  1 LOW BLINK ERASE EOL.
       P-NRO-PEDIDO.
           MOVE "084706" TO LIN-POS-SIZ.
           PERFORM CONTROL-NUMERO.
           IF BAND NOT = ZEROES,
              DISPLAY SPACES LINE 8 POSITION 26 SIZE 29,
              GO TO PIDE-ACCESO.
           IF NUMERO   = ZEROES GO TO PIDE-NRO-PEDIDO.
           MOVE NUMERO TO NUMERO-PEDIDO.
           START PEDIDO KEY IS EQUAL TO XNUMERO-PEDIDO;
                 INVALID KEY
                 DISPLAY "NO EXISTE ESE PEDIDO !"
                          LINE 24 BLINK BEEP ERASE EOL,
                 GO TO P-NRO-PEDIDO.
           IF STATUS-PEDIDO = "99" GO TO PIDE-NRO-PEDIDO.
           GO TO DETERMINA-RUTINA.
       POSICIONA-PROVEEDOR.
           MOVE COD-PROV TO COD-LAB-AIL.
           START AIL KEY IS NOT < CLAVE-AIL-LAB-DESC;
                 INVALID KEY
                 DISPLAY "NO HAY PRODUCTOS PARA ESE PROVEEDOR ! "
                          LINE 24 BLINK ERASE EOL,
                 ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW,
                 GO TO PIDE-ACCESO.
           GO TO DETERMINA-RUTINA.
       POSICIONA-LINEA.
           MOVE COD-PROV TO COD-LAB-AID.
           START AID KEY IS NOT < CLAVE-AID-LABORATORIO;
                 INVALID KEY
                 DISPLAY "NO HAY PRODUCTOS PARA ESE PROVEEDOR ! "
                          LINE 24 BLINK ERASE EOL,
                 ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW,
                 GO TO PIDE-ACCESO.
           GO TO DETERMINA-RUTINA.
       POSICIONA-TROQUEL.
           START AID KEY IS NOT < CLAVE-AID-TROQUEL;
                 INVALID KEY
                 DISPLAY "NO EXISTE UN PRODUCTO CON CODIGO PARECIDO ! "
                          LINE 24 BLINK ERASE EOL,
                 ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW,
                 GO TO PIDE-ACCESO.
           GO TO DETERMINA-RUTINA.
       POSICIONA-NOMBRE.
           START AID KEY IS NOT < CLAVE-ALFAN-AID;
                 INVALID KEY
                 DISPLAY "NO HAY PRODUCTOS CON NOMBRE PARECIDO ! "
                          LINE 24 BLINK ERASE EOL,
                 ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW,
                 GO TO PIDE-ACCESO.
           GO TO DETERMINA-RUTINA.

       DETERMINA-RUTINA.
211092     IF ((TRN NOT < "S0" AND TRN NOT > "S9") OR
081093         (TRN NOT < "-1" AND TRN NOT > "-5") OR TRN = "TR"),
                                                    GO TO RUT-NS.
211092     IF ((TRN NOT < "E0" AND TRN NOT > "E9") OR
081093         (TRN NOT < "+1" AND TRN NOT > "+5")) GO TO RUT-NE.
           IF (TRN = "RM")                          GO TO RUT-RM.
           IF (TRN = "AF" OR TRN = "AR")            GO TO RUT-AF-AR.
                                                    GO TO RUT-FC.

      *-----------------------------------------------------------------
       CONTROL-VTO.
      *............ Verifico si ya esta dada de alta esa factura ..........
           MOVE " " TO WK-ENCUENTRO-VTO.
           MOVE LOW-VALUES TO REG-VENCIM.
           MOVE WK-FACTURA TO NROCOMP1-VENCIM.
       EXISTE-VTO.
           START VENCIM  KEY IS NOT LESS CLAVE-SEC2-VENCIM
                 INVALID KEY
                 MOVE "N" TO WK-ENCUENTRO-VTO.
           IF STATUS-VENCIM = "99" GO TO EXISTE-VTO.
           IF WK-ENCUENTRO-VTO = " "
               PERFORM MISMO-CLIENTE THRU FIN-MISMO-CLIENTE
                       UNTIL EOF-VENCIM OR WK-ENCUENTRO-VTO = "S".
           MOVE WK-ENCUENTRO-VTO TO WK-ESTA-VTO.
      
       F-CONTROL-VTO.
           EXIT.
      *-----------------------------------------------------------------
      *-----------------------------------------------------------------*
       MISMO-CLIENTE.
           READ VENCIM NEXT RECORD WITH NO LOCK,
                AT END
                   MOVE "FF" TO STATUS-VENCIM,
                   GO TO FIN-MISMO-CLIENTE.
           IF STATUS-VENCIM = "99" GO TO MISMO-CLIENTE.
           IF NROCOMP1-VENCIM NOT = WK-FACTURA
                  MOVE "FF" TO STATUS-VENCIM
                  GO TO FIN-MISMO-CLIENTE.
           IF COD-PROV-VENCIM = WK-DISTRIBUIDOR
               MOVE "S" TO WK-ENCUENTRO-VTO,
               GO TO FIN-MISMO-CLIENTE.
       FIN-MISMO-CLIENTE.
           EXIT.
      *-----------------------------------------------------------------*


      /    ***   NOTA DE SALIDA DE STOCK   ***
      *          """""""""""""""""""""""

       RUTINA-NS SECTION 60.
       RUT-NS.
           PERFORM PIDE-PRODUCTO.
           SUBTRACT CANTIDAD-TRN FROM EXISTENCIA-SUC(SUCURSAL-ANT)
                    GIVING INTERMEDIO.
           IF INTERMEDIO < ZEROES,
              IF TRN-ANT = "TR",
                 DISPLAY "TRANSFERENCIA IMPOSIBLE !"
                                  LINE    24 LOW REVERSE BLINK,
                         " (No hay Existencias Suficientes) "
                                  POSITION 0 ERASE EOL,
                 ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW BLINK,
                 GO TO RUT-NS ELSE
200494           ADD INTERMEDIO              TO CANTIDAD-TRN.
           MOVE EXISTENCIA-SUC(SUCURSAL-ANT) TO NUM-MENS.
           MOVE "Existencia Total Divisi¢n"  TO MENSAJE.
           PERFORM DISP-MENSAJE-ERROR.
           MOVE INTERMEDIO           TO  EXISTENCIA-SUC(SUCURSAL-ANT).
           IF TRN-ANT = "TR",
              MOVE A-SUCURSAL        TO  A-SUCURSAL-TRN,
              ADD  CANTIDAD-TRN      TO  EXISTENCIA-SUC(A-SUCURSAL),
              MOVE CANTIDAD-TRN      TO  CANT-IMP-TRN (1),
              SUBTRACT CANTIDAD-TRN FROM PENDIENTE-SUC(A-SUCURSAL),
              IF PENDIENTE-SUC(A-SUCURSAL) < ZEROES,
                 MOVE ZEROES         TO  PENDIENTE-SUC(A-SUCURSAL),
                 GO TO FIN-RUT-NS ELSE
                 GO TO FIN-RUT-NS.
           MOVE CANTIDAD-TRN         TO  INTERMEDIO.
           PERFORM RESTA-PARTIDA VARYING IND FROM 1 BY 1
                                 UNTIL   INTERMEDIO = ZEROES
                                 OR      IND        = IND-P.
060395     IF (TRN = "-1" OR TRN = "S4") GO TO ACT-COMPR-NS.            (Bonificaciones)
140793 PREG-COMPRADA.
           DISPLAY "Resta de las Unidades Compradas en el Mes ? ("
                        LINE    24 LOW,
                   "S"  POSITION 0 HIGH,
                   "|"  POSITION 0 LOW BLINK,
                   "0"  POSITION 0 HIGH,
                   "/"  POSITION 0 LOW,
                   "N"  POSITION 0 HIGH,
                   ") " POSITION 0 LOW ERASE EOL.
           ACCEPT RESPUESTA POSITION 0 PROMPT "*" BLINK NO BEEP;
                  ON EXCEPTION EXC MOVE "N" TO RESPUESTA.
           PERFORM DISP-SPACES.
           IF NEGATIVO       GO TO FIN-RUT-NS.
           IF NOT AFIRMATIVO GO TO PREG-COMPRADA.
060395 ACT-COMPR-NS.
300997     SUBTRACT CANTIDAD-TRN FROM CANT-COMPRADA-MES(MES-NUE)
300997                           GIVING INTERMEDIO.
           MOVE "Acumulado Compras del Mes" TO MENSAJE.
300997     MOVE CANT-COMPRADA-MES(MES-NUE)  TO NUM-MENS.
           PERFORM DISP-MENSAJE-ERROR.
300997     MOVE INTERMEDIO TO CANT-COMPRADA-MES(MES-NUE).
       FIN-RUT-NS.
           PERFORM ARMA-TRNAMP.
           GO TO RUT-NS.

       RESTA-PARTIDA.
           MOVE PARTIDA-AMP(IND)    TO IMPUTACION(IND).
           IF INTERMEDIO > EXISTENCIA-PARTIDA    (IND),
              SUBTRACT     EXISTENCIA-PARTIDA    (IND)
                                  FROM INTERMEDIO,
              MOVE ZEROES           TO EXISTENCIA-PARTIDA(IND) ELSE
              MOVE INTERMEDIO       TO CANT-IMP-TRN      (IND),
              SUBTRACT INTERMEDIO FROM EXISTENCIA-PARTIDA(IND),
              MOVE ZEROES           TO INTERMEDIO.
       SAL-RESTA-PARTIDA.


010704

      *    ***   NOTA DE ENTRADA DE STOCK   ***
      *          """"""""""""""""""""""""

       RUTINA-NE SECTION 60.
       RUT-NE.
           PERFORM PIDE-PRODUCTO.
      *....Cuando es una entrada especial, verifico si tiene un codigo "?"
      *....Si tiene una posicion del vector con "?", acumulo directamente
      *....aqui.
           MOVE 1 TO IND.
020201     IF ORIGEN-PARTIDA(IND) NOT = "?"
                 PERFORM ARMAR-SIGNO THRU FIN-ARMAR-SIGNO,
111113*           MOVE 5012       TO VENCIMIENTO-PARTIDA(1),
                 MOVE VENCIMIENTO-TRN TO VENCIMIENTO-PARTIDA(1),
                 MOVE LOW-VALUES TO ORIGEN-PARTIDA(1).
           ADD  CANTIDAD-TRN       TO EXISTENCIA-PARTIDA (1).
MEL   * q tome el vencimiento cargado
111113     MOVE VENCIMIENTO-TRN TO VENCIMIENTO-PARTIDA(1).
311094     IF ORIGEN-PARTIDA(1)    = LOW-VALUE,
              MOVE "?"             TO ORIGEN-PARTIDA     (1).
230894     IF (COD-PROV-PARTIDA(1) = 0 OR COD-PROV-PARTIDA(1) = 9999),
               MOVE COD-PROV-DIST  TO COD-PROV-PARTIDA   (1),
##2000*         MOVE 5012           TO VENCIMIENTO-PARTIDA(1),
               MOVE VENCIMIENTO-TRN TO VENCIMIENTO-PARTIDA(1),
               MOVE FECHA-COMP-ANT TO FECHA-PARTIDA      (1).
060395     IF (TRN = "+1" OR TRN = "E4"),                               (Bonificaciones)
               MOVE FECHA-COMP-ANT TO FECHA-PARTIDA      (1),
300997         ADD  CANTIDAD-TRN   TO CANT-COMPRADA-MES  (MES-NUE).
311094*********ADD  CANTIDAD-TRN   TO CANT-COMPRADA-MES  (MES-ANT).
           MOVE PARTIDA-AMP(1)     TO IMPUTACION         (1).
230994     MOVE COD-PROV-DIST      TO PROV-IMP-TRN       (1).
           MOVE CANTIDAD-TRN       TO CANT-IMP-TRN       (1).
           ADD  CANTIDAD-TRN       TO EXISTENCIA-SUC     (SUCURSAL-ANT).
       PREG-DEVOLUCION.
           DISPLAY "Es Devoluci¢n de una Factura ? ("
                        LINE    24 LOW,
                   "S"  POSITION 0 HIGH,
                   "|"  POSITION 0 LOW BLINK,
                   "0"  POSITION 0 HIGH,
                   "/"  POSITION 0 LOW,
                   "N"  POSITION 0 HIGH,
                   ") " POSITION 0 LOW ERASE EOL.
           ACCEPT RESPUESTA POSITION 0 PROMPT "*" BLINK NO BEEP;
                  ON EXCEPTION EXC MOVE "N" TO RESPUESTA.
           PERFORM DISP-SPACES.
           IF NEGATIVO       GO TO FIN-RUT-NE.
           IF NOT AFIRMATIVO GO TO PREG-DEVOLUCION.
           SUBTRACT CANTIDAD-TRN         FROM   TOTAL-ACUM-HISTORICO
                                         GIVING INTERMEDIO.
           MOVE "Acumulado Hist¢rico Ventas" TO MENSAJE.
           MOVE TOTAL-ACUM-HISTORICO         TO NUM-MENS.
           PERFORM DISP-MENSAJE-ERROR.
           MOVE INTERMEDIO                   TO TOTAL-ACUM-HISTORICO.
           SUBTRACT CANTIDAD-TRN         FROM   TOTAL-ACUM-SUC
                                                (SUCURSAL-ANT)
                                         GIVING INTERMEDIO.
           MOVE "Acumulado General Ventas"   TO MENSAJE.
           MOVE TOTAL-ACUM-SUC(SUCURSAL-ANT) TO NUM-MENS.
           PERFORM DISP-MENSAJE-ERROR.
           MOVE INTERMEDIO                   TO TOTAL-ACUM-SUC
                                                (SUCURSAL-ANT).
           SUBTRACT CANTIDAD-TRN FROM CANT-VENDIDA-MES(MES-ANT)
                                         GIVING INTERMEDIO.
           MOVE "Acumulado Ventas del Mes"   TO MENSAJE.
           MOVE CANT-VENDIDA-MES(MES-ANT)    TO NUM-MENS.
           PERFORM DISP-MENSAJE-ERROR.
           MOVE INTERMEDIO TO CANT-VENDIDA-MES(MES-ANT).
           SUBTRACT CANTIDAD-TRN         FROM   VENDIDA-SUC
                                                (SUCURSAL-ANT)
                                         GIVING INTERMEDIO.
           MOVE "Acumulado Ventas Divisi¢n"  TO MENSAJE.
           MOVE VENDIDA-SUC(SUCURSAL-ANT)    TO NUM-MENS.
           PERFORM DISP-MENSAJE-ERROR.
           MOVE INTERMEDIO                   TO VENDIDA-SUC
                                                (SUCURSAL-ANT).
       FIN-RUT-NE.
           PERFORM ARMA-TRNAMP.
           GO TO RUT-NE.

020201 ARMAR-SIGNO.
      *...Verifico que haya un lugar libre en las 5 posiciones del vector.
           IF PARTIDA-AMP(5) = LOW-VALUES
               MOVE 5 TO AUX,
               PERFORM HACER-CORRIM THRU FIN-HACER-CORRIM,
               MOVE LOW-VALUES TO PARTIDA-AMP(1).
       FIN-ARMAR-SIGNO.

       HACER-CORRIM.
           COMPUTE IND = AUX - 1.
           MOVE PARTIDA-AMP(IND) TO PARTIDA-AMP(AUX).
           SUBTRACT 1 FROM AUX.
           IF AUX > 1 GO TO HACER-CORRIM.
       FIN-HACER-CORRIM.


      /    ***   REMITO PROVEEDOR   ***
      *          """"""""""""""""

       RUTINA-RM SECTION 70.
       RUT-RM.
           PERFORM PIDE-PRODUCTO.
280203     IF SUCURSAL-ANT = 04
              PERFORM SUCURSAL-IOMA THRU FIN-SUCURSAL-IOMA
              GO TO RUT-RM.
           PERFORM BUSQUEDA-LUGAR.
           MOVE "R"                TO ORIGEN-PARTIDA     (IND-P).
250794     MOVE COD-PROV-DIST      TO COD-PROV-PARTIDA   (IND-P).
           MOVE FECHA-COMP-ANT     TO FECHA-PARTIDA      (IND-P).
           MOVE CANTIDAD-TRN       TO COMPRA-PARTIDA     (IND-P),
                                      EXISTENCIA-PARTIDA (IND-P).
           MOVE VENCIMIENTO-TRN    TO VENCIMIENTO-PARTIDA(IND-P).
300997     ADD  CANTIDAD-TRN       TO CANT-COMPRADA-MES  (MES-NUE).
           ADD  CANTIDAD-TRN       TO EXISTENCIA-SUC     (SUCURSAL-ANT).
           MOVE PARTIDA-AMP(IND-P) TO IMPUTACION         (1).
130592     PERFORM CONTROLA-REPETICION.
110293     IF NOT SEGURISIMO,
130592        DISPLAY "REMITO IGNORADO !" LINE 24 BEEP BLINK ELSE
130592        PERFORM ARMA-TRNAMP.
           GO TO RUT-RM.


280203 SUCURSAL-IOMA.
           PERFORM BUSQUEDA-LUGAR-IOMA THRU SAL-BUSQUEDA-LUGAR-IOMA.
           MOVE IND-P TO NUMERO.
           MOVE "R"                TO ORIGEN-PARTIDAIO     (IND-P).
           MOVE COD-PROV-DIST      TO COD-PROV-PARTIDAIO   (IND-P).
           MOVE FECHA-COMP-ANT     TO FECHA-PARTIDAIO      (IND-P).
           MOVE CANTIDAD-TRN       TO COMPRA-PARTIDAIO     (IND-P),
                                      EXISTENCIA-PARTIDAIO (IND-P).
           MOVE VENCIMIENTO-TRN    TO VENCIMIENTO-PARTIDAIO(IND-P).
      *----Ojo !! aca suma en la posicion 4 del AMP nuestro.
           ADD  CANTIDAD-TRN       TO EXISTENCIA-SUC     (SUCURSAL-ANT).

           MOVE PARTIDA-AMPIO(IND-P) TO IMPUTACION         (1).
280203     PERFORM C-REPET-IOMA.
110293     IF NOT SEGURISIMO,
130592        DISPLAY "REMITO IGNORADO !" LINE 24 BEEP BLINK ELSE
130592        PERFORM ARMA-TRNAMP.

       FIN-SUCURSAL-IOMA.


       BUSQUEDA-LUGAR-IOMA.
           MOVE 1                     TO IND-P.
       CICLO-BUSQ-LUGAR-IOMA.
           IF FECHA-PARTIDAIO(IND-P) = ZEROES
               GO TO SAL-BUSQUEDA-LUGAR-IOMA.
           ADD  1                     TO IND-P.
           IF IND-P NOT > 5                 GO TO CICLO-BUSQ-LUGAR-IOMA.
       CALCULA-PONDERADOS-IOMA.
           MOVE 5                       TO IND-P.
           MOVE "?"                     TO ORIGEN-PARTIDAIO   (2).
##2000     MOVE 5012                    TO VENCIMIENTO-PARTIDAIO(2).
           ADD  EXISTENCIA-PARTIDAIO(1) TO EXISTENCIA-PARTIDAIO (2).
           ADD  COMPRA-PARTIDAIO    (1) TO COMPRA-PARTIDAIO     (2).
           MOVE PARTIDA-AMPIO       (2) TO PARTIDA-AMPIO        (1).
           MOVE PARTIDA-AMPIO       (3) TO PARTIDA-AMPIO        (2).
           MOVE PARTIDA-AMPIO       (4) TO PARTIDA-AMPIO        (3).
           MOVE PARTIDA-AMPIO       (5) TO PARTIDA-AMPIO        (4).
           MOVE LOW-VALUES              TO PARTIDA-AMPIO        (5).
       SAL-BUSQUEDA-LUGAR-IOMA.



      *    ***   FACTURA PROVEEDOR   ***
      *          """""""""""""""""

       RUTINA-FC SECTION 70.
       RUT-FC.
           PERFORM PIDE-PRODUCTO.
           IF RESP-REMITO NOT = "S" GO TO ALTA-FACTURA.
       DESCUENTA-REMITO.
           PERFORM BUSQUEDA-PARTIDA.
           IF IND-P > 5 GO TO RUT-FC.
290696     IF (CANTIDAD-TRN NOT = COMPRA-PARTIDA(IND-P)),
               DISPLAY "ATENCION:" LINE    24 REVERSE,
                       " NO COINCIDE LA CANTIDAD DEL REMITO !"
                                   POSITION 0 BLINK BEEP,
               PERFORM CONFIRMACION,
               IF NOT SEGURISIMO GO TO RUT-FC.
           SUBTRACT CANTIDAD-TRN FROM COMPRA-PARTIDA(IND-P)
                    GIVING INTERMEDIO.
           MOVE "Cantidad Total del Remito"
                                      TO MENSAJE.
           MOVE COMPRA-PARTIDA(IND-P) TO NUM-MENS.
           PERFORM DISP-MENSAJE-ERROR.
           IF INTERMEDIO = ZEROES,
           MOVE COMPRA-PARTIDA(IND-P) TO CANTIDAD-TRN.
           GO TO ACTUALIZA-FACTURA.
       ALTA-FACTURA.
           PERFORM BUSQUEDA-LUGAR.
           MOVE CANTIDAD-TRN          TO COMPRA-PARTIDA     (IND-P),
                                         EXISTENCIA-PARTIDA (IND-P).
300997     ADD  CANTIDAD-TRN          TO CANT-COMPRADA-MES  (MES-NUE).
           ADD  CANTIDAD-TRN          TO EXISTENCIA-SUC(SUCURSAL-ANT).
       ACTUALIZA-FACTURA.
090894     IF (TIPO-COSTO-AMP   NOT = "L" AND
               COSTO-PRECIO-TRN NOT = ZEROES),
               MOVE COSTO-PRECIO-TRN  TO COSTO-ULTIMO-AMP,
               MOVE FECHA-COMP-ANT    TO FECHA-COSTO-AMP.
           MOVE "F"                   TO ORIGEN-PARTIDA     (IND-P).
250794     MOVE COD-PROV-DIST         TO COD-PROV-PARTIDA   (IND-P).
           MOVE FECHA-COMP-ANT        TO FECHA-PARTIDA      (IND-P).
           MOVE VENCIMIENTO-TRN       TO VENCIMIENTO-PARTIDA(IND-P).
           MOVE PARTIDA-AMP(IND-P)    TO IMPUTACION         (1).
130592     PERFORM CONTROLA-REPETICION.
110293     IF NOT SEGURISIMO,
130592        DISPLAY "FACTURA IGNORADA !" LINE 24 BEEP BLINK ELSE
081093        IF RESP-REMITO = "S",
220494           PERFORM ARMA-TRNAMP,
                 MOVE "RF" TO TRN-ANT,
                 PERFORM ARMA-TRNAMP,
                 MOVE "FC" TO TRN-ANT                         ELSE
200494        PERFORM ARMA-TRNAMP.
           GO TO RUT-FC.

130592 CONTROLA-REPETICION SECTION 70.
       INICIA-CONTROL-REPETICION.
110293     MOVE "!" TO RESPUESTA.
           COMPUTE IND-TRN = (IND-P - 1).
       CICLO-CONTROL-REPETICION.
           IF IND-TRN NOT > ZEROES GO TO SAL-CONTROLA-REPETICION.
110293     IF (FECHA-PARTIDA      (IND-P)       =
               FECHA-PARTIDA      (IND-TRN)),
               DISPLAY "ATENCION:" LINE    24 REVERSE,
                       " YA HA INGRESADO A ESTE PRODUCTO EL MISMO DIA !"
                                   POSITION 0 BLINK BEEP,
               PERFORM CONFIRMACION,
               IF NOT SEGURISIMO GO TO SAL-CONTROLA-REPETICION.
           IF (ORIGEN-PARTIDA     (IND-P)   NOT =
               ORIGEN-PARTIDA     (IND-TRN) OR
               COD-PROV-PARTIDA   (IND-P)   NOT =
               COD-PROV-PARTIDA   (IND-TRN) OR
               FECHA-PARTIDA      (IND-P)   NOT =
               FECHA-PARTIDA      (IND-TRN) OR
               COMPRA-PARTIDA     (IND-P)   NOT =
               COMPRA-PARTIDA     (IND-TRN) OR
               VENCIMIENTO-PARTIDA(IND-P)   NOT =
               VENCIMIENTO-PARTIDA(IND-TRN)),
               SUBTRACT 1 FROM IND-TRN,
               GO TO CICLO-CONTROL-REPETICION.
           DISPLAY "LA PARTIDA NUMERO " LINE    24 BLINK BEEP,
                   IND-TRN              POSITION 0 BLINK REVERSE,
                   " TIENE ESOS MISMOS DATOS !"
                                        POSITION 0 BLINK.
110293     PERFORM CONFIRMACION.
       SAL-CONTROLA-REPETICION.


280203 C-REPET-IOMA SECTION 70.
       INICIA-CONTROL-REPETICION-IOMA.
110293     MOVE "!" TO RESPUESTA.
           COMPUTE IND-TRN = (IND-P - 1).
       CICLO-CONTROL-REPETICION-IOMA.
           IF IND-TRN NOT > ZEROES GO TO SAL-CONTROLA-REPETICION-IOMA.
110293     IF (FECHA-PARTIDAIO      (IND-P)       =
               FECHA-PARTIDAIO      (IND-TRN)),
               DISPLAY "ATENCION:" LINE    24 REVERSE,
                       " YA HA INGRESADO A ESTE PRODUCTO EL MISMO DIA !"
                                   POSITION 0 BLINK BEEP,
               PERFORM CONFIRMACION,
               IF NOT SEGURISIMO GO TO SAL-CONTROLA-REPETICION-IOMA.
           IF (ORIGEN-PARTIDAIO     (IND-P)   NOT =
               ORIGEN-PARTIDAIO     (IND-TRN) OR
               COD-PROV-PARTIDAIO   (IND-P)   NOT =
               COD-PROV-PARTIDAIO   (IND-TRN) OR
               FECHA-PARTIDAIO      (IND-P)   NOT =
               FECHA-PARTIDAIO      (IND-TRN) OR
               COMPRA-PARTIDAIO     (IND-P)   NOT =
               COMPRA-PARTIDAIO     (IND-TRN) OR
               VENCIMIENTO-PARTIDAIO(IND-P)   NOT =
               VENCIMIENTO-PARTIDAIO(IND-TRN)),
               SUBTRACT 1 FROM IND-TRN,
               GO TO CICLO-CONTROL-REPETICION-IOMA.
           DISPLAY "LA PARTIDA NUMERO " LINE    24 BLINK BEEP,
                   IND-TRN              POSITION 0 BLINK REVERSE,
                   " TIENE ESOS MISMOS DATOS !"
                                        POSITION 0 BLINK.
110293     PERFORM CONFIRMACION.
       SAL-CONTROLA-REPETICION-IOMA.

      *    ***   ANULACION FACTURA - ANULACION REMITO   ***
      *          """"""""""""""""""""""""""""""""""""

       RUTINA-AF-AR SECTION 70.
       RUT-AF-AR.
           PERFORM PIDE-PRODUCTO.
      *----Tratamiento para la mercaderia en consignacion.
280203     IF SUCURSAL-ANT = 04
               PERFORM BUSQUEDA-PARTIDA-IOMA,
               PERFORM BORRA-PARTIDA-IOMA,
               PERFORM ARMA-TRNAMP,
               GO TO RUT-AF-AR.

           PERFORM BUSQUEDA-PARTIDA.
           IF IND-P > 5 GO TO RUT-AF-AR.
           MOVE PARTIDA-AMP (IND-P)        TO IMPUTACION(1).
300997     SUBTRACT EXISTENCIA-PARTIDA(IND-P)  FROM
300997                  CANT-COMPRADA-MES(MES-NUE) GIVING AUXILIAR.
           SUBTRACT CANTIDAD-TRN               FROM
                    EXISTENCIA-PARTIDA(IND-P)  GIVING INTERMEDIO.
           MOVE "Existencia de la Partida" TO MENSAJE.
           MOVE EXISTENCIA-PARTIDA(IND-P)  TO NUM-MENS.
           PERFORM DISP-MENSAJE-ERROR.
           IF INTERMEDIO > ZEROES,
              MOVE CANTIDAD-TRN           TO CANT-IMP-TRN       (1).
           MOVE INTERMEDIO                TO EXISTENCIA-PARTIDA (IND-P).
           SUBTRACT CANTIDAD-TRN        FROM COMPRA-PARTIDA     (IND-P)
                                      GIVING INTERMEDIO.
           MOVE "Unid.Compradas en Partida"
                                          TO MENSAJE.
           MOVE COMPRA-PARTIDA(IND-P)     TO NUM-MENS.
           PERFORM DISP-MENSAJE-ERROR.
           MOVE INTERMEDIO                TO COMPRA-PARTIDA     (IND-P).
011194     SUBTRACT CANT-IMP-TRN(1)     FROM EXISTENCIA-SUC
                                                         (SUCURSAL-ANT)
                                      GIVING INTERMEDIO.
           MOVE "Existencia en Divisi¢n"  TO MENSAJE.
           MOVE EXISTENCIA-SUC(SUCURSAL-ANT)
                                          TO NUM-MENS.
           PERFORM DISP-MENSAJE-ERROR.
           MOVE INTERMEDIO                TO EXISTENCIA-SUC
                                                         (SUCURSAL-ANT).
           ADD EXISTENCIA-PARTIDA(IND-P)  TO AUXILIAR.
           IF AUXILIAR < ZEROES,
              MOVE ZEROES                 TO AUXILIAR.
300997     MOVE AUXILIAR TO          CANT-COMPRADA-MES(MES-NUE).
           IF EXISTENCIA-PARTIDA(IND-P) = ZEROES,
              MOVE LOW-VALUES             TO PARTIDA-AMP      (IND-P),
              PERFORM CORRIMIENTO-PARTIDAS.
           PERFORM ARMA-TRNAMP.
           GO TO RUT-AF-AR.




       CORRIMIENTO-PARTIDAS SECTION 70.
       INICIA-CORR-PARTIDAS.
           MOVE 1                     TO IND.
           MOVE 2                     TO IND-1.
       CICLO-CORR-PARTIDA.
           IF FECHA-PARTIDA   (IND) = ZEROES,
              MOVE PARTIDA-AMP(IND-1) TO PARTIDA-AMP(IND),
              MOVE LOW-VALUES         TO PARTIDA-AMP(IND-1).
           ADD  1                     TO IND.
           ADD  1                     TO IND-1.
           IF IND-1 NOT > 5 GO TO CICLO-CORR-PARTIDA.
       SAL-CORRIMIENTO-PARTIDA.

250794 BUSQUEDA-PARTIDA SECTION 70.
       INICIA-BUSQUEDA-PARTIDA.
           MOVE 1 TO IND-P.
       CICLO-BUSQUEDA-PARTIDA.
           IF (ORIGEN-PARTIDA     (IND-P) = "?")                 OR
              (COD-PROV-PARTIDA   (IND-P) = COD-PROV-DIST        OR
               COD-PROV-DIST              = 9999)                AND
             ((ORIGEN-PARTIDA     (IND-P) = "R" AND TRN = "FC"   AND
               FECHA-PARTIDA      (IND-P) = FECHA-COMP-ANT       AND
               VENCIMIENTO-PARTIDA(IND-P) = VENCIMIENTO-TRN)     OR
              (FECHA-PARTIDA      (IND-P) = FECHA-COMP-ANT       AND
               VENCIMIENTO-PARTIDA(IND-P) = VENCIMIENTO-TRN      AND
             ((ORIGEN-PARTIDA     (IND-P) = "R" AND TRN = "AR")  OR
              (ORIGEN-PARTIDA     (IND-P) = "F" AND TRN = "AF")))),
               NEXT SENTENCE                                     ELSE
               GO TO INCR-BUSQUEDA-PARTIDA.
           MOVE ORIGEN-PARTIDA     (IND-P) TO VORIGEN-B.
           MOVE COD-PROV-PARTIDA   (IND-P) TO VPROV-B.
           MOVE FECHA-PARTIDA      (IND-P) TO FECHA, PERFORM RUT-FECHA.
           MOVE FECHA                      TO VFECHA-B.
           MOVE VENCIMIENTO-PARTIDA(IND-P) TO FECHA.
           MOVE MES TO AUX MOVE DIA TO MES MOVE AUX TO DIA.
           MOVE FECHA                      TO VVTO-B.
           MOVE COMPRA-PARTIDA     (IND-P) TO VCOMPRADO-B.
           MOVE EXISTENCIA-PARTIDA (IND-P) TO VEXISTENCIA-B.
           INSPECT VPARTIDA-B REPLACING ALL SPACES BY "ú".
       ENCONTRO-PARTIDA.
           DISPLAY "Procesa esta Partida:" LINE    24 LOW REVERSE,
                   VPARTIDA-B              POSITION 0 HIGH,
                   "("                     POSITION 0 LOW,
130994             "!"                     POSITION 0 BLINK,
                   "/N) "                  POSITION 0 LOW ERASE EOL.
           ACCEPT RESPUESTA POSITION 0 PROMPT "?" BLINK NO BEEP;
                  ON EXCEPTION EXC MOVE "N" TO RESPUESTA.
           PERFORM DISP-SPACES.
           IF RESPUESTA = "!" GO TO SAL-BUSQUEDA-PARTIDA.
           IF NOT NEGATIVO    GO TO ENCONTRO-PARTIDA.
       INCR-BUSQUEDA-PARTIDA.
           ADD 1 TO IND-P.
           IF IND-P NOT > 5 GO TO CICLO-BUSQUEDA-PARTIDA.
           DISPLAY "NO EXISTE UNA PARTIDA CON ESOS DATOS ! "
                    LINE 24 BLINK ERASE EOL.
           ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW.
           PERFORM DISP-SPACES.
       SAL-BUSQUEDA-PARTIDA.

250794 BUSQUEDA-PARTIDA-IOMA SECTION 70.
       INICIA-BUSQUEDA-PARTIDA-IOMA.
           MOVE 1 TO IND-P.
       CICLO-BUSQUEDA-PARTIDA-IOMA.

           IF (ORIGEN-PARTIDAIO     (IND-P) = "?")                 OR
              (COD-PROV-PARTIDAIO   (IND-P) = COD-PROV-DIST        OR
               COD-PROV-DIST              = 9999)                AND
             ((ORIGEN-PARTIDAIO     (IND-P) = "R" AND TRN = "FC"   AND
               FECHA-PARTIDAIO      (IND-P) = FECHA-COMP-ANT       AND
               VENCIMIENTO-PARTIDAIO(IND-P) = VENCIMIENTO-TRN)     OR
              (FECHA-PARTIDAIO      (IND-P) = FECHA-COMP-ANT       AND
               VENCIMIENTO-PARTIDAIO(IND-P) = VENCIMIENTO-TRN      AND
             ((ORIGEN-PARTIDAIO     (IND-P) = "R" AND TRN = "AR")  OR
              (ORIGEN-PARTIDAIO     (IND-P) = "F" AND TRN = "AF")))),
               NEXT SENTENCE                                     ELSE
               GO TO INCR-BUSQUEDA-PARTIDA-IOMA.
           MOVE ORIGEN-PARTIDAIO     (IND-P) TO VORIGEN-B.
           MOVE COD-PROV-PARTIDAIO   (IND-P) TO VPROV-B.
           MOVE FECHA-PARTIDAIO      (IND-P) TO FECHA, PERFORM RUT-FECHA.
           MOVE FECHA                      TO VFECHA-B.
           MOVE VENCIMIENTO-PARTIDAIO(IND-P) TO FECHA.
           MOVE MES TO AUX MOVE DIA TO MES MOVE AUX TO DIA.
           MOVE FECHA                      TO VVTO-B.
           MOVE COMPRA-PARTIDAIO     (IND-P) TO VCOMPRADO-B.
           MOVE EXISTENCIA-PARTIDAIO (IND-P) TO VEXISTENCIA-B.
           INSPECT VPARTIDA-B REPLACING ALL SPACES BY "ú".
       ENCONTRO-PARTIDA-IOMA.
           DISPLAY "Procesa esta Partida:" LINE    24 LOW REVERSE,
                   VPARTIDA-B              POSITION 0 HIGH,
                   "("                     POSITION 0 LOW,
130994             "!"                     POSITION 0 BLINK,
                   "/N) "                  POSITION 0 LOW ERASE EOL.
           ACCEPT RESPUESTA POSITION 0 PROMPT "?" BLINK NO BEEP;
                  ON EXCEPTION EXC MOVE "N" TO RESPUESTA.
           PERFORM DISP-SPACES.
           IF RESPUESTA = "!" GO TO SAL-BUSQUEDA-PARTIDA-IOMA.
           IF NOT NEGATIVO    GO TO ENCONTRO-PARTIDA-IOMA.
       INCR-BUSQUEDA-PARTIDA-IOMA.
           ADD 1 TO IND-P.
           IF IND-P NOT > 5 GO TO CICLO-BUSQUEDA-PARTIDA-IOMA.
           DISPLAY "NO EXISTE UNA PARTIDA IOMA CON ESOS DATOS ! "
                    LINE 24 BLINK ERASE EOL.
           ACCEPT RESPUESTA POSITION 0 PROMPT "¯" LOW.
           PERFORM DISP-SPACES.
       SAL-BUSQUEDA-PARTIDA-IOMA.

       BORRA-PARTIDA-IOMA SECTION 70.
       B-PARTIDA-IOMA.
           IF IND-P > 5 GO TO RUT-AF-AR.
           MOVE PARTIDA-AMPIO (IND-P)        TO IMPUTACION(1).
300997*----SUBTRACT EXISTENCIA-PARTIDAIO(IND-P)  FROM
300997*----             CANT-COMPRADA-MES(MES-NUE) GIVING AUXILIAR.
           SUBTRACT CANTIDAD-TRN               FROM
                    EXISTENCIA-PARTIDAIO(IND-P)  GIVING INTERMEDIO.
           MOVE "Existencia de la Partida" TO MENSAJE.
           MOVE EXISTENCIA-PARTIDAIO(IND-P)  TO NUM-MENS.
           PERFORM DISP-MENSAJE-ERROR.
           IF INTERMEDIO > ZEROES,
              MOVE CANTIDAD-TRN     TO CANT-IMP-TRN       (1).
           MOVE INTERMEDIO          TO EXISTENCIA-PARTIDAIO (IND-P).
           SUBTRACT CANTIDAD-TRN        FROM COMPRA-PARTIDAIO   (IND-P)
                                      GIVING INTERMEDIO.
           MOVE "Unid.Compradas en Partida"
                                          TO MENSAJE.
           MOVE COMPRA-PARTIDA(IND-P)     TO NUM-MENS.
           PERFORM DISP-MENSAJE-ERROR.
           MOVE INTERMEDIO                TO COMPRA-PARTIDAIO   (IND-P).
011194     SUBTRACT CANT-IMP-TRN(1)     FROM EXISTENCIA-SUC
                                                         (SUCURSAL-ANT)
                                      GIVING INTERMEDIO.
           MOVE "Existencia en Divisi¢n"  TO MENSAJE.
           MOVE EXISTENCIA-SUC(SUCURSAL-ANT)
                                          TO NUM-MENS.
           PERFORM DISP-MENSAJE-ERROR.
           MOVE INTERMEDIO                TO EXISTENCIA-SUC
                                                         (SUCURSAL-ANT).
           ADD EXISTENCIA-PARTIDAIO(IND-P)  TO AUXILIAR.
           IF AUXILIAR < ZEROES,
              MOVE ZEROES                 TO AUXILIAR.
           IF EXISTENCIA-PARTIDAIO(IND-P) = ZEROES,
              MOVE LOW-VALUES             TO PARTIDA-AMPIO    (IND-P),
              PERFORM CORRIMIENTO-PARTIDAS-IOMA.


       CORRIMIENTO-PARTIDAS-IOMA SECTION 70.
       INICIA-CORR-PARTIDAS-IOMA.
           MOVE 1                     TO IND.
           MOVE 2                     TO IND-1.
       CICLO-CORR-PARTIDA-IOMA.
           IF FECHA-PARTIDAIO   (IND) = ZEROES,
              MOVE PARTIDA-AMPIO(IND-1) TO PARTIDA-AMPIO(IND),
              MOVE LOW-VALUES         TO PARTIDA-AMPIO(IND-1).
           ADD  1                     TO IND.
           ADD  1                     TO IND-1.
           IF IND-1 NOT > 5 GO TO CICLO-CORR-PARTIDA-IOMA.
       SAL-CORRIMIENTO-PARTIDA-IOMA.


       END PROGRAM.
