       IDENTIFICATION                          DIVISION.
       PROGRAM-ID. CLIENTES.
       ENVIRONMENT                             DIVISION.
       CONFIGURATION                           SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT                            SECTION.
       DATA                                    DIVISION.
       WORKING-STORAGE                         SECTION.
       01  WS-MESSAGE  PIC X(20) VALUE SPACES.
       01  WS-SALDO    PIC S9(10) COMP-3 VALUE ZEROS.
       01  WS-SALDO-ED PIC Z.ZZZ.ZZZ.ZZ9,99.

       PROCEDURE                                DIVISION.
           DISPLAY "DIGITE SEU NOME: ". 
           ACCEPT WS-MESSAGE.

           DISPLAY "DIGITE O SALDO: ".
           ACCEPT WS-SALDO.

           DISPLAY "NOME: " WS-MESSAGE.

           COMPUTE WS-SALDO = WS-SALDO * 1,10.
           MOVE WS-SALDO TO WS-SALDO-ED.
           DISPLAY "SALDO ATUAL: " WS-SALDO-ED.

           ADD 500 TO WS-SALDO.
           MOVE WS-SALDO TO WS-SALDO-ED.
           DISPLAY "SALDO ATUAL: " WS-SALDO-ED.
           DISPLAY "-----------------------------"

           SUBTRACT 200 FROM WS-SALDO.
           MOVE WS-SALDO TO WS-SALDO-ED.
           DISPLAY "SALDO ATUAL: " WS-SALDO-ED.
           DISPLAY "-----------------------------"

           MULTIPLY WS-SALDO BY 2 GIVING WS-SALDO.
           MOVE WS-SALDO TO WS-SALDO-ED.
           DISPLAY "SALDO ATUAL: " WS-SALDO-ED.
           DISPLAY "-----------------------------"

           DIVIDE WS-SALDO BY 3 GIVING WS-SALDO.
           MOVE WS-SALDO TO WS-SALDO-ED.
           DISPLAY "SALDO ATUAL: " WS-SALDO-ED.
           DISPLAY "-----------------------------"

           STOP RUN.
