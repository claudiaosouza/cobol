       IDENTIFICATION                          DIVISION.
       PROGRAM-ID. CLIENTES3.
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
       01  WS-TIPO-CONTA PIC 9 VALUE ZEROS.

       PROCEDURE                                DIVISION.
           DISPLAY "DIGITE SEU NOME: ". 
           ACCEPT WS-MESSAGE.

           DISPLAY "DIGITE O SALDO: ".
           ACCEPT WS-SALDO.

           DISPLAY "QUAL TIPO DE CONTA (1 - FISICA / 2 - JURIDICA): ".
           ACCEPT WS-TIPO-CONTA.

           IF WS-SALDO GREATER THAN OR EQUAL TO 5000 AND
                (WS-TIPO-CONTA EQUAL 1 OR WS-TIPO-CONTA EQUAL 2)
               DISPLAY "NOME: " WS-MESSAGE
               MOVE WS-SALDO TO WS-SALDO-ED
               DISPLAY "SALDO ATUAL: " WS-SALDO-ED 
           ELSE
               EVALUATE TRUE
                   WHEN WS-SALDO LESS THAN 5000
                       DISPLAY "SALDO NAO COMPATIVEL"
                   WHEN WS-TIPO-CONTA NOT EQUAL 1  AND
                        WS-TIPO-CONTA NOT EQUAL 2
                       DISPLAY "TIPO CONTA INVALIDO"
               END-EVALUATE
           END-IF.

           STOP RUN.
