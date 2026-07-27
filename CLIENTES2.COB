       IDENTIFICATION                          DIVISION.
       PROGRAM-ID. CLIENTES2.
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

           IF WS-SALDO GREATER THAN OR EQUAL TO 5000
               DISPLAY "NOME: " WS-MESSAGE
               MOVE WS-SALDO TO WS-SALDO-ED
               DISPLAY "SALDO ATUAL: " WS-SALDO-ED
       
               EVALUATE WS-TIPO-CONTA
                   WHEN 1
                       DISPLAY "CONTA FISICA"
                   WHEN 2
                       DISPLAY "CONTA JURIDICA"
                   WHEN OTHER
                       DISPLAY "TIPO DE CONTA INVALIDO"
               END-EVALUATE
           ELSE
               DISPLAY "SALDO NAO COMPATIVEL"
           END-IF.

           STOP RUN.
