       IDENTIFICATION                          DIVISION.
       PROGRAM-ID. CLIENTES4.
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

       PROCEDURE                                 DIVISION.
       0000-PRINCIPAL                            SECTION.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR UNTIL WS-TIPO-CONTA EQUAL 0
           PERFORM 3000-FINALIZAR
           STOP RUN.
       0000-FIM.                                 EXIT.

       1000-INICIALIZAR                          SECTION.
      
           DISPLAY "DIGITE SEU NOME: ".
           ACCEPT WS-MESSAGE.

           DISPLAY "DIGITE O SALDO: ".
           ACCEPT WS-SALDO.

           DISPLAY "TIPO CONTA (1-FISICA / 2-JURIDICA / 0-SAIR): ".
           ACCEPT WS-TIPO-CONTA.
       1000-FIM.                                 EXIT.

       2000-PROCESSAR                            SECTION.
    
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
           DISPLAY "---------------------"
           
           PERFORM 1000-INICIALIZAR.
       2000-FIM.                                 EXIT.

       3000-FINALIZAR                            SECTION.
           DISPLAY "FIM DO PROCESSAMENTO".
       3000-FIM.                                 EXIT.
