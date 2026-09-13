       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULA-FATORIAL.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       FILE SECTION.
       
       WORKING-STORAGE SECTION.
       01 WS-NUMERO           PIC 9(3) VALUE 0.
       01 WS-FATORIAL         PIC 9(10) VALUE 1.
       01 WS-CONTADOR         PIC 9(3) VALUE 0.
       01 WS-RESULTADO        PIC Z(10).
       
       PROCEDURE DIVISION.
           DISPLAY 'INFORME O NUMERO PARA CALCULAR O FATORIAL: '
           ACCEPT WS-NUMERO.
           
           IF WS-NUMERO = 0 OR WS-NUMERO = 1
               MOVE 1 TO WS-FATORIAL
           ELSE
               PERFORM VARYING WS-CONTADOR FROM WS-NUMERO BY -1 
                       UNTIL WS-CONTADOR < 1
                   COMPUTE WS-FATORIAL = WS-FATORIAL *  WS-CONTADOR
               END-PERFORM
           END-IF.

           MOVE WS-FATORIAL TO WS-RESULTADO.

           DISPLAY "FATORIAL DE " WS-NUMERO " = " 
                                 FUNCTION TRIM(WS-RESULTADO).
           
           STOP RUN.
           