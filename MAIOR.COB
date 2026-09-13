       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIOR.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       FILE SECTION.
       
       WORKING-STORAGE SECTION.
       01 WS-IND              PIC 9(3) VALUE ZEROS.
       01 WS-MAIOR            PIC 9(5) VALUE ZEROS.
       01 WS-QTDE             PIC 9(2) VALUE ZEROS.
       01 VALORES.
           05 WS-ITEM OCCURS 10 TIMES DEPENDING ON WS-QTDE.
               10 WS-VALOR  PIC 9(5). 
       
       PROCEDURE DIVISION.
           DISPLAY 'INFORME A QTDE DE VALORES QUE DESEJA INFORMAR: '
           ACCEPT WS-QTDE.
           
           PERFORM VARYING WS-IND FROM 1 BY 1 UNTIL WS-IND > WS-QTDE
               DISPLAY 'INFORME O ' WS-IND ':'
               ACCEPT WS-VALOR(WS-IND)
           END-PERFORM.

           MOVE WS-VALOR(1) TO WS-MAIOR.
           
           PERFORM VARYING WS-IND FROM 2 BY 1 UNTIL WS-IND > WS-QTDE
               IF WS-VALOR(WS-IND) > WS-MAIOR
                   MOVE WS-VALOR(WS-IND) TO WS-MAIOR
               END-IF
           END-PERFORM.
           
           DISPLAY 'O MAIOR VALOR E: ' WS-MAIOR.
           
           STOP RUN.
           