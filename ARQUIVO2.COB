       IDENTIFICATION                          DIVISION.
       PROGRAM-ID. CLIENTES5.
       ENVIRONMENT                             DIVISION.
       CONFIGURATION                           SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT                            SECTION.
       FILE-CONTROL.
           SELECT EARQCLIE ASSIGN TO ".\EARQCLIE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-EARQCLIE.

           SELECT SARQCLIE ASSIGN TO ".\SARQCLIE.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-SARQCLIE.
       DATA                                    DIVISION.
       FILE                                    SECTION.
       FD  EARQCLIE
           RECORDING MODE IS F.
       01  REGISTRO.
           05 REG-AGENCIA    PIC X(4).
           05 REG-CONTA      PIC X(5).
           05 REG-NOME       PIC X(30).
           05 REG-GERENTE    PIC X(003).
           05 REG-TIPO-CONTA PIC 9(01).
           05 REG-SALDO      PIC 9(10)V99.     
      *     LAYOUT 55 POSICOES

       FD   SARQCLIE
           RECORDING MODE IS F.
       01  REG-SAIDA         PIC X(22).

       WORKING-STORAGE                         SECTION.
      *    ------------VARIAVEIS DE STATUS 
       01  FS-EARQCLIE       PIC 9(02).
       01  FS-SARQCLIE       PIC 9(02).

      *    ------------VARIAVEIS ACUMULADORAS 
       01  WRK-CONTADOR      PIC 9(05) VALUES ZEROS.
       01  WRK-CONT-VALIDO   PIC 9(05) VALUES ZEROS.
       01  WRK-CONT-INVALID  PIC 9(05) VALUES ZEROS.

       PROCEDURE                                 DIVISION.

       0000-PRINCIPAL                            SECTION.
           PERFORM 1000-INICIALIZAR.
           PERFORM 2000-PROCESSAR UNTIL FS-EARQCLIE EQUAL 10.
           PERFORM 3000-FINALIZAR.
           STOP RUN.
       0000-FIM.                                 EXIT.

       1000-INICIALIZAR                          SECTION.
           OPEN INPUT EARQCLIE.
           OPEN OUTPUT SARQCLIE.
           IF FS-EARQCLIE NOT EQUAL ZEROS OR
              FS-SARQCLIE NOT EQUAL ZEROS
               DISPLAY "ERRO AO ABRIR O ARQUIVO"
               GOBACK
           END-IF.  

           PERFORM 1100-LEITURA. 
      
       1000-FIM.
       
       1100-LEITURA                              SECTION.
           READ EARQCLIE.
       1100-FIM.                                 EXIT.

       2000-PROCESSAR                            SECTION.
           
           ADD 1 TO WRK-CONTADOR.
   
           IF REG-SALDO GREATER THAN OR EQUAL TO 5000 AND
                (REG-TIPO-CONTA EQUAL 1 OR REG-TIPO-CONTA EQUAL 2)
               
               STRING REG-AGENCIA    DELIMITED BY SIZE
                      REG-CONTA      DELIMITED BY SIZE
                      REG-TIPO-CONTA DELIMITED BY SIZE
                      REG-SALDO      DELIMITED BY SIZE
               INTO REG-SAIDA

               WRITE REG-SAIDA

               IF FS-SARQCLIE NOT EQUAL ZEROS
                   DISPLAY "ERRO AO GRAVAR ARQUIVO"
               END-IF

               ADD 1 TO WRK-CONT-VALIDO
           ELSE
               ADD 1 TO WRK-CONT-INVALID
           END-IF.
       
           PERFORM 1100-LEITURA.
           
       2000-FIM.                                 EXIT.

       3000-FINALIZAR                            SECTION.
           CLOSE EARQCLIE.
           CLOSE SARQCLIE.
           DISPLAY "----------------------".
           DISPLAY "----ESTATISTICA ------".

           DISPLAY "REG LIDOS.....: " WRK-CONTADOR.
           DISPLAY "REG VALIDOS...: " WRK-CONT-VALIDO.
           DISPLAY "REG INVALIDOS.: " WRK-CONT-INVALID.
           DISPLAY "FIM DO PROCESSAMENTO".
       3000-FIM.                                 EXIT.
