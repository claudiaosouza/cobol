       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTE-XML.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TESTE.
          05 NOME      PIC X(10) VALUE 'CLAUDIA'.
          05 IDADE     PIC 9(2)  VALUE 35.
          05 CIDADE    PIC X(15) VALUE 'CURITIBA'.

       01 XML-SAIDA        PIC X(500) VALUE SPACES.
       01 XML-TAMANHO      PIC 9(5) VALUE 0.
       01 XML-CODIGO       PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
           DISPLAY "Iniciando programa...".
           DISPLAY "TESTE.NOME: " NOME.
           DISPLAY "TESTE.IDADE: " IDADE.
           DISPLAY "TESTE.CIDADE: " CIDADE.
           
           XML GENERATE XML-SAIDA
               FROM TESTE
               COUNT IN XML-TAMANHO
               ON EXCEPTION
                   DISPLAY "*** EXCECAO NO XML GENERATE ***"
                   DISPLAY "XML-CODE: " XML-CODE
                   MOVE XML-CODE TO XML-CODIGO
                   DISPLAY "XML-CODIGO capturado: " XML-CODIGO
               NOT ON EXCEPTION
                   DISPLAY "*** XML GERADO COM SUCESSO ***"
                   DISPLAY "Tamanho: " XML-TAMANHO
                   DISPLAY "Conteudo: " XML-SAIDA(1:XML-TAMANHO)
           END-XML.
           
           DISPLAY "Programa finalizado".
           STOP RUN.
