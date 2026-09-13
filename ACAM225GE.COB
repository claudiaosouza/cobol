       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACAM225.

       ENVIRONMENT                                 DIVISION.
       INPUT-OUTPUT                                SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO '.\OPERACOES.INPUT.TXT'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-OPERACOES-INPUT.

           SELECT XMLFILE ASSIGN TO '.\ACAM225.OUTPUT.XML'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-ACAM225-OUTPUT.

       DATA                                        DIVISION.
       FILE                                        SECTION.

       FD  INFILE
           RECORDING MODE IS F.
           COPY BOOKOPER.CPY.
           
       FD  XMLFILE.
       01  XML-LINHA            PIC X(1000).

       WORKING-STORAGE SECTION.
       01  FS-OPERACOES-INPUT   PIC 9(02).
       01  FS-ACAM225-OUTPUT    PIC 9(02).
       01  WRK-IDENT-EMISSOR    PIC X(08) VALUES '60746948'.
       01  WRK-IDENT-DESTINA    PIC X(08) VALUES '00038166'.
       01  WRK-DATA-BASE        PIC X(07) VALUES SPACES.
       01  WRK-REG-OP-CAML      PIC Z(12).
       01  WRK-VALOR-2  PIC ZZZZZZZZZZZZZZZZZZZ.ZZ.
       01  WRK-VALOR-1  PIC ZZZZZZZZZZZZZZZZZZZ.Z.
       01  WRK-VALOR-0  PIC ZZZZZZZZZZZZZZZZZZZ.
       01  WRK-XML-OUTPUT PIC X(1000) VALUES SPACES.
       01  WRK-XML-LENGTH PIC 9(05).
       01  WRK-XML-CODE   PIC 9(05).

       01  Grupo_ACAM225_TransacaoEFX.
           05 NumCtrlIF                    PIC X(020).
           05 CNPJBasePrestEFX             PIC X(008).
           05 DtTransacao                  PIC X(010).
           05 TpOperacao                   PIC X(001).
           05 FormaTpOperacao              PIC X(001).
           05 RegOpCaml                    PIC X(012).
           05 CNPJ_CPFCtrapartExtr         PIC X(014).
           05 TpPessoaUsu                  PIC X(001). 
           05 IdentdPessoaUsu              PIC X(025).
           05 NomUsu                       PIC X(080).
           05 CodPaisEmsIdentdPessoaUsu    PIC X(002).
           05 TpTransacao                  PIC X(002).
           05 IndrTransacao                PIC X(001).
           05 NomIntmExtr                  PIC X(100).
           05 CodPaisIntmExtr              PIC X(002).
           05 NomDestRemExtr              PIC X(100).
           05 CodPaisDestRemExtr          PIC X(002).
           05 CodMoedaISO                  PIC X(003).
           05 CodFormaEntrRecMN            PIC X(001).
           05 VlrME                        PIC X(021).
           05 VlrMN                        PIC X(021).
           05 VlrMNEftEntr                 PIC X(021).

       PROCEDURE                                   DIVISION.
       0000-PRINCIPAL                              SECTION.
           PERFORM 1000-INICIALIZAR.
           PERFORM 1300-PREENCHER-IDENT-ARQ.
           PERFORM 2000-PROCESSAR UNTIL FS-OPERACOES-INPUT EQUAL 10.
           PERFORM 3000-FINALIZAR.
           STOP RUN.
       0000-FIM.                                   EXIT.

       1000-INICIALIZAR                            SECTION.
           OPEN INPUT INFILE.
           OPEN OUTPUT XMLFILE.

           IF FS-OPERACOES-INPUT NOT EQUAL ZEROS
               DISPLAY "ERRO AO ABRIR O ARQUIVO - ENTRADA"
               GOBACK
           END-IF.

            IF FS-ACAM225-OUTPUT NOT EQUAL ZEROS
               DISPLAY "ERRO AO ABRIR O ARQUIVO - SAIDA"
               GOBACK
           END-IF.   
   

           PERFORM 1100-PREENCHER-CABECALHO.
           PERFORM 1200-LEITURA.
         
       1000-FIM.

       1100-PREENCHER-CABECALHO                    SECTION.
                  
           MOVE '<?xml version="1.0" encoding="UTF-8"?>' TO XML-LINHA.
           WRITE XML-LINHA.

           MOVE '<DOC xmlns="http://www.bcb.gov.br/MES/ACAM225.xsd">'
                                                         TO XML-LINHA.
           WRITE XML-LINHA.

           MOVE '<BCARQ>' TO XML-LINHA.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<IdentdEmissor>'
                   WRK-IDENT-EMISSOR
                   '</IdentdEmissor>'
                   DELIMITED BY SIZE
                   INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<IdentdDestinatario>'
                   WRK-IDENT-DESTINA
                   '</IdentdDestinatario>'
                   DELIMITED BY SIZE
                   INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.
           
           MOVE '</BCARQ>' TO XML-LINHA.
           WRITE XML-LINHA.

       1100-FIM.                                   EXIT.

       1200-LEITURA                                SECTION.
           READ INFILE.
       1200-FIM. 
       
       1300-PREENCHER-IDENT-ARQ                    SECTION.
           MOVE '<SISARQ>' TO XML-LINHA.
           WRITE XML-LINHA.

           MOVE '<ACAM225>' TO XML-LINHA.
           WRITE XML-LINHA.

           MOVE '<NomArq>ACAM225</NomArq>' TO XML-LINHA.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<CNPJBaseInf>'
                   WRK-IDENT-DESTINA
                   '</CNPJBaseInf>'
                   DELIMITED BY SIZE
                   INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.
           
           MOVE IN-DT-TRANSACAO TO WRK-DATA-BASE.
           MOVE SPACES TO XML-LINHA.
           STRING '<MesAnoReferencia>'
                   WRK-DATA-BASE
                   '</MesAnoReferencia>'
                   DELIMITED BY SIZE
                   INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.
          
       1300-FIM.                                   EXIT.
                                
       2000-PROCESSAR                              SECTION.

           IF IN-DT-TRANSACAO EQUALS SPACES
               GO TO 2000-FIM
           END-IF.

           MOVE SPACES  TO Grupo_ACAM225_TransacaoEFX.
           MOVE IN-NUM-CTRL-IF                     TO NumCtrlIF.
           MOVE IN-CNPJ-BASE-EFX                   TO CNPJBasePrestEFX.
           MOVE IN-DT-TRANSACAO                    TO DtTransacao.
           MOVE IN-TP-OPERACAO                     TO TpOperacao.
           MOVE IN-FORMA-TP-OPERACAO               TO FormaTpOperacao.
           MOVE IN-REG-OP-CAML                     TO WRK-REG-OP-CAML.
           MOVE FUNCTION TRIM(WRK-REG-OP-CAML)     TO RegOpCaml.
           MOVE FUNCTION TRIM(IN-CNPJ-CPF-CONTRAPARTE)
                                                TO CNPJ_CPFCtrapartExtr. 
           MOVE IN-TP-PESSOA-USU                   TO TpPessoaUsu.
           MOVE IN-IDENT-PESSOA-USU                TO IdentdPessoaUsu.
           MOVE FUNCTION TRIM(IN-NOME-USU)         TO NomUsu.
           MOVE IN-COD-PAIS-USU            TO CodPaisEmsIdentdPessoaUsu.
           MOVE IN-TP-TRANSACAO                    TO TpTransacao.
           MOVE IN-INDR-TRANSACAO                  TO IndrTransacao.
           MOVE FUNCTION TRIM(IN-NOME-INTM-EXTR)   TO NomIntmExtr. 
           MOVE IN-COD-PAIS-INTM-EXTR              TO CodPaisIntmExtr.
           MOVE FUNCTION TRIM(IN-NOME-DEST-REM-EXTR) TO NomDestRemExtr.
           MOVE IN-COD-PAIS-DEST-EXTR             TO CodPaisDestRemExtr.
           MOVE IN-COD-MOEDA-ISO                   TO CodMoedaISO.
           MOVE IN-COD-FORMA-ENTR-MN               TO CodFormaEntrRecMN.

           IF (IN-VLR-ME = FUNCTION INTEGER(IN-VLR-ME))
               MOVE IN-VLR-ME TO WRK-VALOR-0
               MOVE FUNCTION TRIM(WRK-VALOR-0) TO VlrME
           ELSE
               IF (IN-VLR-ME * 10 = FUNCTION INTEGER(IN-VLR-ME * 10))
                   MOVE IN-VLR-ME TO WRK-VALOR-1
                   MOVE FUNCTION TRIM(WRK-VALOR-1) TO VlrME
               ELSE
                   MOVE IN-VLR-ME TO WRK-VALOR-2
                   MOVE FUNCTION TRIM(WRK-VALOR-2) TO VlrME
                END-IF
           END-IF.

           IF (IN-VLR-MN = FUNCTION INTEGER(IN-VLR-MN))
               MOVE IN-VLR-MN TO WRK-VALOR-0
               MOVE FUNCTION TRIM(WRK-VALOR-0) TO VlrMN
           ELSE
               IF (IN-VLR-MN * 10 = FUNCTION INTEGER(IN-VLR-MN * 10))
                   MOVE IN-VLR-MN TO WRK-VALOR-1
                   MOVE FUNCTION TRIM(WRK-VALOR-1) TO VlrMN
               ELSE
                   MOVE IN-VLR-MN TO WRK-VALOR-2
                   MOVE FUNCTION TRIM(WRK-VALOR-2) TO VlrMN
                END-IF
           END-IF.

           IF (IN-VLR-MN-EFT-ENTR 
                                = FUNCTION INTEGER(IN-VLR-MN-EFT-ENTR))
               MOVE IN-VLR-MN-EFT-ENTR TO WRK-VALOR-0
               MOVE FUNCTION TRIM(WRK-VALOR-0) TO VlrMNEftEntr
           ELSE
               IF (IN-VLR-MN-EFT-ENTR * 10 
                            = FUNCTION INTEGER(IN-VLR-MN-EFT-ENTR * 10))
                   MOVE IN-VLR-MN-EFT-ENTR TO WRK-VALOR-1
                   MOVE FUNCTION TRIM(WRK-VALOR-1) TO VlrMNEftEntr
               ELSE
                   MOVE IN-VLR-MN TO WRK-VALOR-2
                   MOVE FUNCTION TRIM(WRK-VALOR-2) TO VlrMNEftEntr
                END-IF
           END-IF.
          
           MOVE SPACES TO WRK-XML-OUTPUT.
           XML GENERATE WRK-XML-OUTPUT
               FROM Grupo_ACAM225_TransacaoEFX
               COUNT IN WRK-XML-LENGTH
               ON EXCEPTION
                   MOVE XML-CODE TO WRK-XML-CODE
                   DISPLAY '*** ON EXCEPTION *** XML-CODE: ' XML-CODE
               NOT ON EXCEPTION
                   DISPLAY '*** NOT ON EXCEPTION ***'
                   DISPLAY 'XML-LENGTH..: ' WRK-XML-LENGTH
                   DISPLAY 'XML.....: ' WRK-XML-OUTPUT(1:WRK-XML-LENGTH)
               END-XML.

           DISPLAY 'XML-CODE....: ' XML-CODE
           DISPLAY 'WRK-XML-CODE: ' WRK-XML-CODE
           DISPLAY 'XML-LENGTH..: ' WRK-XML-LENGTH


           DISPLAY 'VALORES:' Grupo_ACAM225_TransacaoEFX
           DISPLAY 'XML GERADO:' WRK-XML-OUTPUT(1:WRK-XML-LENGTH)

           MOVE SPACES TO XML-LINHA.
           WRITE XML-LINHA FROM WRK-XML-OUTPUT(1:WRK-XML-LENGTH).
           
           PERFORM 1200-LEITURA.

       2000-FIM.                                   EXIT.

       3000-FINALIZAR                              SECTION.

           MOVE '</ACAM225>' TO XML-LINHA.
           WRITE XML-LINHA.

           MOVE '</SISARQ>' TO XML-LINHA.
           WRITE XML-LINHA.

           MOVE '</DOC>' TO XML-LINHA.
           WRITE XML-LINHA.

           CLOSE INFILE.
           CLOSE XMLFILE.
       3000-FIM.                                   EXIT.
