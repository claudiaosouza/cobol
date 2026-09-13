       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACAM225.

       ENVIRONMENT                                 DIVISION.
       INPUT-OUTPUT                                SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO '.\OPERACOES.INPUT.TXT'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-OPERACOES-INPUT.

           SELECT XMLFILE ASSIGN TO '.\ACAM225.OUTPUT.XML'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-ACAM225-OUTPUT.

       DATA                                        DIVISION.
       FILE                                        SECTION.

       FD  INFILE
           RECORDING MODE IS F.
       01  IN-REC.
           05 IN-NUM-CTRL-IF           PIC X(020).
           05 IN-CNPJ-BASE-EFX         PIC X(008).
           05 IN-DT-TRANSACAO          PIC X(010).
           05 IN-TP-OPERACAO           PIC X(001).
           05 IN-FORMA-TP-OPERACAO     PIC X(001).
           05 IN-REG-OP-CAML           PIC 9(012).
           05 IN-CNPJ-CPF-CONTRAPARTE  PIC X(014).
           05 IN-TP-PESSOA-USU         PIC X(001).
           05 IN-IDENT-PESSOA-USU      PIC X(025).
           05 IN-NOME-USU              PIC X(080).
           05 IN-COD-PAIS-USU          PIC X(002).
           05 IN-TP-TRANSACAO          PIC X(002).
           05 IN-INDR-TRANSACAO        PIC X(001).
           05 IN-NOME-INTM-EXTR        PIC X(100).
           05 IN-COD-PAIS-INTM-EXTR    PIC X(002).
           05 IN-NOME-DEST-REM-EXTR    PIC X(0100).
           05 IN-COD-PAIS-DEST-EXTR    PIC X(002).
           05 IN-COD-MOEDA-ISO         PIC X(003).
           05 IN-COD-FORMA-ENTR-MN     PIC X(001).
           05 IN-VLR-ME                PIC 9(19)V99.
           05 IN-VLR-MN                PIC 9(19)V99.
           05 IN-VLR-MN-EFT-ENTR       PIC 9(19)V99.
    
       FD  XMLFILE.
       01  XML-LINHA            PIC X(500).

       WORKING-STORAGE SECTION.
       01  EOF-FLAG             PIC X VALUE "N".
       01  FS-OPERACOES-INPUT   PIC 9(02).
       01  FS-ACAM225-OUTPUT    PIC 9(02).
       01  WRK-IDENT-EMISSOR    PIC X(08) VALUES '60746948'.
       01  WRK-IDENT-DESTINA    PIC X(08) VALUES '00038166'.
       01  WRK-DATA-BASE        PIC X(07) VALUES SPACES.
       01  WRK-REG-OP-CAML      PIC Z(12).
       01  WRK-VALOR-2  PIC ZZZZZZZZZZZZZZZZZZZ.ZZ.
       01  WRK-VALOR-1  PIC ZZZZZZZZZZZZZZZZZZZ.Z.
       01  WRK-VALOR-0  PIC ZZZZZZZZZZZZZZZZZZZ.
       01  WKR-FLAG-IDENT PIC X(01) VALUES 'N'.

       PROCEDURE                                   DIVISION.
       0000-PRINCIPAL                              SECTION.
           PERFORM 1000-INICIALIZAR.
           PERFORM UNTIL FS-OPERACOES-INPUT EQUALS 10
               PERFORM 1200-LEITURA
               IF FS-OPERACOES-INPUT NOT EQUALS 10
                   PERFORM 2000-PROCESSAR
               END-IF
           END-PERFORM.
           PERFORM 3000-FINALIZAR.
           STOP RUN.
       0000-FIM.                                   EXIT.

       1000-INICIALIZAR                            SECTION.
           OPEN INPUT INFILE.
           OPEN OUTPUT XMLFILE.
           IF FS-OPERACOES-INPUT NOT EQUAL ZEROS OR
              FS-ACAM225-OUTPUT NOT EQUAL ZEROS
               DISPLAY "ERRO AO ABRIR O ARQUIVO"
               GOBACK
           END-IF.   

           PERFORM 1100-PREENCHER-CABECALHO.
         
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
           MOVE SPACES TO IN-REC.
           READ INFILE
               AT END MOVE 10 TO FS-OPERACOES-INPUT
           END-READ.
E   
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

           IF WKR-FLAG-IDENT EQUALS 'N'
               PERFORM 1300-PREENCHER-IDENT-ARQ
               MOVE 'S' TO WKR-FLAG-IDENT
           END-IF.

           MOVE '<Grupo_ACAM225_Transacao_EFX>' TO XML-LINHA.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<NumCtrlIF>'
                  IN-NUM-CTRL-IF
                  '</NumCtrlIF>' 
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.
           
           MOVE SPACES TO XML-LINHA.
           STRING '<CNPJBasePrestEFX>'
                  IN-CNPJ-BASE-EFX 
                  '</CNPJBasePrestEFX>' 
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<DtTransacao>' 
                  IN-DT-TRANSACAO 
                  '</DtTransacao>' 
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<TpOperacao>'
                  IN-TP-OPERACAO
                  '</TpOperacao>' 
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<FormaTpOperacao>'
                  IN-FORMA-TP-OPERACAO
                  '</FormaTpOperacao>'
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.
           
           MOVE IN-REG-OP-CAML TO WRK-REG-OP-CAML.
           MOVE SPACES TO XML-LINHA.
           STRING '<RegOpCaml>'
                  FUNCTION TRIM(WRK-REG-OP-CAML)
                  '</RegOpCaml>'
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.
         
           MOVE SPACES TO XML-LINHA.
           STRING '<CNPJ_CPFCtrapartExtr>'
                  FUNCTION TRIM(IN-CNPJ-CPF-CONTRAPARTE)
                  '</CNPJ_CPFCtrapartExtr>'
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.
               
           
           MOVE SPACES TO XML-LINHA.
           STRING '<TpPessoaUsu>'
                  IN-TP-PESSOA-USU
                  '</TpPessoaUsu>'
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<IdentPessoaUsu>'
                  FUNCTION TRIM(IN-IDENT-PESSOA-USU)
                  '</IdentPessoaUsu>'
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.
           
           MOVE SPACES TO XML-LINHA.
           STRING '<NomUsu>'
                  FUNCTION TRIM(IN-NOME-USU)
                  '</NomUsu>'
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<CodPaisEmsIdentdPessoaUsu>'
                  IN-COD-PAIS-USU
                  '</CodPaisEmsIdentdPessoaUsu>'
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<TpTransacao>'
                  IN-TP-TRANSACAO
                  '</TpTransacao>' 
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<IndrTransacao>'
                  IN-INDR-TRANSACAO
                  '</IndrTransacao>'
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA. 

           MOVE SPACES TO XML-LINHA.
           STRING '<NomeIntmExtr>'
                  FUNCTION TRIM(IN-NOME-INTM-EXTR)
                  '</NomeIntmExtr>'
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<CodPaisIntmExtr>'
                  IN-COD-PAIS-INTM-EXTR
                  '</CodPaisIntmExtr>'
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<NomeDestRemExtr>'
                  FUNCTION TRIM(IN-NOME-DEST-REM-EXTR)
                  '</NomeDestRemExtr>'
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.


           MOVE SPACES TO XML-LINHA.
           STRING '<CodPaisDestExtr>'
                  IN-COD-PAIS-DEST-EXTR
                  '</CodPaisDestExtr>'
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.
            
           MOVE SPACES TO XML-LINHA.
           STRING '<CodMoedaISO>'
                  IN-COD-MOEDA-ISO
                  '</CodMoedaISO>'
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.

           MOVE SPACES TO XML-LINHA.
           STRING '<CodFormaEntrMN>'
                  IN-COD-FORMA-ENTR-MN
                  '</CodFormaEntrMN>' 
                  DELIMITED BY SIZE
                  INTO XML-LINHA
           END-STRING.
           WRITE XML-LINHA.
           
           IF (IN-VLR-ME = FUNCTION INTEGER(IN-VLR-ME))
               MOVE IN-VLR-ME TO WRK-VALOR-0
               MOVE SPACES TO XML-LINHA
               STRING '<VlrME>'
                       FUNCTION TRIM(WRK-VALOR-0)
                       '</VlrME>'
                       DELIMITED BY SIZE
                       INTO XML-LINHA
               END-STRING
           ELSE
               IF (IN-VLR-ME * 10 = FUNCTION INTEGER(IN-VLR-ME * 10))
                   MOVE IN-VLR-ME TO WRK-VALOR-1
                   MOVE SPACES TO XML-LINHA
                   STRING '<VlrME>'
                           FUNCTION TRIM(WRK-VALOR-1)
                           '</VlrME>'
                           DELIMITED BY SIZE
                           INTO XML-LINHA
                   END-STRING
               ELSE
                   MOVE IN-VLR-ME TO WRK-VALOR-2
                   MOVE SPACES TO XML-LINHA
                   STRING '<VlrME>'
                           FUNCTION TRIM(WRK-VALOR-2)
                           '</VlrME>'
                           DELIMITED BY SIZE
                           INTO XML-LINHA
                   END-STRING
               END-IF
           END-IF.
           WRITE XML-LINHA.
           
           IF (IN-VLR-MN = FUNCTION INTEGER(IN-VLR-MN))
               MOVE IN-VLR-MN TO WRK-VALOR-0
               MOVE SPACES TO XML-LINHA
               STRING '<VlrMN>'
                       FUNCTION TRIM(WRK-VALOR-0)
                       '</VlrMN>'
                       DELIMITED BY SIZE
                       INTO XML-LINHA
               END-STRING
           ELSE
               IF (IN-VLR-MN * 10 = FUNCTION INTEGER(IN-VLR-MN * 10))
                   MOVE IN-VLR-MN TO WRK-VALOR-1
                   MOVE SPACES TO XML-LINHA
                   STRING '<VlrMN>'
                           FUNCTION TRIM(WRK-VALOR-1)
                           '</VlrMN>'
                           DELIMITED BY SIZE
                           INTO XML-LINHA
                   END-STRING
               ELSE
                   MOVE IN-VLR-MN TO WRK-VALOR-2
                   MOVE SPACES TO XML-LINHA
                   STRING '<VlrMN>'
                           FUNCTION TRIM(WRK-VALOR-2)
                           '</VlrMN>'
                           DELIMITED BY SIZE
                           INTO XML-LINHA
                   END-STRING
               END-IF
           END-IF.
           WRITE XML-LINHA.

           IF (IN-VLR-MN-EFT-ENTR = 
              FUNCTION INTEGER(IN-VLR-MN-EFT-ENTR))
               MOVE IN-VLR-MN-EFT-ENTR TO WRK-VALOR-0
               MOVE SPACES TO XML-LINHA
               STRING '<VlrMNEftEntr>'
                       FUNCTION TRIM(WRK-VALOR-0)
                       '</VlrMNEftEntr>'
                       DELIMITED BY SIZE
                       INTO XML-LINHA
               END-STRING
           ELSE
               IF (IN-VLR-MN-EFT-ENTR * 10 = 
                  FUNCTION INTEGER(IN-VLR-MN-EFT-ENTR * 10))
                   MOVE IN-VLR-MN-EFT-ENTR TO WRK-VALOR-1
                   MOVE SPACES TO XML-LINHA
                   STRING '<VlrMNEftEntr>'
                           FUNCTION TRIM(WRK-VALOR-1)
                           '</VlrMNEftEntr>'
                           DELIMITED BY SIZE
                           INTO XML-LINHA
                   END-STRING
               ELSE
                   MOVE IN-VLR-MN-EFT-ENTR TO WRK-VALOR-2
                   MOVE SPACES TO XML-LINHA
                   STRING '<VlrMNEftEntr>'
                           FUNCTION TRIM(WRK-VALOR-2)
                           '</VlrMNEftEntr>'
                           DELIMITED BY SIZE
                           INTO XML-LINHA
                   END-STRING
               END-IF
           END-IF.
           WRITE XML-LINHA.

           MOVE '</Grupo_ACAM225_Transacao_EFX>' TO XML-LINHA.
           WRITE XML-LINHA.

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
