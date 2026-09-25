      *    OBJECTIFS DU PROGRAMME :
      *    - Calculer le total des dépôts et des retraits sur un livret
      *    - Calculer le montant des intérêts perçus
      *    - Afficher les résultats

      *=================================================================
       IDENTIFICATION DIVISION.
      *=================================================================
       PROGRAM-ID. BILAN-LIVRET.
       
      *=================================================================
       ENVIRONMENT DIVISION.
      *=================================================================
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-LIVRET ASSIGN TO "LIVRET.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS WS-FS-LIVRET.
           
      *=================================================================
       DATA DIVISION.
      *=================================================================
       FILE SECTION.
       FD F-LIVRET.
       01 FS-ENR-LIVRET.
           88 ENDOFFILE VALUE HIGH-VALUES.
           05 TYPE-TX PIC X.
              88 DEPOT   VALUE 'D'.
              88 RETRAIT VALUE 'R'.
           05 MONTANT PIC 9(6)V99.
       
       WORKING-STORAGE SECTION.
       *> FILE STATUS du fichier
       01 WS-FS-LIVRET        PIC 99.
       *> Solde du compte en fin d'année, hors intérêts
       01 WS-SOLDE            PIC S9(6)V99 VALUE ZEROS.
       01 WS-DISP-SOLDE       PIC +++B++9.99.
       *> Total de l'argent déposé
       01 WS-TOTAL-DEPOT      PIC 9(6)V99  VALUE ZEROS.
       01 WS-DISP-DEPOT       PIC +++B++9.99.
       *> Total de l'argent retiré
       01 WS-TOTAL-RETRAIT    PIC S9(6)V99 VALUE ZEROS.
       01 WS-DISP-RETRAIT     PIC +++B++9.99.
       *> Total des intérêts gagnés
       01 WS-TOTAL-INTERETS   PIC S9(6)V99 VALUE ZEROS.
       01 WS-DISP-INTERETS    PIC +++B++9.99.
       *> Solde du compte en fin d'année, avec intérêts
       01 WS-SOLDE-FINAL      PIC S9(6)V99 VALUE ZEROS.
       01 WS-DISP-SOLDE-FINAL PIC +++B++9.99.
       *> Taux d'intérêt à 3 %
       01 WS-TAUX-INTERETS    PIC 9V99    VALUE 0.03.
       
      *=================================================================
       PROCEDURE DIVISION.
      *=================================================================
       0000-MAIN.
           PERFORM 1000-OUVERTURE-LIVRET
           PERFORM 2000-LECTURE-LIVRET
           PERFORM 3000-CALCUL-TOTAUX UNTIL ENDOFFILE
           PERFORM 3010-CALCUL-INTERETS
           PERFORM 3020-CALCUL-SOLDE-FINAL
           PERFORM 4000-AFFICHAGE-RESULTATS
           PERFORM 5000-FERMETURE-LIVRET
           PERFORM 9999-FIN-NORMALE-PGM.

       1000-OUVERTURE-LIVRET.
           OPEN INPUT F-LIVRET.
           IF WS-FS-LIVRET NOT = '00'
              DISPLAY "ERREUR LORS DE L'OUVERTURE DU LIVRET"
              DISPLAY "CODE ERREUR : " WS-FS-LIVRET
              PERFORM 9999-FIN-ANORMALE-PGM
           END-IF.

       2000-LECTURE-LIVRET.
           READ F-LIVRET
              AT END SET ENDOFFILE TO TRUE
           END-READ

           IF WS-FS-LIVRET NOT = '00' AND NOT = '10'
              DISPLAY "ERREUR LORS DE LA LECTURE DU LIVRET"
              DISPLAY "CODE ERREUR : " WS-FS-LIVRET
              PERFORM 9999-FIN-ANORMALE-PGM
           END-IF.

       3000-CALCUL-TOTAUX.
           IF DEPOT
              ADD MONTANT TO WS-TOTAL-DEPOT
              ADD MONTANT TO WS-SOLDE
           ELSE
              IF RETRAIT
                 SUBTRACT MONTANT FROM WS-TOTAL-RETRAIT
                 SUBTRACT MONTANT FROM WS-SOLDE
              ELSE
                 DISPLAY "TYPE DE TRANSACTION INCONNU"
                 PERFORM 9999-FIN-ANORMALE-PGM
              END-IF
           END-IF
           PERFORM 2000-LECTURE-LIVRET.

       3010-CALCUL-INTERETS.
           IF WS-SOLDE > 0
           MULTIPLY WS-SOLDE BY WS-TAUX-INTERETS 
             GIVING WS-TOTAL-INTERETS
           END-IF.

       3020-CALCUL-SOLDE-FINAL.
           ADD WS-TOTAL-INTERETS TO WS-SOLDE GIVING WS-SOLDE-FINAL.

       4000-AFFICHAGE-RESULTATS.
           MOVE WS-SOLDE          TO WS-DISP-SOLDE
           MOVE WS-TOTAL-DEPOT    TO WS-DISP-DEPOT
           MOVE WS-TOTAL-RETRAIT  TO WS-DISP-RETRAIT
           MOVE WS-TOTAL-INTERETS TO WS-DISP-INTERETS
           MOVE WS-SOLDE-FINAL    TO WS-DISP-SOLDE-FINAL

           DISPLAY "--- RÉSUMÉ DU COMPTE POUR L'ANNÉE ÉCOULÉE ---"
           DISPLAY " "
           DISPLAY "TOTAL DES DÉPÔTS EFFECTUÉS SUR L'ANNÉE   : " 
                    WS-DISP-DEPOT " €"
           DISPLAY "TOTAL DES RETRAITS EFFECTUÉS SUR L'ANNÉE : " 
                    WS-DISP-RETRAIT " €"
           DISPLAY "SOLDE FINAL AVANT AJOUT DES INTÉRÊTS     : " 
                    WS-DISP-SOLDE " €"
           DISPLAY "TOTAL DES INTÉRÊTS GAGNÉS                : " 
                    WS-DISP-INTERETS " €"
           DISPLAY "SOLDE FINAL APRÈS AJOUT DES INTÉRÊTS     : " 
                    WS-DISP-SOLDE-FINAL " €".

       5000-FERMETURE-LIVRET.
           CLOSE F-LIVRET 
           IF WS-FS-LIVRET NOT = '00'
              DISPLAY "ERREUR LORS DE LA FERMETURE DU LIVRET"
              DISPLAY "CODE ERREUR : " WS-FS-LIVRET
              PERFORM 9999-FIN-ANORMALE-PGM
           END-IF.

       9999-FIN-ANORMALE-PGM.
           DISPLAY " "
           DISPLAY "#############################################"
           DISPLAY "FIN ANORMALE DU PROGRAMME SUITE A UN PROBLEME"
           DISPLAY "#############################################"
           GOBACK.

       9999-FIN-NORMALE-PGM.
           DISPLAY " "
           DISPLAY "########################"
           DISPLAY "FIN NORMALE DU PROGRAMME"
           DISPLAY "########################"
           GOBACK.
