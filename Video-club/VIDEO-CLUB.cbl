      *    Exercice sur la manipulation des fichiers VSAM

      *    OBJECTIFS DU PROGRAMME :
      *    - Créer un fichier indexé à partir d'un fichier séquzntiel
      *    - Permettre à l'utilisateur de vérifier si un film est 
      *      disponible dans le vidéoclub
      *    - Permettre à l'utilisateur de modifier ou supprimer un film
      *      de la liste

      *=================================================================
       IDENTIFICATION DIVISION.
      *=================================================================
       PROGRAM-ID. VIDEO-CLUB.
       
      *=================================================================
       ENVIRONMENT DIVISION.
      *=================================================================
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           *> Fichier séquentiel qui contient les films
           SELECT F-FILMS-SEQ ASSIGN TO "FILMS.DAT"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS WS-FS-FILMS.

           *> Fichier indexé dans lequel on va copier les films
           SELECT F-FILMS-IDX ASSIGN TO "FILMS-IDX.DAT"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           RECORD KEY   IS FS-CODE-IDX
           ALTERNATE RECORD KEY IS FS-TITRE-IDX
           FILE STATUS WS-FS-FILMS.
           
      *=================================================================
       DATA DIVISION.
      *=================================================================
       FILE SECTION.
       FD F-FILMS-SEQ.
       01 FS-INFO-FILM-SEQ.
           88 FS-ENDFILE-SEQ VALUE HIGH-VALUE.
           05 FS-CODE-SEQ    PIC 9(05).
           05 FS-TITRE-SEQ   PIC X(28).

       FD F-FILMS-IDX.
       01 FS-INFO-FILM-IDX.
           88 FS-ENDFILE-IDX VALUE HIGH-VALUE.
           05 FS-CODE-IDX    PIC 9(05).
           05 FS-TITRE-IDX   PIC X(28).
       
       WORKING-STORAGE SECTION.
       01 WS-FS-FILMS     PIC 99.
       01 WS-CHOIX-ACTION PIC 9.
       01 WS-INFO-FILM.
           05 WS-CODE-SAISI  PIC X(05).
           05 WS-TITRE-SAISI PIC X(28).
       
      *=================================================================
       PROCEDURE DIVISION.
      *=================================================================
       0000-INIT-FICHIER-INDEXE.
           PERFORM 1000-OPEN-SEQ-FILE-INPUT
           PERFORM 1000-OPEN-IDX-FILE-OUTPUT
           PERFORM 2000-LECTURE-FICHIER-SEQ
           PERFORM 2010-REMPLIR-FICHIER-INDEXE UNTIL FS-ENDFILE-SEQ
           PERFORM 5000-FERMETURE-FICHIER-SEQ
           PERFORM 5000-FERMETURE-FICHIER-IDX.

       0010-CHOIX-ACTION.
           DISPLAY " "
           DISPLAY "---------------------------------------------------"
           DISPLAY "Que souhaitez-vous faire ?"
           DISPLAY "- ENTREZ 1 pour vérifier la disponibilité d'un "
           "film à partir de son code"
           DISPLAY "- ENTREZ 2 pour vérifier la disponibilité d'un "
           "film à partir de son titre"
           DISPLAY "- ENTREZ 3 pour supprimer un film"
           DISPLAY "- ENTREZ 4 pour modifier un film"
           DISPLAY "- ENTREZ 5 pour arrêter le programme"
           DISPLAY " "
           DISPLAY "> " WITH NO ADVANCING
           ACCEPT WS-CHOIX-ACTION
           DISPLAY " "

           EVALUATE WS-CHOIX-ACTION
           WHEN 1
              PERFORM 1000-OPEN-IDX-FILE-INPUT
              PERFORM 3010-RECHERCHE-FILM-PAR-CODE
              PERFORM 5000-FERMETURE-FICHIER-IDX
              PERFORM 0010-CHOIX-ACTION
           WHEN 2
              PERFORM 1000-OPEN-IDX-FILE-INPUT
              PERFORM 3020-RECHERCHE-FILM-PAR-TITRE
              PERFORM 5000-FERMETURE-FICHIER-IDX
              PERFORM 0010-CHOIX-ACTION
           WHEN 3
              PERFORM 1000-OPEN-IDX-FILE-IO
              PERFORM 3030-SUPPRIMER-FILM
              PERFORM 5000-FERMETURE-FICHIER-IDX
              PERFORM 0010-CHOIX-ACTION
           WHEN 4
              PERFORM 1000-OPEN-IDX-FILE-IO
              PERFORM 3040-MODIFIER-FILM
              PERFORM 5000-FERMETURE-FICHIER-IDX
              PERFORM 0010-CHOIX-ACTION
           WHEN 5
              PERFORM 9999-FIN-NORMALE-PRGM
           WHEN OTHER
              DISPLAY "Saisie invalide, veuillez réessayer."
              PERFORM 0010-CHOIX-ACTION
           END-EVALUATE.

       1000-OPEN-SEQ-FILE-INPUT.
           OPEN INPUT F-FILMS-SEQ 
           IF WS-FS-FILMS NOT = '00'
              DISPLAY "ERREUR LORS DE L'OUVERTURE DU FICHIER "
                      "SÉQUENTIEL"
              DISPLAY "CODE ERREUR : " WS-FS-FILMS
              PERFORM 9999-FIN-ANORMALE-PRGM
           END-IF.
           
       1000-OPEN-IDX-FILE-OUTPUT.
           OPEN OUTPUT F-FILMS-IDX
           IF WS-FS-FILMS NOT = '00'
              DISPLAY "ERREUR LORS DE L'OUVERTURE DU FICHIER INDEXÉ"
              DISPLAY "CODE ERREUR : " WS-FS-FILMS
              PERFORM 9999-FIN-ANORMALE-PRGM
           END-IF.

       2000-LECTURE-FICHIER-SEQ.
           READ F-FILMS-SEQ
              AT END SET FS-ENDFILE-SEQ TO TRUE
           END-READ
           IF WS-FS-FILMS NOT = '00' AND NOT = '10'
              DISPLAY "ERREUR LORS DE LA LECTURE DU FICHIER SÉQUENTIEL"
              DISPLAY "CODE ERREUR : " WS-FS-FILMS
              PERFORM 9999-FIN-ANORMALE-PRGM
           END-IF.

       2010-REMPLIR-FICHIER-INDEXE.
           WRITE FS-INFO-FILM-IDX FROM FS-INFO-FILM-SEQ
           IF WS-FS-FILMS NOT = '00'
              DISPLAY "ERREUR LORS DE L'ECRITURE"
              DISPLAY "CODE ERREUR : " WS-FS-FILMS
              PERFORM 9999-FIN-ANORMALE-PRGM
           END-IF
           PERFORM 2000-LECTURE-FICHIER-SEQ.

       5000-FERMETURE-FICHIER-SEQ.
           CLOSE F-FILMS-SEQ
           IF WS-FS-FILMS NOT = '00'
              DISPLAY "ERREUR LORS DE LA FERMETURE DU FICHIER "
                      "SÉQUENTIEL"
              DISPLAY "CODE ERREUR : " WS-FS-FILMS
              PERFORM 9999-FIN-ANORMALE-PRGM
           END-IF.

       5000-FERMETURE-FICHIER-IDX.
           CLOSE F-FILMS-IDX 
           IF WS-FS-FILMS NOT = '00'
              DISPLAY "ERREUR LORS DE LA FERMETURE DU FICHIER "
                      "INDEXÉ"
              DISPLAY "CODE ERREUR : " WS-FS-FILMS
              PERFORM 9999-FIN-ANORMALE-PRGM
           END-IF.
           
       1000-OPEN-IDX-FILE-INPUT.
           OPEN INPUT F-FILMS-IDX
           IF WS-FS-FILMS NOT = '00'
              DISPLAY "ERREUR LORS DE L'OUVERTURE DU FICHIER"
              DISPLAY "CODE ERREUR : " WS-FS-FILMS
              PERFORM 9999-FIN-ANORMALE-PRGM
           END-IF.
           
       1000-OPEN-IDX-FILE-IO.
           OPEN I-O F-FILMS-IDX
           IF WS-FS-FILMS NOT = '00'
              DISPLAY "ERREUR LORS DE L'OUVERTURE DU FICHIER"
              DISPLAY "CODE ERREUR : " WS-FS-FILMS
              PERFORM 9999-FIN-ANORMALE-PRGM
           END-IF.

       3010-RECHERCHE-FILM-PAR-CODE.
           DISPLAY "Entrez le code (5 chiffres) : " WITH NO ADVANCING
           ACCEPT WS-CODE-SAISI

           IF FUNCTION TEST-NUMVAL(WS-CODE-SAISI) = 1
              DISPLAY "La saisie doit être un nombre, "
                      "veuillez réessayer."
              DISPLAY " "
              PERFORM 3010-RECHERCHE-FILM-PAR-CODE
           END-IF

           MOVE WS-CODE-SAISI TO FS-CODE-IDX
           
           READ F-FILMS-IDX
              KEY IS FS-CODE-IDX
              INVALID KEY 
                 DISPLAY " "
                 DISPLAY "Le film n'est pas disponible."
              NOT INVALID KEY
                 DISPLAY " "
                 DISPLAY "Code film  : " FS-CODE-IDX
                 DISPLAY "Titre film : " FS-TITRE-IDX
                 DISPLAY " "
                 DISPLAY "Le film est disponible."
           END-READ.

       3020-RECHERCHE-FILM-PAR-TITRE.
           DISPLAY "Entrez le titre du film, sans accents ni "
                      "ponctuation) : " WITH NO ADVANCING
           ACCEPT WS-TITRE-SAISI
           MOVE FUNCTION UPPER-CASE(WS-TITRE-SAISI) TO FS-TITRE-IDX
           
           READ F-FILMS-IDX
              KEY IS FS-TITRE-IDX
              INVALID KEY 
                 DISPLAY " "
                 DISPLAY "Le film est mal orthographié ou "
                         "indisponible."
              NOT INVALID KEY
                 DISPLAY " "
                 DISPLAY "Code film  : " FS-CODE-IDX
                 DISPLAY "Titre film : " FS-TITRE-IDX
                 DISPLAY " "
                 DISPLAY "Le film est disponible."
           END-READ.
        
       3030-SUPPRIMER-FILM.
           *> Affiche la liste des films disponibles
           PERFORM UNTIL FS-ENDFILE-IDX
              READ F-FILMS-IDX NEXT
                 AT END SET FS-ENDFILE-IDX TO TRUE
                 NOT AT END DISPLAY FS-INFO-FILM-IDX
              END-READ
              IF WS-FS-FILMS NOT = '00' AND NOT = '10'
                 DISPLAY "ERREUR LORS DE LA LECTURE DU FICHIER"
                 DISPLAY "CODE ERREUR : " WS-FS-FILMS
                 PERFORM 9999-FIN-ANORMALE-PRGM
              END-IF
           END-PERFORM
           
           DISPLAY " "
           DISPLAY "Quel film souhaitez-vous supprimer ?".
           DISPLAY "Entrez le code film (5 chiffres) : " 
                   WITH NO ADVANCING
           ACCEPT WS-CODE-SAISI
           IF FUNCTION TEST-NUMVAL(WS-CODE-SAISI) = 1
              DISPLAY "La saisie doit être un nombre, "
                      "veuillez réessayer."
              DISPLAY " "
              PERFORM 3030-SUPPRIMER-FILM
           END-IF

           MOVE WS-CODE-SAISI TO FS-CODE-IDX

           DELETE F-FILMS-IDX
              INVALID KEY
                 DISPLAY " "
                 DISPLAY "Le code n'existe pas"
              NOT INVALID KEY
                 DISPLAY " "
                 DISPLAY "Le film a bien été supprimé"
           END-DELETE.

       3040-MODIFIER-FILM.
           PERFORM UNTIL FS-ENDFILE-IDX
              READ F-FILMS-IDX NEXT
                 AT END SET FS-ENDFILE-IDX TO TRUE
                 NOT AT END DISPLAY FS-INFO-FILM-IDX
              END-READ
              IF WS-FS-FILMS NOT = '00' AND NOT = '10'
                 DISPLAY "ERREUR LORS DE LA LECTURE DU FICHIER"
                 DISPLAY "CODE ERREUR : " WS-FS-FILMS
                 PERFORM 9999-FIN-ANORMALE-PRGM
              END-IF
           END-PERFORM

           DISPLAY " "
           DISPLAY "Quel film souhaitez-vous modifier ?".
           DISPLAY "Entrez le code film (5 chiffres) : " 
                   WITH NO ADVANCING
           ACCEPT WS-CODE-SAISI
           IF FUNCTION TEST-NUMVAL(WS-CODE-SAISI) = 1
              DISPLAY " "
              DISPLAY "La saisie doit être un nombre, "
                      "veuillez réessayer."
              DISPLAY " "
              PERFORM 3040-MODIFIER-FILM
           END-IF

           MOVE WS-CODE-SAISI TO FS-CODE-IDX

           READ F-FILMS-IDX
              KEY IS FS-CODE-IDX
              INVALID KEY 
                 DISPLAY " "
                 DISPLAY "Le code n'existe pas."
              NOT INVALID KEY
                 DISPLAY " "
                 DISPLAY "Par quel film souhaitez-vous le remplacer ?"
                 DISPLAY "Saisissez le titre sans accents "
                         "ni ponctuation."
                 DISPLAY "> " WITH NO ADVANCING
                 ACCEPT WS-TITRE-SAISI
      
                 MOVE FUNCTION UPPER-CASE(WS-TITRE-SAISI) 
                   TO FS-TITRE-IDX
      
                 REWRITE FS-INFO-FILM-IDX
                    NOT INVALID KEY
                       DISPLAY " "
                       DISPLAY "Le film a bien été modifié"
                 END-REWRITE
           END-READ.

       9999-FIN-NORMALE-PRGM.
           DISPLAY "************************"
           DISPLAY "FIN NORMALE DU PROGRAMME"
           DISPLAY "************************"
           GOBACK.

       9999-FIN-ANORMALE-PRGM.
           DISPLAY "*************************"
           DISPLAY "FIN ANORMALE DU PROGRAMME"
           DISPLAY "*************************"
           GOBACK.
