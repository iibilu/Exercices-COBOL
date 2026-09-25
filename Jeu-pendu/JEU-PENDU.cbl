       IDENTIFICATION DIVISION.
       PROGRAM-ID. JEU-PENDU.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MOT-A-TROUVER       PIC X(26) VALUE SPACES.
       01 WS-MOT-MASQUE          PIC X(52) VALUE SPACES.
       01 WS-LETTRE-PROPOSEE     PIC X(01) VALUE SPACE.
       01 WS-HISTORIQUE-LETTRES  PIC X(51) VALUE SPACES.
       01 WS-POINTEUR-HISTO-LTR  PIC 9(02) VALUE 1.
       01 WS-LONGUEUR            PIC 9(02) VALUE ZEROS.
       01 WS-LETTRES-RESTANTES   PIC 9(02) VALUE ZEROS.
       01 WS-COMPTEUR            PIC 9(02) VALUE ZEROS.
       01 WS-RANG                PIC 9(02) VALUE 1.
       01 WS-ESSAIS-RATES        PIC 9(02) VALUE ZEROS.
       01 WS-COMPTEUR-TENTATIVES PIC 9(02) VALUE 1.
       01 WS-STATUT-LETTRE       PIC X(01) VALUE 'N'.
           88 LETTRE-TROUVEE               VALUE 'Y'.
           88 LETTRE-NON-TROUVEE           VALUE 'N'.
       01 WS-LETTRE-DANS-LISTE   PIC 9(02) VALUE ZEROS.
           88 WS-LETTRE-NON-PRES           VALUE ZEROS.
           88 LETTRE-DEJA-PRES             VALUE 1 THRU 10.

       
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALISATION
           PERFORM 2000-BOUCLE-JEU UNTIL WS-LETTRES-RESTANTES = 0 
                                      OR WS-ESSAIS-RATES = 10
           PERFORM 4000-FIN-PARTIE
           GOBACK.

       1000-INITIALISATION.
           DISPLAY " "
           DISPLAY "________________________"
                   " BIENVENUE DANS LE JEU DU PENDU "
                   "________________________"
           DISPLAY " "
           DISPLAY "Veuillez saisir le mot à faire deviner : "
                   WITH NO ADVANCING
           ACCEPT WS-MOT-A-TROUVER

           *> Calcul de la longueur
           INSPECT WS-MOT-A-TROUVER TALLYING WS-LONGUEUR 
                   FOR CHARACTERS BEFORE INITIAL SPACE
           MOVE WS-LONGUEUR TO WS-LETTRES-RESTANTES

           *> Cacher le mot saisi avec 20 sauts de ligne
           PERFORM 20 TIMES
              DISPLAY " "
           END-PERFORM

           *> Construction du masque "_ _ _ " pour le joueur qui devine
           MOVE 1 TO WS-RANG
           PERFORM VARYING WS-COMPTEUR FROM 1 BY 1 
                     UNTIL WS-COMPTEUR > WS-LONGUEUR
              MOVE '_ ' TO WS-MOT-MASQUE(WS-RANG:2)
              ADD 2 TO WS-RANG
           END-PERFORM.

       2000-BOUCLE-JEU.
           DISPLAY " "
           DISPLAY "________________________________ "
                   "TENTATIVE N°" WS-COMPTEUR-TENTATIVES
                   " ________________________________"
           DISPLAY " "
           DISPLAY "Mot : " WS-MOT-MASQUE

           *> Affichage de l'historique des propositions
           IF WS-HISTORIQUE-LETTRES NOT = SPACES
              DISPLAY " "
              DISPLAY "Lettres déjà proposées : " 
                      WS-HISTORIQUE-LETTRES
           END-IF

           DISPLAY " "
           DISPLAY "Proposez une lettre : "  WITH NO ADVANCING
           ACCEPT WS-LETTRE-PROPOSEE

           *> On vérifie si la lettre a déjà été proposée
           INSPECT WS-HISTORIQUE-LETTRES TALLYING WS-LETTRE-DANS-LISTE
                                         FOR ALL WS-LETTRE-PROPOSEE
           
           *> Si elle a déjà été proposée on s'arrête là
           IF LETTRE-DEJA-PRES
              DISPLAY " "
              DISPLAY "Cette lettre a déjà été proposée."
              SET WS-LETTRE-NON-PRES TO TRUE
              ADD 1 TO WS-COMPTEUR-TENTATIVES
           *> Sinon, on l'ajoute à la liste et on continue
           ELSE
              STRING WS-LETTRE-PROPOSEE ","
                 DELIMITED BY SIZE
                 INTO WS-HISTORIQUE-LETTRES
                 WITH POINTER WS-POINTEUR-HISTO-LTR

              SET LETTRE-NON-TROUVEE TO TRUE
              MOVE 1 TO WS-RANG
              ADD  1 TO WS-COMPTEUR-TENTATIVES WS-POINTEUR-HISTO-LTR

              *> Parcours du mot pour vérifier la présence de la lettre
              PERFORM VARYING WS-COMPTEUR FROM 1 BY 1 
                        UNTIL WS-COMPTEUR > WS-LONGUEUR
                 *> On remplace le _ si la lettre est présente
                 IF WS-LETTRE-PROPOSEE = WS-MOT-A-TROUVER(WS-COMPTEUR:1)
                    MOVE WS-LETTRE-PROPOSEE TO WS-MOT-MASQUE(WS-RANG:1)
                    SUBTRACT 1 FROM WS-LETTRES-RESTANTES
                    SET LETTRE-TROUVEE TO TRUE
                 END-IF

                 ADD 2 TO WS-RANG
              END-PERFORM

              IF LETTRE-NON-TROUVEE
                 DISPLAY " "
                 DISPLAY "Cette lettre ne fait pas partie du mot."
                 DISPLAY " "
                 ADD 1 TO WS-ESSAIS-RATES
                 PERFORM 3000-DESSIN-PENDU
              ELSE
                 DISPLAY " "
                 DISPLAY "Cette lettre fait partie du mot."
              END-IF
           END-IF.

       3000-DESSIN-PENDU.
           EVALUATE WS-ESSAIS-RATES
              WHEN 1
                 DISPLAY "___/|\___       "
              WHEN 2
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "___/|\___       "
              WHEN 3
                 DISPLAY "   _____________"
                 DISPLAY "    | /         "
                 DISPLAY "    |/          "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "___/|\___       "
              WHEN 4
                 DISPLAY "   _____________"
                 DISPLAY "    | /    |    "
                 DISPLAY "    |/          "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "___/|\___       "
              WHEN 5
                 DISPLAY "   _____________"
                 DISPLAY "    | /    |    "
                 DISPLAY "    |/   -----  "
                 DISPLAY "    |    |   |  "
                 DISPLAY "    |    -----  "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "___/|\___       "
              WHEN 6
                 DISPLAY "   _____________"
                 DISPLAY "    | /    |    "
                 DISPLAY "    |/   -----  "
                 DISPLAY "    |    |   |  "
                 DISPLAY "    |    -----  "
                 DISPLAY "    |      |    "
                 DISPLAY "    |      |    "
                 DISPLAY "    |      |    "
                 DISPLAY "    |      |    "
                 DISPLAY "    |      |    "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "___/|\___       "
              WHEN 7
                 DISPLAY "   _____________"
                 DISPLAY "    | /    |    "
                 DISPLAY "    |/   -----  "
                 DISPLAY "    |    |   |  "
                 DISPLAY "    |    -----  "
                 DISPLAY "    |      |    "
                 DISPLAY "    |     /|    "
                 DISPLAY "    |    / |    "
                 DISPLAY "    |      |    "
                 DISPLAY "    |      |    "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "___/|\___       "
              WHEN 8
                 DISPLAY "   _____________"
                 DISPLAY "    | /    |    "
                 DISPLAY "    |/   -----  "
                 DISPLAY "    |    |   |  "
                 DISPLAY "    |    -----  "
                 DISPLAY "    |      |    "
                 DISPLAY "    |     /|\   "
                 DISPLAY "    |    / | \  "
                 DISPLAY "    |      |    "
                 DISPLAY "    |      |    "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "    |           "
                 DISPLAY "___/|\___       "
              WHEN 9
                 DISPLAY "   _____________"
                 DISPLAY "    | /    |    "
                 DISPLAY "    |/   -----  "
                 DISPLAY "    |    |   |  "
                 DISPLAY "    |    -----  "
                 DISPLAY "    |      |    "
                 DISPLAY "    |     /|\   "
                 DISPLAY "    |    / | \  "
                 DISPLAY "    |      |    "
                 DISPLAY "    |      |    "
                 DISPLAY "    |    /      "
                 DISPLAY "    |   /       "
                 DISPLAY "    |           "
                 DISPLAY "___/|\___       "
              WHEN 10
                 DISPLAY "   _____________"
                 DISPLAY "    | /    |    "
                 DISPLAY "    |/   -----  "
                 DISPLAY "    |    |   |  "
                 DISPLAY "    |    -----  "
                 DISPLAY "    |      |    "
                 DISPLAY "    |     /|\   "
                 DISPLAY "    |    / | \  "
                 DISPLAY "    |      |    "
                 DISPLAY "    |      |    "
                 DISPLAY "    |    /   \  "
                 DISPLAY "    |   /     \ "
                 DISPLAY "    |           "
                 DISPLAY "___/|\___       "
           END-EVALUATE.
           
       4000-FIN-PARTIE.
           DISPLAY " "

           IF WS-LETTRES-RESTANTES = 0
              DISPLAY "_______________________________"
                      " FELICITATIONS ! "
                      "________________________________"
              DISPLAY " "
              DISPLAY "Vous avez gagné !"
           ELSE
              DISPLAY "__________________________________"
                      " DOMMAGE ! "
                      "___________________________________"
              DISPLAY " "
              DISPLAY "Vous avez perdu !"
           END-IF

           DISPLAY " "
           DISPLAY "Le mot était : " WS-MOT-A-TROUVER
           DISPLAY " ".
