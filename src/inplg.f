      SUBROUTINE INPLG(X,Y,N,X0,Y0,N0,INPL)
      INTEGER N,N0,INPL(*)
      DOUBLE PRECISION X(*),Y(*),X0(*),Y0(*)
c
c     points in polygon, following an idea from algorithm 112 from CACM
c     (available at http://www.netlib.org/tomspdf/112.pdf)
c     
c     slightly modified to work not only for simple closed polygons
c     Albrecht Gebhardt <albrecht.gebhardt@uni-klu.ac.at>
c
c     X,Y    dimension N+1, coordinates of polygon vertices
c            position N+1 should be empty, algorithm closes polygon
c            itself  
c     X0,Y0  dimension N0, coordinates of points to check
c     INPL   dimension N0, result code for the points in X0/Y0
c            1: in polygon, 0: outside
c
      INTEGER I,J,CRSCNT,OUTCNT
      DOUBLE PRECISION D0,DJ
c     close polygon
      X(N+1)=X(1)
      Y(N+1)=Y(1)
      
c     loop over points in X0/Y0
      DO 10 J=1,N0
         CRSCNT=0
         OUTCNT=0
         DO 20 I=1,N
            IF ((Y0(J).LE.Y(I)) .EQV. (Y0(J).GT.Y(I+1))) THEN
c              parallel line to x-axis through point J crosses 
c              polygon between vertice I and I+1
               CRSCNT=CRSCNT+1
               IF (Y(I).NE.Y(I+1)) THEN 
c              calculate D0 and DJ
c                I
c              o +  i
c              u |\  n
c              t.| \  t.
c                |  \
c                +====--------J
c               0|..D0\ .....DJ        
c                |     \
c                      I+1

                  D0=X0(J)-X(I)
                  DJ=(Y0(J)-Y(I))*(X(I+1)-X(I))/(Y(I+1)-Y(I))
c                 change orientation if Y(I+1) > Y(I)
                  S0=SIGN(1,Y(I+1)-Y(I))
                  IF (S0*D0.LT.S0*DJ) THEN
c                 point J is on the "outer" side of edge I-->I+1
                     OUTCNT=OUTCNT+1
                  END IF
               ELSE
c              polygon edge parallel to x axis, 
c              check for X0(J) in [X(I),X(I+1)]
                  IF ((X(I).LE.X0(J)).EQV.(X0(J).LE.X(I+1))) THEN
                     INPL(J)=1
                     GO TO 10
                  ELSE
                     CRSCNT=CRSCNT-1
                  END If
               END IF
            END IF
 20      CONTINUE
c        it holds 
c            for interior points: (CRSCNT-2)/2 == OUTCNT
c            for outer points:    CRSCNT       == 2*OUTCNT
         IF (CRSCNT.EQ.(2*OUTCNT)) THEN
            INPL(J)=0
         ELSE
            INPL(J)=1
         END IF
 10   CONTINUE
 21   RETURN
      END
      
