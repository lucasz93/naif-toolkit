C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C     Find the first object having a particular value.
C
      SUBROUTINE OBJFND ( OBJECT, START, OBJLIS, OBJ, FOUND )
 
      IMPLICIT NONE
      INCLUDE              'object.inc'
      INTEGER               OBJECT ( * )
      INTEGER               START
      INTEGER               OBJLIS ( LBCELL:* )
      INTEGER               OBJ    ( 2 )
      INTEGER               OBJTMP ( 2 )
      LOGICAL               FOUND

C
C     Local Variables
C
      INTEGER               I
      INTEGER               J
      INTEGER               OBJSIZ
 
      LOGICAL               GOT

C
C     SPICELIB Calls
C
      INTEGER               TOUCHI


C
C     First get the size of an object in the list.
C
      OBJSIZ = OBJLIS( RMPOBJ ) - 1
C
C     Find the START'th object, and just look at the next one
C     until we match or don't have any more objects.
C
      CALL OBJNTH ( OBJLIS, START, OBJ, FOUND )
 
      DO WHILE ( FOUND )
 
         J    =  1
         I    =  OBJ(1) + 1
         GOT  = .TRUE.
 
         DO WHILE ( J .LE. OBJSIZ .AND. GOT )
            GOT = OBJECT(J) .EQ. OBJLIS(I)
            I   = I + 1
            J   = J + 1
         END DO
 
         IF ( GOT ) THEN
            RETURN
         END IF
 
         CALL OBJNXT ( OBJ, OBJLIS, OBJTMP, FOUND )

         OBJ(1) = TOUCHI( OBJTMP(1) )
         OBJ(2) = TOUCHI( OBJTMP(2) )
 
      END DO
 
      RETURN
      END