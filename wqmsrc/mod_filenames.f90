Module MOD_FILENAMES
  !
  !INTEGER ifindext
  !CHARACTER *(*) fnameprefix
  !CHARACTER *(*) fnameext
Contains
  !
  !functions:
  !	function ifindext()
  !	function fnameprefix()
  !	function fnameext()
  !
  !WLong: added a few functions that deals with filenames and extensions etc
  !
  !
      Integer Function ifindext (FILENAME)
    !
    !
    !Fortran function to find the index of the last dot '.' in a filename string
    !which is useful for detecting whether the filename has an extension
    !
         Implicit None
         Integer (4) :: index_dot
         Character (*) :: FILENAME
         index_dot = index (TRIM(FILENAME), '.', .True.)
         ifindext = index_dot
         Return
      End Function ifindext
  !
      Character (1024) Function fnameprefix (FILENAME)
    !
    !Fortran function to find the filename after removing the extension (.dot etc)
    !
         Implicit None
         Integer (4) :: index_dot
         Character (*) :: FILENAME
         Character (Len=Len(FILENAME)) :: FILENAME_PREFIX, FILENAME_NEW
    !
         FILENAME_NEW = TRIM (FILENAME)
         index_dot = index (TRIM(FILENAME), '.', .True.)
         If (index_dot > 0) Then
            FILENAME_PREFIX = TRIM (FILENAME_NEW(1:index_dot-1))
         Else
            FILENAME_PREFIX = TRIM (FILENAME_NEW)
         End If
         fnameprefix = FILENAME_PREFIX
         Return
      End Function fnameprefix
  !
      Character (1024) Function fnameext (FILENAME)
    !
    !fortran function to find extension name if any
    !
         Implicit None
         Integer (4) :: index_dot, length
         Character (*) :: FILENAME
         Character (Len=Len(FILENAME)) :: FILENAME_EXT, FILENAME_NEW
    !
         FILENAME_NEW = TRIM (FILENAME)
         index_dot = index (TRIM(FILENAME), '.', .True.)
         length = len (TRIM(FILENAME_NEW))
    !
         If (index_dot > 0) Then
            FILENAME_EXT = TRIM (FILENAME_NEW(index_dot:length))
         Else
            FILENAME_EXT = ''
         End If
         fnameext = FILENAME_EXT
         Return
      End Function fnameext
  !
  !
End Module MOD_FILENAMES
!
