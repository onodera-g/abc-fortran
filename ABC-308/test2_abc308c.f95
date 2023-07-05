Module m_valnth
    Integer, Parameter :: kdp = selected_real_kind(15)
    public :: valnth
    private :: kdp
    private :: R_valnth, I_valnth, D_valnth
    interface valnth
        module procedure d_valnth, r_valnth, i_valnth
    end interface valnth
contains

    Function D_valnth(XDONT, NORD) Result(valnth)
        !  Return NORDth value of XDONT, i.e fractile of order NORD/SIZE(XDONT).
        ! __________________________________________________________
        !  This routine uses a pivoting strategy such as the one of
        !  finding the median based on the quicksort algorithm, but
        !  we skew the pivot choice to try to bring it to NORD as
        !  fast as possible. It uses 2 temporary arrays, where it
        !  stores the indices of the values smaller than the pivot
        !  (ILOWT), and the indices of values larger than the pivot
        !  that we might still need later on (IHIGT). It iterates
        !  until it can bring the number of values in ILOWT to
        !  exactly NORD, and then finds the maximum of this set.
        !  Michel Olagnon - Aug. 2000
        ! __________________________________________________________
        ! __________________________________________________________
        Real(Kind=kdp), Dimension(:), Intent(In) :: XDONT
        Real(Kind=kdp) :: valnth
        Integer, Intent(In) :: NORD
        ! __________________________________________________________
        Real(Kind=kdp), Dimension(SIZE(XDONT)) :: XLOWT, XHIGT
        Real(Kind=kdp) :: XPIV, XPIV0, XWRK, XWRK1, XWRK2, XWRK3, XMIN, XMAX
        !
        Integer :: NDON, JHIG, JLOW, IHIG
        Integer :: IMIL, IFIN, ICRS, IDCR, ILOW
        Integer :: JLM2, JLM1, JHM2, JHM1, INTH
        !
        NDON = SIZE(XDONT)
        INTH = MAX(MIN(NORD, NDON), 1)
        !
        !    First loop is used to fill-in XLOWT, XHIGT at the same time
        !
        If (NDON < 2) Then
            If (INTH == 1) VALNTH = XDONT(1)
            Return
        End If
        !
        !  One chooses a pivot, best estimate possible to put fractile near
        !  mid-point of the set of low values.
        !
        If (XDONT(2) < XDONT(1)) Then
            XLOWT(1) = XDONT(2)
            XHIGT(1) = XDONT(1)
        Else
            XLOWT(1) = XDONT(1)
            XHIGT(1) = XDONT(2)
        End If
        !
        If (NDON < 3) Then
            If (INTH == 1) VALNTH = XLOWT(1)
            If (INTH == 2) VALNTH = XHIGT(1)
            Return
        End If
        !
        If (XDONT(3) < XHIGT(1)) Then
            XHIGT(2) = XHIGT(1)
            If (XDONT(3) < XLOWT(1)) Then
                XHIGT(1) = XLOWT(1)
                XLOWT(1) = XDONT(3)
            Else
                XHIGT(1) = XDONT(3)
            End If
        Else
            XHIGT(2) = XDONT(3)
        End If
        !
        If (NDON < 4) Then
            If (INTH == 1) Then
                VALNTH = XLOWT(1)
            Else
                VALNTH = XHIGT(INTH - 1)
            End If
            Return
        End If
        !
        If (XDONT(NDON) < XHIGT(1)) Then
            XHIGT(3) = XHIGT(2)
            XHIGT(2) = XHIGT(1)
            If (XDONT(NDON) < XLOWT(1)) Then
                XHIGT(1) = XLOWT(1)
                XLOWT(1) = XDONT(NDON)
            Else
                XHIGT(1) = XDONT(NDON)
            End If
        Else
            XHIGT(3) = XDONT(NDON)
        End If
        !
        If (NDON < 5) Then
            If (INTH == 1) Then
                VALNTH = XLOWT(1)
            Else
                VALNTH = XHIGT(INTH - 1)
            End If
            Return
        End If
        !

        JLOW = 1
        JHIG = 3
        XPIV = XLOWT(1) + REAL(2*INTH)/REAL(NDON + INTH)*(XHIGT(3) - XLOWT(1))
        If (XPIV >= XHIGT(1)) Then
            XPIV = XLOWT(1) + REAL(2*INTH)/REAL(NDON + INTH)* &
                   (XHIGT(2) - XLOWT(1))
            If (XPIV >= XHIGT(1)) &
                XPIV = XLOWT(1) + REAL(2*INTH)/REAL(NDON + INTH)* &
                       (XHIGT(1) - XLOWT(1))
        End If
        XPIV0 = XPIV
        !
        !  One puts values > pivot in the end and those <= pivot
        !  at the beginning. This is split in 2 cases, so that
        !  we can skip the loop test a number of times.
        !  As we are also filling in the work arrays at the same time
        !  we stop filling in the XHIGT array as soon as we have more
        !  than enough values in XLOWT.
        !
        !
        If (XDONT(NDON) > XPIV) Then
            ICRS = 3
            Do
                ICRS = ICRS + 1
                If (XDONT(ICRS) > XPIV) Then
                    If (ICRS >= NDON) Exit
                    JHIG = JHIG + 1
                    XHIGT(JHIG) = XDONT(ICRS)
                Else
                    JLOW = JLOW + 1
                    XLOWT(JLOW) = XDONT(ICRS)
                    If (JLOW >= INTH) Exit
                End If
            End Do
            !
            !  One restricts further processing because it is no use
            !  to store more high values
            !
            If (ICRS < NDON - 1) Then
                Do
                    ICRS = ICRS + 1
                    If (XDONT(ICRS) <= XPIV) Then
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XDONT(ICRS)
                    Else If (ICRS >= NDON) Then
                        Exit
                    End If
                End Do
            End If
            !
            !
        Else
            !
            !  Same as above, but this is not as easy to optimize, so the
            !  DO-loop is kept
            !
            Do ICRS = 4, NDON - 1
                If (XDONT(ICRS) > XPIV) Then
                    JHIG = JHIG + 1
                    XHIGT(JHIG) = XDONT(ICRS)
                Else
                    JLOW = JLOW + 1
                    XLOWT(JLOW) = XDONT(ICRS)
                    If (JLOW >= INTH) Exit
                End If
            End Do
            !
            If (ICRS < NDON - 1) Then
                Do
                    ICRS = ICRS + 1
                    If (XDONT(ICRS) <= XPIV) Then
                        If (ICRS >= NDON) Exit
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XDONT(ICRS)
                    End If
                End Do
            End If
        End If
        !
        JLM2 = 0
        JLM1 = 0
        JHM2 = 0
        JHM1 = 0
        Do
            If (JLM2 == JLOW .And. JHM2 == JHIG) Then
                !
                !   We are oscillating. Perturbate by bringing JLOW closer by one
                !   to INTH
                !
                If (INTH > JLOW) Then
                    XMIN = XHIGT(1)
                    IHIG = 1
                    Do ICRS = 2, JHIG
                        If (XHIGT(ICRS) < XMIN) Then
                            XMIN = XHIGT(ICRS)
                            IHIG = ICRS
                        End If
                    End Do
                    !
                    JLOW = JLOW + 1
                    XLOWT(JLOW) = XHIGT(IHIG)
                    XHIGT(IHIG) = XHIGT(JHIG)
                    JHIG = JHIG - 1
                Else

                    XMAX = XLOWT(JLOW)
                    JLOW = JLOW - 1
                    Do ICRS = 1, JLOW
                        If (XLOWT(ICRS) > XMAX) Then
                            XWRK = XMAX
                            XMAX = XLOWT(ICRS)
                            XLOWT(ICRS) = XWRK
                        End If
                    End Do
                End If
            End If
            JLM2 = JLM1
            JLM1 = JLOW
            JHM2 = JHM1
            JHM1 = JHIG
            !
            !   We try to bring the number of values in the low values set
            !   closer to INTH.
            !
            Select Case (INTH - JLOW)
            Case (2:)
                !
                !   Not enough values in low part, at least 2 are missing
                !
                INTH = INTH - JLOW
                JLOW = 0
                Select Case (JHIG)
    !!!!!           CASE DEFAULT
    !!!!!              write (unit=*,fmt=*) "Assertion failed"
    !!!!!              STOP
                    !
                    !   We make a special case when we have so few values in
                    !   the high values set that it is bad performance to choose a pivot
                    !   and apply the general algorithm.
                    !
                Case (2)
                    If (XHIGT(1) <= XHIGT(2)) Then
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XHIGT(1)
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XHIGT(2)
                    Else
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XHIGT(2)
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XHIGT(1)
                    End If
                    Exit
                    !
                Case (3)
                    !
                    !
                    XWRK1 = XHIGT(1)
                    XWRK2 = XHIGT(2)
                    XWRK3 = XHIGT(3)
                    If (XWRK2 < XWRK1) Then
                        XHIGT(1) = XWRK2
                        XHIGT(2) = XWRK1
                        XWRK2 = XWRK1
                    End If
                    If (XWRK2 > XWRK3) Then
                        XHIGT(3) = XWRK2
                        XHIGT(2) = XWRK3
                        XWRK2 = XWRK3
                        If (XWRK2 < XHIGT(1)) Then
                            XHIGT(2) = XHIGT(1)
                            XHIGT(1) = XWRK2
                        End If
                    End If
                    JHIG = 0
                    Do ICRS = JLOW + 1, INTH
                        JHIG = JHIG + 1
                        XLOWT(ICRS) = XHIGT(JHIG)
                    End Do
                    JLOW = INTH
                    Exit
                    !
                Case (4:)
                    !
                    !
                    XPIV0 = XPIV
                    IFIN = JHIG
                    !
                    !  One chooses a pivot from the 2 first values and the last one.
                    !  This should ensure sufficient renewal between iterations to
                    !  avoid worst case behavior effects.
                    !
                    XWRK1 = XHIGT(1)
                    XWRK2 = XHIGT(2)
                    XWRK3 = XHIGT(IFIN)
                    If (XWRK2 < XWRK1) Then
                        XHIGT(1) = XWRK2
                        XHIGT(2) = XWRK1
                        XWRK2 = XWRK1
                    End If
                    If (XWRK2 > XWRK3) Then
                        XHIGT(IFIN) = XWRK2
                        XHIGT(2) = XWRK3
                        XWRK2 = XWRK3
                        If (XWRK2 < XHIGT(1)) Then
                            XHIGT(2) = XHIGT(1)
                            XHIGT(1) = XWRK2
                        End If
                    End If
                    !
                    XWRK1 = XHIGT(1)
                    JLOW = JLOW + 1
                    XLOWT(JLOW) = XWRK1
                    XPIV = XWRK1 + 0.5*(XHIGT(IFIN) - XWRK1)
                    !
                    !  One takes values <= pivot to XLOWT
                    !  Again, 2 parts, one where we take care of the remaining
                    !  high values because we might still need them, and the
                    !  other when we know that we will have more than enough
                    !  low values in the end.
                    !
                    JHIG = 0
                    Do ICRS = 2, IFIN
                        If (XHIGT(ICRS) <= XPIV) Then
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XHIGT(ICRS)
                            If (JLOW >= INTH) Exit
                        Else
                            JHIG = JHIG + 1
                            XHIGT(JHIG) = XHIGT(ICRS)
                        End If
                    End Do
                    !
                    Do ICRS = ICRS + 1, IFIN
                        If (XHIGT(ICRS) <= XPIV) Then
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XHIGT(ICRS)
                        End If
                    End Do
                End Select
                !
                !
            Case (1)
                !
                !  Only 1 value is missing in low part
                !
                XMIN = XHIGT(1)
                IHIG = 1
                Do ICRS = 2, JHIG
                    If (XHIGT(ICRS) < XMIN) Then
                        XMIN = XHIGT(ICRS)
                        IHIG = ICRS
                    End If
                End Do
                !
                VALNTH = XHIGT(IHIG)
                Return
                !
                !
            Case (0)
                !
                !  Low part is exactly what we want
                !
                Exit
                !
                !
            Case (-5:-1)
                !
                !  Only few values too many in low part
                !
                XHIGT(1) = XLOWT(1)
                ILOW = 1 + INTH - JLOW
                Do ICRS = 2, INTH
                    XWRK = XLOWT(ICRS)
                    Do IDCR = ICRS - 1, MAX(1, ILOW), -1
                        If (XWRK < XHIGT(IDCR)) Then
                            XHIGT(IDCR + 1) = XHIGT(IDCR)
                        Else
                            Exit
                        End If
                    End Do
                    XHIGT(IDCR + 1) = XWRK
                    ILOW = ILOW + 1
                End Do
                !
                XWRK1 = XHIGT(INTH)
                ILOW = 2*INTH - JLOW
                Do ICRS = INTH + 1, JLOW
                    If (XLOWT(ICRS) < XWRK1) Then
                        XWRK = XLOWT(ICRS)
                        Do IDCR = INTH - 1, MAX(1, ILOW), -1
                            If (XWRK >= XHIGT(IDCR)) Exit
                            XHIGT(IDCR + 1) = XHIGT(IDCR)
                        End Do
                        XHIGT(IDCR + 1) = XLOWT(ICRS)
                        XWRK1 = XHIGT(INTH)
                    End If
                    ILOW = ILOW + 1
                End Do
                !
                VALNTH = XHIGT(INTH)
                Return
                !
                !
            Case (:-6)
                !
                ! last case: too many values in low part
                !

                IMIL = (JLOW + 1)/2
                IFIN = JLOW
                !
                !  One chooses a pivot from 1st, last, and middle values
                !
                If (XLOWT(IMIL) < XLOWT(1)) Then
                    XWRK = XLOWT(1)
                    XLOWT(1) = XLOWT(IMIL)
                    XLOWT(IMIL) = XWRK
                End If
                If (XLOWT(IMIL) > XLOWT(IFIN)) Then
                    XWRK = XLOWT(IFIN)
                    XLOWT(IFIN) = XLOWT(IMIL)
                    XLOWT(IMIL) = XWRK
                    If (XLOWT(IMIL) < XLOWT(1)) Then
                        XWRK = XLOWT(1)
                        XLOWT(1) = XLOWT(IMIL)
                        XLOWT(IMIL) = XWRK
                    End If
                End If
                If (IFIN <= 3) Exit
                !
                XPIV = XLOWT(1) + REAL(INTH)/REAL(JLOW + INTH)* &
                       (XLOWT(IFIN) - XLOWT(1))

                !
                !  One takes values > XPIV to XHIGT
                !
                JHIG = 0
                JLOW = 0
                !
                If (XLOWT(IFIN) > XPIV) Then
                    ICRS = 0
                    Do
                        ICRS = ICRS + 1
                        If (XLOWT(ICRS) > XPIV) Then
                            JHIG = JHIG + 1
                            XHIGT(JHIG) = XLOWT(ICRS)
                            If (ICRS >= IFIN) Exit
                        Else
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XLOWT(ICRS)
                            If (JLOW >= INTH) Exit
                        End If
                    End Do
                    !
                    If (ICRS < IFIN) Then
                        Do
                            ICRS = ICRS + 1
                            If (XLOWT(ICRS) <= XPIV) Then
                                JLOW = JLOW + 1
                                XLOWT(JLOW) = XLOWT(ICRS)
                            Else
                                If (ICRS >= IFIN) Exit
                            End If
                        End Do
                    End If
                Else
                    Do ICRS = 1, IFIN
                        If (XLOWT(ICRS) > XPIV) Then
                            JHIG = JHIG + 1
                            XHIGT(JHIG) = XLOWT(ICRS)
                        Else
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XLOWT(ICRS)
                            If (JLOW >= INTH) Exit
                        End If
                    End Do
                    !
                    Do ICRS = ICRS + 1, IFIN
                        If (XLOWT(ICRS) <= XPIV) Then
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XLOWT(ICRS)
                        End If
                    End Do
                End If
                !
            End Select
            !
        End Do
        !
        !  Now, we only need to find maximum of the 1:INTH set
        !
        VALNTH = MAXVAL(XLOWT(1:INTH))
        Return
        !
        !
    End Function D_valnth

    Function R_valnth(XDONT, NORD) Result(valnth)
        !  Return NORDth value of XDONT, i.e fractile of order NORD/SIZE(XDONT).
        ! __________________________________________________________
        !  This routine uses a pivoting strategy such as the one of
        !  finding the median based on the quicksort algorithm, but
        !  we skew the pivot choice to try to bring it to NORD as
        !  fast as possible. It uses 2 temporary arrays, where it
        !  stores the indices of the values smaller than the pivot
        !  (ILOWT), and the indices of values larger than the pivot
        !  that we might still need later on (IHIGT). It iterates
        !  until it can bring the number of values in ILOWT to
        !  exactly NORD, and then finds the maximum of this set.
        !  Michel Olagnon - Aug. 2000
        ! __________________________________________________________
        ! _________________________________________________________
        Real, Dimension(:), Intent(In) :: XDONT
        Real :: valnth
        Integer, Intent(In) :: NORD
        ! __________________________________________________________
        Real, Dimension(SIZE(XDONT)) :: XLOWT, XHIGT
        Real :: XPIV, XPIV0, XWRK, XWRK1, XWRK2, XWRK3, XMIN, XMAX
        !
        Integer :: NDON, JHIG, JLOW, IHIG
        Integer :: IMIL, IFIN, ICRS, IDCR, ILOW
        Integer :: JLM2, JLM1, JHM2, JHM1, INTH
        !
        NDON = SIZE(XDONT)
        INTH = MAX(MIN(NORD, NDON), 1)
        !
        !    First loop is used to fill-in XLOWT, XHIGT at the same time
        !
        If (NDON < 2) Then
            If (INTH == 1) VALNTH = XDONT(1)
            Return
        End If
        !
        !  One chooses a pivot, best estimate possible to put fractile near
        !  mid-point of the set of low values.
        !
        If (XDONT(2) < XDONT(1)) Then
            XLOWT(1) = XDONT(2)
            XHIGT(1) = XDONT(1)
        Else
            XLOWT(1) = XDONT(1)
            XHIGT(1) = XDONT(2)
        End If
        !
        If (NDON < 3) Then
            If (INTH == 1) VALNTH = XLOWT(1)
            If (INTH == 2) VALNTH = XHIGT(1)
            Return
        End If
        !
        If (XDONT(3) < XHIGT(1)) Then
            XHIGT(2) = XHIGT(1)
            If (XDONT(3) < XLOWT(1)) Then
                XHIGT(1) = XLOWT(1)
                XLOWT(1) = XDONT(3)
            Else
                XHIGT(1) = XDONT(3)
            End If
        Else
            XHIGT(2) = XDONT(3)
        End If
        !
        If (NDON < 4) Then
            If (INTH == 1) Then
                VALNTH = XLOWT(1)
            Else
                VALNTH = XHIGT(INTH - 1)
            End If
            Return
        End If
        !
        If (XDONT(NDON) < XHIGT(1)) Then
            XHIGT(3) = XHIGT(2)
            XHIGT(2) = XHIGT(1)
            If (XDONT(NDON) < XLOWT(1)) Then
                XHIGT(1) = XLOWT(1)
                XLOWT(1) = XDONT(NDON)
            Else
                XHIGT(1) = XDONT(NDON)
            End If
        Else
            XHIGT(3) = XDONT(NDON)
        End If
        !
        If (NDON < 5) Then
            If (INTH == 1) Then
                VALNTH = XLOWT(1)
            Else
                VALNTH = XHIGT(INTH - 1)
            End If
            Return
        End If
        !

        JLOW = 1
        JHIG = 3
        XPIV = XLOWT(1) + REAL(2*INTH)/REAL(NDON + INTH)*(XHIGT(3) - XLOWT(1))
        If (XPIV >= XHIGT(1)) Then
            XPIV = XLOWT(1) + REAL(2*INTH)/REAL(NDON + INTH)* &
                   (XHIGT(2) - XLOWT(1))
            If (XPIV >= XHIGT(1)) &
                XPIV = XLOWT(1) + REAL(2*INTH)/REAL(NDON + INTH)* &
                       (XHIGT(1) - XLOWT(1))
        End If
        XPIV0 = XPIV
        !
        !  One puts values > pivot in the end and those <= pivot
        !  at the beginning. This is split in 2 cases, so that
        !  we can skip the loop test a number of times.
        !  As we are also filling in the work arrays at the same time
        !  we stop filling in the XHIGT array as soon as we have more
        !  than enough values in XLOWT.
        !
        !
        If (XDONT(NDON) > XPIV) Then
            ICRS = 3
            Do
                ICRS = ICRS + 1
                If (XDONT(ICRS) > XPIV) Then
                    If (ICRS >= NDON) Exit
                    JHIG = JHIG + 1
                    XHIGT(JHIG) = XDONT(ICRS)
                Else
                    JLOW = JLOW + 1
                    XLOWT(JLOW) = XDONT(ICRS)
                    If (JLOW >= INTH) Exit
                End If
            End Do
            !
            !  One restricts further processing because it is no use
            !  to store more high values
            !
            If (ICRS < NDON - 1) Then
                Do
                    ICRS = ICRS + 1
                    If (XDONT(ICRS) <= XPIV) Then
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XDONT(ICRS)
                    Else If (ICRS >= NDON) Then
                        Exit
                    End If
                End Do
            End If
            !
            !
        Else
            !
            !  Same as above, but this is not as easy to optimize, so the
            !  DO-loop is kept
            !
            Do ICRS = 4, NDON - 1
                If (XDONT(ICRS) > XPIV) Then
                    JHIG = JHIG + 1
                    XHIGT(JHIG) = XDONT(ICRS)
                Else
                    JLOW = JLOW + 1
                    XLOWT(JLOW) = XDONT(ICRS)
                    If (JLOW >= INTH) Exit
                End If
            End Do
            !
            If (ICRS < NDON - 1) Then
                Do
                    ICRS = ICRS + 1
                    If (XDONT(ICRS) <= XPIV) Then
                        If (ICRS >= NDON) Exit
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XDONT(ICRS)
                    End If
                End Do
            End If
        End If
        !
        JLM2 = 0
        JLM1 = 0
        JHM2 = 0
        JHM1 = 0
        Do
            If (JLM2 == JLOW .And. JHM2 == JHIG) Then
                !
                !   We are oscillating. Perturbate by bringing JLOW closer by one
                !   to INTH
                !
                If (INTH > JLOW) Then
                    XMIN = XHIGT(1)
                    IHIG = 1
                    Do ICRS = 2, JHIG
                        If (XHIGT(ICRS) < XMIN) Then
                            XMIN = XHIGT(ICRS)
                            IHIG = ICRS
                        End If
                    End Do
                    !
                    JLOW = JLOW + 1
                    XLOWT(JLOW) = XHIGT(IHIG)
                    XHIGT(IHIG) = XHIGT(JHIG)
                    JHIG = JHIG - 1
                Else

                    XMAX = XLOWT(JLOW)
                    JLOW = JLOW - 1
                    Do ICRS = 1, JLOW
                        If (XLOWT(ICRS) > XMAX) Then
                            XWRK = XMAX
                            XMAX = XLOWT(ICRS)
                            XLOWT(ICRS) = XWRK
                        End If
                    End Do
                End If
            End If
            JLM2 = JLM1
            JLM1 = JLOW
            JHM2 = JHM1
            JHM1 = JHIG
            !
            !   We try to bring the number of values in the low values set
            !   closer to INTH.
            !
            Select Case (INTH - JLOW)
            Case (2:)
                !
                !   Not enough values in low part, at least 2 are missing
                !
                INTH = INTH - JLOW
                JLOW = 0
                Select Case (JHIG)
    !!!!!           CASE DEFAULT
    !!!!!              write (unit=*,fmt=*) "Assertion failed"
    !!!!!              STOP
                    !
                    !   We make a special case when we have so few values in
                    !   the high values set that it is bad performance to choose a pivot
                    !   and apply the general algorithm.
                    !
                Case (2)
                    If (XHIGT(1) <= XHIGT(2)) Then
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XHIGT(1)
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XHIGT(2)
                    Else
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XHIGT(2)
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XHIGT(1)
                    End If
                    Exit
                    !
                Case (3)
                    !
                    !
                    XWRK1 = XHIGT(1)
                    XWRK2 = XHIGT(2)
                    XWRK3 = XHIGT(3)
                    If (XWRK2 < XWRK1) Then
                        XHIGT(1) = XWRK2
                        XHIGT(2) = XWRK1
                        XWRK2 = XWRK1
                    End If
                    If (XWRK2 > XWRK3) Then
                        XHIGT(3) = XWRK2
                        XHIGT(2) = XWRK3
                        XWRK2 = XWRK3
                        If (XWRK2 < XHIGT(1)) Then
                            XHIGT(2) = XHIGT(1)
                            XHIGT(1) = XWRK2
                        End If
                    End If
                    JHIG = 0
                    Do ICRS = JLOW + 1, INTH
                        JHIG = JHIG + 1
                        XLOWT(ICRS) = XHIGT(JHIG)
                    End Do
                    JLOW = INTH
                    Exit
                    !
                Case (4:)
                    !
                    !
                    XPIV0 = XPIV
                    IFIN = JHIG
                    !
                    !  One chooses a pivot from the 2 first values and the last one.
                    !  This should ensure sufficient renewal between iterations to
                    !  avoid worst case behavior effects.
                    !
                    XWRK1 = XHIGT(1)
                    XWRK2 = XHIGT(2)
                    XWRK3 = XHIGT(IFIN)
                    If (XWRK2 < XWRK1) Then
                        XHIGT(1) = XWRK2
                        XHIGT(2) = XWRK1
                        XWRK2 = XWRK1
                    End If
                    If (XWRK2 > XWRK3) Then
                        XHIGT(IFIN) = XWRK2
                        XHIGT(2) = XWRK3
                        XWRK2 = XWRK3
                        If (XWRK2 < XHIGT(1)) Then
                            XHIGT(2) = XHIGT(1)
                            XHIGT(1) = XWRK2
                        End If
                    End If
                    !
                    XWRK1 = XHIGT(1)
                    JLOW = JLOW + 1
                    XLOWT(JLOW) = XWRK1
                    XPIV = XWRK1 + 0.5*(XHIGT(IFIN) - XWRK1)
                    !
                    !  One takes values <= pivot to XLOWT
                    !  Again, 2 parts, one where we take care of the remaining
                    !  high values because we might still need them, and the
                    !  other when we know that we will have more than enough
                    !  low values in the end.
                    !
                    JHIG = 0
                    Do ICRS = 2, IFIN
                        If (XHIGT(ICRS) <= XPIV) Then
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XHIGT(ICRS)
                            If (JLOW >= INTH) Exit
                        Else
                            JHIG = JHIG + 1
                            XHIGT(JHIG) = XHIGT(ICRS)
                        End If
                    End Do
                    !
                    Do ICRS = ICRS + 1, IFIN
                        If (XHIGT(ICRS) <= XPIV) Then
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XHIGT(ICRS)
                        End If
                    End Do
                End Select
                !
                !
            Case (1)
                !
                !  Only 1 value is missing in low part
                !
                XMIN = XHIGT(1)
                IHIG = 1
                Do ICRS = 2, JHIG
                    If (XHIGT(ICRS) < XMIN) Then
                        XMIN = XHIGT(ICRS)
                        IHIG = ICRS
                    End If
                End Do
                !
                VALNTH = XHIGT(IHIG)
                Return
                !
                !
            Case (0)
                !
                !  Low part is exactly what we want
                !
                Exit
                !
                !
            Case (-5:-1)
                !
                !  Only few values too many in low part
                !
                XHIGT(1) = XLOWT(1)
                ILOW = 1 + INTH - JLOW
                Do ICRS = 2, INTH
                    XWRK = XLOWT(ICRS)
                    Do IDCR = ICRS - 1, MAX(1, ILOW), -1
                        If (XWRK < XHIGT(IDCR)) Then
                            XHIGT(IDCR + 1) = XHIGT(IDCR)
                        Else
                            Exit
                        End If
                    End Do
                    XHIGT(IDCR + 1) = XWRK
                    ILOW = ILOW + 1
                End Do
                !
                XWRK1 = XHIGT(INTH)
                ILOW = 2*INTH - JLOW
                Do ICRS = INTH + 1, JLOW
                    If (XLOWT(ICRS) < XWRK1) Then
                        XWRK = XLOWT(ICRS)
                        Do IDCR = INTH - 1, MAX(1, ILOW), -1
                            If (XWRK >= XHIGT(IDCR)) Exit
                            XHIGT(IDCR + 1) = XHIGT(IDCR)
                        End Do
                        XHIGT(IDCR + 1) = XLOWT(ICRS)
                        XWRK1 = XHIGT(INTH)
                    End If
                    ILOW = ILOW + 1
                End Do
                !
                VALNTH = XHIGT(INTH)
                Return
                !
                !
            Case (:-6)
                !
                ! last case: too many values in low part
                !

                IMIL = (JLOW + 1)/2
                IFIN = JLOW
                !
                !  One chooses a pivot from 1st, last, and middle values
                !
                If (XLOWT(IMIL) < XLOWT(1)) Then
                    XWRK = XLOWT(1)
                    XLOWT(1) = XLOWT(IMIL)
                    XLOWT(IMIL) = XWRK
                End If
                If (XLOWT(IMIL) > XLOWT(IFIN)) Then
                    XWRK = XLOWT(IFIN)
                    XLOWT(IFIN) = XLOWT(IMIL)
                    XLOWT(IMIL) = XWRK
                    If (XLOWT(IMIL) < XLOWT(1)) Then
                        XWRK = XLOWT(1)
                        XLOWT(1) = XLOWT(IMIL)
                        XLOWT(IMIL) = XWRK
                    End If
                End If
                If (IFIN <= 3) Exit
                !
                XPIV = XLOWT(1) + REAL(INTH)/REAL(JLOW + INTH)* &
                       (XLOWT(IFIN) - XLOWT(1))

                !
                !  One takes values > XPIV to XHIGT
                !
                JHIG = 0
                JLOW = 0
                !
                If (XLOWT(IFIN) > XPIV) Then
                    ICRS = 0
                    Do
                        ICRS = ICRS + 1
                        If (XLOWT(ICRS) > XPIV) Then
                            JHIG = JHIG + 1
                            XHIGT(JHIG) = XLOWT(ICRS)
                            If (ICRS >= IFIN) Exit
                        Else
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XLOWT(ICRS)
                            If (JLOW >= INTH) Exit
                        End If
                    End Do
                    !
                    If (ICRS < IFIN) Then
                        Do
                            ICRS = ICRS + 1
                            If (XLOWT(ICRS) <= XPIV) Then
                                JLOW = JLOW + 1
                                XLOWT(JLOW) = XLOWT(ICRS)
                            Else
                                If (ICRS >= IFIN) Exit
                            End If
                        End Do
                    End If
                Else
                    Do ICRS = 1, IFIN
                        If (XLOWT(ICRS) > XPIV) Then
                            JHIG = JHIG + 1
                            XHIGT(JHIG) = XLOWT(ICRS)
                        Else
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XLOWT(ICRS)
                            If (JLOW >= INTH) Exit
                        End If
                    End Do
                    !
                    Do ICRS = ICRS + 1, IFIN
                        If (XLOWT(ICRS) <= XPIV) Then
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XLOWT(ICRS)
                        End If
                    End Do
                End If
                !
            End Select
            !
        End Do
        !
        !  Now, we only need to find maximum of the 1:INTH set
        !
        VALNTH = MAXVAL(XLOWT(1:INTH))
        Return
        !
        !
    End Function R_valnth
    Function I_valnth(XDONT, NORD) Result(valnth)
        !  Return NORDth value of XDONT, i.e fractile of order NORD/SIZE(XDONT).
        ! __________________________________________________________
        !  This routine uses a pivoting strategy such as the one of
        !  finding the median based on the quicksort algorithm, but
        !  we skew the pivot choice to try to bring it to NORD as
        !  fast as possible. It uses 2 temporary arrays, where it
        !  stores the indices of the values smaller than the pivot
        !  (ILOWT), and the indices of values larger than the pivot
        !  that we might still need later on (IHIGT). It iterates
        !  until it can bring the number of values in ILOWT to
        !  exactly NORD, and then finds the maximum of this set.
        !  Michel Olagnon - Aug. 2000
        ! __________________________________________________________
        ! __________________________________________________________
        Integer, Dimension(:), Intent(In) :: XDONT
        Integer :: valnth
        Integer, Intent(In) :: NORD
        ! __________________________________________________________
        Integer, Dimension(SIZE(XDONT)) :: XLOWT, XHIGT
        Integer :: XPIV, XPIV0, XWRK, XWRK1, XWRK2, XWRK3, XMIN, XMAX
        !
        Integer :: NDON, JHIG, JLOW, IHIG
        Integer :: IMIL, IFIN, ICRS, IDCR, ILOW
        Integer :: JLM2, JLM1, JHM2, JHM1, INTH
        !
        NDON = SIZE(XDONT)
        INTH = MAX(MIN(NORD, NDON), 1)
        !
        !    First loop is used to fill-in XLOWT, XHIGT at the same time
        !
        If (NDON < 2) Then
            If (INTH == 1) VALNTH = XDONT(1)
            Return
        End If
        !
        !  One chooses a pivot, best estimate possible to put fractile near
        !  mid-point of the set of low values.
        !
        If (XDONT(2) < XDONT(1)) Then
            XLOWT(1) = XDONT(2)
            XHIGT(1) = XDONT(1)
        Else
            XLOWT(1) = XDONT(1)
            XHIGT(1) = XDONT(2)
        End If
        !
        If (NDON < 3) Then
            If (INTH == 1) VALNTH = XLOWT(1)
            If (INTH == 2) VALNTH = XHIGT(1)
            Return
        End If
        !
        If (XDONT(3) < XHIGT(1)) Then
            XHIGT(2) = XHIGT(1)
            If (XDONT(3) < XLOWT(1)) Then
                XHIGT(1) = XLOWT(1)
                XLOWT(1) = XDONT(3)
            Else
                XHIGT(1) = XDONT(3)
            End If
        Else
            XHIGT(2) = XDONT(3)
        End If
        !
        If (NDON < 4) Then
            If (INTH == 1) Then
                VALNTH = XLOWT(1)
            Else
                VALNTH = XHIGT(INTH - 1)
            End If
            Return
        End If
        !
        If (XDONT(NDON) < XHIGT(1)) Then
            XHIGT(3) = XHIGT(2)
            XHIGT(2) = XHIGT(1)
            If (XDONT(NDON) < XLOWT(1)) Then
                XHIGT(1) = XLOWT(1)
                XLOWT(1) = XDONT(NDON)
            Else
                XHIGT(1) = XDONT(NDON)
            End If
        Else
            XHIGT(3) = XDONT(NDON)
        End If
        !
        If (NDON < 5) Then
            If (INTH == 1) Then
                VALNTH = XLOWT(1)
            Else
                VALNTH = XHIGT(INTH - 1)
            End If
            Return
        End If
        !

        JLOW = 1
        JHIG = 3
        XPIV = XLOWT(1) + REAL(2*INTH)/REAL(NDON + INTH)*(XHIGT(3) - XLOWT(1))
        If (XPIV >= XHIGT(1)) Then
            XPIV = XLOWT(1) + REAL(2*INTH)/REAL(NDON + INTH)* &
                   (XHIGT(2) - XLOWT(1))
            If (XPIV >= XHIGT(1)) &
                XPIV = XLOWT(1) + REAL(2*INTH)/REAL(NDON + INTH)* &
                       (XHIGT(1) - XLOWT(1))
        End If
        XPIV0 = XPIV
        !
        !  One puts values > pivot in the end and those <= pivot
        !  at the beginning. This is split in 2 cases, so that
        !  we can skip the loop test a number of times.
        !  As we are also filling in the work arrays at the same time
        !  we stop filling in the XHIGT array as soon as we have more
        !  than enough values in XLOWT.
        !
        !
        If (XDONT(NDON) > XPIV) Then
            ICRS = 3
            Do
                ICRS = ICRS + 1
                If (XDONT(ICRS) > XPIV) Then
                    If (ICRS >= NDON) Exit
                    JHIG = JHIG + 1
                    XHIGT(JHIG) = XDONT(ICRS)
                Else
                    JLOW = JLOW + 1
                    XLOWT(JLOW) = XDONT(ICRS)
                    If (JLOW >= INTH) Exit
                End If
            End Do
            !
            !  One restricts further processing because it is no use
            !  to store more high values
            !
            If (ICRS < NDON - 1) Then
                Do
                    ICRS = ICRS + 1
                    If (XDONT(ICRS) <= XPIV) Then
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XDONT(ICRS)
                    Else If (ICRS >= NDON) Then
                        Exit
                    End If
                End Do
            End If
            !
            !
        Else
            !
            !  Same as above, but this is not as easy to optimize, so the
            !  DO-loop is kept
            !
            Do ICRS = 4, NDON - 1
                If (XDONT(ICRS) > XPIV) Then
                    JHIG = JHIG + 1
                    XHIGT(JHIG) = XDONT(ICRS)
                Else
                    JLOW = JLOW + 1
                    XLOWT(JLOW) = XDONT(ICRS)
                    If (JLOW >= INTH) Exit
                End If
            End Do
            !
            If (ICRS < NDON - 1) Then
                Do
                    ICRS = ICRS + 1
                    If (XDONT(ICRS) <= XPIV) Then
                        If (ICRS >= NDON) Exit
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XDONT(ICRS)
                    End If
                End Do
            End If
        End If
        !
        JLM2 = 0
        JLM1 = 0
        JHM2 = 0
        JHM1 = 0
        Do
            If (JLM2 == JLOW .And. JHM2 == JHIG) Then
                !
                !   We are oscillating. Perturbate by bringing JLOW closer by one
                !   to INTH
                !
                If (INTH > JLOW) Then
                    XMIN = XHIGT(1)
                    IHIG = 1
                    Do ICRS = 2, JHIG
                        If (XHIGT(ICRS) < XMIN) Then
                            XMIN = XHIGT(ICRS)
                            IHIG = ICRS
                        End If
                    End Do
                    !
                    JLOW = JLOW + 1
                    XLOWT(JLOW) = XHIGT(IHIG)
                    XHIGT(IHIG) = XHIGT(JHIG)
                    JHIG = JHIG - 1
                Else

                    XMAX = XLOWT(JLOW)
                    JLOW = JLOW - 1
                    Do ICRS = 1, JLOW
                        If (XLOWT(ICRS) > XMAX) Then
                            XWRK = XMAX
                            XMAX = XLOWT(ICRS)
                            XLOWT(ICRS) = XWRK
                        End If
                    End Do
                End If
            End If
            JLM2 = JLM1
            JLM1 = JLOW
            JHM2 = JHM1
            JHM1 = JHIG
            !
            !   We try to bring the number of values in the low values set
            !   closer to INTH.
            !
            Select Case (INTH - JLOW)
            Case (2:)
                !
                !   Not enough values in low part, at least 2 are missing
                !
                INTH = INTH - JLOW
                JLOW = 0
                Select Case (JHIG)
    !!!!!           CASE DEFAULT
    !!!!!              write (unit=*,fmt=*) "Assertion failed"
    !!!!!              STOP
                    !
                    !   We make a special case when we have so few values in
                    !   the high values set that it is bad performance to choose a pivot
                    !   and apply the general algorithm.
                    !
                Case (2)
                    If (XHIGT(1) <= XHIGT(2)) Then
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XHIGT(1)
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XHIGT(2)
                    Else
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XHIGT(2)
                        JLOW = JLOW + 1
                        XLOWT(JLOW) = XHIGT(1)
                    End If
                    Exit
                    !
                Case (3)
                    !
                    !
                    XWRK1 = XHIGT(1)
                    XWRK2 = XHIGT(2)
                    XWRK3 = XHIGT(3)
                    If (XWRK2 < XWRK1) Then
                        XHIGT(1) = XWRK2
                        XHIGT(2) = XWRK1
                        XWRK2 = XWRK1
                    End If
                    If (XWRK2 > XWRK3) Then
                        XHIGT(3) = XWRK2
                        XHIGT(2) = XWRK3
                        XWRK2 = XWRK3
                        If (XWRK2 < XHIGT(1)) Then
                            XHIGT(2) = XHIGT(1)
                            XHIGT(1) = XWRK2
                        End If
                    End If
                    JHIG = 0
                    Do ICRS = JLOW + 1, INTH
                        JHIG = JHIG + 1
                        XLOWT(ICRS) = XHIGT(JHIG)
                    End Do
                    JLOW = INTH
                    Exit
                    !
                Case (4:)
                    !
                    !
                    XPIV0 = XPIV
                    IFIN = JHIG
                    !
                    !  One chooses a pivot from the 2 first values and the last one.
                    !  This should ensure sufficient renewal between iterations to
                    !  avoid worst case behavior effects.
                    !
                    XWRK1 = XHIGT(1)
                    XWRK2 = XHIGT(2)
                    XWRK3 = XHIGT(IFIN)
                    If (XWRK2 < XWRK1) Then
                        XHIGT(1) = XWRK2
                        XHIGT(2) = XWRK1
                        XWRK2 = XWRK1
                    End If
                    If (XWRK2 > XWRK3) Then
                        XHIGT(IFIN) = XWRK2
                        XHIGT(2) = XWRK3
                        XWRK2 = XWRK3
                        If (XWRK2 < XHIGT(1)) Then
                            XHIGT(2) = XHIGT(1)
                            XHIGT(1) = XWRK2
                        End If
                    End If
                    !
                    XWRK1 = XHIGT(1)
                    JLOW = JLOW + 1
                    XLOWT(JLOW) = XWRK1
                    XPIV = XWRK1 + 0.5*(XHIGT(IFIN) - XWRK1)
                    !
                    !  One takes values <= pivot to XLOWT
                    !  Again, 2 parts, one where we take care of the remaining
                    !  high values because we might still need them, and the
                    !  other when we know that we will have more than enough
                    !  low values in the end.
                    !
                    JHIG = 0
                    Do ICRS = 2, IFIN
                        If (XHIGT(ICRS) <= XPIV) Then
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XHIGT(ICRS)
                            If (JLOW >= INTH) Exit
                        Else
                            JHIG = JHIG + 1
                            XHIGT(JHIG) = XHIGT(ICRS)
                        End If
                    End Do
                    !
                    Do ICRS = ICRS + 1, IFIN
                        If (XHIGT(ICRS) <= XPIV) Then
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XHIGT(ICRS)
                        End If
                    End Do
                End Select
                !
                !
            Case (1)
                !
                !  Only 1 value is missing in low part
                !
                XMIN = XHIGT(1)
                IHIG = 1
                Do ICRS = 2, JHIG
                    If (XHIGT(ICRS) < XMIN) Then
                        XMIN = XHIGT(ICRS)
                        IHIG = ICRS
                    End If
                End Do
                !
                VALNTH = XHIGT(IHIG)
                Return
                !
                !
            Case (0)
                !
                !  Low part is exactly what we want
                !
                Exit
                !
                !
            Case (-5:-1)
                !
                !  Only few values too many in low part
                !
                XHIGT(1) = XLOWT(1)
                ILOW = 1 + INTH - JLOW
                Do ICRS = 2, INTH
                    XWRK = XLOWT(ICRS)
                    Do IDCR = ICRS - 1, MAX(1, ILOW), -1
                        If (XWRK < XHIGT(IDCR)) Then
                            XHIGT(IDCR + 1) = XHIGT(IDCR)
                        Else
                            Exit
                        End If
                    End Do
                    XHIGT(IDCR + 1) = XWRK
                    ILOW = ILOW + 1
                End Do
                !
                XWRK1 = XHIGT(INTH)
                ILOW = 2*INTH - JLOW
                Do ICRS = INTH + 1, JLOW
                    If (XLOWT(ICRS) < XWRK1) Then
                        XWRK = XLOWT(ICRS)
                        Do IDCR = INTH - 1, MAX(1, ILOW), -1
                            If (XWRK >= XHIGT(IDCR)) Exit
                            XHIGT(IDCR + 1) = XHIGT(IDCR)
                        End Do
                        XHIGT(IDCR + 1) = XLOWT(ICRS)
                        XWRK1 = XHIGT(INTH)
                    End If
                    ILOW = ILOW + 1
                End Do
                !
                VALNTH = XHIGT(INTH)
                Return
                !
                !
            Case (:-6)
                !
                ! last case: too many values in low part
                !

                IMIL = (JLOW + 1)/2
                IFIN = JLOW
                !
                !  One chooses a pivot from 1st, last, and middle values
                !
                If (XLOWT(IMIL) < XLOWT(1)) Then
                    XWRK = XLOWT(1)
                    XLOWT(1) = XLOWT(IMIL)
                    XLOWT(IMIL) = XWRK
                End If
                If (XLOWT(IMIL) > XLOWT(IFIN)) Then
                    XWRK = XLOWT(IFIN)
                    XLOWT(IFIN) = XLOWT(IMIL)
                    XLOWT(IMIL) = XWRK
                    If (XLOWT(IMIL) < XLOWT(1)) Then
                        XWRK = XLOWT(1)
                        XLOWT(1) = XLOWT(IMIL)
                        XLOWT(IMIL) = XWRK
                    End If
                End If
                If (IFIN <= 3) Exit
                !
                XPIV = XLOWT(1) + REAL(INTH)/REAL(JLOW + INTH)* &
                       (XLOWT(IFIN) - XLOWT(1))

                !
                !  One takes values > XPIV to XHIGT
                !
                JHIG = 0
                JLOW = 0
                !
                If (XLOWT(IFIN) > XPIV) Then
                    ICRS = 0
                    Do
                        ICRS = ICRS + 1
                        If (XLOWT(ICRS) > XPIV) Then
                            JHIG = JHIG + 1
                            XHIGT(JHIG) = XLOWT(ICRS)
                            If (ICRS >= IFIN) Exit
                        Else
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XLOWT(ICRS)
                            If (JLOW >= INTH) Exit
                        End If
                    End Do
                    !
                    If (ICRS < IFIN) Then
                        Do
                            ICRS = ICRS + 1
                            If (XLOWT(ICRS) <= XPIV) Then
                                JLOW = JLOW + 1
                                XLOWT(JLOW) = XLOWT(ICRS)
                            Else
                                If (ICRS >= IFIN) Exit
                            End If
                        End Do
                    End If
                Else
                    Do ICRS = 1, IFIN
                        If (XLOWT(ICRS) > XPIV) Then
                            JHIG = JHIG + 1
                            XHIGT(JHIG) = XLOWT(ICRS)
                        Else
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XLOWT(ICRS)
                            If (JLOW >= INTH) Exit
                        End If
                    End Do
                    !
                    Do ICRS = ICRS + 1, IFIN
                        If (XLOWT(ICRS) <= XPIV) Then
                            JLOW = JLOW + 1
                            XLOWT(JLOW) = XLOWT(ICRS)
                        End If
                    End Do
                End If
                !
            End Select
            !
        End Do
        !
        !  Now, we only need to find maximum of the 1:INTH set
        !
        VALNTH = MAXVAL(XLOWT(1:INTH))
        Return
        !
        !
    End Function I_valnth
end module m_valnth
Module m_mrgrnk
    Integer, Parameter :: kdp = selected_real_kind(15)
    public :: mrgrnk
    private :: kdp
    private :: R_mrgrnk, I_mrgrnk, D_mrgrnk
    interface mrgrnk
        module procedure D_mrgrnk, R_mrgrnk, I_mrgrnk
    end interface mrgrnk
contains

    Subroutine D_mrgrnk(XDONT, IRNGT)
        ! __________________________________________________________
        !   MRGRNK = Merge-sort ranking of an array
        !   For performance reasons, the first 2 passes are taken
        !   out of the standard loop, and use dedicated coding.
        ! __________________________________________________________
        ! __________________________________________________________
        Real(kind=kdp), Dimension(:), Intent(In) :: XDONT
        Integer, Dimension(:), Intent(Out) :: IRNGT
        ! __________________________________________________________
        Real(kind=kdp) :: XVALA, XVALB
        !
        Integer, Dimension(SIZE(IRNGT)) :: JWRKT
        Integer :: LMTNA, LMTNC, IRNG1, IRNG2
        Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
        !
        NVAL = Min(SIZE(XDONT), SIZE(IRNGT))
        Select Case (NVAL)
        Case (:0)
            Return
        Case (1)
            IRNGT(1) = 1
            Return
        Case Default
            Continue
        End Select
        !
        !  Fill-in the index array, creating ordered couples
        !
        Do IIND = 2, NVAL, 2
            If (XDONT(IIND - 1) <= XDONT(IIND)) Then
                IRNGT(IIND - 1) = IIND - 1
                IRNGT(IIND) = IIND
            Else
                IRNGT(IIND - 1) = IIND
                IRNGT(IIND) = IIND - 1
            End If
        End Do
        If (Modulo(NVAL, 2) /= 0) Then
            IRNGT(NVAL) = NVAL
        End If
        !
        !  We will now have ordered subsets A - B - A - B - ...
        !  and merge A and B couples into     C   -   C   - ...
        !
        LMTNA = 2
        LMTNC = 4
        !
        !  First iteration. The length of the ordered subsets goes from 2 to 4
        !
        Do
            If (NVAL <= 2) Exit
            !
            !   Loop on merges of A and B into C
            !
            Do IWRKD = 0, NVAL - 1, 4
                If ((IWRKD + 4) > NVAL) Then
                    If ((IWRKD + 2) >= NVAL) Exit
                    !
                    !   1 2 3
                    !
                    If (XDONT(IRNGT(IWRKD + 2)) <= XDONT(IRNGT(IWRKD + 3))) Exit
                    !
                    !   1 3 2
                    !
                    If (XDONT(IRNGT(IWRKD + 1)) <= XDONT(IRNGT(IWRKD + 3))) Then
                        IRNG2 = IRNGT(IWRKD + 2)
                        IRNGT(IWRKD + 2) = IRNGT(IWRKD + 3)
                        IRNGT(IWRKD + 3) = IRNG2
                        !
                        !   3 1 2
                        !
                    Else
                        IRNG1 = IRNGT(IWRKD + 1)
                        IRNGT(IWRKD + 1) = IRNGT(IWRKD + 3)
                        IRNGT(IWRKD + 3) = IRNGT(IWRKD + 2)
                        IRNGT(IWRKD + 2) = IRNG1
                    End If
                    Exit
                End If
                !
                !   1 2 3 4
                !
                If (XDONT(IRNGT(IWRKD + 2)) <= XDONT(IRNGT(IWRKD + 3))) Cycle
                !
                !   1 3 x x
                !
                If (XDONT(IRNGT(IWRKD + 1)) <= XDONT(IRNGT(IWRKD + 3))) Then
                    IRNG2 = IRNGT(IWRKD + 2)
                    IRNGT(IWRKD + 2) = IRNGT(IWRKD + 3)
                    If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD + 4))) Then
                        !   1 3 2 4
                        IRNGT(IWRKD + 3) = IRNG2
                    Else
                        !   1 3 4 2
                        IRNGT(IWRKD + 3) = IRNGT(IWRKD + 4)
                        IRNGT(IWRKD + 4) = IRNG2
                    End If
                    !
                    !   3 x x x
                    !
                Else
                    IRNG1 = IRNGT(IWRKD + 1)
                    IRNG2 = IRNGT(IWRKD + 2)
                    IRNGT(IWRKD + 1) = IRNGT(IWRKD + 3)
                    If (XDONT(IRNG1) <= XDONT(IRNGT(IWRKD + 4))) Then
                        IRNGT(IWRKD + 2) = IRNG1
                        If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD + 4))) Then
                            !   3 1 2 4
                            IRNGT(IWRKD + 3) = IRNG2
                        Else
                            !   3 1 4 2
                            IRNGT(IWRKD + 3) = IRNGT(IWRKD + 4)
                            IRNGT(IWRKD + 4) = IRNG2
                        End If
                    Else
                        !   3 4 1 2
                        IRNGT(IWRKD + 2) = IRNGT(IWRKD + 4)
                        IRNGT(IWRKD + 3) = IRNG1
                        IRNGT(IWRKD + 4) = IRNG2
                    End If
                End If
            End Do
            !
            !  The Cs become As and Bs
            !
            LMTNA = 4
            Exit
        End Do
        !
        !  Iteration loop. Each time, the length of the ordered subsets
        !  is doubled.
        !
        Do
            If (LMTNA >= NVAL) Exit
            IWRKF = 0
            LMTNC = 2*LMTNC
            !
            !   Loop on merges of A and B into C
            !
            Do
                IWRK = IWRKF
                IWRKD = IWRKF + 1
                JINDA = IWRKF + LMTNA
                IWRKF = IWRKF + LMTNC
                If (IWRKF >= NVAL) Then
                    If (JINDA >= NVAL) Exit
                    IWRKF = NVAL
                End If
                IINDA = 1
                IINDB = JINDA + 1
                !
                !   Shortcut for the case when the max of A is smaller
                !   than the min of B. This line may be activated when the
                !   initial set is already close to sorted.
                !
                !          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
                !
                !  One steps in the C subset, that we build in the final rank array
                !
                !  Make a copy of the rank array for the merge iteration
                !
                JWRKT(1:LMTNA) = IRNGT(IWRKD:JINDA)
                !
                XVALA = XDONT(JWRKT(IINDA))
                XVALB = XDONT(IRNGT(IINDB))
                !
                Do
                    IWRK = IWRK + 1
                    !
                    !  We still have unprocessed values in both A and B
                    !
                    If (XVALA > XVALB) Then
                        IRNGT(IWRK) = IRNGT(IINDB)
                        IINDB = IINDB + 1
                        If (IINDB > IWRKF) Then
                            !  Only A still with unprocessed values
                            IRNGT(IWRK + 1:IWRKF) = JWRKT(IINDA:LMTNA)
                            Exit
                        End If
                        XVALB = XDONT(IRNGT(IINDB))
                    Else
                        IRNGT(IWRK) = JWRKT(IINDA)
                        IINDA = IINDA + 1
                        If (IINDA > LMTNA) Exit ! Only B still with unprocessed values
                        XVALA = XDONT(JWRKT(IINDA))
                    End If
                    !
                End Do
            End Do
            !
            !  The Cs become As and Bs
            !
            LMTNA = 2*LMTNA
        End Do
        !
        Return
        !
    End Subroutine D_mrgrnk

    Subroutine R_mrgrnk(XDONT, IRNGT)
        ! __________________________________________________________
        !   MRGRNK = Merge-sort ranking of an array
        !   For performance reasons, the first 2 passes are taken
        !   out of the standard loop, and use dedicated coding.
        ! __________________________________________________________
        ! _________________________________________________________
        Real, Dimension(:), Intent(In) :: XDONT
        Integer, Dimension(:), Intent(Out) :: IRNGT
        ! __________________________________________________________
        Real :: XVALA, XVALB
        !
        Integer, Dimension(SIZE(IRNGT)) :: JWRKT
        Integer :: LMTNA, LMTNC, IRNG1, IRNG2
        Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
        !
        NVAL = Min(SIZE(XDONT), SIZE(IRNGT))
        Select Case (NVAL)
        Case (:0)
            Return
        Case (1)
            IRNGT(1) = 1
            Return
        Case Default
            Continue
        End Select
        !
        !  Fill-in the index array, creating ordered couples
        !
        Do IIND = 2, NVAL, 2
            If (XDONT(IIND - 1) <= XDONT(IIND)) Then
                IRNGT(IIND - 1) = IIND - 1
                IRNGT(IIND) = IIND
            Else
                IRNGT(IIND - 1) = IIND
                IRNGT(IIND) = IIND - 1
            End If
        End Do
        If (Modulo(NVAL, 2) /= 0) Then
            IRNGT(NVAL) = NVAL
        End If
        !
        !  We will now have ordered subsets A - B - A - B - ...
        !  and merge A and B couples into     C   -   C   - ...
        !
        LMTNA = 2
        LMTNC = 4
        !
        !  First iteration. The length of the ordered subsets goes from 2 to 4
        !
        Do
            If (NVAL <= 2) Exit
            !
            !   Loop on merges of A and B into C
            !
            Do IWRKD = 0, NVAL - 1, 4
                If ((IWRKD + 4) > NVAL) Then
                    If ((IWRKD + 2) >= NVAL) Exit
                    !
                    !   1 2 3
                    !
                    If (XDONT(IRNGT(IWRKD + 2)) <= XDONT(IRNGT(IWRKD + 3))) Exit
                    !
                    !   1 3 2
                    !
                    If (XDONT(IRNGT(IWRKD + 1)) <= XDONT(IRNGT(IWRKD + 3))) Then
                        IRNG2 = IRNGT(IWRKD + 2)
                        IRNGT(IWRKD + 2) = IRNGT(IWRKD + 3)
                        IRNGT(IWRKD + 3) = IRNG2
                        !
                        !   3 1 2
                        !
                    Else
                        IRNG1 = IRNGT(IWRKD + 1)
                        IRNGT(IWRKD + 1) = IRNGT(IWRKD + 3)
                        IRNGT(IWRKD + 3) = IRNGT(IWRKD + 2)
                        IRNGT(IWRKD + 2) = IRNG1
                    End If
                    Exit
                End If
                !
                !   1 2 3 4
                !
                If (XDONT(IRNGT(IWRKD + 2)) <= XDONT(IRNGT(IWRKD + 3))) Cycle
                !
                !   1 3 x x
                !
                If (XDONT(IRNGT(IWRKD + 1)) <= XDONT(IRNGT(IWRKD + 3))) Then
                    IRNG2 = IRNGT(IWRKD + 2)
                    IRNGT(IWRKD + 2) = IRNGT(IWRKD + 3)
                    If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD + 4))) Then
                        !   1 3 2 4
                        IRNGT(IWRKD + 3) = IRNG2
                    Else
                        !   1 3 4 2
                        IRNGT(IWRKD + 3) = IRNGT(IWRKD + 4)
                        IRNGT(IWRKD + 4) = IRNG2
                    End If
                    !
                    !   3 x x x
                    !
                Else
                    IRNG1 = IRNGT(IWRKD + 1)
                    IRNG2 = IRNGT(IWRKD + 2)
                    IRNGT(IWRKD + 1) = IRNGT(IWRKD + 3)
                    If (XDONT(IRNG1) <= XDONT(IRNGT(IWRKD + 4))) Then
                        IRNGT(IWRKD + 2) = IRNG1
                        If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD + 4))) Then
                            !   3 1 2 4
                            IRNGT(IWRKD + 3) = IRNG2
                        Else
                            !   3 1 4 2
                            IRNGT(IWRKD + 3) = IRNGT(IWRKD + 4)
                            IRNGT(IWRKD + 4) = IRNG2
                        End If
                    Else
                        !   3 4 1 2
                        IRNGT(IWRKD + 2) = IRNGT(IWRKD + 4)
                        IRNGT(IWRKD + 3) = IRNG1
                        IRNGT(IWRKD + 4) = IRNG2
                    End If
                End If
            End Do
            !
            !  The Cs become As and Bs
            !
            LMTNA = 4
            Exit
        End Do
        !
        !  Iteration loop. Each time, the length of the ordered subsets
        !  is doubled.
        !
        Do
            If (LMTNA >= NVAL) Exit
            IWRKF = 0
            LMTNC = 2*LMTNC
            !
            !   Loop on merges of A and B into C
            !
            Do
                IWRK = IWRKF
                IWRKD = IWRKF + 1
                JINDA = IWRKF + LMTNA
                IWRKF = IWRKF + LMTNC
                If (IWRKF >= NVAL) Then
                    If (JINDA >= NVAL) Exit
                    IWRKF = NVAL
                End If
                IINDA = 1
                IINDB = JINDA + 1
                !
                !   Shortcut for the case when the max of A is smaller
                !   than the min of B. This line may be activated when the
                !   initial set is already close to sorted.
                !
                !          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
                !
                !  One steps in the C subset, that we build in the final rank array
                !
                !  Make a copy of the rank array for the merge iteration
                !
                JWRKT(1:LMTNA) = IRNGT(IWRKD:JINDA)
                !
                XVALA = XDONT(JWRKT(IINDA))
                XVALB = XDONT(IRNGT(IINDB))
                !
                Do
                    IWRK = IWRK + 1
                    !
                    !  We still have unprocessed values in both A and B
                    !
                    If (XVALA > XVALB) Then
                        IRNGT(IWRK) = IRNGT(IINDB)
                        IINDB = IINDB + 1
                        If (IINDB > IWRKF) Then
                            !  Only A still with unprocessed values
                            IRNGT(IWRK + 1:IWRKF) = JWRKT(IINDA:LMTNA)
                            Exit
                        End If
                        XVALB = XDONT(IRNGT(IINDB))
                    Else
                        IRNGT(IWRK) = JWRKT(IINDA)
                        IINDA = IINDA + 1
                        If (IINDA > LMTNA) Exit ! Only B still with unprocessed values
                        XVALA = XDONT(JWRKT(IINDA))
                    End If
                    !
                End Do
            End Do
            !
            !  The Cs become As and Bs
            !
            LMTNA = 2*LMTNA
        End Do
        !
        Return
        !
    End Subroutine R_mrgrnk
    Subroutine I_mrgrnk(XDONT, IRNGT)
        ! __________________________________________________________
        !   MRGRNK = Merge-sort ranking of an array
        !   For performance reasons, the first 2 passes are taken
        !   out of the standard loop, and use dedicated coding.
        ! __________________________________________________________
        ! __________________________________________________________
        Integer, Dimension(:), Intent(In)  :: XDONT
        Integer, Dimension(:), Intent(Out) :: IRNGT
        ! __________________________________________________________
        Integer :: XVALA, XVALB
        !
        Integer, Dimension(SIZE(IRNGT)) :: JWRKT
        Integer :: LMTNA, LMTNC, IRNG1, IRNG2
        Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
        !
        NVAL = Min(SIZE(XDONT), SIZE(IRNGT))
        Select Case (NVAL)
        Case (:0)
            Return
        Case (1)
            IRNGT(1) = 1
            Return
        Case Default
            Continue
        End Select
        !
        !  Fill-in the index array, creating ordered couples
        !
        Do IIND = 2, NVAL, 2
            If (XDONT(IIND - 1) <= XDONT(IIND)) Then
                IRNGT(IIND - 1) = IIND - 1
                IRNGT(IIND) = IIND
            Else
                IRNGT(IIND - 1) = IIND
                IRNGT(IIND) = IIND - 1
            End If
        End Do
        If (Modulo(NVAL, 2) /= 0) Then
            IRNGT(NVAL) = NVAL
        End If
        !
        !  We will now have ordered subsets A - B - A - B - ...
        !  and merge A and B couples into     C   -   C   - ...
        !
        LMTNA = 2
        LMTNC = 4
        !
        !  First iteration. The length of the ordered subsets goes from 2 to 4
        !
        Do
            If (NVAL <= 2) Exit
            !
            !   Loop on merges of A and B into C
            !
            Do IWRKD = 0, NVAL - 1, 4
                If ((IWRKD + 4) > NVAL) Then
                    If ((IWRKD + 2) >= NVAL) Exit
                    !
                    !   1 2 3
                    !
                    If (XDONT(IRNGT(IWRKD + 2)) <= XDONT(IRNGT(IWRKD + 3))) Exit
                    !
                    !   1 3 2
                    !
                    If (XDONT(IRNGT(IWRKD + 1)) <= XDONT(IRNGT(IWRKD + 3))) Then
                        IRNG2 = IRNGT(IWRKD + 2)
                        IRNGT(IWRKD + 2) = IRNGT(IWRKD + 3)
                        IRNGT(IWRKD + 3) = IRNG2
                        !
                        !   3 1 2
                        !
                    Else
                        IRNG1 = IRNGT(IWRKD + 1)
                        IRNGT(IWRKD + 1) = IRNGT(IWRKD + 3)
                        IRNGT(IWRKD + 3) = IRNGT(IWRKD + 2)
                        IRNGT(IWRKD + 2) = IRNG1
                    End If
                    Exit
                End If
                !
                !   1 2 3 4
                !
                If (XDONT(IRNGT(IWRKD + 2)) <= XDONT(IRNGT(IWRKD + 3))) Cycle
                !
                !   1 3 x x
                !
                If (XDONT(IRNGT(IWRKD + 1)) <= XDONT(IRNGT(IWRKD + 3))) Then
                    IRNG2 = IRNGT(IWRKD + 2)
                    IRNGT(IWRKD + 2) = IRNGT(IWRKD + 3)
                    If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD + 4))) Then
                        !   1 3 2 4
                        IRNGT(IWRKD + 3) = IRNG2
                    Else
                        !   1 3 4 2
                        IRNGT(IWRKD + 3) = IRNGT(IWRKD + 4)
                        IRNGT(IWRKD + 4) = IRNG2
                    End If
                    !
                    !   3 x x x
                    !
                Else
                    IRNG1 = IRNGT(IWRKD + 1)
                    IRNG2 = IRNGT(IWRKD + 2)
                    IRNGT(IWRKD + 1) = IRNGT(IWRKD + 3)
                    If (XDONT(IRNG1) <= XDONT(IRNGT(IWRKD + 4))) Then
                        IRNGT(IWRKD + 2) = IRNG1
                        If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD + 4))) Then
                            !   3 1 2 4
                            IRNGT(IWRKD + 3) = IRNG2
                        Else
                            !   3 1 4 2
                            IRNGT(IWRKD + 3) = IRNGT(IWRKD + 4)
                            IRNGT(IWRKD + 4) = IRNG2
                        End If
                    Else
                        !   3 4 1 2
                        IRNGT(IWRKD + 2) = IRNGT(IWRKD + 4)
                        IRNGT(IWRKD + 3) = IRNG1
                        IRNGT(IWRKD + 4) = IRNG2
                    End If
                End If
            End Do
            !
            !  The Cs become As and Bs
            !
            LMTNA = 4
            Exit
        End Do
        !
        !  Iteration loop. Each time, the length of the ordered subsets
        !  is doubled.
        !
        Do
            If (LMTNA >= NVAL) Exit
            IWRKF = 0
            LMTNC = 2*LMTNC
            !
            !   Loop on merges of A and B into C
            !
            Do
                IWRK = IWRKF
                IWRKD = IWRKF + 1
                JINDA = IWRKF + LMTNA
                IWRKF = IWRKF + LMTNC
                If (IWRKF >= NVAL) Then
                    If (JINDA >= NVAL) Exit
                    IWRKF = NVAL
                End If
                IINDA = 1
                IINDB = JINDA + 1
                !
                !   Shortcut for the case when the max of A is smaller
                !   than the min of B. This line may be activated when the
                !   initial set is already close to sorted.
                !
                !          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
                !
                !  One steps in the C subset, that we build in the final rank array
                !
                !  Make a copy of the rank array for the merge iteration
                !
                JWRKT(1:LMTNA) = IRNGT(IWRKD:JINDA)
                !
                XVALA = XDONT(JWRKT(IINDA))
                XVALB = XDONT(IRNGT(IINDB))
                !
                Do
                    IWRK = IWRK + 1
                    !
                    !  We still have unprocessed values in both A and B
                    !
                    If (XVALA > XVALB) Then
                        IRNGT(IWRK) = IRNGT(IINDB)
                        IINDB = IINDB + 1
                        If (IINDB > IWRKF) Then
                            !  Only A still with unprocessed values
                            IRNGT(IWRK + 1:IWRKF) = JWRKT(IINDA:LMTNA)
                            Exit
                        End If
                        XVALB = XDONT(IRNGT(IINDB))
                    Else
                        IRNGT(IWRK) = JWRKT(IINDA)
                        IINDA = IINDA + 1
                        If (IINDA > LMTNA) Exit ! Only B still with unprocessed values
                        XVALA = XDONT(JWRKT(IINDA))
                    End If
                    !
                End Do
            End Do
            !
            !  The Cs become As and Bs
            !
            LMTNA = 2*LMTNA
        End Do
        !
        Return
        !
    End Subroutine I_mrgrnk
end module m_mrgrnk
module global_variable
    integer(4) N
end module
program marge
    use m_valnth
    use m_mrgrnk
    !implicit none
    integer(4) s, e
    Integer(4), Parameter :: nbcl = 1000
    Integer(4), Parameter :: nth = 31
    Integer(4), Parameter :: kdp = selected_real_kind(15)
    Real(4), Dimension(10000) :: xvalt
    Integer(4), Dimension(10000) :: jvalt
    !Integer, Dimension(100000) :: jvalt2
    Integer(4), Dimension(10000) :: jrnkt
    Integer(4) ::  ibcl, lrnk, jres
    Real(4) :: tdep1, tfin1
    integer(4), allocatable::tmp(:)
    !
    N = 10000
    Call cpu_time(tdep1)
    Do ibcl = 1, nbcl
        Call random_number(xvalt(:))
        jvalt = Nint(1000.0*xvalt)
        lrnk = 10 + modulo(ibcl, 10)
        jres = valnth(jvalt, lrnk)
        Call mrgrnk(jvalt, jrnkt)
    End Do
    Call cpu_time(tfin1)
    write (*, *) ((tfin1 - tdep1))*1000.0/real(nbcl), " ms"
    !
    s = 1; e = 10000
    allocate (tmp(N))
    Call cpu_time(tdep1)
    Do ibcl = 1, nbcl
        Call random_number(xvalt(:))
        jvalt = Nint(1000.0*xvalt)
        lrnk = 10 + modulo(ibcl, 10)
        jres = valnth(jvalt, lrnk)
        Call margesort(jvalt, tmp, s, e)
    End Do
    Call cpu_time(tfin1)
    write (*, *) ((tfin1 - tdep1))*1000.0/real(nbcl), " ms"
    !
    Do ibcl = 1, nbcl
        Call random_number(xvalt(:))
        lrnk = 10 + modulo(ibcl, 10)
        xres = valnth(xvalt, lrnk)
        Call mrgrnk(xvalt, jrnkt)
        If (xvalt(jrnkt(lrnk)) /= xres) then
            write (unit=*, fmt=*) "*** Check Failed"
            write (unit=*, fmt=*) xvalt(jrnkt(lrnk))
            write (unit=*, fmt=*) xres
            write (unit=*, fmt=*) ibcl, "seed ", jseet
            stop
        End If
    End Do
end program marge
recursive subroutine margesort(x, tmp, left, right)
    integer(4) left, right, mid
    integer(4) N
    integer(4) x(N), tmp(N)
    !integer y(N), tmp2(N)
    integer(4) i, j, k

    if (left >= right) return

    mid = (left + right)/2
    !分割できるだけ分割する
    call margesort(x, tmp, left, mid)
    call margesort(x, tmp, mid + 1, right)
    !
    j = 0
    tmp(left:mid) = x(left:mid)
    !tmp2(left:mid) = y(left:mid)
    do i = mid + 1, right
        tmp(i) = x(right - j)
        !tmp2(i) = y(right - j)
        j = j + 1
    end do
    !
    i = left
    j = right
    !write (*, '(3x,*(i0,1x),a)', advance='no') x(left:right)
    !write (*, '(a)', advance='no') '>>'
    do k = left, right
        if (tmp(i) < tmp(j)) then
            x(k) = tmp(i)
            ! y(k) = tmp2(i)
            i = i + 1
        else
            x(k) = tmp(j)
            !y(k) = tmp2(j)
            j = j - 1
        end if
    end do
    !write (*, '(3x,*(i0,1x))') x(left:right)
end subroutine margesort
