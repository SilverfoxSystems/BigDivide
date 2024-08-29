Option Explicit Off
Imports HyperLib

'!!! For this code to work, because of the numeric methods used, "Skip integer overflow checks" option must be checked in the Advanced compile settings

Public Class Form1

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Hyper.displayMode = Hyper.displayModeType.inDecimalBase2_64
        Hyper.maxDigitsInString = 7 'display range

        ' input some random numbers for a and d ->
        Dim a As New Hyper(-251, -253)    'assign 3 * 8 bytes for a, with its loest exponent (2^64)^253                  
        a(-252) = -79786875757656175
        a(-253) = 79978687575765619 '
        a(-251) = 7978687575765619

        Dim d As New Hyper(-444, -444) 'reserve 8 bytes for the divisor
        d(-444) = 1 + (2 ^ 53)
        d(-5) = 1 + 2 ^ 44 'mantissa automatically gets extened when assigning vslues at exponents out of current range; now the size of "d" is ((444-5) * 8) bytes

        Debug.WriteLine("")

        Dim rdiv As Hyper = QuickDivide(a, d)

        Debug.WriteLine("=================== result:")
        Debug.WriteLine(rdiv)

        rdiv *= d

        Debug.WriteLine("=================== check:")
        Debug.WriteLine(rdiv)

    End Sub



    Private Function QuickDivide(dividend As Hyper, d As Hyper) As Hyper
        Return dividend * ReciprocalVal(d)
    End Function
    Private Function ReciprocalVal(d As Hyper) As Hyper

        precision% = 1444 'number of 64-bit digits to extract. Must be larger than exponent range
        precision2% = 222 'may be zero

        Dim r As New Hyper(precision, 0) ' New Hyper(0, 0)
        Dim bp, r1 As Hyper

        hiExp% = d.FindHighExponent
        lowExp% = d.FindLowExponent '000000000000000000000000000000000000000000000 d.PartSize
        lowVal& = d(lowExp)
        If lowVal And 1 = 0 Then
            Debug.WriteLine("even nr")
            'if the least significant bit is 0, then we can help ourselves with dividing/multiplying the dividend, and then the result, by 2^n.

            d(lowExp - 1) = 1
            lowVal = 1
            lowExp -= 1
        End If
        mq& = GetMagicNr(lowVal)
        pos% = lowExp '
        d.Round(-pos)
        d.PartSize = 0
        bp = New Hyper("1")
        pos1% = 0

mainloop:
        ' get the sequence which, when multiplied by divisor, nullifies itself

        r1 = New Hyper(pos1, pos1)
        r1(pos1) = bp(pos1) * mq

        r(pos1) = r1(pos1)
        bp -= d * r1
        bp.Negate()

        pos1 += 1

        If pos1 > precision Then GoTo nx
        'reciprocal values of large numbers tend to repeat at very large intervals, so we'll be satisfied with our precision                 

        GoTo mainloop

nx:
        r1 = r * d

        hi% = r1.FindHighExponent
        r.Divide(r1(hi), precision2)
        r.PartSize = hi + r1.PartSize + r.PartSize + lowExp  '.PartSize            
        d.PartSize = -lowExp

        Debug.WriteLine("--=-=-=-=- recip*d:")
        Debug.WriteLine(r * d) 'should output close to 1
        Debug.WriteLine(r)
        Debug.WriteLine("--=-=-=-=-")

        Return r
    End Function

    Private Function GetMagicNr&(a&)

        ' Magic number or "reciprocal integer" - GET THE 64-BIT NUMBER WHICH, when multiplied by the lowest digit, gives 1 as the remainder of 2^64               
        ' Only for odd numbers.

        bt& = 1 'bit tester
        d& = a 'bit mask

        r& = 0 : i& = 0 : r0& = 0

        For i = 0 To 63

            If bt And r Then GoTo skip

            r += d
            r0 = r0 Or bt
skip:
            bt <<= 1 : d <<= 1
        Next

        Return r0
    End Function

End Class
