'**************************************************
' FILE:         torrentCodec
' AUTHOR:       Paulo Santos
' CREATED:      2005.JAN.07
' COPYRIGHT:    Copyright 2005 © Paulo Santos
'               All Rights Reserved.
' DESCRIPTION:
'       Code and Decode torrent file information.
'
' MODIFICATION HISTORY:
' 01    2005.JAN.07
'       Paulo Santos
'       Added Fragment Decoder
'       Added Stream Decoder
'
' 02    2005.JAN.08
'       Paulo Santos
'       Added Encoder
'       Update on Fragment Decode to avoid duplicated coding
'***************************************************

Namespace torrent

    Public Class BCodec

        Public Sub New()
            '*
            '* Simply declares the class as public available
            '*
        End Sub

        '* FUNCTION  : Encode -- OVERLOADED --
        '* OBJECTIVE : Encode a fragment to a String
        '* INPUT     : Any available format
        '* OUTPUT    : Encoded String
        '*
        Public Overloads Shared Function Encode(ByVal Dictionary As torrent.Base.Dictionary) As String

            Dim strExit As New System.Text.StringBuilder
            Dim strName As String

            '*
            '* Encode a Dictionary Data Type
            '*
            strExit.Append("d")
            For Each strName In Dictionary.AllKeys
                strExit.Append(Encode(strName))
                strExit.Append(Encode(Dictionary(strName)))
            Next
            strExit.Append("e")

            Return strExit.ToString

        End Function

        Public Overloads Shared Function Encode(ByVal ObjectArray As Array) As String

            Dim strExit As New System.Text.StringBuilder
            Dim objObject As Object

            '*
            '* Encode an Array Data Type
            '*

            strExit.Append("l")
            For Each objObject In ObjectArray
                strExit.Append(Encode(objObject))
            Next
            strExit.Append("e")

            Return strExit.ToString

        End Function

        Public Overloads Shared Function Encode(ByVal Text As String) As String

            '*
            '* Encode a String Data Type
            '*
            Return Text.Length.ToString & ":" & Text

        End Function

        Public Overloads Shared Function Encode(ByVal Number As Double) As String

            '*
            '* Encode a Double Data Type
            '* Just keep in mind that torrent Data Type are Integers only
            '*
            Return "i" & Number.ToString(New String("#"c, 309) & "0").Trim() & "e"

        End Function

        Public Overloads Shared Function Encode(ByVal Number As Single) As String

            '*
            '* Encode a Single Data Type
            '* Just keep in mind that torrent Data Type are Integers only
            '*
            Return "i" & Number.ToString(New String("#"c, 39) & "0").Trim() & "e"

        End Function

        Public Overloads Shared Function Encode(ByVal Number As Long) As String

            '*
            '* Encode a Long (int64) Data Type
            '* Just keep in mind that torrent Data Type are Integers only
            '*
            Return "i" & Number.ToString(New String("#"c, 19) & "0").Trim() & "e"

        End Function

        Public Overloads Shared Function Encode(ByVal Number As Integer) As String

            '*
            '* Encode a Integer (int32) Data Type
            '* Just keep in mind that torrent Data Type are Integers only
            '*
            Return "i" & Number.ToString("#########0").Trim() & "e"

        End Function

        Public Overloads Shared Function Encode(ByVal Number As Short) As String

            '*
            '* Encode a Short (Int16) Data Type
            '* Just keep in mind that torrent Data Type are Integers only
            '*
            Return "i" & Number.ToString("####0").Trim() & "e"

        End Function

        Public Overloads Shared Function Encode(ByVal Number As Byte) As String

            '*
            '* Encode a Byte Data Type
            '* Just keep in mind that torrent Data Type are Integers only
            '*
            Return "i" & Number.ToString("##0").Trim() & "e"

        End Function

        '* FUNCTION  : Decode -- OVERLOADED --
        '* OBJECTIVE : Decode a fragment or a .torrent file
        '* INPUT     : Either a fragment or a FileStream
        '* OUTPUT    : Object
        '*
        '* REMARKS   : Once that encoded information can contain multiple
        '*             data formats the function return a generic object
        '*             thus allowing several data types.
        '*
        Public Overloads Shared Function Decode(ByVal DataStream As System.IO.Stream, Optional ByVal LastByteRead As Short = (-1)) As Object

            If (Rnd() > 0.8) Then System.Threading.Thread.Sleep(100)

            '*
            '* Read the first byte of the stream in order to 
            '* determine what kind of object we will create
            '*
            If (LastByteRead = (-1)) Then LastByteRead = DataStream.ReadByte
            Select Case (Chr(LastByteRead))
                Case "d"
                    '*
                    '* Dictionary Data Type
                    '*
                    Dim oDictionary As New torrent.Base.Dictionary
                    Dim Key As String
                    Dim Value As Object
                    Dim Jump As Integer = 1
                    Dim KeyJump As Integer
                    Dim ValueJump As Integer

                    '*
                    '* Note that it calls the Decode Function Recursively
                    '* this way we don't need several different functions 
                    '* to do the same thing
                    '*
                    Do While True
                        '*
                        '* Checks the next byte
                        '*
                        LastByteRead = DataStream.ReadByte()
                        If (Chr(LastByteRead) = "e") Then
                            Exit Do
                        End If
                        Key = Decode(DataStream, LastByteRead) ' <-- Ignores the first character of the fragment once we know the type

                        '*
                        '* Validate the Key we just got
                        '*
                        If (Key Is Nothing) Then
                            '*
                            '* An error ocurred while decoding the key
                            '* with no key why go further?
                            '*

                            'TODO: add log info (Dicionary Entry with No Key)
                            Return Nothing
                        ElseIf (Not (Key.GetType() Is GetType(String))) OrElse ((Key.GetType() Is GetType(String)) AndAlso (Key = "")) Then
                            '*
                            '* We cannot have a Key-Value pair with anything but a valid string as key
                            '*

                            'TODO: add log info (Dicionary Entry with Invalid Key)
                            Return Nothing
                        End If

                        '*
                        '* Retrieve the Value
                        '*
                        Value = Decode(DataStream)

                        '*
                        '* Validate it
                        '*
                        If (Value Is Nothing) Then
                            '*
                            '* An error occured while decoding the value
                            '*

                            'TODO: add log info (Dicionary Entry with no Value)
                            Return Nothing
                        End If

                        '*
                        '* Well, we have both the Key and the Value, so we return the entry
                        '*
                        oDictionary.Add(Key, Value)
                    Loop

                    '*
                    '* Returns the Dicitionary Found
                    '*
                    Return oDictionary
                Case "i"
                    '*
                    '* Integer Data Type
                    '*

                    '*
                    '* For the Integer Data Type we just keep adding the number 
                    '* and scalating the values as we go up, until we find an "e"
                    '* that means the "end" of the number.
                    '*
                    '* If we find anything else in the middle it's an error
                    '*
                    '* We need also to pay attention to negative numbers
                    '* once we are dealing with Integer the minus sing MUST be on the
                    '* first position of the number
                    '*
                    '* Ex: i-10e = -10
                    '*     i1-0e = Nothing
                    '*     i-e   = Nothing
                    '*

                    Dim chrChar As Char
                    Dim flgError As Boolean

                    Dim ValueString As New System.Text.StringBuilder
                    Dim ValueNumber As Double

                    Do While True
                        Try
                            chrChar = Chr(DataStream.ReadByte)
                        Catch ex As Exception
                            Exit Do
                        End Try
                        If (chrChar = "e") Then
                            flgError = (ValueString.Length = 0)
                            'TODO: Add log entry Invalid Integer Value
                            Exit Do
                        ElseIf (chrChar <> "-" AndAlso (chrChar < "0" Or chrChar > "9")) OrElse _
                            (chrChar = "-" And ValueString.Length <> 0) Then
                            flgError = True
                            'TODO: Add log entry Invalid Integer Value
                            Exit Do
                        End If

                        ValueString.Append(chrChar)
                    Loop

                    '*
                    '* Check for error
                    '*
                    If (flgError) Then
                        'TODO: Add log entry Invalid Integer Value
                        Return Nothing
                    End If

                    '*
                    '* Return the value accordingly
                    '*
                    ValueNumber = Val(ValueString.ToString)

                    If (ValueNumber < Single.MinValue) OrElse (ValueNumber > Single.MaxValue) Then
                        '*
                        '* Return Double
                        '*
                        Return ValueNumber
                    ElseIf (ValueNumber < Long.MinValue) OrElse (ValueNumber > Long.MaxValue) Then
                        '*
                        '* Return Single
                        '*
                        Return CSng(ValueNumber)
                    ElseIf (ValueNumber < Integer.MinValue) OrElse (ValueNumber > Integer.MaxValue) Then
                        '*
                        '* Return Long
                        '*
                        Return CLng(ValueNumber)
                    ElseIf (ValueNumber < Short.MinValue) OrElse (ValueNumber > Short.MaxValue) Then
                        '*
                        '* Return Integer
                        '*
                        Return CInt(ValueNumber)
                    ElseIf (ValueNumber < Byte.MinValue) OrElse (ValueNumber > Byte.MaxValue) Then
                        '*
                        '* Return Short
                        '*
                        Return CShort(ValueNumber)
                    Else
                        '*
                        '* Return Byte
                        '*
                        Return CByte(ValueNumber)
                    End If
                Case "0"
                    '*
                    '* Special String Type : Null String
                    '*
                    If (Chr(DataStream.ReadByte) <> ":") Then
                        'TODO: Add log entry Invalid String Value
                        Return Nothing
                    Else
                        Return ""
                    End If
                Case "1", "2", "3", "4", "5", "6", "7", "8", "9"
                    '*
                    '* String Data Type
                    '*
                    Dim intByte As Integer
                    Dim strByteCount As New System.Text.StringBuilder
                    Dim lngByteCount As Long
                    Dim strStringValue As New System.Text.StringBuilder

                    '*
                    '* The Strings are coded by its byte numbers followed by a colon
                    '*
                    If (LastByteRead = (-1)) Then
                        intByte = DataStream.ReadByte
                    Else
                        intByte = LastByteRead
                    End If
                    Do While True
                        If (intByte = (-1)) OrElse ((Not Char.IsDigit(Chr(intByte))) AndAlso (Chr(intByte) <> ":")) Then
                            'TODO: Add log Invalid String Format
                            Return Nothing
                        End If
                        If (Chr(intByte) = ":") Then
                            Exit Do
                        Else
                            strByteCount.Append(Chr(intByte))
                        End If
                        intByte = DataStream.ReadByte
                    Loop

                    lngByteCount = Val(strByteCount.ToString)
                    Do While (lngByteCount > 0)
                        intByte = DataStream.ReadByte
                        If (intByte = (-1)) Then
                            'TODO: Add log Invalid String Format
                            Return Nothing
                        End If
                        strStringValue.Append(Chr(intByte))
                        lngByteCount -= 1
                    Loop

                    '*
                    '* Returns the string found
                    '*
                    Return strStringValue.ToString
                Case "l"
                    '*
                    '* Array Data Type
                    '*

                    '*
                    '* An array data type can contain any other data types,
                    '* including another array. It begins with an "l" and ends with an "e"
                    '*
                    Dim aObject() As Object = {}
                    Dim intByte As Integer

                    '*
                    '* Adds the Objects as it comes by. If any object returns invalid
                    '* the entire array is considered invalid
                    '*
                    Do While True
                        intByte = DataStream.ReadByte
                        If (intByte = (-1)) Then
                            '*
                            '* Reached the end of the stream before finishing the list
                            '*

                            'TODO: Add log entry Invalid Array
                            Return Nothing
                        End If
                        '*
                        '* Check the end of the Array
                        '*
                        If (Chr(intByte) = "e") Then
                            Exit Do
                        End If

                        '*
                        '* Resize the Array
                        '*
                        ReDim Preserve aObject(aObject.Length)

                        '*
                        '* Decode the Value
                        '*
                        aObject(aObject.Length - 1) = Decode(DataStream, intByte)
                        If (aObject(aObject.Length - 1) Is Nothing) Then
                            '*
                            '* Object Invalid
                            '*

                            'TODO: Add log entry Invalid Array
                            Return Nothing
                        End If
                    Loop

                    Return aObject
                Case Else
                    DataStream.ReadByte()
                    Return Nothing
            End Select

        End Function

        Public Overloads Shared Function Decode(ByVal Fragment As String) As Object

            '*
            '* Converts the String Fragment into a Byte Array
            '*
            Dim aByte() As Byte = {}
            Dim chrChar As Char

            For Each chrChar In Fragment.ToCharArray
                ReDim Preserve aByte(aByte.Length)
                aByte(aByte.Length - 1) = Asc(chrChar)
            Next

            '*
            '* Decode the Fragment as a Stream
            '*
            Return Decode(aByte)

        End Function

        Public Overloads Shared Function Decode(ByVal ByteArray As Byte()) As Object

            '*
            '* Decode the ByteArray as a Stream
            '*
            Return Decode(New System.IO.MemoryStream(ByteArray))

        End Function

    End Class

End Namespace