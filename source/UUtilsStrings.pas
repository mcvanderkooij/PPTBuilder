unit UUtilsStrings;

interface

uses
  TypInfo;

type
  TArrayOfString = Array of String;

function Split(strValue, strDelimiter: String): TArrayOfString;
function Join(const arrValue: TArrayOfString; const strDelimiter: String): String;
function PosIEx(const SubStr, S: string; Offset: Integer = 1): Integer;

function AnsiEncode64(S: AnsiString): AnsiString;
function AnsiDecode64(S: AnsiString): AnsiString;

function RespaceOverviewName(strText: string; blnAddTabs: boolean): string;

procedure StringToSet(Info: PTypeInfo; var SetParam; const Value: AnsiString);
function SetToString(Info: PTypeInfo; const SetParam; Brackets: Boolean): AnsiString;

implementation

uses
  Math, SysUtils, UStringLogicalComparer;

function Split(strValue, strDelimiter: String): TArrayOfString;
var
  nPos: Integer;
  iDelimeterLength: integer;
begin
  if (pos(strDelimiter, strValue) = 0) and (Trim(strValue) = '') then begin
    SetLength(Result, 1);
    Exit;
  end;

  iDelimeterLength := Length(strDelimiter);
  SetLength(Result, 0);

  repeat
    nPos := Pos(strDelimiter, strValue);
    if (nPos > 0) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := Copy(strValue, 1, nPos - 1);
      Delete(strValue, 1, nPos+iDelimeterLength -1);
      nPos := Pos(strDelimiter, strValue);
    end;
  until (nPos = 0);
  SetLength(Result, Length(Result) + 1);
  Result[Length(Result) - 1] := strValue;
end;

function Join(const arrValue: TArrayOfString; const strDelimiter: String): String;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(arrValue) - 1 do
  begin
    if (i > 0) then
      Result := Result + strDelimiter;
    Result := Result + arrValue[i];
  end;
end;

var
 LookUpTable : array of Char;

procedure InitializeLookUpTable;
var
 I: Byte;
 S1, S2: String;

begin
 SetLength(LookUpTable, 256);
 for I := 0 to 255 do
  begin
   S1 := Char(I);
   S2 := UpperCase(S1);
   LookUpTable[I] := S2[1];
  end;
end;


function PosIEx(const SubStr, S: string; Offset: Integer = 1): Integer;
var
 I2, SubStrLength, StrLength : Integer;
 SubStrFirstCharUpper : Char;

begin
  Result := 0;
  if (Offset <= 0) then
    Exit;
  if S <> '' then
    StrLength := Length(S)
  else
    Exit;
  if SubStr <> '' then
    SubStrLength := Length(SubStr)
  else
    Exit;
  if (SubStrLength <= 0) then
    Exit;
  if (StrLength <= 0) then
    Exit;
  if (Integer(Offset) > StrLength) then
    Exit;
  if SubStrLength > 1 then begin
    if (StrLength - Integer(Offset) + 1 < SubStrLength) then //No room for match
      Exit;
    Result := Offset;
    SubStrFirstCharUpper := LookUpTable[Ord(SubStr[1])];
    repeat
      if SubStrFirstCharUpper = LookUpTable[Ord(S[Result])] then begin
        if Result + SubStrLength - 1 > StrLength then begin
          Result := 0;
          Exit;
        end;
        I2 := 1;
        repeat
          if S[Result+I2] <> SubStr[I2+1] then begin
            if LookUpTable[Ord(S[Result+I2])] <> LookUpTable[Ord(SubStr[I2+1])] then
              Break;
          end;
          Inc(I2);
          if (I2 >= SubStrLength) then
            Exit;
        until(False);
      end;
      Inc(Result);
    until(Result > StrLength);
    Result := 0;
  end else begin
    SubStrFirstCharUpper := LookUpTable[Ord(SubStr[1])];
    Result := Offset;
    repeat
      if SubStrFirstCharUpper = LookUpTable[Ord(S[Result])] then
        Exit;
      Inc(Result);
    until (Result > StrLength);
    Result := 0;
  end;
end;

const
  Codes64: AnsiString = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+/';

function AnsiEncode64(S: AnsiString): AnsiString;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Ord(s[i]);
    b := b * 256 + x;
    a := a + 8;
    while a >= 6 do
    begin
      a := a - 6;
      x := b div (1 shl a);
      b := b mod (1 shl a);
      Result := Result + Codes64[x + 1];
    end;
  end;
  if a > 0 then
  begin
    x := b shl (6 - a);
    Result := Result + Codes64[x + 1];
  end;
end;

function AnsiDecode64(S: AnsiString): AnsiString;
var
  i: Integer;
  a: Integer;
  x: Integer;
  b: Integer;
begin
  Result := '';
  a := 0;
  b := 0;
  for i := 1 to Length(s) do
  begin
    x := Pos(s[i], codes64) - 1;
    if x >= 0 then
    begin
      b := b * 64 + x;
      a := a + 6;
      if a >= 8 then
      begin
        a := a - 8;
        x := b shr a;
        b := b mod (1 shl a);
        x := x mod 256;
        Result := Result + AnsiChar(x);
      end;
    end
    else
      Exit;
  end;
end;

function FindLastDelimiter(const Delimiters, S: string; StartIdx: Integer = -1): Integer;
var
  Stop: Boolean;
  Len: Integer;
begin
  Result := 0;

  if StartIdx = -1 then begin
    StartIdx := Length(S) -1;
  end;
  Stop := False;
  while (not Stop) and (StartIdx > 0) do
    if IsDelimiter(Delimiters, S, StartIdx) then
    begin
      Result := StartIdx;
      Stop := True;
    end
    else
      Dec(StartIdx);
end;

function StringReplace(const S, OldPattern, NewPattern: string; FirstIndex: integer;
  Flags: TReplaceFlags): string; overload;
//const
//  FirstIndex = Low(string);
var
  SearchStr, Patt, NewStr: string;
  Offset, I, L: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := AnsiUpperCase(S);
    Patt := AnsiUpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  if SearchStr.Length <> S.Length then
  begin
    I := FirstIndex;
    L := OldPattern.Length;
    while I <= High(S) do
    begin
      if string.Compare(S, I - FirstIndex, OldPattern, 0, L, True) = 0 then
      begin
        Result := Result + NewPattern;
        Inc(I, L);
        if not (rfReplaceAll in Flags) then
        begin
          Result := Result + S.Substring(I - FirstIndex, MaxInt);
          Break;
        end;
      end
      else
      begin
        Result := Result + S[I];
        Inc(I);
      end;
    end;
  end
  else
  begin
    while SearchStr <> '' do
    begin
      Offset := PosIEx(Patt, SearchStr, FirstIndex);
      if Offset = 0 then
      begin
        Result := Result + NewStr;
        Break;
      end;
      Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
      NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
      if not (rfReplaceAll in Flags) then
      begin
        Result := Result + NewStr;
        Break;
      end;
      SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
    end;
  end;
end;

function RespaceOverviewName(strText: string; blnAddTabs: boolean): string;
var
  iPos: integer;

  function posColonOrStart(strText: string): integer;
  begin
    Result := Max(pos(':', strText), low(string));
  end;

begin
  Result := trim(strText);
  // first remove all extra spaces
  Result := StringReplace(Result, #9, ' ',   [rfReplaceAll]);
  Result := StringReplace(Result, '  ', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, ' :', ':', [rfReplaceAll]);
  Result := StringReplace(Result, ': ', ':', [rfReplaceAll]);
  Result := StringReplace(Result, ' ,', ',', [rfReplaceAll]);
  Result := StringReplace(Result, ', ', ',', [rfReplaceAll]);
  Result := StringReplace(Result, ' -', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '- ', '-', [rfReplaceAll]);
  Result := StringReplace(Result, 'en ', 'en', posColonOrStart(Result), [rfReplaceAll]);
  Result := StringReplace(Result, ' en', 'en', posColonOrStart(Result), [rfReplaceAll]);

  if blnAddTabs then begin
    iPos := FindLastDelimiter(' ', Result);
    if iPos > 0 then begin
      if ((iPos + 1) < Length(Result)) and IsDigit(Result[iPos + 1]) then begin
        Result[iPos] := #9
      end else begin
        iPos := FindLastDelimiter(' ', Result, iPos -1);
        if ((iPos + 1) < Length(Result)) and IsDigit(Result[iPos + 1]) then begin
          Result[iPos] := #9
        end;
      end;
    end;
    Result := StringReplace(Result, ':', #9': ', [rfReplaceAll]);
  end else
    Result := StringReplace(Result, ':', ' : ', [rfReplaceAll]);

  Result := StringReplace(Result, ',', ', ', [rfReplaceAll]);
  Result := StringReplace(Result, '-', ' - ', [rfReplaceAll]);
  Result := StringReplace(Result, 'en', ' en ', posColonOrStart(Result), [rfReplaceAll]);

  if Length(Result) > 0 then begin
    Result[1] := UpCase(Result[1]);
  end;
end;

function GetOrdValue(Info: PTypeInfo; const SetParam): Integer;
begin
  Result := 0;

  case GetTypeData(Info)^.OrdType of
    otSByte, otUByte:
      Result := Byte(SetParam);
    otSWord, otUWord:
      Result := Word(SetParam);
    otSLong, otULong:
      Result := Integer(SetParam);
  end;
end;

procedure SetOrdValue(Info: PTypeInfo; var SetParam; Value: Integer);
begin
  case GetTypeData(Info)^.OrdType of
    otSByte, otUByte:
      Byte(SetParam) := Value;
    otSWord, otUWord:
      Word(SetParam) := Value;
    otSLong, otULong:
      Integer(SetParam) := Value;
  end;
end;

function SetToString(Info: PTypeInfo; const SetParam; Brackets: Boolean): AnsiString;
var
  S: TIntegerSet;
  TypeInfo: PTypeInfo;
  I: Integer;
begin
  Result := '';

  Integer(S) := GetOrdValue(Info, SetParam);
  TypeInfo := GetTypeData(Info)^.CompType^;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  if Brackets then
    Result := '[' + Result + ']';
end;

procedure StringToSet(Info: PTypeInfo; var SetParam; const Value: AnsiString);
var
  P: PAnsiChar;
  EnumInfo: PTypeInfo;
  EnumName: AnsiString;
  EnumValue, SetValue: Longint;

  function NextWord(var P: PAnsiChar): AnsiString;
  var
    I: Integer;
  begin
    I := 0;
    // scan til whitespace
    while not (P[I] in [',', ' ', #0,']']) do
      Inc(I);
    SetString(Result, P, I);
    // skip whitespace
    while P[I] in [',', ' ',']'] do
      Inc(I);
    Inc(P, I);
  end;

begin
  SetOrdValue(Info, SetParam, 0);
  if Value = '' then
    Exit;

  SetValue := 0;
  P := PAnsiChar(Value);
  // skip leading bracket and whitespace
  while P^ in ['[',' '] do
    Inc(P);
  EnumInfo := GetTypeData(Info)^.CompType^;
  EnumName := NextWord(P);
  while EnumName <> '' do
  begin
    EnumValue := GetEnumValue(EnumInfo, EnumName);
    if EnumValue < 0 then
    begin
      SetOrdValue(Info, SetParam, 0);
      Exit;
    end;
    Include(TIntegerSet(SetValue), EnumValue);
    EnumName := NextWord(P);
  end;
  SetOrdValue(Info, SetParam, SetValue);
end;

//Example usage:
//
//var
//  A: TAlignSet;
//  S: AnsiString;
//begin
//  // set to string
//  A := [alClient, alLeft, alTop];
//  S := SetToString(TypeInfo(TAlignSet), A, True);
//  ShowMessage(Format('%s ($%x)', [S, Byte(A)]));
//
//  // string to set
//  S := '[alNone, alRight, alCustom]';
//  StringToSet(TypeInfo(TAlignSet), A, S);
//  ShowMessage(Format('%s ($%x)', [SetToString(TypeInfo(TAlignSet), A, True), Byte(A)]));
//end;

initialization
  InitializeLookUpTable;
end.
