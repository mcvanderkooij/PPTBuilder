unit USplitBibleVerses;

interface

uses
  Classes, SysUtils, UUtilsStrings, USlideLayout;

type
  TSplitOption = (soNumbers, soLineBreaks);
  TSplitOptions = set of TSplitOption;

function SplitBibleVerses(strLine: String; iMaxLineLength: integer; content: TLayoutItem; splitOptions: TSplitOptions): TArrayOfString;

implementation

uses
  RegExpr;

function SplitBibleVerses(strLine: String; iMaxLineLength: integer; content: TLayoutItem; splitOptions: TSplitOptions): TArrayOfString;
var
  iBestFind: integer;

  function MatchRegex(strLine, strRegEx: string; iMaxLineLength: integer): integer;
  var
    iOccurences, i: integer;
    regex: TRegExpr;
  begin
    Result := -1;
    iOccurences := 0;
    regex := TRegExpr.Create;
    try
      regex.ModifierM := True;
      regex.Expression := strRegEx;
      if regex.Exec(strLine) then begin
        repeat
          if regex.MatchPos[0] < iMaxLineLength then begin
            Result := regex.MatchPos[0];
          end else
            break;
        until not regex.ExecNext;
      end;
    finally
      regex.Free;
    end;
  end;

  function GetBestFind(strLine: string; iMaxLineLength: integer; content: TLayoutItem): integer;
  var
    iTextMaxLineLength, iNextBreak, iNextBestBreak: integer;
  begin
    iTextMaxLineLength := iMaxLineLength;
    Result := -1;
    while (iTextMaxLineLength > 100) and (Result = -1) do begin

      // does it fit anyway?
      if (Length(strLine) < iTextMaxLineLength) then begin
        if content.TextFit(strLine) then begin
          Result := -1;
          Exit;
        end;
      end;

      // first only find the max line length
      if Assigned(content) then begin
        while (iTextMaxLineLength > 100) do begin
          if not content.TextFit(copy(strLine, 1, iTextMaxLineLength)) then begin
            dec(iTextMaxLineLength, 10);
          end else
            break;
        end;
      end;

      iNextBestBreak := -1;
      // empty line wins
      iNextBreak := MatchRegex(strLine, '(\r\n){2,}', iTextMaxLineLength);
      if iNextBreak <> -1 then begin
        if iNextBestBreak < iNextBreak then
          iNextBestBreak := iNextBreak;
        if (iNextBreak / iTextMaxLineLength > 0.5) then
          Result := iNextBreak;
      end;

      if soNumbers in splitOptions then begin
        if Result = -1 then begin
          iNextBreak := MatchRegex(strLine, '\d+', iTextMaxLineLength);
          if iNextBreak <> -1 then begin
            if iNextBestBreak < iNextBreak then
              iNextBestBreak := iNextBreak;
            if (iNextBreak / iTextMaxLineLength > 0.6) then
              Result := iNextBreak;
          end;
        end;
      end;

      if soLineBreaks in splitOptions then begin
        if Result = -1 then begin
          iNextBreak := MatchRegex(strLine, '\r\n+', iTextMaxLineLength);
          if iNextBreak <> -1 then begin
            if iNextBestBreak < iNextBreak then
              iNextBestBreak := iNextBreak;
            if (iNextBreak / iTextMaxLineLength > 0.7) then
              Result := iNextBreak;
          end;
        end;
      end;

      if Result = -1 then begin
        iNextBreak := MatchRegex(strLine, '\w+', iTextMaxLineLength);
        if iNextBreak <> -1 then begin
          if iNextBestBreak < iNextBreak then
            iNextBestBreak := iNextBreak;
        end;
      end;

      if Result = -1 then begin
        if iNextBestBreak > 0 then
          Result := iNextBestBreak;
      end;

      // does the result fit in content?
      if Assigned(content) and (Result <> -1) then begin
        if not content.TextFit(copy(strLine, 1, Result-1)) then begin
          iTextMaxLineLength := Result -1;
          Result := -1;
        end;
      end else begin
        // never loop with less space
        Exit;
      end;
    end;
  end;

begin
  SetLength(Result, 0);
  while Length(strLine) > 0 do begin
    iBestFind := GetBestFind(strLine, iMaxLineLength, content);

    if iBestFind = -1 then begin
      break;
    end;

    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := copy(strLine, 1, iBestFind-1);

    strLine := trim(copy(strLine, iBestFind, MaxInt));
  end;
  // add all that was left
  SetLength(Result, Length(Result) + 1);
  Result[High(Result)] := copy(strLine, 1, MaxInt);
end;

end.
