unit USplitBibleVerses;

interface

uses
  Classes, SysUtils, UUtilsStrings, USlideLayout;

type
  TSplitOption = (soNumbers, soLineBreaks);
  TSplitOptions = set of TSplitOption;

function SplitBibleVerses(strLine: UTF8String; iMaxLineLength: integer; content: TLayoutItem; splitOptions: TSplitOptions): TArrayOfString;

implementation

uses
  RegularExpressionsCore;

function SplitBibleVerses(strLine: UTF8String; iMaxLineLength: integer; content: TLayoutItem; splitOptions: TSplitOptions): TArrayOfString;
var
  regex: TPerlRegEx;

  iPos: integer;
  iBestFind: integer;
  blnHasMaxOccurences: boolean;

  function MatchRegex(strRegEx: string; iMaxLineLength, iMaxOccurences: integer; out blnHasMaxOccurences: boolean): integer;
  var
    iOccurences: integer;
  begin
    Result := -1;
    iOccurences := 0;
    blnHasMaxOccurences := false;
    regex.RegEx := strRegEx;
    regex.Start := iPos+10;
    regex.Stop := iPos + iMaxLineLength;

    if regex.MatchAgain then begin
      repeat
        Result := regex.MatchedOffset;
        inc(iOccurences);
        if (iMaxOccurences > 0) and (iOccurences >= iMaxOccurences) then
          Exit;

      until not regex.MatchAgain;
    end;
  end;

  function GetBestFind(iMaxLineLength: integer; content: TLayoutItem): integer;
  var
    iNextLineBreak, iNextBreak: integer;
    iTextMaxLineLength: integer;
  begin
    iTextMaxLineLength := iMaxLineLength;
    Result := -1;
    while (iTextMaxLineLength > 100) and (Result = -1) do begin

      if (Length(strLine) < (iPos + iTextMaxLineLength)) then begin
        if content.TextFit(copy(strLine, iPos, iTextMaxLineLength)) then begin
          Result := -1;
          Exit;
        end;
      end;

      iNextLineBreak := 0;
      if soLineBreaks in splitOptions then begin
        iNextLineBreak := MatchRegex('\r+', iTextMaxLineLength, 20, blnHasMaxOccurences);
      end;

//      if (iNextLineBreak > 0) then begin
//        inc(iNextLineBreak);
//        // when line in last 80% of text, use it anyway
//        if ((iNextLineBreak - iPos) / iTextMaxLineLength > 0.7) or blnHasMaxOccurences then begin
//          Result := iNextLineBreak;
//        end;
//      end;

      // look for better verse position
      if Result = -1 then begin
        iNextBreak := 0;
        if soNumbers in splitOptions then begin
          iNextBreak := MatchRegex('\d+', iTextMaxLineLength, 20, blnHasMaxOccurences);
        end;
        if iNextBreak > 0 then begin
          if ((iNextBreak - iPos) / iTextMaxLineLength > 0.8) or (iNextBreak > iNextLineBreak) or blnHasMaxOccurences then
            Result := iNextBreak
          else
            Result := iNextLineBreak;
        end;
      end;

      if Result = -1 then begin
        if (iNextLineBreak > 0) then begin
          Result := iNextLineBreak;
        end;
      end;

      // look for word break
      if Result = -1 then begin
        iNextBreak := MatchRegex('\w+', iTextMaxLineLength, 0, blnHasMaxOccurences);
        if iNextBreak > 0 then begin
          if iNextBreak > iNextLineBreak then
            Result := iNextBreak
          else
            Result := iNextLineBreak;
        end;
      end;

      if Assigned(content) and (Result <> -1) then begin
        if not content.TextFit(copy(strLine, iPos, Result-iPos)) then begin
          Result := -1;
          dec(iTextMaxLineLength, 10);
        end;
      end else begin
        // never loop with less space
        Exit;
      end;
    end;
  end;

begin
  strLine := StringReplace(strLine, #13#10, #13, [rfReplaceAll]);

  regex := TPerlRegEx.Create();
  try
    regex.Subject := strLine;
    regex.Options := [preMultiLine];

    SetLength(Result, 0);
    iPos := 1;
    while (iPos < Length(strLine)) and (iPos > 0) do begin
      iBestFind := GetBestFind(iMaxLineLength, content);

      if iBestFind = -1 then begin
        break;
      end;

      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := StringReplace(copy(strLine, iPos, iBestFind-iPos), #13, #13#10, [rfReplaceAll]);
      iPos := iBestFind;
    end;
    // add all that was left
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := StringReplace(copy(strLine, iPos, MaxInt), #13, #13#10, [rfReplaceAll]);
  finally
    regex.Free;
  end;
end;

end.
