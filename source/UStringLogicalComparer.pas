unit UStringLogicalComparer;

interface

function CompareNatural(s1, s2: string): integer; overload;
function CompareNatural(s1, s2: string; blnZeroesFirst: boolean): integer; overload;
function CompareNum(s1: string; var i1: integer; s2: string; var i2: integer; blnZeroesFirst: boolean): integer;

function IsLetter(ch: char): boolean;
function IsDigit(ch: char): boolean;
function IsLetterOrDigit(ch: char): boolean;

implementation

uses
  SysUtils;

function IsLetter(ch: char): boolean;
begin
  Result := False;
  if (ch >= 'a') and (ch <= 'z') then
    Result := True
  else if (ch >= 'A') and (ch <= 'Z') then
    Result := True
end;

function IsDigit(ch: char): boolean;
begin
  Result := False;
  if (ch >= '0') and (ch <= '9') then
    Result := True;
end;

function IsLetterOrDigit(ch: char): boolean;
begin
  result := IsLetter(ch) or IsDigit(ch);
end;


function CompareNatural(s1, s2: string): integer;
begin
  Result := CompareNatural(s1, s2, false);
end;

function CompareNatural(s1, s2: string; blnZeroesFirst: boolean): integer;
var
  sp1, sp2: boolean;
  i1, i2, r: integer;
  c1, c2: char;
  letter1, letter2: boolean;
begin
  Result := 0;
  if Length(s1) = 0 then begin
    if Length(s2) = 0 then begin
      Exit;
    end;
    Result := -1;
    Exit;
  end;
  if Length(s2) = 0 then begin
    Result := 1;
    Exit;
  end;
  sp1 := IsLetterOrDigit(s1[1]);
  sp2 := IsLetterOrDigit(s2[1]);
  if sp1 and not sp2 then begin
    Result := 1;
    Exit;
  end;
  if not sp1 and sp2 then begin
    Result := -1;
    Exit;
  end;

  i1 := 1;
  i2 := 1;
  r := 0;
  while (true) do begin
    c1 := s1[i1];
    c2 := s2[i2];
    sp1 := IsDigit(c1);
    sp2 := IsDigit(c2);
    if not sp1 and not sp2 then begin
      letter1 := IsLetter(c1);
      letter2 := IsLetter(c2);
      if letter1 and letter2 then begin
        r := CompareText(c1, c2);
        if r <> 0 then begin
          Result := r;
          Exit;
        end;
      end else if not letter1 and not letter2 then begin
        if ord(c1) < ord(c2) then begin
          Result := -1;
          Exit;
        end else if ord(c1) > ord(c2) then begin
          Result := 1;
          Exit;
        end else begin
          Result := 0;
          //Exit;
        end;
      end else if not letter1 and letter2 then begin
        Result := -1;
        Exit;
      end else if letter1 and not letter2 then begin
        Result := 1;
        Exit;
      end;
    end else if sp1 and sp2 then begin
      r := CompareNum(s1, i1, s2, i2, blnZeroesFirst);
      if r <> 0 then begin
        Result := r;
        Exit;
      end;
    end else if sp1 then begin
      Result := -1;
      Exit;
    end else if sp2 then begin
      Result := 1;
      Exit;
    end;
    inc(i1);
    inc(i2);
    if  (i1 > length(s1)) and (i2 > length(s2)) then begin
      Result := 0;
      Exit;
    end else if (i1 > length(s1)) then begin
      Result := -1;
      Exit;
    end else if (i2 > length(s2)) then begin
      Result := 1;
      Exit;
    end;
  end;
end;

//lookahead
procedure ScanNumEnd( s: string; start: integer; var iEnd: integer; var nzStart: integer );
var
  countZeros: boolean;
begin
  nzStart := start;
  iEnd := start;
  countZeros := true;
  while (IsDigit( s[iEnd] ) ) do begin
    if ( countZeros and (s[iEnd] = '0' ) ) then begin
      inc(nzStart);
    end else
      countZeros := false;
    inc(iEnd);
    if ( iEnd > Length(s) ) then
      break;
  end;
end;


function CompareNum(s1: string; var i1: integer; s2: string; var i2: integer; blnZeroesFirst: boolean): integer;
var
  nzStart1, nzStart2: integer;
  end1, end2: integer;
  start1, start2: integer;
  zl1, zl2: integer;
  nzLength1, nzLength2: integer;
  j1, j2: integer;
  r: integer;
  length1, length2: integer;
begin
  nzStart1 := i1;
  nzStart2 := i2;
  end1 := i1;
  end2 := i2;
  ScanNumEnd( s1, i1, end1, nzStart1);
  ScanNumEnd( s2, i2, end2, nzStart2);
  start1 := i1;
  i1 := end1 -1;
  start2 := i2;
  i2 := end2 -1;
  if blnZeroesFirst then begin
    zl1 := nzStart1 - start1;
    zl2 := nzStart2 - start2;
    if zl1 > zl2 then begin
      Result := -1;
      Exit;
    end;
    if zl1 < zl2 then begin
      Result := 1;
      Exit;
    end;
  end;

  nzLength1 := end1 - nzStart1;
  nzLength2 := end2 - nzStart2;

  if ( nzLength1 < nzLength2 ) then begin
    Result := -1;
    Exit;
  end else if ( nzLength1 > nzLength2 ) then begin
    Result := 1;
    Exit;
  end;

  j1 := nzStart1;
  j2 := nzStart2;
  while (j1 <= i1) do begin
    r := CompareText(s1[j1], s2[j2] );
    if r <> 0 then begin
      Result := r;
      Exit;
    end;
    inc(j1);
    inc(j2);
  end;

  // the nz parts are equal
  length1 := end1 - start1;
  length2 := end2 - start2;
  if ( length1 = length2 ) then begin
    Result := 0;
    Exit;
  end;
  if ( length1 > length2 ) then begin
    Result := -1;
    Exit;
  end;
  Result := 1;
end;

end.
