unit USlideVariables;

interface

uses
  Classes, UFastKeysSS;

procedure DetectSlideVariables(strText: string; slVariables: TStringList);
procedure CleanupSlideVariables(vars: TFastKeyValuesSS; slVariables: TStringList);
function EditSlideVariables(vars, options: TFastKeyValuesSS): boolean;

implementation

uses
  RegularExpressionsCore, Dialogs, UfrmEditVariables;

procedure DetectSlideVariables(strText: string; slVariables: TStringList);
var
  regex: TPerlRegEx;
  strKey: string;
begin
  regex := TPerlRegEx.Create;
  try
    regex.RegEx := '%([^<>]+)%';
    regex.Options := [preUnGreedy];
    regex.Subject := strText;
    regex.Start := 1;

    while regex.MatchAgain do begin
      strKey := copy(regex.MatchedText, 2, Length(regex.MatchedText) -2);
      slVariables.Add(strKey);
    end;
  finally
    regex.Free;
  end;
end;

procedure CleanupSlideVariables(vars: TFastKeyValuesSS; slVariables: TStringList);
var
  index, iFound: integer;
begin
  index := 0;
  while index < vars.Count do begin
    iFound := slVariables.IndexOf(vars.KeyOfIndex[index]);
    if iFound <> -1 then begin
      slVariables.Delete(iFound);
      inc(index);
    end else begin
      vars.Delete(index);
    end;
  end;
  for index := 0 to slVariables.Count -1 do begin
    vars[slVariables[index]] := '';
  end;
end;

function EditSlideVariables(vars, options: TFastKeyValuesSS): boolean;
begin
  Result := EditFastKeySSVariables(vars, options);
end;

end.
