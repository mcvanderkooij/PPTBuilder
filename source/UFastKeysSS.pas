unit UFastKeysSS;

interface

uses
  Windows, Classes, SysUtils, UFastkeys, Data.DBXJSON;

type
  TFastKeyPairSS = record
    Key: string;
    Value: string;
  end;
  TFastKeyPairsSS = array of TFastKeyPairSS;

  TFastKeyValuesSS = class(TFastKeyValuesBase)
  private
    FFastKeyPairs: TFastKeyPairsSS;
    function GetCount: integer;
    function GetValues(strKey: string): string;
    procedure SetValues(strKey: string; const Value: string);
    function GetKeyOfIndex(i: integer): string;
    function GetValueOfIndex(i: integer): string;
    procedure SetValueOfIndex(i: integer; const Value: string);
    procedure CheckIndex(i: integer);
  protected
    function GetAsJSonObject: TJSONObject; override;
    procedure SetAsJSonObject(const Value: TJSONObject); override;
  public
    property Count: integer read GetCount;
    property Values[strKey: string]: string read GetValues write SetValues; default;
    property KeyOfIndex[i: integer]: string read GetKeyOfIndex;
    property ValueOfIndex[i: integer]: string read GetValueOfIndex write SetValueOfIndex;

    procedure Clear; override;
    procedure Delete(index: integer); override;
    function FindKey(strKey: string; out index: integer): boolean;
    function IndexOfKey(strKey: string): integer;
    procedure Merge(oStrings: TStrings; options: TFastKeyMergeOptions = [moAdd, moUpdate]); overload; override;
    procedure SaveToStrings(oStrings: TStrings); override;
  end;


implementation

uses
  UUtilsStrings, GNUGetText, UUtilsJSON;

{ TFastKeyValuesSS }

procedure TFastKeyValuesSS.CheckIndex(i: integer);
begin
  if (i < 0) or (i >= Length(FFastKeyPairs)) then
    raise Exception.Create(_('Index out of bounds'));
end;

procedure TFastKeyValuesSS.Clear;
begin
  SetLength(FFastKeyPairs, 0);
end;

procedure TFastKeyValuesSS.Delete(index: integer);
var
  iLength: integer;
  iTailElements: integer;
begin
  CheckIndex(index);
  iLength := Length(FFastKeyPairs);
  Finalize(FFastKeyPairs[index]);
  iTailElements := iLength - index;
  if iTailElements > 0 then
    Move(FFastKeyPairs[index + 1], FFastKeyPairs[index], SizeOf(TFastKeyPairSS) * iTailElements);
  ZeroMemory(@FFastKeyPairs[iLength -1], sizeof(TFastKeyPairSS));
  SetLength(FFastKeyPairs, iLength -1);
end;

function TFastKeyValuesSS.FindKey(strKey: string; out index: integer): boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := Length(FFastKeyPairs) - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;

    C := AnsiCompareStr(FFastKeyPairs[I].Key, strKey);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TFastKeyValuesSS.GetAsJSonObject: TJSONObject;
var
  i: integer;
  oJsonItems: TJSONObject;
begin
  Result := inherited GetAsJSonObject;

  oJsonItems := TJSONObject.Create;
  for i := 0 to Count -1 do begin
    oJsonItems.AddPair(KeyOfIndex[i], EscapeString(ValueOfIndex[i]));
  end;
  Result.AddPair('Values', oJsonItems);
end;

function TFastKeyValuesSS.GetCount: integer;
begin
  Result := Length(FFastKeyPairs);
end;

function TFastKeyValuesSS.GetKeyOfIndex(i: integer): string;
begin
  CheckIndex(i);
  Result := FFastKeyPairs[i].Key;
end;

function TFastKeyValuesSS.GetValueOfIndex(i: integer): string;
begin
  CheckIndex(i);
  Result := FFastKeyPairs[i].Value;
end;

function TFastKeyValuesSS.GetValues(strKey: string): string;
var
  index: integer;
begin
  if FindKey(strKey, index) then
    Result := FFastKeyPairs[index].Value
  else
    Result := '';
end;

function TFastKeyValuesSS.IndexOfKey(strKey: string): integer;
begin
  if not FindKey(strKey, Result) then
    Result := -1;
end;

procedure TFastKeyValuesSS.Merge(oStrings: TStrings; options: TFastKeyMergeOptions);
var
  i, iPos, index: integer;
  strLine: string;

  strKey, strValue: string;

  oTempKeys: TFastKeyValuesSS;
begin
  Assert(Assigned(oStrings));
  if options = [] then
    Exit;

  if moDelete in options then begin
    oTempKeys := TFastKeyValuesSS.Create;
    try
      oTempKeys.LoadFromStrings(oStrings);
      i := 0;
      while i < Count do begin
        if not oTempKeys.FindKey(KeyOfIndex[i], index) then begin
          ValueOfIndex[i] := '';
        end;
        inc(i);
      end;
    finally
      oTempKeys.Free;
    end;
  end;
  for i := 0 to oStrings.Count - 1 do begin
    strLine := oStrings[i];
    iPos := Pos('=', strLine);
    if iPos > 0 then begin
      strKey := copy(strLine, 1, iPos -1);
      strValue := copy(strLine, iPos + 1, MaxInt);

      if FindKey(strKey, index) then begin
        if moUpdate in options then
          ValueOfIndex[index] := strValue;
      end else begin
        if moAdd in options then
          Values[strKey] := strValue;
      end;
    end;
  end;
end;

procedure TFastKeyValuesSS.SaveToStrings(oStrings: TStrings);
var
  i: integer;
  strLine: string;
begin
  Assert(Assigned(oStrings));
  for i := 0 to Length(FFastKeyPairs) - 1 do begin
    if FFastKeyPairs[i].Value <> '' then begin
      strLine := FFastKeyPairs[i].Key + '=' + FFastKeyPairs[i].Value;
      oStrings.Add(strLine);
    end;
  end;
end;

procedure TFastKeyValuesSS.SetAsJSonObject(const Value: TJSONObject);
var
  i: integer;
  oJsonItems: TJSONObject;
begin
  inherited SetAsJSonObject(Value);

  Clear;
  oJsonItems := UUtilsJSON.GetAsObject(Value, 'Values');
  if Assigned(oJsonItems) then begin
    for i := 0 to oJsonItems.Size -1 do begin
      Values[oJsonItems.Get(i).JsonString.Value] := oJsonItems.Get(i).JsonValue.Value;
    end;
  end;
end;

procedure TFastKeyValuesSS.SetValueOfIndex(i: integer; const Value: string);
begin
  CheckIndex(i);
  FFastKeyPairs[i].Value := Value;
end;

procedure TFastKeyValuesSS.SetValues(strKey: string; const Value: string);
var
  index, iCount: integer;
begin
  if FindKey(strKey, index) then begin
    FFastKeyPairs[index].Value := Value;
  end else begin
    iCount := Length(FFastKeyPairs);
    SetLength(FFastKeyPairs, iCount + 1);
    if index < iCount then begin
      System.Move(FFastKeyPairs[Index], FFastKeyPairs[Index + 1],
        (iCount - Index) * SizeOf(TStringItem));
    end;
    pointer(FFastKeyPairs[index].Key) := nil;
    pointer(FFastKeyPairs[index].Value) := nil;

    FFastKeyPairs[index].Key := strKey;
    FFastKeyPairs[index].Value := Value;
  end;
end;


end.
