unit UFastKeysSO;

interface

uses
  Windows, Classes, SysUtils, UFastkeys, UFastkeysSS, Data.DBXJSON, UUtilsJSON, System.JSON;

type
  TValueObject = class(TJSONPersistent)
  protected
  public
  end;

  TFastKeyPairSO = record
    Key: string;
    Value: TValueObject;
  end;
  TFastKeyPairsSO = array of TFastKeyPairSO;

  TFastKeyValuesSO = class(TFastKeyValuesBase)
  private
    FFastKeyPairs: TFastKeyPairsSO;
    function GetCount: integer;
    function GetKeyOfIndex(i: integer): string;
    procedure CheckIndex(i: integer);
  protected
    function GetValues(strKey: string): TValueObject;
    procedure SetValues(strKey: string; const Value: TValueObject);
    function GetValueOfIndex(i: integer): TValueObject;
    procedure SetValueOfIndex(i: integer; const Value: TValueObject);

    function CreateAsJSON(const Value: TJSONObject): TValueObject; overload; virtual;
    function CreateAsJSON(const Value: string): TValueObject; overload; virtual;
    function GetAsJSonObject: TJSONObject; override;
    procedure SetAsJSonObject(const Value: TJSONObject); override;
  public
    property Count: integer read GetCount;
    property Values[strKey: string]: TValueObject read GetValues write SetValues; default;
    property KeyOfIndex[i: integer]: string read GetKeyOfIndex;
    property ValueOfIndex[i: integer]: TValueObject read GetValueOfIndex write SetValueOfIndex;

    procedure Clear; override;
    procedure Delete(index: integer); override;
    function FindKey(strKey: string; out index: integer): boolean;
    function IndexOfKey(strKey: string): integer;
    procedure Merge(oStrings: TStrings; options: TFastKeyMergeOptions = [moAdd, moUpdate]); overload; override;
    procedure SaveToStrings(oStrings: TStrings); override;
  end;


implementation

uses
  UUtilsStrings, GNUGetText;

{ TFastKeyValuesSS }

procedure TFastKeyValuesSO.CheckIndex(i: integer);
begin
  if (i < 0) or (i >= Length(FFastKeyPairs)) then
    raise Exception.Create(_('Index out of bounds'));
end;

procedure TFastKeyValuesSO.Clear;
var
  i: integer;
begin
  for i := 0 to Length(FFastKeyPairs) -1 do begin
    FFastKeyPairs[i].Value.Free;
    FFastKeyPairs[i].Value := nil;
  end;

  SetLength(FFastKeyPairs, 0);
end;

function TFastKeyValuesSO.CreateAsJSON(const Value: string): TValueObject;
begin
  Result := TValueObject.Create;
  Result.AsJSon := Value;
end;

function TFastKeyValuesSO.CreateAsJSON(const Value: TJSONObject): TValueObject;
begin
  Result := TValueObject.Create;
  Result.AsJSonObject := Value;
end;

procedure TFastKeyValuesSO.Delete(index: integer);
var
  iLength: integer;
  iTailElements: integer;
begin
  CheckIndex(index);
  iLength := Length(FFastKeyPairs);
  FFastKeyPairs[index].Value.Free;
  FFastKeyPairs[index].Value := nil;
  Finalize(FFastKeyPairs[index]);
  iTailElements := iLength - index;
  if iTailElements > 0 then
    Move(FFastKeyPairs[index + 1], FFastKeyPairs[index], SizeOf(TFastKeyPairSO) * iTailElements);
  ZeroMemory(@FFastKeyPairs[iLength -1], sizeof(TFastKeyPairSO));
  SetLength(FFastKeyPairs, iLength -1);
end;

function TFastKeyValuesSO.FindKey(strKey: string; out index: integer): boolean;
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

function TFastKeyValuesSO.GetAsJSonObject: TJSONObject;
var
  i: integer;
  oJsonItems: TJSONObject;
begin
  Result := inherited GetAsJSonObject;

  oJsonItems := TJSONObject.Create;
  for i := 0 to Count -1 do begin
    oJsonItems.AddPair(KeyOfIndex[i], ValueOfIndex[i].AsJSonObject);
  end;
  Result.AddPair('Values', oJsonItems);

end;

function TFastKeyValuesSO.GetCount: integer;
begin
  Result := Length(FFastKeyPairs);
end;

function TFastKeyValuesSO.GetKeyOfIndex(i: integer): string;
begin
  CheckIndex(i);
  Result := FFastKeyPairs[i].Key;
end;

function TFastKeyValuesSO.GetValueOfIndex(i: integer): TValueObject;
begin
  CheckIndex(i);
  Result := FFastKeyPairs[i].Value;
end;

function TFastKeyValuesSO.GetValues(strKey: string): TValueObject;
var
  index: integer;
begin
  if FindKey(strKey, index) then
    Result := FFastKeyPairs[index].Value
  else
    Result := nil;
end;

function TFastKeyValuesSO.IndexOfKey(strKey: string): integer;
begin
  if not FindKey(strKey, Result) then
    Result := -1;
end;

procedure TFastKeyValuesSO.Merge(oStrings: TStrings; options: TFastKeyMergeOptions);
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
          ValueOfIndex[i] := nil;
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
          ValueOfIndex[index] := CreateAsJSON(strValue);
      end else begin
        if moAdd in options then
          Values[strKey] := CreateAsJSON(strValue);
      end;
    end;
  end;
end;

procedure TFastKeyValuesSO.SaveToStrings(oStrings: TStrings);
var
  i: integer;
  strLine: string;
begin
  Assert(Assigned(oStrings));
  for i := 0 to Length(FFastKeyPairs) - 1 do begin
    if FFastKeyPairs[i].Value <> nil then begin
      strLine := FFastKeyPairs[i].Key + '=' + FFastKeyPairs[i].Value.AsJSon;
      oStrings.Add(strLine);
    end;
  end;
end;

procedure TFastKeyValuesSO.SetAsJSonObject(const Value: TJSONObject);
var
  i: integer;
  oJsonItems: TJSONObject;
begin
  inherited SetAsJSonObject(Value);

  Clear;
  oJsonItems := UUtilsJSON.GetAsObject(Value, 'Values');
  if Assigned(oJsonItems) then begin
    for i := 0 to oJsonItems.Size -1 do begin
      Values[oJsonItems.Get(i).JsonString.Value] := CreateAsJSON(oJsonItems.Get(i).JsonValue as TJSONObject);
    end;
  end;
end;

procedure TFastKeyValuesSO.SetValueOfIndex(i: integer; const Value: TValueObject);
begin
  CheckIndex(i);
  FFastKeyPairs[i].Value := Value;
end;

procedure TFastKeyValuesSO.SetValues(strKey: string; const Value: TValueObject);
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

