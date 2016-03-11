unit UUtilsJSON;

interface

uses
  Data.DBXJSON, System.JSON, Types, Classes;

type
  TJSONPersistent = class(TObject)
  protected
    function GetAsJSon: string;
    procedure SetAsJSon(const Value: string);
    function GetAsJSonObject: TJSONObject; virtual;
    procedure SetAsJSonObject(const Value: TJSONObject); virtual;
  public
    constructor CreateAsJSON(strJSON: String); overload; virtual;

    property AsJSon: string read GetAsJSon write SetAsJSon;
    property AsJSonObject: TJSONObject read GetAsJSonObject write SetAsJSonObject;
  end;

function GetAsObject(json: TJSONObject; strName: string): TJSONObject;
function GetAsArray(json: TJSONObject; strName: string): TJSONArray;
function GetAsString(json: TJSONObject; strName: string; strDefault: string = ''): string;
function GetAsDouble(json: TJSONObject; strName: string; dblDefault: Double = 0): Double; overload;
function GetAsInteger(json: TJSONObject; strName: string; iDefault: integer = 0): integer; overload;
function GetAsBoolean(json: TJSONObject; strName: string): boolean;
function GetAsRect(json: TJSONObject; strName: string): TRect;
procedure GetAsStringList(json: TJSONObject; strName: string; list: TStringList);

function CreateBoolPair(strName: string; blnValue: boolean): TJSONPair;
function CreateRectPair(strName: string; rect: TRect): TJSONPair;
function CreateStringListPair(strName: string; list: TStringList): TJSONPair;
function EscapeString(const AValue: string): string;


implementation

uses
  SysUtils;

function GetAsObject(json: TJSONObject; strName: string): TJSONObject;
var
  oPair: TJSONPair;
begin
  assert(Assigned(json), 'json parameter not assigned');
  Result := nil;
  oPair := json.Get(strName);
  if Assigned(oPair) then begin
    Result:= oPair.JsonValue as TJSONObject;
  end;
end;

function GetAsArray(json: TJSONObject; strName: string): TJSONArray;
var
  oPair: TJSONPair;
begin
  assert(Assigned(json), 'json parameter not assigned');
  Result := nil;
  oPair := json.Get(strName);
  if Assigned(oPair) then begin
    Result:= oPair.JsonValue as TJSONArray;
  end;
end;

function GetAsString(json: TJSONObject; strName, strDefault: string): string;
var
  oPair: TJSONPair;
  oString: TJSONString;
begin
  assert(Assigned(json), 'json parameter not assigned');
  Result := strDefault;
  oPair := json.Get(strName);
  if Assigned(oPair) then begin
    oString := oPair.JsonValue as TJSONString;
    if Assigned(oString) then begin
      Result := oString.Value;
    end;
  end;
end;

function GetAsDouble(json: TJSONObject; strName: string; dblDefault: Double): Double;
var
  oPair: TJSONPair;
  oNumber: TJSONNumber;
begin
  assert(Assigned(json), 'json parameter not assigned');
  Result := dblDefault;
  oPair := json.Get(strName);
  if Assigned(oPair) then begin
    oNumber := oPair.JsonValue as TJSONNumber;
    if Assigned(oNumber) then begin
      Result := oNumber.AsDouble;
    end;
  end;
end;

function GetAsInteger(json: TJSONObject; strName: string; iDefault: integer): integer;
var
  oPair: TJSONPair;
  oNumber: TJSONNumber;
begin
  assert(Assigned(json), 'json parameter not assigned');
  Result := iDefault;
  oPair := json.Get(strName);
  if Assigned(oPair) then begin
    oNumber := oPair.JsonValue as TJSONNumber;
    if Assigned(oNumber) then begin
      Result := oNumber.AsInt64;
    end;
  end;
end;

function GetAsBoolean(json: TJSONObject; strName: string): boolean;
var
  oPair: TJSONPair;
begin
  assert(Assigned(json), 'json parameter not assigned');
  Result := False;
  oPair := json.Get(strName);
  if Assigned(oPair) then begin
    Result := oPair.JsonValue is TJSONTrue;
  end;
end;

function CreateBoolPair(strName: string; blnValue: boolean): TJSONPair;
begin
  if blnValue then
    Result := TJSONPair.Create(strName, TJSONTrue.Create)
  else
    Result := TJSONPair.Create(strName, TJSONFalse.Create);
end;

function GetAsRect(json: TJSONObject; strName: string): TRect;
var
  oPair: TJSONPair;
  oRectObject: TJSONObject;
begin
  assert(Assigned(json), 'json parameter not assigned');
  oPair := json.Get(strName);
  if Assigned(oPair) then begin
    oRectObject := oPair.JsonValue as TJSONObject;
    Result.Left := GetAsInteger(oRectObject, 'Left', 0);
    Result.Top := GetAsInteger(oRectObject, 'Top', 0);
    Result.Right := GetAsInteger(oRectObject, 'Right', 0);
    Result.Bottom := GetAsInteger(oRectObject, 'Bottom', 0);
  end else begin
    Result := Rect(0,0,0,0);
  end;
end;

function CreateRectPair(strName: string; rect: TRect): TJSONPair;
var
  oRectObject: TJSONObject;
begin
  oRectObject := TJSONObject.Create;
  oRectObject.AddPair('Left', TJSONNumber.Create(rect.Left));
  oRectObject.AddPair('Top', TJSONNumber.Create(rect.Top));
  oRectObject.AddPair('Right', TJSONNumber.Create(rect.Right));
  oRectObject.AddPair('Bottom', TJSONNumber.Create(rect.Bottom));
  Result := TJSONPair.Create(strName, oRectObject);
end;

procedure GetAsStringList(json: TJSONObject; strName: string; list: TStringList);
var
  oPair: TJSONPair;
  oListArray: TJSONArray;
  i: integer;
begin
  assert(Assigned(json), 'json parameter not assigned');
  list.Clear;
  oPair := json.Get(strName);
  if Assigned(oPair) then begin
    oListArray := oPair.JsonValue as TJSONArray;
    for i := 0 to oListArray.Size -1 do begin
      list.Add(oListArray.Get(i).Value);
    end;
  end;
end;

function CreateStringListPair(strName: string; list: TStringList): TJSONPair;
var
  oListArray: TJSONArray;
  i: Integer;
begin
  oListArray := TJSONArray.Create;
  for i := 0 to list.Count -1 do begin
    oListArray.AddElement( TJSONString.Create( EscapeString(list[i])) );
  end;
  Result := TJSONPair.Create(strName, oListArray);
end;

{ TJSONPersistent }

constructor TJSONPersistent.CreateAsJSON(strJSON: String);
begin
  Create;
  AsJSon := strJSON;
end;

function TJSONPersistent.GetAsJSon: string;
var
  oJsonObject: TJSONObject;
begin
  oJsonObject := GetAsJSonObject;
  try
    Result := oJsonObject.ToString();
  finally
    oJsonObject.Free;
  end;
end;

function TJSONPersistent.GetAsJSonObject: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

procedure TJSONPersistent.SetAsJSon(const Value: string);
var
  oJsonObject: TJSONObject;
begin
  oJsonObject := TJSONObject.ParseJSONValue(Value) as TJSONObject;
  try
    SetAsJSonObject(oJsonObject);
  finally
    oJsonObject.Free;
  end;
end;

procedure TJSONPersistent.SetAsJSonObject(const Value: TJSONObject);
begin
  //
end;

function EscapeString(const AValue: string): string;
const
  ESCAPE = '\';
  // QUOTATION_MARK = '"';
  REVERSE_SOLIDUS = '\';
  SOLIDUS = '/';
  BACKSPACE = #8;
  FORM_FEED = #12;
  NEW_LINE = #10;
  CARRIAGE_RETURN = #13;
  HORIZONTAL_TAB = #9;
var
  AChar: Char;
begin
  Result := '';
  for AChar in AValue do
  begin
    case AChar of
      // !! Double quote (") is handled by TJSONString
      // QUOTATION_MARK: Result := Result + ESCAPE + QUOTATION_MARK;
      REVERSE_SOLIDUS: Result := Result + ESCAPE + REVERSE_SOLIDUS;
      SOLIDUS: Result := Result + ESCAPE + SOLIDUS;
      BACKSPACE: Result := Result + ESCAPE + 'b';
      FORM_FEED: Result := Result + ESCAPE + 'f';
      NEW_LINE: Result := Result + ESCAPE + 'n';
      CARRIAGE_RETURN: Result := Result + ESCAPE + 'r';
      HORIZONTAL_TAB: Result := Result + ESCAPE + 't';
      else
      begin
        if (Integer(AChar) < 32) or (Integer(AChar) > 126) then
          Result := Result + ESCAPE + 'u' + IntToHex(Integer(AChar), 4)
        else
          Result := Result + AChar;
      end;
    end;
  end;
end;

end.
