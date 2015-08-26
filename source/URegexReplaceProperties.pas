unit URegexReplaceProperties;

interface

uses
  RegularExpressionsCore, UFastKeysSS;

type
  TRegexReplaceProperties = class(TPerlRegEx)
  protected
    FProperties: TFastKeyValuesSS;
{$ifdef VER230}
    procedure DoPerlRegExReplaceEvent(Sender: TObject; var ReplaceWith: UTF8String);
{$else}
    procedure DoPerlRegExReplaceEvent(Sender: TObject; var ReplaceWith: string);
{$endif}
  public
    constructor Create(properties: TFastKeyValuesSS); virtual;

    function ReplaceWithProperties(strText: string): string;
  end;

implementation

uses
  SysUtils;

{ TRegexReplaceProperties }

constructor TRegexReplaceProperties.Create(properties: TFastKeyValuesSS);
begin
  inherited Create;
  FProperties := properties;
  Options := [preUnGreedy];
  OnReplace := DoPerlRegExReplaceEvent;
end;

{$ifdef VER230}
procedure TRegexReplaceProperties.DoPerlRegExReplaceEvent(Sender: TObject;
  var ReplaceWith: UTF8String);
{$else}
procedure TRegexReplaceProperties.DoPerlRegExReplaceEvent(Sender: TObject;
  var ReplaceWith: string);
{$endif}
var
  strKey: string;
  index: integer;
begin
  strKey := copy(MatchedText, 2, Length(MatchedText) -2);
  if FProperties.FindKey(strKey, index) then begin
    ReplaceWith := FProperties.ValueOfIndex[index];
  end else begin
    ReplaceWith := MatchedText;
  end;
end;

function TRegexReplaceProperties.ReplaceWithProperties(strText: string): string;
begin
  RegEx := '(%[^<>]+%)';
  Subject := strText;
  ReplaceAll;
  Result := Subject;
end;

end.
