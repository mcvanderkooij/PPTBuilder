unit UProject;

interface

uses
  Classes, UFastKeysSS, UUtilsJSON, Data.DBXJSON, USnapshot;

type
  TProject = class(TJSONPersistent)
  private
    FSlides: TStringList;
    FAfterCollecte: string;
    FProperties: TFastKeyValuesSS;
    function GetSlides: TStringList;
    procedure SetSlides(const Value: TStringList);
  protected
    function GetAsJSonObject: TJSONObject; override;
    procedure SetAsJSonObject(const Value: TJSONObject); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function CreateHash: string;

    property Slides: TStringList read GetSlides write SetSlides;
    property Properties: TFastKeyValuesSS read FProperties;

    property AfterCollecte: string read FAfterCollecte write FAfterCollecte;
  end;

  TSnapshotProject = class(TSnapshot)
  protected
    FAsString: string;
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
  public
    constructor Create(oProject: TProject); virtual;
    procedure RestoreSnapshot(oProject: TProject); virtual;
  end;

implementation

uses
  UUtilsStrings, UUtils, USlide;

{ TProject }

constructor TProject.Create;
begin
  inherited Create;
  FSlides := TStringList.Create;
  FProperties := TFastKeyValuesSS.Create;
end;

function TProject.CreateHash: string;
begin
  Result := THashString.S_HashValueAsHex(FSlides.Text + FProperties.SaveToString);
end;

destructor TProject.Destroy;
begin
  FProperties.Free;
  FSlides.Free;
  inherited;
end;

function TProject.GetAsJSonObject: TJSONObject;
var
  oJsonArray: TJSONArray;
  i: Integer;
  slide: TSlide;
begin
  Result := inherited GetAsJSonObject;

  Result.AddPair('Properties', FProperties.AsJSonObject);
  oJsonArray := TJSONArray.Create;
  try
    for i := 0 to Slides.Count -1 do begin
      slide := TSlide.Create(Slides[i]);
      try
        oJsonArray.Add( slide.AsJSonObject);
      finally
        slide.Free;
      end;
    end;
    Result.AddPair('Slides', oJsonArray);
  except
    oJsonArray.Free;
  end;
end;

function TProject.GetSlides: TStringList;
begin
  Result := FSlides;
end;

procedure TProject.SetAsJSonObject(const Value: TJSONObject);
var
  oProperties: TJSONObject;
  oJsonArray: TJSONArray;
  i: Integer;
  slide: TSlide;
begin
  inherited SetAsJSonObject(Value);
  oProperties := GetAsObject(Value, 'Properties');
  if Assigned(oProperties) then
    FProperties.AsJSonObject := oProperties
  else
    FProperties.Clear;
  oJsonArray := GetAsArray(Value, 'Slides');
  if Assigned(oJsonArray) then begin
    for i := 0 to oJsonArray.Size -1 do begin
      slide := TSlide.Create(oJsonArray.Get(i) as TJSONObject);
      try
        Slides.Add(slide.AsJSON);
      finally
        slide.Free;
      end;
    end;
  end;
end;

procedure TProject.SetSlides(const Value: TStringList);
begin
  if FSlides.Text <> Value.Text then begin
    FSlides.Text := Value.Text;
  end;
end;

{ TSnapshotProject }

constructor TSnapshotProject.Create(oProject: TProject);
begin
  inherited Create;
  FAsString := oProject.AsJSon;
end;

function TSnapshotProject.GetAsString: string;
begin
  Result := FAsString;
end;

procedure TSnapshotProject.RestoreSnapshot(oProject: TProject);
begin
  oProject.AsJSon := FAsString;
end;

procedure TSnapshotProject.SetAsString(const Value: string);
begin
  FAsString := Value;
end;

end.
