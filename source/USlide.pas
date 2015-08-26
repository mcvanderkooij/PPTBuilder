unit USlide;

interface

uses
  USlideLayout, System.Generics.Collections, Classes, UFastKeysSO, UUtilsStrings,
  UFastKeysSS, Data.DBXJSON, USourceInfo;

const
  CNEWSLIDE = '<---';

type
  TContentType = (ctNo, ctText, ctTextMemo, ctPicture, ctPictureFit, ctOverview, ctRibbon, ctExtSlide, ctOverviewSubs);
  TOverviewType = (otIgnore, otSong, otReading, otText);

  ISlideEditForm = interface
    ['{6942209E-D7F5-4DFC-BBCB-03458168F659}']
    function GetSlideAsString: string;
    procedure SetSlideAsString(const Value: string);
    function Edit: boolean;
  end;

  TSlideItem = class(TValueObject)
  private
    FContentType: TContentType;
    FContentSources: TSourceInfos;
  protected
    function GetAsJSonObject: TJSONObject; override;
    procedure SetAsJSonObject(const Value: TJSONObject); override;
  public
    constructor Create; overload; virtual;
    constructor Create(type_: TContentType; oSources: TSourceInfos); overload; virtual;
    constructor Create(json: TJSONObject); overload; virtual;
    destructor Destroy; override;

    property ContentType: TContentType read FContentType write FContentType;
    property ContentSources: TSourceInfos read FContentSources write FContentSources;
  end;

  TSlideItems = class(TFastKeyValuesSO)
  private
    function GetValueOfIndex(i: integer): TSlideItem;
    procedure SetValueOfIndex(i: integer; const Value: TSlideItem);
  protected
    function GetValues(strKey: string): TSlideItem;
    procedure SetValues(strKey: string; const Value: TSlideItem);
    function CreateAsJSON(const Value: TJSONObject): TValueObject; override;
    function CreateAsJSON(const Value: string): TValueObject; override;
  public
    property Values[strKey: string]: TSlideItem read GetValues write SetValues; default;
    property ValueOfIndex[i: integer]: TSlideItem read GetValueOfIndex write SetValueOfIndex;

    function AddSlideItem(strKey: string; type_: TContentType;
      oSources: TSourceInfos): TSlideItem; overload;
    function AddSlideItem(strKey: string; type_: TContentType;
      oSource: TSourceInfo): TSlideItem; overload;
    function AddSlideItemString(strKey: string; type_: TContentType;
      strText: string): TSlideItem;
    function AddSlideItemFileName(strKey: string; type_: TContentType;
      strFileName: string): TSlideItem;
    function AddSlideItemPPT(strKey: string; type_: TContentType;
      strFileName, strSlide, strShape: string): TSlideItem;
  end;

  TSlide = class(TSlideItems)
  private
    FLayout: TSlideLayout;
    FSlideTemplateName: string;
    FSlideName: string;
    FOverviewType: TOverviewType;
    FPictoName: TSourceInfo;
    FOverviewName: string;
    FAutomaticSmallNumbers: boolean;
    FVariables: TFastKeyValuesSS;
    FInternalHideRibbon: boolean;
    FIsSubOverview: boolean;
    FShowInOverview: boolean;
    function GetVariables: TFastKeyValuesSS;
    procedure SetPictoName(const Value: TSourceInfo);
  protected
    function GetAsJSonObject: TJSONObject; override;
    procedure SetAsJSonObject(const Value: TJSONObject); override;
  public
    constructor Create(layout: TSlideLayout); overload; virtual;
    constructor Create(strJSON: string); overload; virtual;
    constructor Create(json: TJSONObject); overload; virtual;
    destructor Destroy; override;

    property SlideName: string read FSlideName write FSlideName;
    property OverviewType: TOverviewType read FOverviewType write FOverviewType;
    property OverviewName: string read FOverviewName write FOverviewName;
    property ShowInOverview: boolean read FShowInOverview write FShowInOverview;
    property IsSubOverview: boolean read FIsSubOverview write FIsSubOverview;
    property PictoName: TSourceInfo read FPictoName write SetPictoName;
    property AutomaticSmallNumbers: boolean read FAutomaticSmallNumbers write FAutomaticSmallNumbers;
    property SlideTemplateName: string read FSlideTemplateName write FSlideTemplateName;
    property Layout: TSlideLayout read FLayout write FLayout;
    property Variables: TFastKeyValuesSS read GetVariables;

    property InternalHideRibbon: boolean read FInternalHideRibbon write FInternalHideRibbon;
  end;

//  TSlides = class(TObjectList<TSlide>)
//
//  end;

function LoadAndSplitMemoFile(strFileName: string): TSourceInfos;

implementation

uses
  SysUtils, USlideTemplate, UBuildPowerPoint, UUtilsJSON;



function LoadAndSplitMemoFile(strFileName: string): TSourceInfos;
var
  slMemo: TStringList;
  strText: string;
  i: integer;
  aContentStrings: TArrayOfString;
begin
  Result := TSourceInfos.Create;
  slMemo := TStringList.Create;
  try
    if FileExists(strFileName) then
      slMemo.LoadFromFile(strFileName);
      strText := slMemo.Text;
      strText := StringReplace(strText, #13#10, #13, [rfReplaceAll]);
      aContentStrings := Split(strText, CNEWSLIDE + #13);
      for i := 0 to Length(aContentStrings) -1 do begin
        Result.Add(SourceInfoFromMemoText(aContentStrings[i]));
      end;
  finally
    slMemo.Free;
  end;
end;


{ TSlide }

constructor TSlide.Create(layout: TSlideLayout);
begin
  inherited Create;
  FLayout := layout;
  FOverviewType := otIgnore;
  FAutomaticSmallNumbers := false;
  FVariables := TFastKeyValuesSS.Create;
  FInternalHideRibbon := False;
  FIsSubOverview := False;
  FShowInOverview := True;
  FPictoName := TSourceInfo.Create;
end;

constructor TSlide.Create(strJSON: string);
begin
  Create(TSlideLayout(nil));
  SetAsJSon(strJSON);
end;

constructor TSlide.Create(json: TJSONObject);
begin
  Create(TSlideLayout(nil));
  SetAsJSonObject(json);
end;

destructor TSlide.Destroy;
begin
  FPictoName.Free;
  FVariables.Free;
  inherited;
end;

function TSlide.GetAsJSonObject: TJSONObject;
begin
  Result := inherited GetAsJSonObject;
  Result.AddPair('LayoutName', EscapeString(FLayout.Name));
  Result.AddPair('SlideTemplateName', EscapeString(FSlideTemplateName));
  Result.AddPair('SlideName', EscapeString(FSlideName));
  Result.AddPair('OverviewType', TJSONNumber.Create(ord(FOverviewType)));
  Result.AddPair('PictoName', FPictoName.AsJSonObject);
  Result.AddPair('OverviewName', EscapeString(FOverviewName));
  Result.AddPair(CreateBoolPair('AutomaticSmallNumbers', FAutomaticSmallNumbers));
  Result.AddPair('Variables', FVariables.AsJSonObject);
  Result.AddPair(CreateBoolPair('IsSubOverview', FIsSubOverview));
  Result.AddPair(CreateBoolPair('ShowInOverview', FShowInOverview));
end;

function TSlide.GetVariables: TFastKeyValuesSS;
begin
  Result := FVariables;
end;

procedure TSlide.SetAsJSonObject(const Value: TJSONObject);
var
  strLayout: string;
  template: TSlideTemplate;
  templateArea: TSlideItem;

  oVariables: TJSONObject;
begin
  inherited SetAsJSonObject(Value);

  strLayout := UUtilsJSON.GetAsString(Value, 'LayoutName');
  FLayout := GetSlideLayouts.FindByName(strLayout);
  FSlideTemplateName := UUtilsJSON.GetAsString(Value, 'SlideTemplateName');
  FSlideName := UUtilsJSON.GetAsString(Value, 'SlideName');
  FOverviewType := TOverviewType( UUtilsJSON.GetAsInteger(Value, 'OverviewType'));
  FPictoName.Free;
  FPictoName := nil;
  FPictoName := TSourceInfo.Create(UUtilsJSON.GetAsObject(Value, 'PictoName'));
  FOverviewName := UUtilsJSON.GetAsString(Value, 'OverviewName');
  FAutomaticSmallNumbers := UUtilsJSON.GetAsBoolean(Value, 'AutomaticSmallNumbers');

  oVariables := UUtilsJSON.GetAsObject(Value, 'Variables');
  if Assigned(oVariables) then begin
    FVariables.AsJSonObject := oVariables;
  end else begin
    FVariables.Clear;
  end;

  FIsSubOverview := UUtilsJSON.GetAsBoolean(Value, 'IsSubOverview');
  FShowInOverview := UUtilsJSON.GetAsBoolean(Value, 'ShowInOverview');

  // load some defaults
  template := GetSlideTemplates.FindByName(FSlideTemplateName);
  if Assigned(template) then begin
    if (IndexOfKey('ribbon') < 0) then begin
      templateArea := template.AreaData['ribbon'];
      if (FLayout.IndexOfKey('ribbon') >= 0) and Assigned(templateArea) then begin
        Values['ribbon'] := TSlideItem.Create(templateArea.ContentType, templateArea.ContentSources);
      end;
    end;
  end;

end;

procedure TSlide.SetPictoName(const Value: TSourceInfo);
begin
  FreeAndNil(FPictoName);
  FPictoName := Value;
end;

{ TSlideItem }

constructor TSlideItem.Create(type_: TContentType; oSources: TSourceInfos);
begin
  Create;
  FContentType := type_;

  FreeAndNil(FContentSources);
  FContentSources := oSources;
end;

constructor TSlideItem.Create(json: TJSONObject);
begin
  Create;
  SetAsJSonObject(json);
end;

destructor TSlideItem.Destroy;
begin
  FContentSources.Free;
  inherited;
end;

constructor TSlideItem.Create;
begin
  inherited Create;
  FContentType := ctNo;
  FContentSources := TSourceInfos.Create;
end;

function TSlideItem.GetAsJSonObject: TJSONObject;
var
  i: integer;
  oJsonArray: TJSONArray;
begin
  Result := inherited GetAsJSonObject;

  Result.AddPair('ContentType', TJSONNumber.Create(ord(FContentType)) );
  oJsonArray := TJSONArray.Create;
  try
    for i := 0 to FContentSources.Count -1 do begin
      oJsonArray.AddElement(FContentSources[i].AsJSonObject);
    end;
    Result.AddPair('ContentSources', oJsonArray);
  except
    oJsonArray.Free;
  end;
end;

procedure TSlideItem.SetAsJSonObject(const Value: TJSONObject);
var
  i: integer;
  oJsonArray: TJSONArray;
begin
  inherited;
  FContentType := TContentType(GetAsInteger(Value, 'ContentType', 0));
  oJsonArray := GetAsArray(Value, 'ContentSources');
  if Assigned(oJsonArray) then begin
    FContentSources.Clear;
    for i := 0 to oJsonArray.Size -1 do begin
      FContentSources.Add(TSourceInfo.Create(oJsonArray.Get(i) as TJSONObject));
    end;
  end;
end;

{ TSlideItems }

function TSlideItems.AddSlideItem(strKey: string; type_: TContentType;
  oSources: TSourceInfos): TSlideItem;
begin
  Result := Values[strKey];
  if Assigned(Result) then begin
    Result.ContentType := type_;
    Result.ContentSources.Free;
    Result.ContentSources := oSources;
  end else begin
    Result := TSlideItem.Create(type_, oSources);
    Values[strKey] := Result;
  end;
end;

function TSlideItems.AddSlideItem(strKey: string; type_: TContentType;
  oSource: TSourceInfo): TSlideItem;
var
  oSources: TSourceInfos;
begin
  oSources := TSourceInfos.Create;
  oSources.Add(oSource);
  Result := AddSlideItem(strKey, type_, oSources);
end;

function TSlideItems.AddSlideItemFileName(strKey: string; type_: TContentType;
  strFileName: string): TSlideItem;
begin
  Result := AddSlideItem(strKey, type_, TSourceInfo.CreateAsFileName(strFileName));
end;

function TSlideItems.AddSlideItemPPT(strKey: string; type_: TContentType;
  strFileName, strSlide, strShape: string): TSlideItem;
begin
  Result := AddSlideItem(strKey, type_, TSourceInfo.CreateAsPPT(strFileName, strSlide, strShape));
end;

function TSlideItems.AddSlideItemString(strKey: string; type_: TContentType;
  strText: string): TSlideItem;
begin
  Result := AddSlideItem(strKey, type_, TSourceInfo.CreateAsString(strText));
end;

function TSlideItems.CreateAsJSON(const Value: TJSONObject): TValueObject;
begin
  Result := TSlideItem.Create;
  Result.AsJSonObject := Value;
end;

function TSlideItems.CreateAsJSON(const Value: string): TValueObject;
begin
  Result := TSlideItem.Create;
  Result.AsJSon := Value;
end;

function TSlideItems.GetValueOfIndex(i: integer): TSlideItem;
begin
  Result := TSlideItem(inherited GetValueOfIndex(i));
end;

function TSlideItems.GetValues(strKey: string): TSlideItem;
begin
  Result := TSlideItem(inherited GetValues(strKey));
end;

procedure TSlideItems.SetValueOfIndex(i: integer; const Value: TSlideItem);
begin
  inherited SetValueOfIndex(i, Value);
end;

procedure TSlideItems.SetValues(strKey: string; const Value: TSlideItem);
begin
  inherited SetValues(strKey, Value);
end;

end.
