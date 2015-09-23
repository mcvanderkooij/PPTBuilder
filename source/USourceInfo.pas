unit USourceInfo;

interface

uses
  TeeGenericTree, Classes, System.Generics.Collections, Data.DBXJSON, UUtilsJSON;

type
  TStringTree = TNode<String>;

  TSourceInfoType = (sitUnknown, sitString, sitPPT, sitFileName, sitTemplate, sitBook);

  TSourceInfo = class(TJSONPersistent)
  private
    FFileName: string;
    FContentTypeOverride: boolean;
    FShapeName: string;
    FDescription: string;
    FText: string;
    FSlideName: string;
    FSourceType: TSourceInfoType;
    FRemark: string;
  protected
    function GetAsJSonObject: TJSONObject; override;
    procedure SetAsJSonObject(const Value: TJSONObject); override;
  public
    property SourceType: TSourceInfoType read FSourceType write FSourceType;
    property Description: string read FDescription write FDescription;
    property Remark: string read FRemark write FRemark;
    property Text: string read FText write FText;
    property FileName: string read FFileName write FFileName;
    property SlideName: string read FSlideName write FSlideName;
    property ShapeName: string read FShapeName write FShapeName;
    property ContentTypeOverride: boolean read FContentTypeOverride write FContentTypeOverride;

    constructor Create; overload; virtual;
    constructor Create(strJSON: string); overload; virtual;
    constructor Create(json: TJSONObject); overload; virtual;

    function DeepCopy: TSourceInfo;
    procedure ClearAsString;

    constructor CreateAsString(strText: string; blnContentTypeOverride: boolean = false); virtual;
    constructor CreateAsTemplate(strTemplateName: string; blnContentTypeOverride: boolean = false); virtual;
    constructor CreateAsPPT(strFilename, strSlide, strShape: string; blnContentTypeOverride: boolean = false); virtual;
    constructor CreateAsFileName(strFileName: string; blnContentTypeOverride: boolean = false); virtual;
    constructor CreateAsBook(strBook, strChapter, strVerse: string; blnContentTypeOverride: boolean = false); virtual;
  end;

  TSourceInfos = class(TObjectList<TSourceInfo>)
  public
    function DeepCopy: TSourceInfos;
  end;

function SourceInfoToString(sourceinfo: TSourceInfo; blnDoImplode: boolean = true): string;
function SourceInfoFromString(source: string): TSourceInfo;

function SourceInfoToMemoText(sourceinfo: TSourceInfo): string;
function SourceInfoFromMemoText(strText: string): TSourceInfo;

procedure SortSourceInfoStrings(slStrings: TStrings);
procedure SortStrings(slStrings: TStringList);

implementation

uses
  System.SysUtils,
  USettings, UUtilsStrings, UStringLogicalComparer;

function SourceInfoToString(sourceinfo: TSourceInfo; blnDoImplode: boolean = true): string;
begin
  Assert(sourceinfo.SourceType <> sitUnknown);
  if blnDoImplode then begin
    sourceinfo.FileName := GetSettings.DirImplode(sourceinfo.FileName);
  end;
  Result := sourceinfo.AsJSon;
end;

function SourceInfoFromString(source: string): TSourceInfo;
begin
  Result := TSourceInfo.Create(source);
  Assert(Result.SourceType <> sitUnknown);
end;

const
  CTEMPLATEID = '#template:';

function SourceInfoToMemoText(sourceinfo: TSourceInfo): string;
begin
  case sourceinfo.SourceType of
    sitString: Result := sourceinfo.Text;
    sitPPT: ;
    sitFileName: ;
    sitTemplate: Result := CTEMPLATEID + sourceinfo.Text + '#';
  else
    Result := '';
  end;
end;

function SourceInfoFromMemoText(strText: string): TSourceInfo;
var
  iPosTemplateStart, iPosTemplateEnd: integer;
  strTemplate: string;
begin
  iPosTemplateStart := pos(CTEMPLATEID, strText);
  if iPosTemplateStart > 0 then begin
    iPosTemplateEnd := PosIEx('#', strText, iPosTemplateStart + Length(CTEMPLATEID));
    iPosTemplateStart := iPosTemplateStart + Length(CTEMPLATEID);
    strTemplate := copy(strText, iPosTemplateStart, iPosTemplateEnd - iPosTemplateStart);
    Result := TSourceInfo.CreateAsTemplate(strTemplate);
  end else begin
    Result := TSourceInfo.CreateAsString(strText);
  end;
end;

function SortItem(List: TStringList; Index1, Index2: Integer): Integer;
var
  src1, src2: TSourceInfo;
begin
  src1 := nil;
  src2 := nil;
  try
    src1 := SourceInfoFromString(List[Index1]);
    src2 := SourceInfoFromString(List[Index2]);
    Result := CompareNatural(src1.Description, src2.Description);
  finally
    src1.Free;
    src2.Free;
  end;
end;

function SortStringsItem(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := CompareNatural(List[Index1], List[Index2]);
end;

procedure SortSourceInfoStrings(slStrings: TStrings);
var
  slItems: TStringList;
begin
  slItems := TStringList.Create;
  try
    slItems.Assign(slStrings);
    slItems.CustomSort(SortItem);
    slStrings.Assign(slItems);
  finally
    slItems.Free;
  end;
end;

procedure SortStrings(slStrings: TStringList);
begin
  slStrings.CustomSort(SortStringsItem);
end;

{ TSourceInfo }

procedure TSourceInfo.ClearAsString;
begin
  SourceType := sitString;
  Description := '';
  Text := '';
  FileName := '';
  SlideName := '';
  ShapeName := '';
  ContentTypeOverride := false;
end;

constructor TSourceInfo.Create(strJSON: string);
begin
  Create;
  SetAsJSon(strJSON);
end;

constructor TSourceInfo.Create(json: TJSONObject);
begin
  Create;
  SetAsJSonObject(json);
end;

constructor TSourceInfo.Create;
begin
  //
end;

constructor TSourceInfo.CreateAsBook(strBook, strChapter, strVerse: string;
  blnContentTypeOverride: boolean);
begin
  SourceType := sitBook;
  Description := '';
  Remark := '';
  Text := '';
  FileName := strBook;
  SlideName := strChapter;
  ShapeName := strVerse;
  ContentTypeOverride := blnContentTypeOverride;
end;

constructor TSourceInfo.CreateAsFileName(strFileName: string;
  blnContentTypeOverride: boolean);
begin
  SourceType := sitFileName;
  Description := '';
  Remark := '';
  Text := '';
  FileName := strFileName;
  SlideName := '';
  ShapeName := '';
  ContentTypeOverride := blnContentTypeOverride;
end;

constructor TSourceInfo.CreateAsPPT(strFilename, strSlide, strShape: string;
  blnContentTypeOverride: boolean);
begin
  SourceType := sitPPT;
  Description := '';
  Remark := '';
  Text := '';
  FileName := strFilename;
  SlideName := strSlide;
  ShapeName := strShape;
  ContentTypeOverride := blnContentTypeOverride;
end;

constructor TSourceInfo.CreateAsString(strText: string;
  blnContentTypeOverride: boolean);
begin
  ClearAsString;
  Text := strText;
  ContentTypeOverride := blnContentTypeOverride;
end;

constructor TSourceInfo.CreateAsTemplate(strTemplateName: string;
  blnContentTypeOverride: boolean);
begin
  SourceType := sitTemplate;
  Description := '';
  Remark := '';
  Text := strTemplateName;
  FileName := '';
  SlideName := '';
  ShapeName := '';
  ContentTypeOverride := blnContentTypeOverride;
end;

function TSourceInfo.DeepCopy: TSourceInfo;
begin
  Result := TSourceInfo.Create;
  Result.SourceType := SourceType;
  Result.Description := Description;
  Result.Remark := Remark;
  Result.Text := Text;
  Result.FileName := GetSettings.DirExplode(FileName);
  Result.SlideName := SlideName;
  Result.ShapeName := ShapeName;
  Result.ContentTypeOverride := ContentTypeOverride;
end;

function TSourceInfo.GetAsJSonObject: TJSONObject;
var
  strFileName: string;
begin
  Result := inherited GetAsJSonObject;

  Assert(FSourceType <> sitUnknown);

  strFileName := GetSettings.DirImplode(FFileName);
  Result.AddPair('SourceType', TJSONNumber.Create(ord(FSourceType)));
  Result.AddPair('Description', EscapeString(FDescription));
  Result.AddPair('Remark', EscapeString(FRemark));
  Result.AddPair('Text', EscapeString(FText));
  Result.AddPair('FileName', EscapeString(strFileName));
  Result.AddPair('SlideName', EscapeString(FSlideName));
  Result.AddPair('ShapeName', EscapeString(FShapeName));
  Result.AddPair(CreateBoolPair('ContentTypeOverride', FContentTypeOverride));
end;

procedure TSourceInfo.SetAsJSonObject(const Value: TJSONObject);
begin
  inherited SetAsJSonObject(Value);
  FSourceType := TSourceInfoType(UUtilsJSON.GetAsInteger(Value, 'SourceType', 0));
  FDescription := UUtilsJSON.GetAsString(Value, 'Description');
  FRemark := UUtilsJSON.GetAsString(Value, 'Remark');
  FText := UUtilsJSON.GetAsString(Value, 'Text');
  FFileName := GetSettings.DirExplode(UUtilsJSON.GetAsString(Value, 'FileName'));
  FSlideName := UUtilsJSON.GetAsString(Value, 'SlideName');
  FShapeName := UUtilsJSON.GetAsString(Value, 'ShapeName');
  FContentTypeOverride := UUtilsJSON.GetAsBoolean(Value, 'ContentTypeOverride');
end;

{ TSourceInfos }

function TSourceInfos.DeepCopy: TSourceInfos;
var
  i: Integer;
begin
  Result := TSourceInfos.Create;
  for i := 0 to Count -1 do begin
    Result.Add(Items[i].DeepCopy);
  end;
end;

end.
