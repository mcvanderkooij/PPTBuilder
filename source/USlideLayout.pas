unit USlideLayout;

interface

uses
  System.Generics.Collections, UITypes, Types, Graphics, Data.DBXJSON, System.JSON,
  UFastKeysSO;

const
  CScreenWidth = 720;
  CScreenHeight = 540;
  CViewMarge = 10;
  CRibbonWidth = 45;
  CFooterHeight = 40;
  CFooterLeftWidth = 200;
  CRemarkHeight = 40;
  CRemarkMarge = 5;
  CTABSTOP1 = 100;

  CFooterColor = TColor($0C0CB1);

  CContent2Height = (CScreenHeight-CFooterHeight - CViewMarge) div 2;
  CContent3Height = (CScreenHeight-CFooterHeight - CViewMarge) div 3;

type
  TAreaAlignment = (alLeft, alCenter, alRight);

  TLayoutItem = class(TValueObject)
  private
    FVisible: boolean;
    FArea: TRect;
    FColor: TColor;
    FFontSize: integer;
    FUseBackground: boolean;
    FBackgroundColor: TColor;
    FAreaAlignment: TAreaAlignment;
    FAutosize: boolean;
  protected
    function GetAsJSonObject: TJSONObject; override;
    procedure SetAsJSonObject(const Value: TJSONObject); override;
  public
    constructor Create; virtual;

    function GetMaxRows(iFontSize: integer): integer;
    function TextFit(strText: string): boolean;
    function TextWidth(strText: string): integer;
    function TextWidthFit(strText: string): boolean;

    property Visible: boolean read FVisible write FVisible;
    property Area: TRect read FArea write FArea;
    property FontColor: TColor read FColor write FColor;
    property FontSize: integer read FFontSize write FFontSize;
    property AreaAlignment: TAreaAlignment read FAreaAlignment write FAreaAlignment;
    property UseBackground: boolean read FUseBackground write FUseBackground;
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property Autosize: boolean read FAutosize write FAutosize;
  end;

  TSlideLayout = class(TFastKeyValuesSO)
  private
    FName: string;
    FShowRibbon: boolean;
    function GetValues(strKey: string): TLayoutItem;
    procedure SetValues(strKey: string; const Value: TLayoutItem);
  protected
    function GetAsJSonObject: TJSONObject; override;
    procedure SetAsJSonObject(const Value: TJSONObject); override;
    function CreateAsJSON(const Value: string): TValueObject; override;
    function CreateAsJSON(const Value: TJSONObject): TValueObject; override;
  public
    constructor Create;
    destructor Destroy; override;
    function DeepCopy: TSlideLayout;

    property Values[strKey: string]: TLayoutItem read GetValues write SetValues; default;
    property ShowRibbon: boolean read FShowRibbon write FShowRibbon;

    property Name: string read FName write FName;
  end;

  TSlideLayouts = class(TObjectList<TSlideLayout>)
  public
    destructor Destroy; override;

    function AddName(strName: string): TSlideLayout;
    function FindByName(strName: string): TSlideLayout;

    procedure SaveLayouts;
    procedure LoadLayouts;
  end;

function GetSlideLayouts: TSlideLayouts;
procedure FillSlideLayouts;

implementation

uses
  SysUtils, Classes,
  UUtilsStrings, USettings, Math, UUtilsJSON, UUtils;


var
  gl_SlideLayouts: TSlideLayouts = nil;

function GetSlideLayouts: TSlideLayouts;
begin
  if not Assigned(gl_SlideLayouts) then begin
    gl_SlideLayouts := TSlideLayouts.Create;
    FillSlideLayouts;
    //gl_SlideLayouts.SaveLayouts;

    //gl_SlideLayouts.LoadLayouts;
  end;
  Result := gl_SlideLayouts;
end;

procedure AddFooter(layout: TSlideLayout; iFooterLeftWidth: integer);
begin
  // 45px
  layout['footer'] := TLayoutItem.Create;
  layout['footer'].Visible := true;
  layout['footer'].Area := Rect(iFooterLeftWidth, CScreenHeight-CFooterHeight, CScreenWidth, CScreenHeight);
  if iFooterLeftWidth = 0 then
    layout['footer'].AreaAlignment := alCenter
  else
    layout['footer'].AreaAlignment := alLeft;
  layout['footer'].FontColor := TColors.White;
  layout['footer'].FontSize := 26;
  layout['footer'].UseBackground := true;
  layout['footer'].BackgroundColor := CFooterColor;
end;

procedure AddFooterLeft(layout: TSlideLayout);
begin
  // 45px
  layout['footer-left'] := TLayoutItem.Create;
  layout['footer-left'].Visible := true;
  layout['footer-left'].Area := Rect(0, CScreenHeight-CFooterHeight, CFooterLeftWidth, CScreenHeight);
  layout['footer-left'].AreaAlignment := alLeft;
  layout['footer-left'].FontColor := TColors.White;
  layout['footer-left'].FontSize := 26;
  layout['footer-left'].UseBackground := true;
  layout['footer-left'].BackgroundColor := CFooterColor;
end;

procedure AddRibbon(layout: TSlideLayout);
begin
  layout['ribbon'] := TLayoutItem.Create;
  layout['ribbon'].Visible := true;
  layout['ribbon'].Area := Rect(CScreenWidth-CViewMarge-CRibbonWidth, 0, CScreenWidth-CViewMarge, CScreenHeight-CFooterHeight);
  layout['ribbon'].AreaAlignment := alCenter;
end;

procedure AddRemark(layout: TSlideLayout);
begin
  // 45px
  layout['remark'] := TLayoutItem.Create;
  layout['remark'].Visible := true;
  layout['remark'].Area := Rect(0, CScreenHeight-CFooterHeight - CRemarkHeight -CRemarkMarge, CScreenWidth-CViewMarge-CRibbonWidth, CScreenHeight-CFooterHeight - CRemarkMarge);
  layout['remark'].AreaAlignment := alRight;
  layout['remark'].FontColor := TColors.White;
  layout['remark'].FontSize := 28;
  layout['remark'].UseBackground := true;
  layout['remark'].BackgroundColor := CFooterColor;
  layout['remark'].Autosize := true;
end;

procedure FillSlideLayouts;
var
  layout: TSlideLayout;
begin
  layout := gl_SlideLayouts.AddName('Titlepage-layout');
  AddFooter(layout, 0);
  layout['content'] := TLayoutItem.Create;
  layout['content'].Visible := true;
  layout['content'].Area := Rect(0, 0, CScreenWidth, CScreenHeight-CFooterHeight);
  layout['content'].FontColor := TColors.White;
  layout['content'].FontSize := 36;

  layout := gl_SlideLayouts.AddName('Content-layout');
  AddRibbon(layout);
  AddFooter(layout, 0);
  layout['content'] := TLayoutItem.Create;
  layout['content'].Visible := true;
  layout['content'].Area := Rect(CViewMarge, CViewMarge, CScreenWidth-CViewMarge-CRibbonWidth, CScreenHeight-CFooterHeight);
  layout['content'].FontColor := TColors.White;
  layout['content'].FontSize := 28;

  layout := gl_SlideLayouts.AddName('Form-layout');
  AddFooter(layout, 0);
  layout['content'] := TLayoutItem.Create;
  layout['content'].Visible := true;
  layout['content'].Area := Rect(CViewMarge*2, CViewMarge, CScreenWidth-CViewMarge*2, CScreenHeight-CFooterHeight);
  layout['content'].FontColor := TColors.White;
  layout['content'].FontSize := 28;

  layout := gl_SlideLayouts.AddName('Songs-layout');
  AddRibbon(layout);
  AddFooter(layout, CFooterLeftWidth);
  AddFooterLeft(layout);
  layout['content'] := TLayoutItem.Create;
  layout['content'].Visible := true;
  layout['content'].Area := Rect(CViewMarge, CViewMarge, CScreenWidth-CViewMarge-CRibbonWidth, CScreenHeight-CFooterHeight);
  layout['content'].FontColor := TColors.White;
  layout['content'].FontSize := 28;
  AddRemark(layout);

  layout := gl_SlideLayouts.AddName('Center-layout');
  AddRibbon(layout);
  AddFooter(layout, 0);
  layout['content'] := TLayoutItem.Create;
  layout['content'].Visible := true;
  layout['content'].Area := Rect(CViewMarge, CViewMarge, CScreenWidth-CViewMarge-CRibbonWidth, CScreenHeight-CFooterHeight);
  layout['content'].AreaAlignment := alCenter;
  layout['content'].FontColor := TColors.White;
  layout['content'].FontSize := 28;

  layout := gl_SlideLayouts.AddName('Content2-layout');
  AddRibbon(layout);
  AddFooter(layout, 0);
  layout['content1'] := TLayoutItem.Create;
  layout['content1'].Visible := true;
  layout['content1'].Area := Rect(CViewMarge, CViewMarge, CScreenWidth-CViewMarge-CRibbonWidth, CViewMarge + CContent2Height);
  layout['content1'].FontColor := TColors.White;
  layout['content1'].FontSize := 28;
  layout['content'] := TLayoutItem.Create;
  layout['content'].Visible := true;
  layout['content'].Area := Rect(CViewMarge, CViewMarge + CContent2Height, CScreenWidth-CViewMarge-CRibbonWidth, CScreenHeight-CFooterHeight);
  layout['content'].FontColor := TColors.White;
  layout['content'].FontSize := 28;

  layout := gl_SlideLayouts.AddName('Content3-layout');
  AddRibbon(layout);
  AddFooter(layout, 0);
  layout['content1'] := TLayoutItem.Create;
  layout['content1'].Visible := true;
  layout['content1'].Area := Rect(CViewMarge, CViewMarge, CScreenWidth-CViewMarge-CRibbonWidth, CViewMarge + CContent3Height);
  layout['content1'].FontColor := TColors.White;
  layout['content1'].FontSize := 28;
  layout['content'] := TLayoutItem.Create;
  layout['content'].Visible := true;
  layout['content'].Area := Rect(CViewMarge, CViewMarge + CContent3Height, CScreenWidth-CViewMarge-CRibbonWidth, CViewMarge + CContent3Height + CContent3Height);
  layout['content'].FontColor := TColors.White;
  layout['content'].FontSize := 28;
  layout['content3'] := TLayoutItem.Create;
  layout['content3'].Visible := true;
  layout['content3'].Area := Rect(CViewMarge, CViewMarge + CContent3Height + CContent3Height, CScreenWidth-CViewMarge-CRibbonWidth, CScreenHeight-CFooterHeight);
  layout['content3'].FontColor := TColors.White;
  layout['content3'].FontSize := 28;
end;

{ TSlideLayout }

constructor TSlideLayout.Create;
begin
  inherited;
end;

function TSlideLayout.CreateAsJSON(const Value: string): TValueObject;
begin
  Result := TLayoutItem.CreateAsJSON(Value);
end;

function TSlideLayout.CreateAsJSON(const Value: TJSONObject): TValueObject;
begin
  Result := TLayoutItem.Create;
  Result.AsJSonObject := Value;
end;

function TSlideLayout.DeepCopy: TSlideLayout;
begin
  Assert(false);
  Result := TSlideLayout.Create;
  Result.Name := FName;
  Result['footer'] := TLayoutItem.CreateAsJSON(Values['footer'].AsJSon);
  Result['content'] := TLayoutItem.CreateAsJSON(Values['content'].AsJSon);
  Result.ShowRibbon := FShowRibbon;
end;

destructor TSlideLayout.Destroy;
begin
  inherited;
end;

function TSlideLayout.GetAsJSonObject: TJSONObject;
begin
  Result := inherited GetAsJSonObject;

  Result.AddPair('Name', EscapeString(FName));
  Result.AddPair(CreateBoolPair('ShowRibbon', FShowRibbon));
end;

function TSlideLayout.GetValues(strKey: string): TLayoutItem;
begin
  Result := inherited GetValues(strKey) as TLayoutItem;
end;

procedure TSlideLayout.SetAsJSonObject(const Value: TJSONObject);
begin
  inherited;

  FName := UUtilsJSON.GetAsString(Value, 'Name');
  FShowRibbon := UUtilsJSON.GetAsBoolean(Value, 'ShowRibbon');
end;

procedure TSlideLayout.SetValues(strKey: string; const Value: TLayoutItem);
begin
  inherited SetValues(strKey, Value);
end;

{ TLayoutItem }

constructor TLayoutItem.Create;
begin
  inherited;
  FVisible := false;
  FUseBackground := false;
  FAreaAlignment := alLeft;
  FAutoSize := false;
end;

function TLayoutItem.GetAsJSonObject: TJSONObject;
begin
  Result := inherited GetAsJSonObject;

  Result.AddPair(CreateBoolPair('Visible', FVisible));
  Result.AddPair(CreateRectPair('Area', FArea));
  Result.AddPair('Color', TJSONNumber.Create(ord(FColor)));
  Result.AddPair('FontSize', TJSONNumber.Create(FFontSize));
  Result.AddPair(CreateBoolPair('UseBackground', FUseBackground));
  Result.AddPair('BackgroundColor', TJSONNumber.Create(ord(FBackgroundColor)));
  Result.AddPair('AreaAlignment', TJSONNumber.Create(ord(FAreaAlignment)));
  Result.AddPair(CreateBoolPair('AutoSize', FAutoSize));
end;

function TLayoutItem.GetMaxRows(iFontSize: integer): integer;
var
  bitmap: TBitmap;
  iTextHeight: integer;
begin
  bitmap := TBitmap.Create;
  try
    bitmap.Canvas.Font.Name := GetSettings.FontName;
    bitmap.Canvas.Font.Size := iFontSize;
    iTextHeight := round(bitmap.Canvas.TextHeight('Wq') * 0.70);
    Result := Max(1, FArea.Height div iTextHeight);
  finally
    bitmap.Free;
  end;
end;

procedure TLayoutItem.SetAsJSonObject(const Value: TJSONObject);
begin
  inherited;

  FVisible := UUtilsJSON.GetAsBoolean(Value, 'Visible');
  FArea := UUtilsJSON.GetAsRect(Value, 'Area');
  FColor := TColor(UUtilsJSON.GetAsInteger(Value, 'Color'));
  FFontSize := UUtilsJSON.GetAsInteger(Value, 'FontSize');
  FUseBackground := UUtilsJSON.GetAsBoolean(Value, 'UseBackground');
  FBackgroundColor := TColor(UUtilsJSON.GetAsInteger(Value, 'BackgroundColor'));
  FAreaAlignment := TAreaAlignment(UUtilsJSON.GetAsInteger(Value, 'AreaAlignment'));
  FAutoSize := UUtilsJSON.GetAsBoolean(Value, 'AutoSize');
end;

function TLayoutItem.TextFit(strText: string): boolean;
var
  bitmap: TBitmap;
  rectText: TRect;
begin
  bitmap := TBitmap.Create;
  try
    bitmap.Canvas.Font.Name := GetSettings.FontName;
    bitmap.Canvas.Font.Size := round(FFontSize * 0.8);

    rectText.Left := 0;
    rectText.Right := FArea.Width;
    rectText.Top := 0;
    rectText.Bottom := FArea.Height;
    bitmap.Canvas.TextRect(rectText, strText, [tfWordBreak, tfCalcRect]);

    bitmap.SetSize(rectText.Width, rectText.Height);
    bitmap.Canvas.TextRect(rectText, strText, [tfWordBreak]);
//    bitmap.SaveToFile('d:\temp\_dump.bmp');

    Result := rectText.Height <= (FArea.Height);
  finally
    bitmap.Free;
  end;
end;

function TLayoutItem.TextWidth(strText: string): integer;
var
  bitmap: TBitmap;
  rectText: TRect;
begin
  bitmap := TBitmap.Create;
  try
    bitmap.Canvas.Font.Name := GetSettings.FontName;
    bitmap.Canvas.Font.Size := round(FFontSize);

    rectText.Left := 0;
    rectText.Right := FArea.Width;
    rectText.Top := 0;
    rectText.Bottom := FArea.Height;
    bitmap.Canvas.TextRect(rectText, strText, [tfSingleLine, tfCalcRect]);
    Result := rectText.Width;
  finally
    bitmap.Free;
  end;

end;

function TLayoutItem.TextWidthFit(strText: string): boolean;
var
  bitmap: TBitmap;
  rectText: TRect;
  iMaxWidth: integer;
begin
  iMaxWidth := FArea.Width;
  if pos(#9, strText) > 0 then begin
    iMaxWidth := iMaxWidth - CTABSTOP1;
    strText := copy(strText, pos(#9, strText) + 1, MaxInt);
  end;
  bitmap := TBitmap.Create;
  try
    bitmap.Canvas.Font.Name := GetSettings.FontName;
    bitmap.Canvas.Font.Size := round(FFontSize);

    rectText.Left := 0;
    rectText.Right := iMaxWidth;
    rectText.Top := 0;
    rectText.Bottom := FArea.Height;
    bitmap.Canvas.TextRect(rectText, strText, [tfSingleLine, tfCalcRect]);
    Result := rectText.Width <= iMaxWidth;
  finally
    bitmap.Free;
  end;
end;

{ TSlideLayouts }

function TSlideLayouts.AddName(strName: string): TSlideLayout;
begin
  Result := FindByName(strName);
  if Result = nil then begin
    Result := TSlideLayout.Create;
    Result.Name := strName;
    inherited Add(Result);
  end;
end;

destructor TSlideLayouts.Destroy;
begin
  Clear;
  inherited;
end;

function TSlideLayouts.FindByName(strName: string): TSlideLayout;
var
  i: Integer;
begin
  for i := 0 to Count -1 do begin
    if Items[i].Name = strName then begin
      Result := Items[i];
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TSlideLayouts.SaveLayouts;
var
  strDir: string;
  i: Integer;
begin
  strDir := GetSettings.GetContentDir + 'layouts\';
  ForceDirectories(strDir);
  for i := 0 to Count -1 do begin
    SaveUnicodeToFile(strDir + Items[i].Name + '.json', Items[i].AsJSon);
  end;
end;

procedure TSlideLayouts.LoadLayouts;
var
  slLayoutFiles: TStringList;
  strDir: string;
  i: Integer;
  layout: TSlideLayout;
begin
  strDir := GetSettings.GetContentDir + 'layouts\';
  slLayoutFiles := TStringList.Create;
  try
    FindFilesWithExtensions(slLayoutFiles, false, strDir, '', '.json');

    for i := 0 to slLayoutFiles.Count -1 do begin
      layout := AddName(ChangeFileExt(slLayoutFiles[i], ''));
      layout.AsJSon := LoadUnicodeFromFile( strDir + slLayoutFiles[i] );
    end;
  finally
    slLayoutFiles.Free;
  end;
end;

initialization
  gl_SlideLayouts := nil;
finalization
  gl_SlideLayouts.Free;
  gl_SlideLayouts := nil;
end.
