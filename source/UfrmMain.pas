unit UfrmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ButtonGroup, Vcl.Menus,
  UProject, Vcl.ExtCtrls, Vcl.AppEvnts, UFastKeysSO;

type
  TfrmMain = class(TForm)
    MainMenu1: TMainMenu;
    mniFile: TMenuItem;
    mniFileNew: TMenuItem;
    mniFileOpen: TMenuItem;
    mniFileSave: TMenuItem;
    mniFileSaveAs: TMenuItem;
    N1: TMenuItem;
    mniFileBuildPPT: TMenuItem;
    lbSlides: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    edtSpeaker: TEdit;
    OpenDialogProject: TOpenDialog;
    SaveDialogProject: TSaveDialog;
    SaveDialogPPT: TSaveDialog;
    ppmSlides: TPopupMenu;
    mniSlidesDelete: TMenuItem;
    Label3: TLabel;
    edtCollecte1: TEdit;
    Label4: TLabel;
    edtCollecte2: TEdit;
    mniSlidesCopy: TMenuItem;
    CategoryPanelGroup1: TCategoryPanelGroup;
    CategoryPanel1: TCategoryPanel;
    ApplicationEvents1: TApplicationEvents;
    mniSettings: TMenuItem;
    N2: TMenuItem;
    btnSpeakerAdd: TButton;
    btnSpeakerSelect: TButton;
    btnCollecte1Add: TButton;
    btnCollecte1Select: TButton;
    btnCollecte2Add: TButton;
    btnCollecte2Select: TButton;
    mniFileRecentlyUsed: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure ButtonGroupNewSlideButtonClicked(Sender: TObject; Index: Integer);
    procedure lbSlidesDblClick(Sender: TObject);
    procedure lbSlidesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbSlidesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbSlidesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure mniFileOpenClick(Sender: TObject);
    procedure mniFileSaveClick(Sender: TObject);
    procedure mniFileSaveAsClick(Sender: TObject);
    procedure mniFileBuildPPTClick(Sender: TObject);
    procedure ppmSlidesPopup(Sender: TObject);
    procedure mniSlidesDeleteClick(Sender: TObject);
    procedure mniSlidesCopyClick(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure edtCollecte1Exit(Sender: TObject);
    procedure lbSlidesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure mniSettingsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSpeakerAddClick(Sender: TObject);
    procedure btnCollecte1AddClick(Sender: TObject);
    procedure btnCollecte2AddClick(Sender: TObject);
    procedure btnSpeakerSelectClick(Sender: TObject);
    procedure btnCollecte1SelectClick(Sender: TObject);
    procedure btnCollecte2SelectClick(Sender: TObject);
    procedure mniFileClick(Sender: TObject);
  private
    { Private declarations }
    FStartingPoint: TPoint;
    FFileName: string;
    FSavedProjectHash: string;
    FLastProjectHash: string;
    FHasChanged: boolean;
    FPictos: TFastKeyValuesSO;
    FShowQuickstart: boolean;
    procedure ClearSlides;

    procedure DoBuild;
    function DoProjectCreate(strLiturgy: string): string;
    function DoOpen(strFileName: string): string;
    function DoSave(strFileName: string): string;
    function PerformOpen(strFileName: string): string;
    procedure DoSlideCopy;
    procedure DoSlideDelete;
    procedure FillTemplates;
    procedure FillLiturgies;
    procedure ShowSettings;
    procedure mniFileNewClick(Sender: TObject);
    procedure mniRecentFileClick(Sender: TObject);

    function CreateProjectHash: string;
    function CheckChanged: boolean;
    procedure SetHasChanged(const Value: boolean);
  public
    procedure DoShowQuickStart;
    procedure ProjectToForm(project: TProject);
    procedure ProjectFromForm(var project: TProject);

    function SlidesToString: string;
    procedure SlidesFromString(strText: string);

    property HasChanged: boolean read FHasChanged write SetHasChanged;
    { Public declarations }
  end;

  TPictoObject = class(TValueObject)
  private
    FPicto: TBitmap;
  public
    constructor Create(strPictoFileName: string); virtual;
    destructor Destroy; override;

    property Picto: TBitmap read FPicto;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  PNGImage, GR32, GR32_Misc2,
  GnuGetText, UUtils, UUtilsForms, USlide, USlideTemplate, UBuildPowerpoint, UMRUList,
  ULiturgy, USourceInfo, UfrmSettings, USettings, UfrmSelectString, UfrmQuickStart;

procedure TfrmMain.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  HasChanged := FLastProjectHash <> FSavedProjectHash;

  btnSpeakerAdd.Enabled := (edtSpeaker.Text <> '') and (GetSettings.Speakers.IndexOf(edtSpeaker.Text) = -1);
  btnCollecte1Add.Enabled := (edtCollecte1.Text <> '') and (GetSettings.Collecte1.IndexOf(edtCollecte1.Text) = -1);
  btnCollecte2Add.Enabled := (edtCollecte2.Text <> '') and (GetSettings.Collecte2.IndexOf(edtCollecte2.Text) = -1);

  if FShowQuickstart then begin
    FShowQuickstart := False;
    DoShowQuickStart;
  end;
end;

procedure TfrmMain.btnCollecte1AddClick(Sender: TObject);
begin
  GetSettings.Collecte1.Add(edtCollecte1.Text);
  GetSettings.Save;
end;

procedure TfrmMain.btnCollecte1SelectClick(Sender: TObject);
var
  strSelected: string;
  blnStringsChanged: boolean;
begin
  strSelected := edtCollecte1.Text;
  if SelectString(GetSettings.Collecte1, _('Select Collecte 1'), strSelected, blnStringsChanged) then begin
    edtCollecte1.Text := strSelected;
    if blnStringsChanged then
      GetSettings.Save;
  end;
end;

procedure TfrmMain.btnCollecte2AddClick(Sender: TObject);
begin
  GetSettings.Collecte2.Add(edtCollecte2.Text);
  GetSettings.Save;
end;

procedure TfrmMain.btnCollecte2SelectClick(Sender: TObject);
var
  strSelected: string;
  blnStringsChanged: boolean;
begin
  strSelected := edtCollecte2.Text;
  if SelectString(GetSettings.Collecte2, _('Select Collecte 2'), strSelected, blnStringsChanged) then begin
    edtCollecte2.Text := strSelected;
    if blnStringsChanged then
      GetSettings.Save;
  end;
end;

procedure TfrmMain.btnSpeakerAddClick(Sender: TObject);
begin
  GetSettings.Speakers.Add(edtSpeaker.Text);
  GetSettings.Save;
end;

procedure TfrmMain.btnSpeakerSelectClick(Sender: TObject);
var
  strSelected: string;
  blnStringsChanged: boolean;
begin
  strSelected := edtSpeaker.Text;
  if SelectString(GetSettings.Speakers, _('Select Speaker'), strSelected, blnStringsChanged) then begin
    edtSpeaker.Text := strSelected;
    if blnStringsChanged then
      GetSettings.Save;
  end;
end;

procedure TfrmMain.ButtonGroupNewSlideButtonClicked(Sender: TObject;
  Index: Integer);
var
  buttongroup: TButtonGroup;
  button: TGrpButtonItem;
  slide: TSlide;
  template: TSlideTemplate;
  iInsertPos: integer;
begin
  iInsertPos := lbSlides.ItemIndex;
  if iInsertPos = -1 then begin
    iInsertPos := lbSlides.Items.Count;
  end else begin
    //inc(iInsertPos);
  end;
  buttongroup := Sender as TButtonGroup;
  if Assigned(buttongroup) then begin
    button := buttongroup.Items[Index];
    if Assigned(button) then begin
      template := TSlideTemplate(button.Data);
      if Assigned(template) then begin
        slide := template.DoOnAdd(true);
        if Assigned(slide) then begin
          lbSlides.Items.InsertObject(iInsertPos, slide.SlideName, slide);
          lbSlides.ItemIndex := iInsertPos;
          FLastProjectHash := CreateProjectHash;
        end;
      end;
    end;
  end;
end;

function TfrmMain.CheckChanged: boolean;
begin
  Result := True;
  if (FLastProjectHash <> '') and (FLastProjectHash <> FSavedProjectHash) then begin
    case QuestionYNC(_('Do you want to save changes?')) of
      cbUnknown: Result := false;
      cbTrue: FFileName := DoSave(FFileName);
    end;
  end;
end;

procedure TfrmMain.ClearSlides;
var
  i: integer;
begin
  for i := 0 to lbSlides.Items.Count -1 do begin
    lbSlides.Items.Objects[i].Free;
  end;
  lbSlides.Clear;
end;

function TfrmMain.CreateProjectHash: string;
var
  project: TProject;
begin
  project := nil;
  ProjectFromForm(project);
  try
    Result := project.CreateHash;
  finally
    project.Free;
  end;
end;

procedure TfrmMain.DoBuild;
var
  project: TProject;
begin
  if SaveDialogPPT.Execute then begin
    project := nil;
    ProjectFromForm(project);
    try
      BuildPowerpoint(SaveDialogPPT.FileName, project);
    finally
      project.Free;
    end;
  end;
end;

function TfrmMain.DoProjectCreate(strLiturgy: string): string;
var
  project: TProject;
  liturgy: TLiturgy;
begin
//  if FFileName <> '' then begin
//    DoSave(FFileName);
//  end;
  if CheckChanged then begin
    project := TProject.Create;
    try
      liturgy := GetLiturgies.FindByName(strLiturgy);
      if Assigned(liturgy) then begin
        liturgy.FillProject(project);
      end;
      ProjectToForm(project);
      FSavedProjectHash := project.CreateHash;
      FLastProjectHash := FSavedProjectHash;
    finally
      project.Free;
    end;
    Result := '';
  end else
    Result := FFileName;
end;

function TfrmMain.DoOpen(strFileName: string): string;
var
  project: TProject;
begin
  if CheckChanged then begin
    OpenDialogProject.InitialDir := extractFilePath(strFileName);
    if OpenDialogProject.Execute and FileExists(OpenDialogProject.FileName) then begin
      Result := PerformOpen(OpenDialogProject.FileName);
    end;
  end;
end;

function TfrmMain.DoSave(strFileName: string): string;
var
  project: TProject;
begin
  if strFileName = '' then begin
    if SaveDialogProject.Execute then begin
      strFileName := SaveDialogProject.FileName;
    end;
  end;
  project := nil;
  try
    if strFileName <> '' then begin
      GetMRUList.AddMRU(strFileName);
      ProjectFromForm(project);
      SaveUnicodeToFile(strFileName, project.AsJSon);
      FSavedProjectHash := project.CreateHash;
      FLastProjectHash := FSavedProjectHash;
    end;
  finally
    project.Free;
  end;
  Result := strFileName;
end;

procedure TfrmMain.DoSlideCopy;
begin
  if lbSlides.ItemIndex <> -1 then begin
    lbSlides.Items.AddObject(
       lbSlides.Items[lbSlides.ItemIndex],
       TSlide.Create(TSlide(lbSlides.Items.Objects[lbSlides.ItemIndex]).AsJSon)
    );
    FLastProjectHash := CreateProjectHash;
  end;
end;

procedure TfrmMain.DoSlideDelete;
begin
  if lbSlides.ItemIndex <> -1 then begin
    lbSlides.Items.Objects[lbSlides.ItemIndex].Free;
    lbSlides.Items.Delete(lbSlides.ItemIndex);
    FLastProjectHash := CreateProjectHash;
  end;
end;

procedure TfrmMain.edtCollecte1Exit(Sender: TObject);
begin
  FLastProjectHash := CreateProjectHash;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckChanged;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
  FShowQuickstart := False;

  FHasChanged := False;
  FillTemplates;
  FillLiturgies;

  FPictos := TFastKeyValuesSO.Create;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ClearSlides;
  FPictos.Free;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  lbSlides.Repaint;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if (GetSettings.FTPUserName = '') or (GetSettings.FTPPassword = '') then begin
    ShowSettings;
  end;
  FShowQuickstart := True;
end;

procedure TfrmMain.FillLiturgies;
var
  liturgies: TLiturgies;
  i: Integer;
  mniNew: TmenuItem;
begin
  liturgies := GetLiturgies;

  while mniFileNew.Count > 0 do
    mniFileNew.Delete(0);

  for i := 0 to liturgies.Count -1 do begin
    mniNew := TMenuItem.Create(mniFileNew);
    mniNew.Caption := liturgies[i].Name;
    mniNew.OnClick := mniFileNewClick;
    mniFileNew.Add(mniNew);
  end;
end;

procedure TfrmMain.FillTemplates;
var
  i, iTemplate, iPanel: integer;
  templates: TSlideTemplates;
  button: TGrpButtonItem;

  panel, panelFound: TCategoryPanel;
  group: TButtonGroup;
begin
  templates := GetSlideTemplates;

  for i := 0 to CategoryPanelGroup1.Panels.Count -1 do begin
    TObject(CategoryPanelGroup1.Panels[i]).Free;
  end;
  CategoryPanelGroup1.Panels.Clear;

  for iTemplate := 0 to templates.Count -1 do begin
    if templates[iTemplate].CategoryName <> '' then begin
      panelFound := nil;
      for iPanel := 0 to CategoryPanelGroup1.Panels.Count -1 do begin
        panel := CategoryPanelGroup1.Panels[iPanel];
        if panel.Caption = templates[iTemplate].CategoryName then begin
          panelFound := panel;
          break;
        end;
      end;
      if not Assigned(panelFound) then begin
        panelFound := TCategoryPanel.Create(CategoryPanelGroup1);
        panelFound.Caption := templates[iTemplate].CategoryName;
        panelFound.PanelGroup := CategoryPanelGroup1;

        panelFound.Height := 25;
      end;

      group := nil;
      for i := 0 to panelFound.ComponentCount -1 do begin
        if panelFound.Components[i] is TButtonGroup then begin
          group := panelFound.Components[i] as TButtonGroup;
          break;
        end;
      end;

      if not Assigned(group) then begin
        group := TButtonGroup.Create(panelFound);
        group.Parent := panelFound;
        group.ButtonOptions := [gboFullSize, gboShowCaptions];
        group.Align := alClient;
        group.OnButtonClicked := ButtonGroupNewSlideButtonClicked;
      end;

      button := group.Items.Add;
      button.Caption := templates[iTemplate].Name;
      button.Data := templates[iTemplate];

      panelFound.Height := 30 + group.Items.Count * group.ButtonHeight;
    end;
  end;
  CategoryPanelGroup1.CollapseAll;
end;

procedure TfrmMain.lbSlidesDblClick(Sender: TObject);
var
  slide: TSlide;
  templates: TSlideTemplates;
  template: TSlideTemplate;
begin
  if lbSlides.ItemIndex <> -1 then begin
    slide := lbSlides.Items.Objects[lbSlides.ItemIndex] as TSlide;
    if Assigned(slide) then begin
      templates := GetSlideTemplates;
      template := templates.FindByName(slide.SlideTemplateName);
      if Assigned(template) then begin
        template.DoOnEdit(slide);
        lbSlides.Items[lbSlides.ItemIndex] := slide.SlideName;
      end;
    end;
    FLastProjectHash := CreateProjectHash;
  end;
end;

procedure TfrmMain.lbSlidesDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DropPosition, StartPosition: Integer;
  DropPoint: TPoint;

  strSlide: string;
  oSlide: TObject;
begin
  DropPoint.X := X;
  DropPoint.Y := Y;
  with Source as TListBox do begin
    StartPosition := ItemAtPos(FStartingPoint, True);
    DropPosition := ItemAtPos(DropPoint,True);

    if StartPosition < DropPosition then
      dec(DropPosition);

    if (StartPosition <> DropPosition) and (StartPosition <> -1) then begin
      strSlide := Items[StartPosition];
      oSlide := Items.Objects[StartPosition];

      Items.Delete(StartPosition);
      if DropPosition < 0 then begin
        items.AddObject(strSlide, oSlide);
        ItemIndex := Items.Count -1;
      end else begin
        items.InsertObject(DropPosition, strSlide, oSlide);
        ItemIndex := DropPosition;
      end;
      FLastProjectHash := CreateProjectHash;
    end;
  end;
end;

procedure TfrmMain.lbSlidesDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source = lbSlides;
end;

procedure TfrmMain.lbSlidesDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
const
  aColors: array[0..1] of TColor = (clWhite, $f0f0f0);
var
  rectText: TRect;
  rectPicto: TRect;

  slide: TSlide;
  strText: string;
begin
  rectText := Rect;
  rectText.Left := rectText.Left + 32 + 6;
  strText := lbSlides.Items[Index];

  if odFocused in State then begin
    lbSlides.Canvas.Font.Color := clWhite;
    lbSlides.Canvas.Brush.Color := clBlue;
  end;
  lbSlides.Canvas.FillRect(Rect);

  slide := TSlide(lbSlides.Items.Objects[Index]);

  if Assigned(slide) then begin

    if slide.IsSubOverview then
      lbSlides.Canvas.Font.Style := lbSlides.Canvas.Font.Style + [fsItalic];

    lbSlides.Canvas.TextRect(rectText, strText, [tfSingleLine, tfVerticalCenter]);

    if FPictos[slide.PictoName.FileName] = nil then begin
      FPictos[slide.PictoName.FileName] := TPictoObject.Create(slide.PictoName.FileName);
    end;
    rectPicto := Rect;
    rectPicto.Right := rectPicto.Left + 32;

    lbSlides.Canvas.Draw(rectPicto.Left+1, rectPicto.Top+1, TPictoObject(FPictos[slide.PictoName.FileName]).Picto);
  end;
  if odFocused in State then
  begin
    lbSlides.Canvas.Brush.Color := aColors[Index mod 2] xor $132828;
    lbSlides.Canvas.DrawFocusRect(Rect);
  end;
end;

procedure TfrmMain.lbSlidesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FStartingPoint.X := X;
  FStartingPoint.Y := Y;
end;

procedure TfrmMain.mniFileBuildPPTClick(Sender: TObject);
begin
  DoBuild;
end;

procedure TfrmMain.mniFileClick(Sender: TObject);
var
  i: integer;
  mniRecentFile: TMenuItem;
  mrus: TMRUList;
begin
  mniFileRecentlyUsed.Clear;

  while mniFileRecentlyUsed.Count > 0 do
    mniFileRecentlyUsed.Delete(0);

  mrus := GetMRUList;
  for i := 0 to mrus.Count -1 do begin
    mniRecentFile := TMenuItem.Create(mniFileRecentlyUsed);
    mniRecentFile.Caption := mrus[i];
    mniRecentFile.OnClick := mniRecentFileClick;
    mniFileRecentlyUsed.Add(mniRecentFile);
  end;
end;

procedure TfrmMain.mniFileNewClick(Sender: TObject);
begin
  FFileName := DoProjectCreate( (Sender as TMenuItem).Caption );
end;

procedure TfrmMain.mniFileOpenClick(Sender: TObject);
begin
  FFileName := DoOpen(FFileName);
end;

procedure TfrmMain.mniFileSaveAsClick(Sender: TObject);
begin
  FFileName := DoSave('');
end;

procedure TfrmMain.mniFileSaveClick(Sender: TObject);
begin
  FFileName := DoSave(FFileName);
end;

procedure TfrmMain.mniRecentFileClick(Sender: TObject);
begin
  FFileName := PerformOpen( (Sender as TMenuItem).Caption );
end;

procedure TfrmMain.mniSettingsClick(Sender: TObject);
begin
  ShowSettings;
end;

procedure TfrmMain.mniSlidesCopyClick(Sender: TObject);
begin
  DoSlideCopy;
end;

procedure TfrmMain.mniSlidesDeleteClick(Sender: TObject);
begin
  DoSlideDelete;
end;

function TfrmMain.PerformOpen(strFileName: string): string;
var
  project: TProject;
begin
  project := TProject.Create;
  project.AsJSon := LoadUnicodeFromFile(strFileName);
  if Assigned(project) then begin
    try
      GetMRUList.AddMRU(strFileName);

      ProjectToForm(project);
      FSavedProjectHash := project.CreateHash;
      FLastProjectHash := FSavedProjectHash;
      Result := strFileName;
    finally
      project.Free;
    end;
  end;
end;

procedure TfrmMain.ppmSlidesPopup(Sender: TObject);
begin
  // enabled state
  mniSlidesDelete.Enabled := lbSlides.ItemIndex <> -1;
end;

procedure TfrmMain.ProjectFromForm(var project: TProject);
begin
  if not Assigned(project) then begin
    project := TProject.Create;
  end;
  project.Properties['speaker'] := edtSpeaker.Text;
  project.Properties['collecte1'] := edtCollecte1.Text;
  project.Properties['collecte2'] := edtCollecte2.Text;
  project.Slides.Text := SlidesToString;
end;

procedure TfrmMain.ProjectToForm(project: TProject);
begin
  edtSpeaker.Text := project.Properties['speaker'];
  edtCollecte1.Text := project.Properties['collecte1'];
  edtCollecte2.Text := project.Properties['collecte2'];
  SlidesFromString(project.Slides.Text);
end;

procedure TfrmMain.SetHasChanged(const Value: boolean);
var
  strCaption: string;
begin
  if FHasChanged <> Value then begin
    FHasChanged := Value;

    strCaption := _('Powerpoint Builder');
    if FHasChanged then
      strCaption := strCaption + ' ' + _('(changed)');
    Caption := strCaption;
  end;
end;

procedure TfrmMain.DoShowQuickStart;
var
  strLiturgy: string;
begin
  case ShowQuickStart(strLiturgy) of
    qsCancel: Exit;
    qsOpen: FFileName := DoOpen(FFileName);
    qsOpenFile: FFileName := PerformOpen(strLiturgy);
    qsNew: FFileName := DoProjectCreate( strLiturgy );
  end;
end;

procedure TfrmMain.ShowSettings;
var
  frmSettings: TfrmSettings;
begin
  frmSettings := TfrmSettings.Create(Application.MainForm);
  try
    frmSettings.ShowModal;
  finally
    frmSettings.Free;
  end;
end;

procedure TfrmMain.SlidesFromString(strText: string);
var
  slSlides: TStringList;
  strSlide: string;
  slide: TSlide;
  i: integer;
begin
  ClearSlides;
  slSlides := TStringList.Create;
  try
    slSlides.Text := strText;
    for i := 0 to slSlides.Count-1 do begin
      strSlide := slSlides[i];
      if strSlide <> '' then begin
        slide := TSlide.Create(strSlide);
        lbSlides.Items.AddObject(slide.SlideName, slide);
      end;
    end;
  finally
    slSlides.Free;
  end;
end;

function TfrmMain.SlidesToString: string;
var
  slSlides: TStringList;
  slide: TSlide;
  i: integer;
begin
  Result := '';
  slSlides := TStringList.Create;
  try
    for i := 0 to lbSlides.Items.Count-1 do begin
      slide := lbSlides.Items.Objects[i] as TSlide;
      slSlides.Add(slide.AsJSon);
      Result := slSlides.Text;
    end;
  finally
    slSlides.Free;
  end;
end;

{ TPictoObject }

constructor TPictoObject.Create(strPictoFileName: string);
var
  png: TPngImage;
  bmp32, bmp32Dest: TBitmap32;
  bAlphaChannelUsed: boolean;
begin
  inherited Create;

  FPicto := TBitmap.Create;
  bmp32 := TBitmap32.Create;
  bmp32Dest := TBitmap32.Create;
  try
    bmp32Dest.SetSize(32, 32);
    if strPictoFileName = '' then
      bmp32Dest.Clear(clWhite32)
    else
      bmp32Dest.Clear(clBlack32);

    if FileExists(strPictoFileName) then begin
      png := TPngImageX.Create;
      try
        png.LoadFromFile(strPictoFileName);
        LoadPNGintoBitmap32(bmp32, png, bAlphaChannelUsed);

        bmp32.DrawTo(bmp32Dest, bmp32Dest.BoundsRect);
      finally
        png.Free;
      end;
    end;

    FPicto.Assign(bmp32Dest);
  finally
    bmp32Dest.Free;
    bmp32.Free;
  end;
end;

destructor TPictoObject.Destroy;
begin
  FPicto.Free;
  inherited;
end;

end.
