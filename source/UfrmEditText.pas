unit UfrmEditText;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, GR32_Image,
  USlide, USourceInfo, Vcl.ExtCtrls, UFastKeysSS;

type
  TfrmEditText = class(TForm, ISlideEditForm)
    Label2: TLabel;
    Label3: TLabel;
    edtOverviewName: TEdit;
    ImgViewPicto: TImgView32;
    btnOK: TButton;
    btnCancel: TButton;
    mmoText: TMemo;
    btnInsertSlide: TButton;
    btnPaste: TButton;
    cbxAutomaticSmallNumbers: TCheckBox;
    Shape1: TShape;
    btnInternet: TButton;
    btnFill: TButton;
    cbxPartOfForm: TCheckBox;
    cbxShowInOverview: TCheckBox;
    btnSelectPictoNone: TButton;
    btnConvertToAGB: TButton;
    btnConvertToDL: TButton;
    btnConvertToGBA: TButton;
    btnConvertToHC: TButton;
    procedure ImgViewPictoClick(Sender: TObject);
    procedure btnInsertSlideClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mmoTextKeyPress(Sender: TObject; var Key: Char);
    procedure btnPasteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnInternetClick(Sender: TObject);
    procedure btnFillClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSelectPictoNoneClick(Sender: TObject);
    procedure btnConvertToHCClick(Sender: TObject);
    procedure btnConvertToAGBClick(Sender: TObject);
    procedure btnConvertToDLClick(Sender: TObject);
    procedure btnConvertToGBAClick(Sender: TObject);
  private
    FStartSlide: string;
    FConvertedSlide: string;
    FSlideTemplateName: string;
    FPictoName: TSourceInfo;
    FIsBibleText: boolean;
    FSlideVariables: TFastKeyValuesSS;
    FSlideOptions: TFastKeyValuesSS;
    FOverviewType: TOverviewType;
    { Private declarations }
  protected
    function GetSlideAsString: string;
    procedure SetSlideAsString(const Value: string);
    function Edit: boolean;
    procedure ConvertToTemplate(strReadingTemplate, strTextTemplate: string);
  public
    property SlideAsString: string read GetSlideAsString write SetSlideAsString;
    property IsBibleText: boolean read FIsBibleText write FIsBibleText;
  end;

implementation

{$R *.dfm}

uses
  Clipbrd,
  GnuGetText, UUtilsStrings, UUtils, UUtilsForms, USettings, USplitBibleVerses,
  USlideLayout, UfrmBrowseFTP, USlideTemplate, USlideVariables;

{ TfrmEditText }

procedure TfrmEditText.btnConvertToAGBClick(Sender: TObject);
begin
  ConvertToTemplate(CTEMPLATE_READING_AGB, CTEMPLATE_TEXT_AGB);
end;

procedure TfrmEditText.btnConvertToDLClick(Sender: TObject);
begin
  ConvertToTemplate(CTEMPLATE_READING_DL, CTEMPLATE_TEXT_DL);
end;

procedure TfrmEditText.btnConvertToGBAClick(Sender: TObject);
begin
  ConvertToTemplate(CTEMPLATE_READING_GBA, CTEMPLATE_TEXT_GBA);
end;

procedure TfrmEditText.btnConvertToHCClick(Sender: TObject);
begin
  ConvertToTemplate(CTEMPLATE_READING_HC, CTEMPLATE_TEXT_HC);
end;

procedure TfrmEditText.ConvertToTemplate(strReadingTemplate, strTextTemplate: string);
var
  template: TSlideTemplate;
  slide: TSlide;
begin
  case FOverviewType of
    otIgnore: ;
    otSong: ;
    otReading: template := GetSlideTemplates.FindByName(strReadingTemplate);
    otText: template := GetSlideTemplates.FindByName(strTextTemplate);
  end;

  if Assigned(template) then begin
    Hide;
    slide := template.DoOnAdd(true);
    if Assigned(slide) then begin
      FConvertedSlide := slide.AsJSon;
    end;
    ModalResult := mrOK;
  end;
end;

procedure TfrmEditText.btnFillClick(Sender: TObject);
var
  slVariables: TStringList;
begin
  slVariables := TStringList.Create;
  try
    slVariables.Duplicates := dupIgnore;
    slVariables.Sorted := true;
    DetectSlideVariables(mmoText.Text, slVariables);
    CleanupSlideVariables(FSlideVariables, slVariables);
    EditSlideVariables(FSlideVariables, FSlideOptions);
  finally
    slVariables.Free;
  end;
end;

procedure TfrmEditText.btnInsertSlideClick(Sender: TObject);
begin
  if mmoText.SelText = CNEWSLIDE then begin
    mmoText.SelText := '';
  end else begin
    mmoText.SelLength := 0;
    mmoText.SelText := CNEWSLIDE;
  end;
end;

procedure TfrmEditText.btnInternetClick(Sender: TObject);
begin
  OpenInternetSource;
end;

procedure TfrmEditText.btnPasteClick(Sender: TObject);
var
  strText: string;
  aText: TArrayOfString;
  i: integer;
  layout: TSlideLayout;
  content: TLayoutItem;
  options: TSplitOptions;
begin
  strText := Clipboard.AsText;
  if strText <> '' then begin
    content := nil;
    layout := GetSlideLayouts.FindByName('Content-layout');
    if Assigned(layout) then begin
      content := layout['content'];
    end;

    options := [soLineBreaks];
    if IsBibleText then
      options := options + [soNumbers];

    aText := SplitBibleVerses(strText, 550, content, options);
    //mmoText.Lines.Clear;
    if mmoText.Lines.Count > 0 then begin
      mmoText.Lines.Add(CNEWSLIDE);
    end;
    //mmoText.Lines.Text := aText[0];
    mmoText.Lines.Text := mmoText.Lines.Text + aText[0];
    for i := 1 to Length(aText) -1 do begin
      mmoText.Lines.Add(CNEWSLIDE);
      mmoText.Lines.Text := mmoText.Lines.Text + aText[i];
    end;
  end;
end;

procedure TfrmEditText.btnSelectPictoNoneClick(Sender: TObject);
begin
  FPictoName.FileName := '';
  ImgViewPicto.Bitmap.Clear;
end;

function TfrmEditText.Edit: boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TfrmEditText.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
  FSlideVariables := TFastKeyValuesSS.Create;
  FSlideOptions := TFastKeyValuesSS.Create;
  FConvertedSlide := '';
end;

procedure TfrmEditText.FormDestroy(Sender: TObject);
begin
  FPictoName.Free;
  FSlideOptions.Free;
  FSlideVariables.Free;
end;

procedure TfrmEditText.FormShow(Sender: TObject);
var
  template: TSlideTemplate;
begin
  template := GetSlideTemplates.FindByName(FSlideTemplateName);
  btnFill.Visible := Assigned(template) and (epSlideVariables in template.EditPossibilities);
  btnInsertSlide.Caption := CNEWSLIDE;
end;

function TfrmEditText.GetSlideAsString: string;
var
  slide: TSlide;
  slideItemContent, slideItemFooter: TSlideItem;
  aContentStrings: TArrayOfString;
  i: integer;
  strPrefix: string;
begin
  if FConvertedSlide <> '' then begin
    Result := FConvertedSlide;
    Exit;
  end;

  slide := TSlide.Create(FStartSlide);
  try
    slide.SlideName := slide.SlideTemplateName + ' : ' + edtOverviewName.Text;
    slide.OverviewName := edtOverviewName.Text;
    slide.ShowInOverview := cbxShowInOverview.Checked;
    slide.IsSubOverview := cbxPartOfForm.Checked;
    slide.PictoName := FPictoName.DeepCopy;
    slide.AutomaticSmallNumbers := cbxAutomaticSmallNumbers.Checked;
    slide.Variables.LoadFromString(FSlideVariables.SaveToString);

    slideItemFooter := slide['footer'];
    slideItemFooter.ContentSources.Clear;
    case slide.OverviewType of
      otIgnore: ;
      otSong: strPrefix := _('Singing') + ': ';
      otReading: strPrefix := _('Reading') + ': ';
      otText: strPrefix := _('Text') + ': ';
    end;
    slideItemFooter.ContentSources.Add(TSourceInfo.CreateAsString(strPrefix + RespaceOverviewName(edtOverviewName.Text, false)));

    slideItemContent := slide['content'];
    slideItemContent.ContentSources.Clear;
    aContentStrings := split(mmoText.Lines.Text, CNEWSLIDE);
    for i := 0 to Length(aContentStrings) -1 do begin
      slideItemContent.ContentSources.Add(SourceInfoFromMemoText(trim(aContentStrings[i])));
    end;
    Result := slide.AsJSon;
  finally
    slide.Free;
  end;

end;

procedure TfrmEditText.ImgViewPictoClick(Sender: TObject);
begin
  FPictoName := SelectPicto(FPictoName);
  ViewPNG(FPictoName.FileName, ImgViewPicto.Bitmap);
end;

procedure TfrmEditText.mmoTextKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^A then begin
    (Sender as TMemo).SelectAll;
    Key := #0;
  end;
end;

procedure TfrmEditText.SetSlideAsString(const Value: string);
var
  slide: TSlide;
  slideItemContent: TSlideItem;
  i: Integer;

  sourceInfo: TSourceInfo;
  template: TSlideTemplate;
begin
  FStartSlide := Value;
  slide := TSlide.Create(FStartSlide);
  try
    FSlideTemplateName := slide.SlideTemplateName;
    FSlideVariables.LoadFromString(slide.Variables.SaveToString);
    FOverviewType := slide.OverviewType;

    template := GetSlideTemplates.FindByName(FSlideTemplateName);
    if Assigned(template) then begin
      FSlideOptions.LoadFromString(template.VariableOptions.SaveToString);
    end;
    edtOverviewName.Text := slide.OverviewName;
    cbxShowInOverview.Checked := slide.ShowInOverview;
    cbxPartOfForm.Checked := slide.IsSubOverview;
    FreeAndNil(FPictoName);
    FPictoName := slide.PictoName.DeepCopy;
    ViewPNG(FPictoName.FileName, ImgViewPicto.Bitmap);
    cbxAutomaticSmallNumbers.Checked := slide.AutomaticSmallNumbers;
    IsBibleText := slide.OverviewType in [otReading, otText];
    mmoText.Lines.Clear;
    slideItemContent := slide['content'];

    sourceInfo := slideItemContent.ContentSources[0];
    mmoText.Lines.Text := SourceInfoToMemoText(sourceInfo);
    for i := 1 to slideItemContent.ContentSources.Count -1 do begin
      mmoText.Lines.Add(CNEWSLIDE + #13);
      sourceInfo := slideItemContent.ContentSources[i];
      mmoText.Lines.Text := mmoText.Lines.Text + SourceInfoToMemoText(sourceInfo);
    end;

  finally
    slide.Free;
  end;
end;

end.
