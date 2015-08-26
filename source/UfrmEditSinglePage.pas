unit UfrmEditSinglePage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, GR32_Image,
  USlide, USourceInfo, Vcl.ExtCtrls;

type
  TfrmEditSinglePage = class(TForm, ISlideEditForm)
    Label2: TLabel;
    Label3: TLabel;
    edtOverviewName: TEdit;
    ImgViewPicto: TImgView32;
    btnSelectPictoNone: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    PageControlContent: TPageControl;
    tsText: TTabSheet;
    tsMemo: TTabSheet;
    tsPicture: TTabSheet;
    ImgViewPicture: TImgView32;
    mmoContent: TMemo;
    edtContent: TEdit;
    Shape1: TShape;
    procedure ImgViewPictoClick(Sender: TObject);
    procedure btnSelectPictoNoneClick(Sender: TObject);
    procedure ImgViewPictureClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FStartSlide: string;
    FPictoName: TSourceInfo;
    FPictureName: TSourceInfo;
    FContentSubdir: string;
    { Private declarations }
  protected
    function GetSlideAsString: string;
    procedure SetSlideAsString(const Value: string);
    function Edit: boolean;

  public
    property SlideAsString: string read GetSlideAsString write SetSlideAsString;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  GnuGetText, UUtils, USettings, UUtilsStrings, UUtilsForms, USlideTemplate;

{ TfrmEditSinglePage }

procedure TfrmEditSinglePage.btnSelectPictoNoneClick(Sender: TObject);
begin
  FPictoName.FileName := '';
  ImgViewPicto.Bitmap.Clear;
end;

function TfrmEditSinglePage.Edit: boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TfrmEditSinglePage.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);

  FPictoName := TSourceInfo.Create;
  FPictureName := TSourceInfo.Create;
end;

procedure TfrmEditSinglePage.FormDestroy(Sender: TObject);
begin
  FPictureName.Free;
  FPictoName.Free;
end;

function TfrmEditSinglePage.GetSlideAsString: string;
var
  slide: TSlide;

  slideItemContent, slideItemFooter: TSlideItem;
begin
  slide := TSlide.Create(FStartSlide);
  try
    slide.SlideName := slide.SlideTemplateName + ' : ' + edtOverviewName.Text;
    slide.OverviewName := edtOverviewName.Text;
    slide.PictoName := FPictoName.DeepCopy;

    slideItemFooter := slide['footer'];
    if Assigned(slideItemFooter) then begin
      slideItemFooter.ContentSources.Clear;
      slideItemFooter.ContentSources.Add(TSourceInfo.CreateAsString(edtOverviewName.Text));
    end;

    slideItemContent := slide['content'];
    if Assigned(slideItemContent) then begin
      slideItemContent.ContentSources.Clear;
      case slideItemContent.ContentType of
        ctText: slideItemContent.ContentSources.Add(TSourceInfo.CreateAsString(edtContent.Text));
        ctTextMemo: slideItemContent.ContentSources.Add(TSourceInfo.CreateAsString(mmoContent.Lines.Text));
        ctPicture,
        ctPictureFit: slideItemContent.ContentSources.Add(FPictureName.DeepCopy);
      end;
    end;
    Result := slide.AsJSon;
  finally
    slide.Free;
  end;

end;

procedure TfrmEditSinglePage.ImgViewPictoClick(Sender: TObject);
begin
  FPictoName := SelectPicto(FPictoName);
  ViewPNG(FPictoName.FileName, ImgViewPicto.Bitmap);
end;

procedure TfrmEditSinglePage.ImgViewPictureClick(Sender: TObject);
begin
  FPictureName := SelectPicture(FPictureName, FContentSubdir);
  ViewPicture(FPictureName.FileName, ImgViewPicture.Bitmap);
end;

procedure TfrmEditSinglePage.SetSlideAsString(const Value: string);
var
  slide: TSlide;
  slideItemContent, slideItemFooter: TSlideItem;
  template: TSlideTemplate;

  source: TSourceInfo;
begin
  FStartSlide := Value;
  slide := TSlide.Create(FStartSlide);
  try
    slideItemFooter := slide['footer'];
    if Assigned(slideItemFooter) then begin
      if slideItemFooter.ContentSources.Count > 0 then begin
        source := slideItemFooter.ContentSources[0];
        edtOverviewName.Text := source.Text;
      end;
    end;

    FContentSubdir := '';
    template := GetSlideTemplates.FindByName(slide.SlideTemplateName);
    if Assigned(template) then begin
      FContentSubdir := template.SelectContentSubDir;
    end;

    FreeAndNil(FPictoName);
    FPictoName := slide.PictoName.DeepCopy;
    ViewPNG(FPictoName.FileName, ImgViewPicto.Bitmap);

    slideItemContent := slide['content'];
    if Assigned(slideItemContent) then begin
      if slideItemContent.ContentSources.Count > 0 then begin
        source := slideItemContent.ContentSources[0];
        case source.SourceType of
          sitUnknown: ;
          sitString: begin
            if slideItemContent.ContentType = ctText then begin
              edtContent.text := source.Text;
              PageControlContent.ActivePage := tsText;
            end else if slideItemContent.ContentType = ctTextMemo then begin
              mmoContent.Lines.Text := source.Text;
              PageControlContent.ActivePage := tsMemo;
            end;
          end;
          sitPPT: ;
          sitFileName: begin
            FreeAndNil(FPictureName);
            FPictureName := source.DeepCopy;
            ViewPicture(FPictureName.FileName, ImgViewPicture.Bitmap);
            PageControlContent.ActivePage := tsPicture;
          end;
        end;
      end;
      PageControlContent.Visible := slideItemContent.ContentType in [ctText, ctTextMemo, ctPicture, ctPictureFit];
    end;
  finally
    slide.Free;
  end;

end;

end.
