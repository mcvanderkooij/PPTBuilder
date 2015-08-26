unit UfrmEditExtSlide;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, GR32_Image, Vcl.AppEvnts, UfrmPPTView,
  USlide, USourcePPT, USourceInfo, Vcl.ExtCtrls;

type
  TfrmEditExtSlide = class(TForm, ISlideEditForm)
    btnOK: TButton;
    btnCancel: TButton;
    edtOverviewName: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbVerses: TListBox;
    btnVerseAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    ImgViewPicto: TImgView32;
    ApplicationEvents1: TApplicationEvents;
    btnSelectPictoNone: TButton;
    Shape1: TShape;
    procedure btnVerseAddClick(Sender: TObject);
    procedure ImgViewPictoClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure lbVersesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnDeleteClick(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure lbVersesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbVersesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbVersesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbVersesDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSelectPictoNoneClick(Sender: TObject);
  private
    FStartSlide: string;
    FPictoName: TSourceInfo;
    FStartingPoint: TPoint;
    FfrmPPTViewer: TfrmPPTViewer;
    { Private declarations }
  protected
    function GetSlideAsString: string;
    procedure SetSlideAsString(const Value: string);
    function Edit: boolean;

    procedure DoEdit;
  public
    property SlideAsString: string read GetSlideAsString write SetSlideAsString;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Math, PowerPoint_TLB,
  GnuGetText, UUtils, UUtilsForms, UUtilsStrings, USettings;

procedure TfrmEditExtSlide.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  btnEdit.Enabled := lbVerses.ItemIndex <> -1;
  btnDelete.Enabled := lbVerses.ItemIndex <> -1;
end;

procedure TfrmEditExtSlide.btnDeleteClick(Sender: TObject);
begin
  if lbVerses.ItemIndex <> -1 then
    lbVerses.Items.Delete(lbVerses.ItemIndex);
end;

procedure TfrmEditExtSlide.btnEditClick(Sender: TObject);
begin
  DoEdit;
end;

procedure TfrmEditExtSlide.btnSelectPictoNoneClick(Sender: TObject);
begin
  FPictoName.FileName := '';
  ImgViewPicto.Bitmap.Clear;
end;

procedure TfrmEditExtSlide.btnVerseAddClick(Sender: TObject);
var
  selection: TSourceInfo;
  selections: TSourceInfos;
  i: integer;
begin
  FfrmPPTViewer.SelectionMode := smSlide;
  if lbVerses.Items.Count > 0 then begin
    selection := SourceInfoFromString(lbVerses.Items[0]);
    try
      selection.SlideName := '';
      selection.ShapeName := '';
      FfrmPPTViewer.Selection := selection;
    finally
      selection.Free;
    end;
  end;
  FfrmPPTViewer.ShowDescription := false;
  FfrmPPTViewer.DoMultiSelection := true;
  if FfrmPPTViewer.ShowModal = mrOK then begin
    selections := FfrmPPTViewer.SelectionMulti;
    try
      for i := 0 to selections.Count -1 do begin
        selections[i].Description := selections[i].SlideName;
        lbVerses.Items.Add(SourceInfoToString(selections[i]));
      end;
    finally
      selections.Free;
    end;
  end;
end;

procedure TfrmEditExtSlide.DoEdit;
var
  selectionStart, selectionOK: TSourceInfo;
begin
  if lbVerses.ItemIndex <> -1 then begin
    selectionOK := nil;
    selectionStart := SourceInfoFromString(lbVerses.Items[lbVerses.ItemIndex]);
    try
      FfrmPPTViewer.SelectionMode := smSlide;
      FfrmPPTViewer.Selection := selectionStart;
      FfrmPPTViewer.ShowDescription := false;
      FfrmPPTViewer.DoMultiSelection := false;
      if FfrmPPTViewer.ShowModal = mrOK then begin
        selectionOK := FfrmPPTViewer.Selection;
        selectionOK.Description := FfrmPPTViewer.Description;
        lbVerses.Items[lbVerses.ItemIndex] := SourceInfoToString(selectionOK);
      end;
    finally
      selectionStart.Free;
      selectionOK.Free;
    end;
  end;
end;

function TfrmEditExtSlide.Edit: boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TfrmEditExtSlide.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
  FfrmPPTViewer := TfrmPPTViewer.Create(Application.MainForm);
end;

procedure TfrmEditExtSlide.FormDestroy(Sender: TObject);
begin
  FPictoName.Free;
  FfrmPPTViewer.Free;
end;

function TfrmEditExtSlide.GetSlideAsString: string;
var
  slide: TSlide;

  slideItemContent, slideItemFooter: TSlideItem;
  i: integer;
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
      if slideItemContent.ContentType = ctExtSlide then begin
        slideItemContent.ContentSources.Clear;
      end else begin
        // keep the first one
        while slideItemContent.ContentSources.Count > 1 do
          slideItemContent.ContentSources.Delete(1);
      end;
      for i := 0 to lbVerses.Items.Count -1 do begin
        slideItemContent.ContentSources.Add(SourceInfoFromString(lbVerses.Items[i]));
      end;
    end;
    Result := slide.AsJSon;
  finally
    slide.Free;
  end;
end;

procedure TfrmEditExtSlide.ImgViewPictoClick(Sender: TObject);
begin
  FPictoName := SelectPicto(FPictoName);
  ViewPNG(FPictoName.FileName, ImgViewPicto.Bitmap);
end;

procedure TfrmEditExtSlide.lbVersesDblClick(Sender: TObject);
begin
  DoEdit;
end;

procedure TfrmEditExtSlide.lbVersesDragDrop(Sender, Source: TObject; X, Y: Integer);
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

    if StartPosition <> DropPosition then begin
      strSlide := Items[StartPosition];
      oSlide := Items.Objects[StartPosition];

      Items.Delete(StartPosition);
      if DropPosition < 0 then
        items.AddObject(strSlide, oSlide)
      else
        items.InsertObject(DropPosition, strSlide, oSlide);
    end;
  end;
end;

procedure TfrmEditExtSlide.lbVersesDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source = lbVerses;
end;

procedure TfrmEditExtSlide.lbVersesDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  selection: TSourceInfo;
begin
  selection := SourceInfoFromString(lbVerses.Items[Index]);
  try
    lbVerses.Canvas.TextRect(Rect, Rect.Left, Rect.Top, selection.Description);
  finally
    selection.Free;
  end;
end;

procedure TfrmEditExtSlide.lbVersesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FStartingPoint.X := X;
  FStartingPoint.Y := Y;
end;

procedure TfrmEditExtSlide.SetSlideAsString(const Value: string);
var
  slide: TSlide;
  slideItemContent: TSlideItem;
  iStart, i, iCount: integer;
  sourceInfo: TSourceInfo;
begin
  FStartSlide := Value;
  slide := TSlide.Create(FStartSlide);
  try
    edtOverviewName.Text := slide.OverviewName;
    FreeAndNil(FPictoName);
    FPictoName := slide.PictoName.DeepCopy;
    ViewPNG(FPictoName.FileName, ImgViewPicto.Bitmap);
    lbVerses.Items.Clear;

    slideItemContent := slide['content'];
    iCount := slideItemContent.ContentSources.Count;
    iStart := 1;
    if slideItemContent.ContentType = ctExtSlide then begin
      iStart := 0;
    end;
    for i := iStart to iCount -1 do begin
      sourceInfo := slideItemContent.ContentSources[i];
      if (sourceInfo.FileName <> '') then begin
        lbVerses.Items.Add(SourceInfoToString(slideItemContent.ContentSources[i]));
      end;
    end;
  finally
    slide.Free;
  end;
end;

end.
