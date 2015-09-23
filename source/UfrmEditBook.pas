unit UfrmEditBook;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, USourceInfo, Vcl.AppEvnts, Vcl.StdCtrls,
  GR32_Image, Vcl.ExtCtrls, USlide, UfrmBrowseBook;

type
  TfrmEditBook = class(TForm, ISlideEditForm)
    Label2: TLabel;
    Label3: TLabel;
    Shape1: TShape;
    edtOverviewName: TEdit;
    ImgViewPicto: TImgView32;
    cbxPartOfForm: TCheckBox;
    cbxShowInOverview: TCheckBox;
    btnSelectPictoNone: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    lbVerses: TListBox;
    btnVerseAdd: TButton;
    btnVerseEdit: TButton;
    btnVerseDelete: TButton;
    btnVerseCopy: TButton;
    ApplicationEvents1: TApplicationEvents;
    procedure btnSelectPictoNoneClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ImgViewPictoClick(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure btnVerseDeleteClick(Sender: TObject);
    procedure btnVerseEditClick(Sender: TObject);
    procedure btnVerseAddClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbVersesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbVersesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbVersesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbVersesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbVersesDblClick(Sender: TObject);
  private
    FBook: string;
    FStartSlide: string;
    FStartingPoint: TPoint;
    FPictoName: TSourceInfo;
    FfrmBrowseBook: TfrmBrowseBook;
    { Private declarations }
  protected
    function Edit: boolean;
    function GetSlideAsString: string;
    procedure SetSlideAsString(const Value: string);

    procedure DoEdit;
  public
    { Public declarations }
    property SlideAsString: string read GetSlideAsString write SetSlideAsString;

    constructor Create(AOwner: TComponent; strBook: string); virtual;
  end;

implementation

{$R *.dfm}

uses
  GNUGetText,
  UUtilsForms, UUtils;

procedure TfrmEditBook.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  btnVerseEdit.Enabled := lbVerses.ItemIndex <> -1;
  btnVerseDelete.Enabled := lbVerses.ItemIndex <> -1;
end;

procedure TfrmEditBook.btnSelectPictoNoneClick(Sender: TObject);
begin
  FPictoName.FileName := '';
  ImgViewPicto.Bitmap.Clear;
end;

procedure TfrmEditBook.btnVerseAddClick(Sender: TObject);
var
  selection: TSourceInfo;
  selections: TSourceInfos;
  i: integer;
begin
  if lbVerses.Items.Count > 0 then begin
    selection := SourceInfoFromString(lbVerses.Items[0]);
    try
      selection.SlideName := '';
      selection.ShapeName := '';
      FfrmBrowseBook.Selection := selection;
    finally
      selection.Free;
    end;
  end else begin
    FfrmBrowseBook.OpenBook(FBook);
  end;
  FfrmBrowseBook.DoMultiSelection := true;
  if FfrmBrowseBook.ShowModal = mrOK then begin
    selections := FfrmBrowseBook.SelectionMulti;
    try
      for i := 0 to selections.Count -1 do begin
        if edtOverviewName.Text = '' then begin
          edtOverviewName.Text := ChangeFileExt(selections[i].SlideName, '');
        end;

        selections[i].Description := ChangeFileExt(selections[i].SlideName, '') + ' : ' + selections[i].ShapeName;
        lbVerses.Items.Add(SourceInfoToString(selections[i]));
      end;
    finally
      selections.Free;
    end;
  end;

end;

procedure TfrmEditBook.btnVerseDeleteClick(Sender: TObject);
begin
  if lbVerses.ItemIndex <> -1 then
    lbVerses.Items.Delete(lbVerses.ItemIndex);
end;

procedure TfrmEditBook.btnVerseEditClick(Sender: TObject);
begin
  DoEdit;
end;

constructor TfrmEditBook.Create(AOwner: TComponent; strBook: string);
begin
  inherited Create(Aowner);
  FBook := strBook;

  Caption := _('Edit') + ' ' + strBook;
end;

procedure TfrmEditBook.DoEdit;
var
  selectionStart, selectionOK: TSourceInfo;
begin
  if lbVerses.ItemIndex <> -1 then begin
    selectionOK := nil;
    selectionStart := SourceInfoFromString(lbVerses.Items[lbVerses.ItemIndex]);
    try
      FfrmBrowseBook.Selection := selectionStart;
      FfrmBrowseBook.DoMultiSelection := false;
      if FfrmBrowseBook.ShowModal = mrOK then begin
        selectionOK := FfrmBrowseBook.Selection;
        lbVerses.Items[lbVerses.ItemIndex] := SourceInfoToString(selectionOK);
      end;
    finally
      selectionStart.Free;
      selectionOK.Free;
    end;
  end;
end;

function TfrmEditBook.Edit: boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TfrmEditBook.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
  FfrmBrowseBook := TfrmBrowseBook.Create(Application.MainForm);
end;

procedure TfrmEditBook.FormDestroy(Sender: TObject);
begin
  FPictoName.Free;
  FfrmBrowseBook.Free;
end;

function TfrmEditBook.GetSlideAsString: string;
var
  slide: TSlide;

  slideItemContent, slideItemFooter, slideItemFooterLeft: TSlideItem;
  selection: TSourceInfo;
  i: integer;
  strFooterLeftPrefix: string;
begin
  slide := TSlide.Create(FStartSlide);
  try
    slide.SlideName := slide.SlideTemplateName + ' : ' + edtOverviewName.Text;
    slide.OverviewName := edtOverviewName.Text;
    slide.ShowInOverview := cbxShowInOverview.Checked;
    slide.IsSubOverview := cbxPartOfForm.Checked;
    slide.PictoName := FPictoName.DeepCopy;

    slideItemFooter := slide['footer'];
    if Assigned(slideItemFooter) then begin
      slideItemFooter.ContentSources.Clear;
      slideItemFooter.ContentSources.Add(TSourceInfo.CreateAsString(edtOverviewName.Text));
    end;

    slideItemFooterLeft := slide['footer-left'];
    if Assigned(slideItemFooterLeft) then begin
      slideItemFooterLeft.ContentSources.Clear;
    end;

    slideItemContent := slide['content'];
    if Assigned(slideItemContent) then begin
      slideItemContent.ContentSources.Clear;

      strFooterLeftPrefix := slide.Variables['FooterLeftPrefix'];

      for i := 0 to lbVerses.Items.Count -1 do begin
        selection := SourceInfoFromString(lbVerses.Items[i]);

        if Assigned(slideItemFooterLeft) then begin
          slideItemFooterLeft.ContentSources.Add(TSourceInfo.CreateAsString(strFooterLeftPrefix + ' ' + selection.ShapeName));
        end;

        slideItemContent.ContentSources.Add(SourceInfoFromString(lbVerses.Items[i]));
      end;
    end;
    Result := slide.AsJSon;
  finally
    slide.Free;
  end;
end;

procedure TfrmEditBook.ImgViewPictoClick(Sender: TObject);
begin
  FPictoName := SelectPicto(FPictoName);
  ViewPNG(FPictoName.FileName, ImgViewPicto.Bitmap);
end;

procedure TfrmEditBook.lbVersesDblClick(Sender: TObject);
begin
  DoEdit;
end;

procedure TfrmEditBook.lbVersesDragDrop(Sender, Source: TObject; X, Y: Integer);
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

procedure TfrmEditBook.lbVersesDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source = lbVerses;
end;

procedure TfrmEditBook.lbVersesDrawItem(Control: TWinControl; Index: Integer;
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

procedure TfrmEditBook.lbVersesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FStartingPoint.X := X;
  FStartingPoint.Y := Y;
end;

procedure TfrmEditBook.SetSlideAsString(const Value: string);
var
  slide: TSlide;
  slideItemContent: TSlideItem;
  i, iCount: integer;
  sourceInfo: TSourceInfo;
begin
  FStartSlide := Value;
  slide := TSlide.Create(FStartSlide);
  try
    edtOverviewName.Text := slide.OverviewName;
    cbxShowInOverview.Checked := slide.ShowInOverview;
    cbxPartOfForm.Checked := slide.IsSubOverview;
    FreeAndNil(FPictoName);
    FPictoName := slide.PictoName.DeepCopy;
    ViewPNG(FPictoName.FileName, ImgViewPicto.Bitmap);
    lbVerses.Items.Clear;

    slideItemContent := slide['content'];
    iCount := slideItemContent.ContentSources.Count;
    for i := 0 to iCount -1 do begin
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
