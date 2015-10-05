unit UfrmEditSong;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, GR32_Image, Vcl.AppEvnts, UfrmPPTView,
  USlide, USourceInfo, Vcl.ExtCtrls;

type
  TfrmEditSong = class(TForm, ISlideEditForm)
    btnOK: TButton;
    btnCancel: TButton;
    edtOverviewName: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbVerses: TListBox;
    btnVerseAdd: TButton;
    btnVerseEdit: TButton;
    btnVerseDelete: TButton;
    ImgViewPicto: TImgView32;
    ApplicationEvents1: TApplicationEvents;
    Shape1: TShape;
    btnVerseCopy: TButton;
    btnInternet: TButton;
    cbxShowInOverview: TCheckBox;
    cbxPartOfForm: TCheckBox;
    btnSelectPictoNone: TButton;
    procedure btnVerseAddClick(Sender: TObject);
    procedure ImgViewPictoClick(Sender: TObject);
    procedure btnVerseEditClick(Sender: TObject);
    procedure lbVersesDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnVerseDeleteClick(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure lbVersesDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbVersesDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbVersesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lbVersesDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVerseCopyClick(Sender: TObject);
    procedure btnInternetClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSelectPictoNoneClick(Sender: TObject);
  private
    FStartSlide: string;
    FPictoName: TSourceInfo;
    FStartingPoint: TPoint;
    FfrmPPTViewer: TfrmPPTViewer;
    FSlideTemplateName: string;
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
  GnuGetText, UUtils, UUtilsForms, UUtilsStrings, USettings, UfrmPictureSelector,
  UfrmPictureDescription, USlideTemplate, USourcePPT, UfrmBrowseFTP;

procedure TfrmEditSong.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  btnVerseCopy.Enabled := lbVerses.ItemIndex <> -1;
  btnVerseEdit.Enabled := lbVerses.ItemIndex <> -1;
  btnVerseDelete.Enabled := lbVerses.ItemIndex <> -1;
end;

procedure TfrmEditSong.btnVerseDeleteClick(Sender: TObject);
begin
  if lbVerses.ItemIndex <> -1 then
    lbVerses.Items.Delete(lbVerses.ItemIndex);
end;

procedure TfrmEditSong.btnVerseEditClick(Sender: TObject);
begin
  DoEdit;
end;

procedure TfrmEditSong.btnInternetClick(Sender: TObject);
begin
  OpenInternetSource('');
end;

procedure TfrmEditSong.btnSelectPictoNoneClick(Sender: TObject);
begin
  FPictoName.FileName := '';
  ImgViewPicto.Bitmap.Clear;
end;

procedure TfrmEditSong.btnVerseAddClick(Sender: TObject);
var
  selection: TSourceInfo;
  source: TSourceInfo;
  selections: TSourceInfos;
  i,j: integer;
  frmPictureDescription: TfrmPictureDescription;
  slide: PowerPoint_TLB._Slide;
  shape: PowerPoint_TLB.Shape;
  strTempFile: string;
  template: TSlideTemplate;

  strMemoText, strFooterText, strRemark: string;
begin
  template := GetSlideTemplates.FindByName(FSlideTemplateName);
  if Assigned(template) then begin
    if (epSongIsPPT in template.EditPossibilities) then begin
      FfrmPPTViewer.SelectionMode := smPicture;
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
      //FfrmPPTViewer.Description := 'vers ';
      FfrmPPTViewer.ShowDescription := false;
      FfrmPPTViewer.ShowRemark := false;
      FfrmPPTViewer.DoMultiSelection := true;
      if FfrmPPTViewer.ShowModal = mrOK then begin
        selections := FfrmPPTViewer.SelectionMulti;
        try
          frmPictureDescription := TfrmPictureDescription.Create(Self);
          try
            for i := 0 to selections.Count -1 do begin
              shape := GetCachedPPTs.GetShape(selections[i]);
              slide := GetCachedPPTs.GetSlide(selections[i]);
              if Assigned(shape) and Assigned(slide) then begin
                strTempFile := GetTempDir + 'exp.png';
                slide.Export(strTempFile, 'png', 0, 0);
                //shape.Export(strTempFile, ppShapeFormatPNG, round(shape.Width), round(shape.Height), ppScaleToFit);     //ppScaleToFit
                ViewPNG(strTempFile, frmPictureDescription.ImgView321.Bitmap);
                frmPictureDescription.Description := _('verse') + ' ';
                frmPictureDescription.Remark := '';

                frmPictureDescription.OtherDescriptions.Clear;
                for j := 0 to lbVerses.Items.Count -1 do begin
                  source := SourceInfoFromString(lbVerses.Items[j]);
                  try
                    frmPictureDescription.OtherDescriptions.Add(source.Description);
                  finally
                    source.Free;
                  end;
                end;

                if frmPictureDescription.ShowModal = mrOK then begin
                  selections[i].Description := frmPictureDescription.Description;
                  selections[i].Remark := frmPictureDescription.Remark;
                  lbVerses.Items.Add(SourceInfoToString(selections[i], false));
                end;
              end;
            end;
          finally
            frmPictureDescription.Free;
          end;
        finally
          selections.Free;
        end;
      end;
    end else if (epSongIsMemo in template.EditPossibilities) then begin
      strFooterText := _('verse') + ' ';
      strRemark := '';
      strMemoText := '';
      if ShowMemoDlg(_('Singing'), strMemoText, strFooterText, strRemark, _('Enter text'), _('Sub text'), _('Remark / Antiphon')) then begin
        selection := TSourceInfo.CreateAsString(strMemoText);
        try
          selection.Description := strFooterText;
          selection.Remark := strRemark;
          lbVerses.Items.Add(SourceInfoToString(selection, false));
        finally
          selection.Free;
        end;
      end;
    end else if (epSongIsPicture in template.EditPossibilities) then begin
      strFooterText := _('verse') + ' ';
      selection := TSourceInfo.CreateAsFileName('');
      try
        if SelectPictureWithFooter(selection, strFooterText, strRemark, template.SelectContentSubDir, _('Sub text'), _('Remark / Antiphon')) then begin
          selection.Description := strFooterText;
          selection.Remark := strRemark;
          lbVerses.Items.Add(SourceInfoToString(selection, false));
        end;
      finally
        selection.Free;
      end;
    end else begin
      Exit;
    end;
  end;
  SortSourceInfoStrings(lbVerses.Items);
end;

procedure TfrmEditSong.btnVerseCopyClick(Sender: TObject);
begin
  if lbVerses.ItemIndex <> -1 then begin
    lbVerses.Items.Add(lbVerses.Items[lbVerses.ItemIndex]);
  end;
end;

procedure TfrmEditSong.DoEdit;
var
  selectionStart, selectionOK: TSourceInfo;

  template: TSlideTemplate;
  strMemoText, strFooterText, strRemarkText: string;
begin
  if lbVerses.ItemIndex <> -1 then begin
    template := GetSlideTemplates.FindByName(FSlideTemplateName);
    if Assigned(template) then begin
      selectionOK := nil;
      selectionStart := SourceInfoFromString(lbVerses.Items[lbVerses.ItemIndex]);
      try
        if (epSongIsPPT in template.EditPossibilities) then begin
          FfrmPPTViewer.SelectionMode := smPicture;
          FfrmPPTViewer.Description := selectionStart.Description;
          FfrmPPTViewer.Remark := selectionStart.Remark;
          FfrmPPTViewer.Selection := selectionStart;
          FfrmPPTViewer.ShowDescription := true;
          FfrmPPTViewer.ShowRemark := true;
          FfrmPPTViewer.DoMultiSelection := false;
          if FfrmPPTViewer.ShowModal <> mrOK then
            Exit;
          selectionOK := FfrmPPTViewer.Selection;
          selectionOK.Description := FfrmPPTViewer.Description;
          selectionOK.Remark := FfrmPPTViewer.Remark;
        end else if (epSongIsMemo in template.EditPossibilities) then begin
          strFooterText := selectionStart.Description;
          strRemarkText := selectionStart.Remark;
          strMemoText := selectionStart.Text;
          if ShowMemoDlg(_('Singing'), strMemoText, strFooterText, strRemarkText, _('Enter text'), _('Sub text'), _('Remark / Antiphon')) then begin
            selectionOK := TSourceInfo.CreateAsString(strMemoText);
            selectionOK.Description := strFooterText;
            selectionOK.Remark := strRemarkText;
          end;
        end else if (epSongIsPicture in template.EditPossibilities) then begin
          strFooterText := selectionStart.Description;
          strRemarkText := selectionStart.Remark;
          if SelectPictureWithFooter(selectionStart, strFooterText, strRemarkText, template.SelectContentSubDir, _('Sub text'), _('Remark / Antiphon')) then begin
            selectionOK := selectionStart;
            selectionStart := nil;
            selectionOK.Description := strFooterText;
            selectionOK.Remark := strRemarkText;
            selectionOK.SlideName := '';
            selectionOK.ShapeName := '';
          end;
        end else begin
          Exit;
        end;
        lbVerses.Items[lbVerses.ItemIndex] := SourceInfoToString(selectionOK, false);
      finally
        selectionStart.Free;
        selectionOK.Free;
      end;
    end;
  end;
end;

function TfrmEditSong.Edit: boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TfrmEditSong.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
  FfrmPPTViewer := TfrmPPTViewer.Create(Application.MainForm);
end;

procedure TfrmEditSong.FormDestroy(Sender: TObject);
begin
  FPictoName.Free;
  FfrmPPTViewer.Free;
end;

procedure TfrmEditSong.FormShow(Sender: TObject);
var
  template: TSlideTemplate;
begin
  template := GetSlideTemplates.FindByName(FSlideTemplateName);
  btnInternet.Visible := Assigned(template) and (epSongIsMemo in template.EditPossibilities);
end;

function TfrmEditSong.GetSlideAsString: string;
var
  slide: TSlide;
  slideItemContent, slideItemFooter, slideItemFooterLeft, slideItemRemark: TSlideItem;
  i: integer;
  selection: TSourceInfo;
  template: TSlideTemplate;
begin
  slide := TSlide.Create(FStartSlide);
  try
    template := GetSlideTemplates.FindByName(FSlideTemplateName);
    Assert(Assigned(template));

    slide.SlideName := slide.SlideTemplateName + ' : ' + edtOverviewName.Text;
    slide.OverviewName := edtOverviewName.Text;
    slide.ShowInOverview := cbxShowInOverview.Checked;
    slide.PictoName := FPictoName.DeepCopy;
    slide.IsSubOverview := cbxPartOfForm.Checked;

    slideItemFooter := slide['footer'];
    slideItemFooterLeft := slide['footer-left'];
    slideItemContent := slide['content'];
    slideItemRemark := slide['remark'];

    slideItemFooter.ContentSources.Clear;
    if Assigned(slideItemFooterLeft) then begin
      slideItemFooterLeft.ContentSources.Clear;
    end;
    slideItemContent.ContentSources.Clear;
    if Assigned(slideItemRemark) then begin
      slideItemRemark.ContentSources.Clear;
    end;

    for i := 0 to lbVerses.Items.Count -1 do begin
      selection := SourceInfoFromString(lbVerses.Items[i]);
      slideItemFooter.ContentSources.Add(TSourceInfo.CreateAsString(RespaceOverviewName(edtOverviewName.Text, false)));
      if Assigned(slideItemFooterLeft) then begin
        slideItemFooterLeft.ContentSources.Add(TSourceInfo.CreateAsString(selection.Description));
      end;
      if Assigned(slideItemRemark) then begin
        slideItemRemark.ContentSources.Add(TSourceInfo.CreateAsString(selection.Remark));
      end;
      slideItemContent.ContentSources.Add(selection);
    end;

    Result := slide.AsJSon;
  finally
    slide.Free;
  end;
end;

procedure TfrmEditSong.ImgViewPictoClick(Sender: TObject);
begin
  FPictoName := SelectPicto(FPictoName);
  ViewPNG(FPictoName.FileName, ImgViewPicto.Bitmap);
end;

procedure TfrmEditSong.lbVersesDblClick(Sender: TObject);
begin
  DoEdit;
end;

procedure TfrmEditSong.lbVersesDragDrop(Sender, Source: TObject; X, Y: Integer);
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

procedure TfrmEditSong.lbVersesDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source = lbVerses;
end;

procedure TfrmEditSong.lbVersesDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  selection: TSourceInfo;
  strText: string;
begin
  selection := SourceInfoFromString(lbVerses.Items[Index]);
  try
    strText := selection.Description;
    if selection.Remark <> '' then
      strText := strText + ' (' + selection.Remark + ')';
    lbVerses.Canvas.TextRect(Rect, Rect.Left, Rect.Top, strText);
  finally
    selection.Free;
  end;
end;

procedure TfrmEditSong.lbVersesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FStartingPoint.X := X;
  FStartingPoint.Y := Y;
end;

procedure TfrmEditSong.SetSlideAsString(const Value: string);
var
  slide: TSlide;
  slideItemContent: TSlideItem;
  i, iCount: integer;

  sourceInfo: TSourceInfo;
begin
  FStartSlide := Value;
  slide := TSlide.Create(FStartSlide);
  try
    FSlideTemplateName := slide.SlideTemplateName;
    edtOverviewName.Text := slide.OverviewName;
    FreeAndNil(FPictoName);
    FPictoName := slide.PictoName.DeepCopy;
    ViewPNG(FPictoName.FileName, ImgViewPicto.Bitmap);
    lbVerses.Items.Clear;

    cbxShowInOverview.Checked := slide.ShowInOverview;
    cbxPartOfForm.Checked := slide.IsSubOverview;

    slideItemContent := slide['content'];

    iCount := slideItemContent.ContentSources.Count;
    for i := 0 to iCount -1 do begin
      if slideItemContent.ContentSources[i].SourceType <> sitUnknown then begin
        sourceInfo := slideItemContent.ContentSources[i];
        if ((sourceInfo.SourceType in [sitPPT, sitFileName]) and (sourceInfo.FileName <> ''))
           or ((sourceInfo.SourceType in [sitString]) and (sourceInfo.Text <> '')) then begin
          lbVerses.Items.Add(SourceInfoToString(sourceInfo, false));
        end;
      end;
    end;
  finally
    slide.Free;
  end;
end;

end.
