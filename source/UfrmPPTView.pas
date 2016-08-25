unit UfrmPPTView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, ActiveX, PowerPoint_TLB, Office_TLB,
  GR32_Image, GR32, pngImage, jpeg, USourceInfo, USourcePPT, Vcl.AppEvnts;

type
  TSelectionMode = (smSlide, smPicture);

  TfrmPPTViewer = class(TForm)
    OpenDialog1: TOpenDialog;
    btnOpenComputer: TButton;
    lblPPTFile: TLabel;
    tvPPT: TTreeView;
    ImgView321: TImgView32;
    btnCancel: TButton;
    btnSelect: TButton;
    ApplicationEvents1: TApplicationEvents;
    lblDescription: TLabel;
    edtDescription: TEdit;
    btnOpenInternet: TButton;
    lblRemark: TLabel;
    edtRemark: TEdit;
    procedure btnOpenComputerClick(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure btnOpenInternetClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FSelectionMode: TSelectionMode;
    FSelectionValid: boolean;
    FSelection: TSourceInfo;
    FAutoOpen: string;
    procedure ViewJPG(strFileName: string; bmp32: TBitmap32);
    procedure ViewPicture(shape: PowerPoint_TLB.Shape; bmp32: TBitmap32);
    procedure ViewSlide(slide: PowerPoint_TLB._Slide; bmp32: TBitmap32);
    function GetSelectionMulti: TSourceInfos;
    procedure SetSelectionMulti(const Value: TSourceInfos);
    procedure ClearSelection;
    procedure ClearTree;
    function GetDoMultiSelection: boolean;
    procedure SetDoMultiSelection(const Value: boolean);
    procedure SetSelectionMode(const Value: TSelectionMode);
    procedure SetShowDescription(const Value: boolean);
    function GetShowDescription: boolean;
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    procedure SetSelection(const Value: TSourceInfo);
    procedure ValidateSelection;
    function GetSelection: TSourceInfo;
    function GetRemark: string;
    function GetShowRemark: boolean;
    procedure SetRemark(const Value: string);
    procedure SetShowRemark(const Value: boolean);
  public
    procedure OpenFile(strFileName: string);

    property DoMultiSelection: boolean read GetDoMultiSelection write SetDoMultiSelection;
    property SelectionMode: TSelectionMode read FSelectionMode write SetSelectionMode;
    property SelectionValid: boolean read FSelectionValid;
    property Selection: TSourceInfo read GetSelection write SetSelection;
    property SelectionMulti: TSourceInfos read GetSelectionMulti write SetSelectionMulti;
    property Description: string read GetDescription write SetDescription;
    property ShowDescription: boolean read GetShowDescription write SetShowDescription;
    property Remark: string read GetRemark write SetRemark;
    property ShowRemark: boolean read GetShowRemark write SetShowRemark;
    property AutoOpen: string read FAutoOpen write FAutoOpen;
    { Public declarations }
  end;

implementation

uses
  ComObj, GR32_Misc2, GnuGetText, UUtils, UfrmBrowseFTP;

{$R *.dfm}

procedure TfrmPPTViewer.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  btnSelect.Enabled := SelectionValid;
end;

procedure TfrmPPTViewer.btnOpenInternetClick(Sender: TObject);
var
  strFileName: string;
begin
  strFileName := '';
  if SelectFTPFile(strFileName, '.ppt|.pptx', AutoOpen) then begin
    OpenFile(CopyExternalFileToLocal(strFileName));
  end;
end;

procedure TfrmPPTViewer.btnOpenComputerClick(Sender: TObject);
begin
  ClearTree;
  if OpenDialog1.Execute then begin
    OpenFile(OpenDialog1.FileName);
  end;
end;

procedure TfrmPPTViewer.ClearSelection;
begin
  ImgView321.Bitmap.Clear;
  FreeAndNil(FSelection);
  FSelection := TSourceInfo.CreateAsPPT('' ,'', '');
  FSelectionValid := false;
end;

procedure TfrmPPTViewer.ClearTree;
begin
  tvPPT.Items.Clear;
end;

procedure TfrmPPTViewer.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
  SelectionMode := smPicture;
  //SelectionMode := smSlide;
  FSelection := nil;
  ClearSelection;
  lblPPTFile.Caption := '';
end;

procedure TfrmPPTViewer.FormDestroy(Sender: TObject);
begin
  FSelection.Free;
end;

function TfrmPPTViewer.GetDescription: string;
begin
  Result := edtDescription.Text;
end;

function TfrmPPTViewer.GetDoMultiSelection: boolean;
begin
  Result := tvPPT.MultiSelect;
end;

function TfrmPPTViewer.GetRemark: string;
begin
  Result := edtRemark.Text;
end;

function TfrmPPTViewer.GetSelection: TSourceInfo;
begin
  Result := FSelection.DeepCopy;
end;

function TfrmPPTViewer.GetSelectionMulti: TSourceInfos;
var
  node: TTreeNode;
  iShape: integer;
  slide: PowerPoint_TLB._Slide;
  source: TSourceInfo;
begin
  Result := TSourceInfos.Create;
  node := tvPPT.Items.GetFirstNode;
  while Assigned(node) do begin
    if node.Selected then begin
      source := TSourceInfo.Create;
      Result.Add(source);
      source.SourceType := sitPPT;
      source.FileName := FSelection.FileName;
      if (node.Level = 1) then begin
        source.SlideName := node.Parent.Text;
        source.ShapeName := node.Text;
      end else if (tvPPT.Selected.Level = 0) then begin
        source.SlideName := node.Text;
        source.ShapeName := '';

        slide := GetCachedPPTs.GetSlide(source);
        if Assigned(slide) then begin
          if FSelectionMode = smPicture then begin
            for iShape := 1 to slide.Shapes.Count do begin
              if slide.Shapes.Item(iShape).type_ = msoPicture then begin
                source.ShapeName := slide.Shapes.Item(iShape).Name;
                break;
              end;
            end;
          end;
        end;
      end;
    end;
    node := node.GetNext;
  end;
end;

function TfrmPPTViewer.GetShowDescription: boolean;
begin
  Result := edtDescription.Visible;
end;

function TfrmPPTViewer.GetShowRemark: boolean;
begin
  Result := edtRemark.Visible;
end;

procedure TfrmPPTViewer.OpenFile(strFileName: string);
var
  cachedPPT: TCachedPPT;
  i,j: integer;
  slideNode: TTreeNode;
  tree: TStringTree;
begin
  ClearSelection;
  ClearTree;
  cachedPPT := GetCachedPPTs.Get(strFileName);
  if Assigned(cachedPPT) then begin
    FSelection.FileName := strFileName;

    tree := cachedPPT.SlidesAndShapes;
    try
      for i := 0 to tree.Count -1 do begin
        slideNode := tvPPT.Items.AddChild(nil, tree.Item[i].Data);
        if (FSelectionMode = smPicture) and (tree.Item[i].Count > 1) then begin
          for j := 0 to tree.Item[i].Count -1 do begin
            tvPPT.Items.AddChild(slideNode, tree.Item[i].Item[j].Data );
          end;
        end;
      end;
    finally
      FreeAndNil(tree);
    end;
  end;
  lblPPTFile.Caption := FSelection.FileName;
end;

procedure TfrmPPTViewer.SetDescription(const Value: string);
begin
  edtDescription.Text := Value;
end;

procedure TfrmPPTViewer.SetDoMultiSelection(const Value: boolean);
begin
  tvPPT.MultiSelect := Value;
end;

procedure TfrmPPTViewer.SetRemark(const Value: string);
begin
  edtRemark.Text := Value;
end;

procedure TfrmPPTViewer.SetSelection(const Value: TSourceInfo);
var
  node: TTreeNode;
begin
  if Value.FileName = '' then Exit;
  OpenFile(Value.FileName);
  if tvPPT.Items.Count = 0 then Exit;
  if Value.SlideName <> '' then begin
    node := tvPPT.Items.GetFirstNode;
    while Assigned(node) do begin
      if (node.Level = 0) and (node.Text = Value.SlideName) then begin
        node.MakeVisible;
        tvPPT.Selected := node;

        if Value.ShapeName <> '' then begin
          node := node.getFirstChild;
          while Assigned(node) do begin
            if (node.Level = 1) and (node.Text = Value.ShapeName) then begin
              node.MakeVisible;
              tvPPT.Selected := node;
              node := nil;
            end else begin
              node := node.getNextSibling;
            end;
          end;
        end;

        node := nil;
      end else begin
        node := node.getNextSibling;
      end;
    end;
  end;
  ValidateSelection;
end;

procedure TfrmPPTViewer.SetSelectionMode(const Value: TSelectionMode);
begin
  FSelectionMode := Value;
  case FSelectionMode of
    smSlide: Caption := _('Select Slide');
    smPicture: Caption := _('Select Picture from Slide');
  end;
end;

procedure TfrmPPTViewer.SetSelectionMulti(const Value: TSourceInfos);
begin
  if Value.Count > 0 then begin
    if Value[0].FileName <> FSelection.FileName then begin
      OpenFile(Value[0].FileName);
    end;
  end;
end;

procedure TfrmPPTViewer.SetShowDescription(const Value: boolean);
begin
  lblDescription.Visible := Value;
  edtDescription.Visible := Value;
end;

procedure TfrmPPTViewer.SetShowRemark(const Value: boolean);
begin
  lblRemark.Visible := Value;
  edtRemark.Visible := Value;
end;

procedure TfrmPPTViewer.TreeView1DblClick(Sender: TObject);
begin
  ValidateSelection;
end;

procedure TfrmPPTViewer.ValidateSelection;
var
  iShape: integer;
  shape: PowerPoint_TLB.Shape;
  slide: PowerPoint_TLB._Slide;
begin
  FSelectionValid := false;
  if Assigned(tvPPT.Selected) then begin
    if (tvPPT.Selected.Level = 1) then begin
      FSelection.SlideName := tvPPT.Selected.Parent.Text;
      FSelection.ShapeName := tvPPT.Selected.Text;

      shape := GetCachedPPTs.GetShape(FSelection);
      if Assigned(shape) then begin
        if shape.type_ = msoPicture then begin
          ViewPicture(shape, ImgView321.Bitmap);
          FSelectionValid := FSelectionMode = smPicture;
        end;
      end;

    end else if (tvPPT.Selected.Level = 0) then begin
      FSelection.SlideName := tvPPT.Selected.Text;
      FSelection.ShapeName := '';

      slide := GetCachedPPTs.GetSlide(FSelection);
      if Assigned(slide) then begin
        if FSelectionMode = smPicture then begin
          for iShape := 1 to slide.Shapes.Count do begin
            if slide.Shapes.Item(iShape).type_ = msoPicture then begin
              ViewSlide(slide, ImgView321.Bitmap);
              //ViewPicture(slide.Shapes.Item(iShape), ImgView321.Bitmap);
              FSelection.ShapeName := slide.Shapes.Item(iShape).Name;
              FSelectionValid := true;
              break;
            end;
          end;
          if FSelection.ShapeName = '' then begin
            ImgView321.Bitmap.Clear;
          end;
        end else begin
          ViewSlide(slide, ImgView321.Bitmap);
          FSelectionValid := FSelectionMode = smSlide;
        end;
      end;
    end;
  end;
end;

procedure TfrmPPTViewer.ViewJPG(strFileName: string; bmp32: TBitmap32);
var
  JpgImage: TJpegImage;
begin
  bmp32.Clear(clBlack32);
  if FileExists(strFileName) then begin
    JpgImage := TJpegImage.Create;
    try
      JpgImage.LoadFromFile(strFileName);
      bmp32.SetSizeFrom(JpgImage);
      bmp32.Clear(clBlack32);
      bmp32.Canvas.Draw(0, 0, JpgImage);
    finally
      JpgImage.Free;
    end;
  end;
end;

procedure TfrmPPTViewer.ViewPicture(shape: PowerPoint_TLB.Shape;
  bmp32: TBitmap32);
var
  strTempFile: string;
begin
  strTempFile := GetTempDir + 'shape-5A257AC3-73C8-472E-A424-85D08C0A43FD.png';
  shape.Export(strTempFile, ppShapeFormatPNG, round(shape.Width), round(shape.Height), ppScaleToFit);     //ppScaleToFit
  ViewPNG(strTempFile, bmp32);
//  strTempFile := 'd:\temp\exp.jpg';
//  shape.Export(strTempFile, ppShapeFormatJPG, round(shape.Width), round(shape.Height), ppScaleToFit);
//  ViewJPG(strTempFile, bmp32);
end;

procedure TfrmPPTViewer.ViewSlide(slide: PowerPoint_TLB._Slide;
  bmp32: TBitmap32);
var
  strTempFile: string;
begin
  strTempFile := GetTempDir + 'shape-5A257AC3-73C8-472E-A424-85D08C0A43FD.png';
  slide.Export(strTempFile, 'png', 0, 0);

  ViewPNG(strTempFile, bmp32);
end;

end.
