unit UfrmPictureSelector;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, GR32_Image, Vcl.ComCtrls,
  Vcl.AppEvnts, USourcePPT, USourceInfo;

type
  TfrmPictureSelector = class(TForm)
    ImgViewSelection: TImgView32;
    btnCancel: TButton;
    btnSelect: TButton;
    lbPictures: TListBox;
    ApplicationEvents1: TApplicationEvents;
    lblFooterText: TLabel;
    edtFooterText: TEdit;
    lblRemarkText: TLabel;
    edtRemarkText: TEdit;
    procedure FormShow(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FStartDir: string;
    FImageFileName: string;
    FImageOnShow: TSourceInfo;
    FSubDir: string;
    function GetSelection: TSourceInfo;
    procedure SetSelection(const Value: TSourceInfo);
    function GetFooterText: string;
    function GetShowFooterText: boolean;
    procedure SetFooterText(const Value: string);
    procedure SetShowFooterText(const Value: boolean);
    function GetFooter: string;
    procedure SetFooter(const Value: string);
    function GetRemark: string;
    function GetRemarkText: string;
    function GetShowRemarkText: boolean;
    procedure SetRemark(const Value: string);
    procedure SetRemarkText(const Value: string);
    procedure SetShowRemarkText(const Value: boolean);
    { Private declarations }
  public
    property Selection: TSourceInfo read GetSelection write SetSelection;
    property StartDir: string read FStartDir write FStartDir;
    property SubDir: string read FSubDir write FSubDir;

    property Footer: string read GetFooter write SetFooter;
    property FooterText: string read GetFooterText write SetFooterText;
    property ShowFooterText: boolean read GetShowFooterText write SetShowFooterText;

    property Remark: string read GetRemark write SetRemark;
    property RemarkText: string read GetRemarkText write SetRemarkText;
    property ShowRemarkText: boolean read GetShowRemarkText write SetShowRemarkText;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  GnuGetText, UUtils, USettings;

{ TfrmPictureSelector }

procedure TfrmPictureSelector.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
var
  oSelection: TSourceInfo;
begin
  if Visible then begin
    oSelection := Selection;
    try
      if oSelection.FileName <> FImageFileName then begin
        FImageFileName := oSelection.FileName;
        if FImageFileName <> '' then begin
          if CompareText(ExtractFileExt(FImageFileName), '.png') = 0 then begin
            ViewPNG(FImageFileName, ImgViewSelection.Bitmap);
          end else begin
            ImgViewSelection.Bitmap.LoadFromFile(FImageFileName);
          end;
        end;
      end;
      btnSelect.Enabled := oSelection.FileName <> '';
    finally
      oSelection.Free;
    end;
  end;
end;

procedure TfrmPictureSelector.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
  FImageOnShow := TSourceInfo.CreateAsFileName('');
  FStartDir := GetSettings.GetContentDir;
  FSubDir := '';
end;

procedure TfrmPictureSelector.FormDestroy(Sender: TObject);
begin
  FImageOnShow.Free;
  inherited;
end;

procedure TfrmPictureSelector.FormShow(Sender: TObject);
begin
  lbPictures.Items.Clear;
  FindFilesWithExtensions(lbPictures.Items, true, FStartDir, FSubDir, '.png;.jpg');
  if FImageOnShow.FileName <> '' then begin
    SetSelection(FImageOnShow);
    FImageOnShow.FileName := '';
  end;
end;

function TfrmPictureSelector.GetFooter: string;
begin
  Result := lblFooterText.Caption;
end;

function TfrmPictureSelector.GetFooterText: string;
begin
  Result := edtFooterText.Text;
end;

function TfrmPictureSelector.GetRemark: string;
begin
  Result := lblRemarkText.Caption;
end;

function TfrmPictureSelector.GetRemarkText: string;
begin
  Result := edtRemarkText.Text;
end;

function TfrmPictureSelector.GetSelection: TSourceInfo;
begin
  Result := TSourceInfo.CreateAsFileName('');
  if lbPictures.ItemIndex <> -1 then
    Result.FileName := FStartDir + lbPictures.Items[lbPictures.ItemIndex];
end;

function TfrmPictureSelector.GetShowFooterText: boolean;
begin
  Result := edtFooterText.Visible;
end;

function TfrmPictureSelector.GetShowRemarkText: boolean;
begin
  Result := edtRemarkText.Visible;
end;

procedure TfrmPictureSelector.SetFooter(const Value: string);
begin
  lblFooterText.Caption := Value;
end;

procedure TfrmPictureSelector.SetFooterText(const Value: string);
begin
  edtFooterText.Text := Value;
end;

procedure TfrmPictureSelector.SetRemark(const Value: string);
begin
  lblRemarkText.Caption := Value;
end;

procedure TfrmPictureSelector.SetRemarkText(const Value: string);
begin
  edtRemarkText.Text := Value;
end;

procedure TfrmPictureSelector.SetSelection(const Value: TSourceInfo);
var
  i: Integer;
begin
  if Visible then begin
    for i := 0 to lbPictures.Items.Count -1 do begin
      if (FStartDir + lbPictures.Items[i]) = Value.FileName then begin
        lbPictures.ItemIndex := i;
      end;
    end;
  end else begin
    FImageOnShow.Free;
    FImageOnShow := Value.DeepCopy;
  end;
end;

procedure TfrmPictureSelector.SetShowFooterText(const Value: boolean);
begin
  edtFooterText.Visible := Value;
  lblFooterText.Visible := Value;
end;

procedure TfrmPictureSelector.SetShowRemarkText(const Value: boolean);
begin
  edtRemarkText.Visible := Value;
  lblRemarkText.Visible := Value;
end;

end.
