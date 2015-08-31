unit UfrmPictureDescription;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, GR32_Image, Vcl.AppEvnts;

type
  TfrmPictureDescription = class(TForm)
    ImgView321: TImgView32;
    edtDescription: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    ApplicationEvents1: TApplicationEvents;
    edtRemark: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
  private
    FOtherDescriptions: TStringList;
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    function GetRemark: string;
    procedure SetRemark(const Value: string);
    { Private declarations }
  public
    property OtherDescriptions: TStringList read FOtherDescriptions;
    property Description: string read GetDescription write SetDescription;
    property Remark: string read GetRemark write SetRemark;
    { Public declarations }
  end;

implementation

uses
  GnuGetText;

{$R *.dfm}

{ TfrmPictureDescription }

procedure TfrmPictureDescription.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  btnOK.Enabled := (FOtherDescriptions.IndexOf(edtDescription.Text) = -1) and (edtDescription.Text <> '');
  if btnOK.Enabled then begin
    edtDescription.Color := clWindow;
  end else begin
    edtDescription.Color := clRed;
  end;
end;

procedure TfrmPictureDescription.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);

  FOtherDescriptions := TStringList.Create;
end;

procedure TfrmPictureDescription.FormDestroy(Sender: TObject);
begin
  FOtherDescriptions.Free;
end;

procedure TfrmPictureDescription.FormShow(Sender: TObject);
begin
  edtDescription.SetFocus;
  edtDescription.SelStart := Length(edtDescription.Text);
end;

function TfrmPictureDescription.GetDescription: string;
begin
  Result := edtDescription.Text;
end;

function TfrmPictureDescription.GetRemark: string;
begin
  Result := edtRemark.Text;
end;

procedure TfrmPictureDescription.SetDescription(const Value: string);
begin
  edtDescription.Text := Value;
end;

procedure TfrmPictureDescription.SetRemark(const Value: string);
begin
  edtRemark.Text := Value;
end;

end.
