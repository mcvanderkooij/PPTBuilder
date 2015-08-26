unit UfrmSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmSettings = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    edtUsername: TEdit;
    Label2: TLabel;
    edtPassword: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  GNUGetText, USettings;

procedure TfrmSettings.btnOKClick(Sender: TObject);
begin
  GetSettings.FTPUserName := edtUsername.Text;
  GetSettings.FTPPassword := edtPassword.Text;
  GetSettings.Save;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  edtUsername.Text := GetSettings.FTPUserName;
  edtPassword.Text := GetSettings.FTPPassword;
end;

end.
