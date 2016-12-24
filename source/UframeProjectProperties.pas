unit UframeProjectProperties;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.AppEvnts, Vcl.StdCtrls;

type
  TFrameProjectProperties = class(TFrame)
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtSpeaker: TEdit;
    edtCollecte1: TEdit;
    edtCollecte2: TEdit;
    btnSpeakerAdd: TButton;
    btnSpeakerSelect: TButton;
    btnCollecte1Add: TButton;
    btnCollecte1Select: TButton;
    btnCollecte2Add: TButton;
    btnCollecte2Select: TButton;
    ApplicationEvents1: TApplicationEvents;
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure btnSpeakerAddClick(Sender: TObject);
    procedure btnCollecte1AddClick(Sender: TObject);
    procedure btnCollecte2AddClick(Sender: TObject);
    procedure btnSpeakerSelectClick(Sender: TObject);
    procedure btnCollecte1SelectClick(Sender: TObject);
    procedure btnCollecte2SelectClick(Sender: TObject);
    procedure edtSpeakerExit(Sender: TObject);
  private
    FOnChanged: TNotifyEvent;
    function GetCollecte1: string;
    function GetCollecte2: string;
    function GetSpeaker: string;
    procedure SetCollecte1(const Value: string);
    procedure SetCollecte2(const Value: string);
    procedure SetSpeaker(const Value: string);
  protected
    procedure DoChanged;
  public
    property Speaker: string read GetSpeaker write SetSpeaker;
    property Collecte1: string read GetCollecte1 write SetCollecte1;
    property Collecte2: string read GetCollecte2 write SetCollecte2;
  published
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

implementation

{$R *.dfm}

uses
  GNUGetText,
  USettings, UfrmSelectString;

procedure TFrameProjectProperties.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
begin
  btnSpeakerAdd.Enabled := (edtSpeaker.Text <> '') and (GetSettings.Speakers.IndexOf(edtSpeaker.Text) = -1);
  btnCollecte1Add.Enabled := (edtCollecte1.Text <> '') and (GetSettings.Collecte1.IndexOf(edtCollecte1.Text) = -1);
  btnCollecte2Add.Enabled := (edtCollecte2.Text <> '') and (GetSettings.Collecte2.IndexOf(edtCollecte2.Text) = -1);
end;

procedure TFrameProjectProperties.btnCollecte1AddClick(Sender: TObject);
begin
  GetSettings.Collecte1.Add(edtCollecte1.Text);
  GetSettings.Save;
end;

procedure TFrameProjectProperties.btnCollecte1SelectClick(Sender: TObject);
var
  strSelected: string;
  blnStringsChanged: boolean;
begin
  strSelected := edtCollecte1.Text;
  if SelectString(GetSettings.Collecte1, _('Select Collecte 1'), strSelected, blnStringsChanged) then begin
    edtCollecte1.Text := strSelected;
    if blnStringsChanged then
    begin
      GetSettings.Save;
    end;
    DoChanged;
  end;
end;

procedure TFrameProjectProperties.btnCollecte2AddClick(Sender: TObject);
begin
  GetSettings.Collecte2.Add(edtCollecte2.Text);
  GetSettings.Save;
end;

procedure TFrameProjectProperties.btnCollecte2SelectClick(Sender: TObject);
var
  strSelected: string;
  blnStringsChanged: boolean;
begin
  strSelected := edtCollecte2.Text;
  if SelectString(GetSettings.Collecte2, _('Select Collecte 2'), strSelected, blnStringsChanged) then begin
    edtCollecte2.Text := strSelected;
    if blnStringsChanged then
    begin
      GetSettings.Save;
    end;
    DoChanged;
  end;
end;

procedure TFrameProjectProperties.btnSpeakerAddClick(Sender: TObject);
begin
  GetSettings.Speakers.Add(edtSpeaker.Text);
  GetSettings.Save;
end;

procedure TFrameProjectProperties.btnSpeakerSelectClick(Sender: TObject);
var
  strSelected: string;
  blnStringsChanged: boolean;
begin
  strSelected := edtSpeaker.Text;
  if SelectString(GetSettings.Speakers, _('Select Speaker'), strSelected, blnStringsChanged) then begin
    edtSpeaker.Text := strSelected;
    if blnStringsChanged then begin
      GetSettings.Save;
    end;
    DoChanged;
  end;
end;

procedure TFrameProjectProperties.DoChanged;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TFrameProjectProperties.edtSpeakerExit(Sender: TObject);
begin
  DoChanged;
end;

function TFrameProjectProperties.GetCollecte1: string;
begin
  Result := edtCollecte1.Text;
end;

function TFrameProjectProperties.GetCollecte2: string;
begin
  Result := edtCollecte2.Text;
end;

function TFrameProjectProperties.GetSpeaker: string;
begin
  Result := edtSpeaker.Text;
end;

procedure TFrameProjectProperties.SetCollecte1(const Value: string);
begin
  edtCollecte1.Text := Value;
end;

procedure TFrameProjectProperties.SetCollecte2(const Value: string);
begin
  edtCollecte2.Text := Value;
end;

procedure TFrameProjectProperties.SetSpeaker(const Value: string);
begin
  edtSpeaker.Text := Value;
end;

end.
