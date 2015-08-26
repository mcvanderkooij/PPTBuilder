unit UfrmMemoDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, UUtils, ComCtrls;

type
  TButtonArray = array of TButton;

  TfrmMemoDlg = class(TForm)
    pnlHeaderMessage: TPanel;
    pnlButtons: TPanel;
    pnlFooterMessage: TPanel;
    btnYes: TButton;
    btnNo: TButton;
    btnOk: TButton;
    lblHeader: TLabel;
    lblFooter: TLabel;
    btnCancel: TButton;
    mmoMessage: TRichEdit;
    edtFooterText: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FHeader: string;
    FTitle: string;
    FFooter: string;
    FButtons: TMemoDlgBtn;
    FMessagereadonly: boolean;
    procedure SetButtons(const Value: TMemoDlgBtn);
    procedure SetFooter(const Value: string);
    procedure SetHeader(const Value: string);
    procedure SetTitle(const Value: string);
    function GetMessageText: TStrings;
    procedure SetMessagereadonly(const Value: boolean);
    function GetFooterText: string;
    procedure SetFooterText(const Value: string);
    function GetShowFooterText: boolean;
    procedure SetShowFooterText(const Value: boolean);
    { Private declarations }
  protected
    procedure ReallignButtons;
  public
    property Title: string read FTitle write SetTitle;
    property Header: string read FHeader write SetHeader;
    property Footer: string read FFooter write SetFooter;
    property FooterText: string read GetFooterText write SetFooterText;
    property ShowFooterText: boolean read GetShowFooterText write SetShowFooterText;
    property MessageText: TStrings read GetMessageText;
    property Messagereadonly: boolean read FMessagereadonly write SetMessagereadonly;
    property Buttons: TMemoDlgBtn read FButtons write SetButtons;
    { Public declarations }
  end;

implementation

uses
  GNUGetText;

{$R *.dfm}

procedure TfrmMemoDlg.FormCreate(Sender: TObject);
begin
  //ChangeBidiAsNeeded(Self);
  TranslateComponent(self);
  //AutoFitButtonCaptions(Self);
  FHeader := '';
  FTitle := Application.Title;
  FFooter := '';
  FButtons := mdOK;
end;

procedure TfrmMemoDlg.FormResize(Sender: TObject);
begin
  ReallignButtons;
end;

procedure TfrmMemoDlg.FormShow(Sender: TObject);
begin
  pnlHeaderMessage.Visible := FHeader <> '';
  pnlFooterMessage.Visible := FFooter <> '';
  ReallignButtons;
end;

function TfrmMemoDlg.GetFooterText: string;
begin
  Result := edtFooterText.Text;
end;

function TfrmMemoDlg.GetMessageText: TStrings;
begin
  Result := mmoMessage.Lines;
end;

function TfrmMemoDlg.GetShowFooterText: boolean;
begin
  Result := edtFooterText.Visible;
end;

procedure TfrmMemoDlg.ReallignButtons;
const
  CButtonWidth = 75;
  CButtonSpace = 10;
var
  aButtonArray: TButtonArray;
  i: integer;
  iTotalButtonWidth: integer;
begin
  case FButtons of
    mdYesNo: begin
        SetLength(aButtonArray, 2);
        aButtonArray[0] := btnYes;
        aButtonArray[1] := btnNo;
      end;
    mdOK: begin
        SetLength(aButtonArray, 1);
        aButtonArray[0] := btnOk;
      end;
    mdOKCancel: begin
        SetLength(aButtonArray, 2);
        aButtonArray[0] := btnOk;
        aButtonArray[1] := btnCancel;
      end;
  end;
  btnYes.Visible := FButtons = mdYesNo;
  btnNo.Visible := FButtons = mdYesNo;
  btnOk.Visible := FButtons in [mdOK, mdOKCancel];
  btnCancel.Visible := FButtons in [mdOKCancel];

  iTotalButtonWidth := Length(aButtonArray) * (CButtonWidth + CButtonSpace) - CButtonSpace;
  for i := 0 to Length(aButtonArray) - 1 do begin
    if BiDiMode = bdRightToLeft then
      aButtonArray[i].Left := (Self.Width div 2) + (iTotalButtonWidth div 2) + (i+1) * (CButtonWidth + CButtonSpace)
    else
      aButtonArray[i].Left := (Self.Width div 2) - (iTotalButtonWidth div 2) + i * (CButtonWidth + CButtonSpace);
    aButtonArray[i].Width := CButtonWidth;
  end;
end;

procedure TfrmMemoDlg.SetButtons(const Value: TMemoDlgBtn);
begin
  if FButtons <> Value then begin
    FButtons := Value;
    ReallignButtons;
  end;
end;

procedure TfrmMemoDlg.SetFooter(const Value: string);
begin
  if (FFooter <> Value) then begin
    FFooter := Value;
    lblFooter.Caption := FFooter;
  end;
end;

procedure TfrmMemoDlg.SetFooterText(const Value: string);
begin
  edtFooterText.Text := Value;
end;

procedure TfrmMemoDlg.SetHeader(const Value: string);
begin
  if (FHeader <> Value) then begin
    FHeader := Value;
    lblHeader.Caption := FHeader;
  end;
end;

procedure TfrmMemoDlg.SetMessagereadonly(const Value: boolean);
begin
  if FMessagereadonly <> Value then begin
    FMessagereadonly := Value;
    mmoMessage.ReadOnly := Value;
    mmoMessage.WordWrap := not Value;
  end;
end;

procedure TfrmMemoDlg.SetShowFooterText(const Value: boolean);
begin
  if Value then begin
    lblFooter.Align := alLeft;
    lblFooter.Alignment := taRightJustify;
  end else begin
    lblFooter.Align := alClient;
    lblFooter.Alignment := taCenter;
  end;
  edtFooterText.Visible := Value;
end;

procedure TfrmMemoDlg.SetTitle(const Value: string);
begin
  if (FTitle <> Value) then begin
    FTitle := Value;
    Caption := FTitle;
  end;
end;

end.
