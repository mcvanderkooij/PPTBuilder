unit UUtilsFeedback;

interface

uses
  Windows, Forms, ComCtrls, StdCtrls, Classes;

type
  ISyncFeedback = interface
    ['{BE462EDD-CCB8-45F9-859A-80C3C8D3485E}']
    procedure FeedbackStart(strPart: string; iMax: integer);
    procedure FeedbackProgress(iPosition: integer);
    procedure FeedbackReady;
  end;

  TFeedbackForm = class(TCustomForm)
  private
    FMessageString: string;
    function GetPB_Position: integer;
    procedure SetPB_Position(const Value: integer);
    function GetPB_Max: integer;
    procedure SetPB_Max(const Value: integer);
    procedure SetMessageString(const Value: string);
  protected
    lblMessage: TLabel;
    pbProgress: TProgressBar;
    FLastUpdateTime: Cardinal;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure NeedsUpdate;
  published
  public
    constructor CreateNew(AOwner: TComponent); reintroduce;

    property MessageString: string read FMessageString write SetMessageString;
    property PB_Position: integer read GetPB_Position write SetPB_Position;
    property PB_Max: integer read GetPB_Max write SetPB_Max;
  end;

implementation

uses
  Sysutils, GNUGetText, UUtils;

{ TFeedbackForm }

constructor TFeedbackForm.CreateNew(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  Width := 600;
  Height := 200;
  Font.Size := 10;
  Caption := Application.Title;
  OnCloseQuery := FormCloseQuery;

  lblMessage := TLabel.Create(Self);
  lblMessage.Parent := Self;
  lblMessage.Caption := _('Initializing');
  lblMessage.Left := (ClientWidth - lblMessage.Width) div 2;
  lblMessage.Top := 30;

  pbProgress := TProgressBar.Create(Self);
  pbProgress.Parent := Self;
  pbProgress.Min := 0;
  pbProgress.Max := 100;
  pbProgress.Position := 0;
  pbProgress.Left := 10;
  pbProgress.Top := 100;
  pbProgress.Width := ClientWidth - 20;
  FLastUpdateTime := 0; // always refresh the first time
  ChangeScale(96, Screen.PixelsPerInch);

  //ChangeBidiAsNeeded(Self);
end;

procedure TFeedbackForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := PB_Max = 100;
end;

function TFeedbackForm.GetPB_Max: integer;
begin
  Result := pbProgress.Max;
end;

function TFeedbackForm.GetPB_Position: integer;
begin
  Result := pbProgress.Position;
end;

procedure TFeedbackForm.NeedsUpdate;
begin
  // update at the max of xxx ms
  if (FLastUpdateTime + 1000) < GetTickCount then begin
    lblMessage.Caption := FMessageString + ' ' + Format( _('(%d of %d)'), [pbProgress.Position, pbProgress.Max]);
    lblMessage.Left := (ClientWidth - lblMessage.Width) div 2;
    Application.ProcessMessages;
    FLastUpdateTime := GetTickCount;
  end;
end;

procedure TFeedbackForm.SetMessageString(const Value: string);
begin
  if FMessageString <> Value then begin
    FMessageString := Value;
    NeedsUpdate;
  end;
end;

procedure TFeedbackForm.SetPB_Max(const Value: integer);
begin
  pbProgress.Max := Value;
end;

procedure TFeedbackForm.SetPB_Position(const Value: integer);
begin
  if pbProgress.Position <> Value then begin
    pbProgress.Position := Value;
    NeedsUpdate;
  end;
end;


end.
