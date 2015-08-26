unit UfrmQuickStart;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.AppEvnts;

type
  TQuickStartResult = (qsCancel, qsOpen, qsOpenFile, qsNew);

  TfrmQuickStart = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    btnOpenExisting: TButton;
    btnClose: TButton;
    lbLiturgies: TListBox;
    Label3: TLabel;
    lbRecentFiles: TListBox;
    btnOpenRecent: TButton;
    btnNew: TButton;
    ApplicationEvents1: TApplicationEvents;
    procedure btnOpenExistingClick(Sender: TObject);
    procedure lbLiturgiesDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbRecentFilesDblClick(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
  private
    FQuickStartResult: TQuickStartResult;
    FNewProjectLiturgy: string;
    FOpenFile: string;
    { Private declarations }
  protected
    procedure FillLiturgies;
    procedure FillRecentFiles;
  public
    { Public declarations }
    property QuickStartResult: TQuickStartResult read FQuickStartResult;
    property NewProjectLiturgy: string read FNewProjectLiturgy;
    property OpenFile: string read FOpenFile;
  end;

function ShowQuickStart(out strLiturgyOrFile: string): TQuickStartResult;

implementation

uses
  GNUGetText, Math, ULiturgy, UMRUList;

{$R *.dfm}

function ShowQuickStart(out strLiturgyOrFile: string): TQuickStartResult;
var
  frmQuickStart: TfrmQuickStart;
begin
  strLiturgyOrFile := '';
  frmQuickStart := TfrmQuickStart.Create(Application.MainForm);
  try
    frmQuickStart.ShowModal;
    Result := frmQuickStart.QuickStartResult;
    case Result of
      qsCancel: ;
      qsOpen: ;
      qsOpenFile: strLiturgyOrFile := frmQuickStart.OpenFile;
      qsNew: strLiturgyOrFile := frmQuickStart.NewProjectLiturgy;
    end;
  finally
    frmQuickStart.Free;
  end;
end;

{ TfrmQuickStart }

procedure TfrmQuickStart.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  btnNew.Enabled := lbLiturgies.ItemIndex <> -1;
  btnOpenRecent.Enabled := lbRecentFiles.ItemIndex <> -1;
end;

procedure TfrmQuickStart.btnOpenExistingClick(Sender: TObject);
begin
  FQuickStartResult := qsOpen;
  ModalResult := mrOk;
end;

procedure TfrmQuickStart.FillLiturgies;
var
  liturgies: TLiturgies;
  i: Integer;
begin
  lbLiturgies.Items.Clear;
  liturgies := GetLiturgies;
  for i := 0 to liturgies.Count -1 do begin
    lbLiturgies.Items.Add(liturgies[i].Name);
  end;
  lbLiturgies.ItemIndex := Min(0, liturgies.Count -1);
end;

procedure TfrmQuickStart.FillRecentFiles;
var
  mrus: TMRUList;
  i: Integer;
begin
  lbRecentFiles.Items.Clear;
  mrus := GetMRUList;
  for i := 0 to mrus.Count -1 do begin
    lbRecentFiles.Items.Add(mrus[i]);
  end;
  lbRecentFiles.ItemIndex := Min(0, mrus.Count -1);
end;

procedure TfrmQuickStart.FormCreate(Sender: TObject);
begin
  TranslateComponent(Self);

  FillLiturgies;
  FillRecentFiles;
end;

procedure TfrmQuickStart.lbLiturgiesDblClick(Sender: TObject);
begin
  if lbLiturgies.ItemIndex <> -1 then begin
    FQuickStartResult := qsNew;
    FNewProjectLiturgy := lbLiturgies.Items[lbLiturgies.ItemIndex];
    ModalResult := mrOk;
  end;
end;

procedure TfrmQuickStart.lbRecentFilesDblClick(Sender: TObject);
begin
  if lbRecentFiles.ItemIndex <> -1 then begin
    FQuickStartResult := qsOpenFile;
    FOpenFile := lbRecentFiles.Items[lbRecentFiles.ItemIndex];
    ModalResult := mrOk;
  end;
end;

end.
