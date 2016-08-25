unit UfrmBrowseFTP;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, USourceInfo,
  Vcl.StdCtrls, Vcl.AppEvnts;

type
  TfrmBrowseFTP = class(TForm)
    tvDirs: TTreeView;
    lvFiles: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    pnlFolder: TPanel;
    lblFolder: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    ApplicationEvents1: TApplicationEvents;
    procedure tvDirsChange(Sender: TObject; nodeSelected: TTreeNode);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure lvFilesDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAutoOpen: string;
    procedure AddToTree(parent: TStringTree; parentNode: TTreeNode);
    function GetSelectedFTPFileName: string;
    procedure SetAutoOpen(const Value: string);
    { Private declarations }
  public
    property SelectedFTPFileName: string read GetSelectedFTPFileName;
    property AutoOpen: string read FAutoOpen write SetAutoOpen;
    { Public declarations }
  end;

function SelectFTPFile(var strFileName: string; strExtensions: string; strAutoOpen: string): boolean;
function CopyExternalFileToLocal(strFileName: string): string;
procedure OpenInternetSource(strAutoOpen: string);

implementation

uses
  ShellApi,
  GNUGetText, UBrowseFTP, USettings, UUtilsStrings, UTempActions;

{$R *.dfm}

var
  gl_frmBrowseFTP: TfrmBrowseFTP;

function SelectFTPFile(var strFileName: string; strExtensions: string; strAutoOpen: string): boolean;

begin
  if not Assigned(gl_frmBrowseFTP) then begin
    gl_frmBrowseFTP := TfrmBrowseFTP.Create(Application.MainForm);
  end;
  try
    GetBrowseFTP.Extensions.Clear;
    GetBrowseFTP.Extensions.Delimiter := '|';
    GetBrowseFTP.Extensions.DelimitedText := strExtensions;
    gl_frmBrowseFTP.AutoOpen := strAutoOpen;
    Result := gl_frmBrowseFTP.ShowModal = mrOK;
    if Result then begin
      strFileName := gl_frmBrowseFTP.SelectedFTPFileName;
    end;
  finally
    //gl_frmBrowseFTP.Free;
  end;
end;

function CopyExternalFileToLocal(strFileName: string): string;
var
  strLocalFile, strLocalFileOld: string;
begin
  Result := '';
  strLocalFile := '<ftp>' + StringReplace(strFileName, '/', '\', [rfReplaceAll]);
  strLocalFile := GetSettings.DirExplode(strLocalFile);
  if FileExists(strLocalFile) then begin
    strLocalFileOld := strLocalFile + '.old';
    if FileExists(strLocalFileOld) then begin
      DeleteFile(strLocalFileOld);
    end;

    // just use the already downloaded file when in use
    if not RenameFile(strLocalFile, strLocalFileOld) then begin
      Result := strLocalFile;
      Exit;
    end;
  end;
  ForceDirectories(ExtractFilePath(strLocalFile));
  if GetBrowseFTP.Download(strFileName, strLocalFile) then begin
    Result := strLocalFile;
  end;
end;

procedure OpenInternetSource(strAutoOpen: string);
var
  strFileName: string;
begin
  strFileName := '';
  if SelectFTPFile(strFileName, '.ppt|.pptx|.doc|.docx|.xls|.xlsx|.rtf|.txt|.pdf', strAutoOpen) then begin
    strFileName := CopyExternalFileToLocal(strFileName);
    ShellExecute(0, '', PChar(strFileName), '', '', SW_SHOWNORMAL);
  end;

end;

procedure TfrmBrowseFTP.AddToTree(parent: TStringTree; parentNode: TTreeNode);
var
  i: Integer;
  treeNode: TTreeNode;
begin
  for i := 0 to parent.Count -1 do begin
    treeNode := tvDirs.Items.AddChild(parentNode, parent.Item[i].Data);
    AddToTree(parent.Item[i], treeNode);
  end;
end;

procedure TfrmBrowseFTP.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
var
  aFolders: TArrayOfString;
  index: integer;
  node: TTreeNode;
begin
  btnOK.Enabled := lvFiles.ItemIndex <> -1;

  if FAutoOpen <> '' then begin
    SetWaitCursor;
    aFolders := Split(FAutoOpen, '/');
    FAutoOpen := '';
    node := tvDirs.Items.GetFirstNode;
    for index := 0 to Length(aFolders) -1 do begin
      while Assigned(node) do begin
        if node.Text = aFolders[index] then begin
          tvDirsChange(nil, node);
          node.Selected := true;
          node := node.getFirstChild;
          break;
        end;
        node := node.getNextSibling;
      end;
    end;
  end;
end;

procedure TfrmBrowseFTP.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
  // todo GKVKandelaar
  tvDirs.Items.AddChildObject(nil, 'BeamTeam', pointer(-1));
  tvDirs.FullExpand;
end;

function TfrmBrowseFTP.GetSelectedFTPFileName: string;
begin
  Result := lblFolder.Caption;
  if lvFiles.ItemIndex <> -1 then
    Result := Result + '/' + lvFiles.Items[lvFiles.ItemIndex].Caption;
end;

procedure TfrmBrowseFTP.lvFilesDblClick(Sender: TObject);
begin
 if lvFiles.ItemIndex <> -1 then
   ModalResult := mrOk;
end;

procedure TfrmBrowseFTP.SetAutoOpen(const Value: string);
begin
  FAutoOpen := Value;
end;

procedure TfrmBrowseFTP.tvDirsChange(Sender: TObject; nodeSelected: TTreeNode);
var
  node: TTreeNode;
  strPath: string;
  slDirs, slFiles: TStringList;
  i: integer;
  item: TListItem;
begin
  if Assigned(nodeSelected) then begin

    slDirs := nil;
    slFiles := TStringList.Create;
    try
      slFiles.CaseSensitive := false;
      if integer(nodeSelected.Data) = -1  then begin
        slDirs := TStringList.Create;
        slDirs.CaseSensitive := false;
      end;
      // create path
      strPath := '';
      node := nodeSelected;
      while Assigned(node) do begin
        if strPath = '' then
          strPath := node.Text
        else
          strPath := node.Text + '/' + strPath;
        node := node.Parent;
      end;
      strPath := '/' + strPath;

      if (strPath <> lblFolder.Caption) or Assigned(slDirs) then begin
        lvFiles.Items.Clear;
        lblFolder.Caption := '';

        if GetBrowseFTP.GetFilesAndDirs(strPath, slFiles, slDirs) then begin
          if Assigned(slDirs) then begin
            // delete children
            nodeSelected.DeleteChildren;
            for i := 0 to slDirs.Count -1 do begin
              tvDirs.Items.AddChildObject(nodeSelected, slDirs[i], pointer(-1));
            end;
            nodeSelected.Data := nil;
            nodeSelected.Expand(true);
          end;
          // fill list
          for i := 0 to slFiles.Count -1 do begin
            item := lvFiles.Items.Add;
            item.Caption := slFiles[i];
          end;
          lblFolder.Caption := strPath;
        end else begin
          tvDirs.ClearSelection();
        end;
      end;
    finally
      slFiles.Free;
      slDirs.Free;
    end;
  end;
end;

end.
