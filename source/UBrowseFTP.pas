unit UBrowseFTP;

interface

uses
  Classes, IdFTP, IdFTPList;

type
  TBrowseFTP = class
  private
    FStartDir: string;
    FSkipFolders: TStringList;
    FExtensions: TStringList;
  protected
    function CreateIdFTP: TIdFTP;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetFilesAndDirs(strPath: string; slFiles, slFolders: TStringList): boolean;
    function Download(strServerFileName, strLocalFileName: string): boolean;

    property StartDir: string read FStartDir write FStartDir;
    property SkipFolders: TStringList read FSkipFolders;
    property Extensions: TStringList read FExtensions;
  end;

function GetBrowseFTP: TBrowseFTP;

implementation

uses
  IdComponent, IdExplicitTLSClientServerBase, IdAllFTPListParsers, SysUtils,
  UTempActions, USettings;

var
  gl_BrowseFTP: TBrowseFTP;

function GetBrowseFTP: TBrowseFTP;
begin
  if not Assigned(gl_BrowseFTP) then begin
    gl_BrowseFTP := TBrowseFTP.Create;
    // todo GKVKandelaar
    gl_BrowseFTP.StartDir := '/BeamTeam';
    gl_BrowseFTP.SkipFolders.Add('/BeamTeam/0UD');
    gl_BrowseFTP.SkipFolders.Add('/BeamTeam/Computers');
    gl_BrowseFTP.SkipFolders.Add('/BeamTeam/#recycle');
    gl_BrowseFTP.Extensions.Add('.ppt');
    gl_BrowseFTP.Extensions.Add('.pptx');
  end;
  Result := gl_BrowseFTP;
end;

function ExtractUrlFilePath(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim + '/', FileName);
  Result := Copy(FileName, 1, I);
end;

function ExtractUrlFileName(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim + '/', FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

{ TBrowseFTP }

constructor TBrowseFTP.Create;
begin
  inherited Create;
  FSkipFolders := TStringList.Create;
  FExtensions := TStringList.Create;
end;

function TBrowseFTP.CreateIdFTP: TIdFTP;
begin
  Result := TIdFTP.Create(nil);
  // todo GKVKandelaar
  Result.Host := 'opslag.gkvdekandelaar.nl';
  Result.ProxySettings.ProxyType := fpcmNone;
  Result.ProxySettings.Port := 0;
  Result.UseTLS := utNoTLSSupport;
  Result.Username := GetSettings.FTPUserName;
  Result.Password := GetSettings.FTPPassword;
  Result.ConnectTimeout := 1000;
end;

destructor TBrowseFTP.Destroy;
begin
  FExtensions.Free;
  FSkipFolders.Free;
  inherited;
end;

function TBrowseFTP.Download(strServerFileName,
  strLocalFileName: string): boolean;
var
  ftp: TIdFTP;
begin
  try
    SetWaitCursor;
    ftp := CreateIdFTP;
    try
      ftp.Connect;
      ftp.ChangeDir(ExtractUrlFilePath(strServerFileName));
      ftp.Get(ExtractUrlFileName(strServerFileName), strLocalFileName, false );
      Result := True;
    finally
      ftp.Free;
    end;
  except
    Result := False;
  end;
end;

function TBrowseFTP.GetFilesAndDirs(strPath: string; slFiles, slFolders: TStringList): boolean;
var
  ftp: TIdFTP;
  i: integer;
  strFileName: string;
begin
  try
    SetWaitCursor;
    ftp := CreateIdFTP;
    try
      ftp.Connect;
      if strPath <> '' then begin
        ftp.ChangeDir(strPath);
      end;
      ftp.List;
      for i := 0 to ftp.DirectoryListing.Count -1 do begin
        strFileName := ftp.DirectoryListing[i].FileName;
        if ftp.DirectoryListing[i].ItemType = ditFile then begin
          if Assigned(slFiles) then begin
            if Extensions.IndexOf(ExtractFileExt(strFileName)) <> -1 then begin
              slFiles.Add(strFileName);
            end;
          end;
        end else if ftp.DirectoryListing[i].ItemType = ditDirectory then begin
          if Assigned(slFolders) then begin
            if SkipFolders.IndexOf(strPath + '/' + strFileName) = -1 then begin
              slFolders.Add(strFileName);
            end;
          end;
        end
      end;
    finally
      ftp.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

initialization
  gl_BrowseFTP := nil;
finalization
  gl_BrowseFTP.Free;
  gl_BrowseFTP := nil;
end.
