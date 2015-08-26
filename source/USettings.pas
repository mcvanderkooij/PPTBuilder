unit USettings;

interface

uses
  Classes, UFastKeysSS;

type
  TSettings = class
  private
    FFontName: string;
	  FDirs: TFastKeyValuesSS;
    FAppDataDir: string;
    FMyDocDir: string;
    FFTPPassword: string;
    FFTPUserName: string;
    FMRUs: TStringList;

  protected
    FSpeakers: TStringList;
    FCollecte1: TStringList;
    FCollecte2: TStringList;
    function SettingsFileName: string;
  public
    property FontName: string read FFontName;

    property FTPUserName: string read FFTPUserName write FFTPUserName;
    property FTPPassword: string read FFTPPassword write FFTPPassword;

    property Speakers: TStringList read FSpeakers;
    property Collecte1: TStringList read FCollecte1;
    property Collecte2: TStringList read FCollecte2;

    property MRUs: TStringList read FMRUs;

	  constructor Create; virtual;
	  destructor Destroy; override;
    procedure SetDefaults;

    function GetContentDir: string;
    function GetAppDataDir: string;
    function GetMyDocDir: string;
	
	  function GetDir(strDirName: string): string;
	  function DirImplode(strFileName: string): string;
	  function DirExplode(strFileName: string): string;

    procedure Load;
    procedure Save;
  end;

function GetSettings: TSettings;

implementation

uses
  SysUtils, RegularExpressionsCore, IniFiles,
  UUtilsStrings, skushell, UUtils;

var
  gl_Settings: TSettings;

function GetSettings: TSettings;
begin
  if not Assigned(gl_Settings) then begin
    gl_Settings := TSettings.Create;
    gl_Settings.Load;
  end;
  Result := gl_Settings;
end;

{ TSettings }

constructor TSettings.Create;
begin
  inherited Create;
  FAppDataDir := '';
  FMyDocDir := '';
  FDirs := TFastKeyValuesSS.Create;

  FSpeakers := TStringList.Create;
  FSpeakers.Delimiter := #9;
  FSpeakers.StrictDelimiter := true;
  FSpeakers.Duplicates := dupIgnore;
  FSpeakers.Sorted := True;
  FCollecte1 := TStringList.Create;
  FCollecte1.Delimiter := #9;
  FCollecte1.StrictDelimiter := true;
  FCollecte1.Duplicates := dupIgnore;
  FCollecte1.Sorted := True;
  FCollecte2 := TStringList.Create;
  FCollecte2.Delimiter := #9;
  FCollecte2.StrictDelimiter := true;
  FCollecte2.Duplicates := dupIgnore;
  FCollecte2.Sorted := True;

  FMRUs := TStringList.Create;
  FMRUs.Delimiter := #9;
  FMRUs.StrictDelimiter := true;

  SetDefaults;
end;

destructor TSettings.Destroy;
begin
  FreeAndNil(FDirs);

  FreeAndNil(FSpeakers);
  FreeAndNil(FCollecte1);
  FreeAndNil(FCollecte2);
  FreeAndNil(FMRUs);

  inherited;
end;

function TSettings.DirExplode(strFileName: string): string;
var
  regex: TPerlRegEx;
  strRight: string;
begin
  regex := TPerlRegEx.Create;
  try
    regex.RegEx := '^<([^<>]+)>(.+)';
    regex.Subject := strFileName;

    if regex.Match and (regex.GroupCount = 2) then begin
      strRight := regex.Groups[2];
      while pos('\', strRight) = 1 do
        delete(strRight, 1, 1);

      Result := GetDir( regex.Groups[1] ) + strRight;
    end else begin
      Result := strFileName;
    end;
  finally
    regex.Free;
  end;
end;

function TSettings.DirImplode(strFileName: string): string;
var
  i: integer;
  strDir: string;
  iFoundLength: integer;
begin
  iFoundLength := -1;
  for i := 0 to FDirs.Count -1 do begin
    strDir := FDirs.ValueOfIndex[i];
    if PosIEx(strDir, strFileName) = 1 then begin
      if (iFoundLength = -1) or (iFoundLength < Length(strDir)) then begin
        iFoundLength := Length(strDir);
        Result := '<' + FDirs.KeyOfIndex[i] + '>' + copy(strFileName, Length(strDir)+1);
      end;
    end;
  end;
  if iFoundLength = -1 then begin
    Result := strFileName;
  end;
end;

function TSettings.GetAppDataDir: string;
begin
  if FAppDataDir = '' then begin
    FAppDataDir := IncludeTrailingPathDelimiter(getSpecialFolder(rtAppData)) + 'Eendsoft\PPTBuilder\';
    ForceDirectories(FAppDataDir);
  end;
  Result := FAppDataDir;
end;

function TSettings.GetContentDir: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))) + 'content');
end;

function TSettings.GetDir(strDirName: string): string;
begin
  Result := FDirs[strDirName];
end;

function TSettings.GetMyDocDir: string;
begin
  if FMyDocDir = '' then begin
    FMyDocDir := IncludeTrailingPathDelimiter(getSpecialFolder(rtPersonal)) + 'Eendsoft\PPTBuilder\';
    ForceDirectories(FMyDocDir);
  end;
  Result := FMyDocDir;
end;

procedure TSettings.Load;
var
  iniFile: TIniFile;
begin
  if FileExists(SettingsFileName) then begin
    iniFile := TIniFile.Create(SettingsFileName);
    try
      FFTPUserName := iniFile.ReadString('FTP', 'FTPUserName', FFTPUserName);
      FFTPPassword := iniFile.ReadString('FTP', 'FTPPassword', string(AnsiEncode64(AnsiString(FFTPPassword))));
      FFTPPassword := string(AnsiDecode64(AnsiString(FFTPPassword)));

      FSpeakers.DelimitedText := iniFile.ReadString('Lists', 'Speakers', '');
      FCollecte1.DelimitedText := iniFile.ReadString('Lists', 'Collecte1', '');
      FCollecte2.DelimitedText := iniFile.ReadString('Lists', 'Collecte2', '');

      FMRUs.DelimitedText := iniFile.ReadString('Lists', 'MRUs', '');
    finally
      iniFile.Free;
    end;
  end;
end;

procedure TSettings.Save;
var
  iniFile: TIniFile;
begin
  iniFile := TIniFile.Create(SettingsFileName);
  try
    iniFile.WriteString('FTP', 'FTPUserName', FFTPUserName);
    iniFile.WriteString('FTP', 'FTPPassword', string(AnsiEncode64(AnsiString(FFTPPassword))));

    iniFile.WriteString('Lists', 'Speakers', FSpeakers.DelimitedText);
    iniFile.WriteString('Lists', 'Collecte1', FCollecte1.DelimitedText);
    iniFile.WriteString('Lists', 'Collecte2', FCollecte2.DelimitedText);

    iniFile.WriteString('Lists', 'MRUs', FMRUs.DelimitedText);

    iniFile.UpdateFile;
  finally
    iniFile.Free;
  end;
end;

procedure TSettings.SetDefaults;
begin
  FFontName := 'Tahoma';
  FDirs['content'] := GetContentDir;
  FDirs['ftp'] := GetAppDataDir + 'ftp\';
  FDirs['appdata'] := GetAppDataDir;
  FDirs['mydoc'] := GetMyDocDir;
end;

function TSettings.SettingsFileName: string;
begin
  Result := GetAppDataDir + 'settings.ini';
end;

initialization
  gl_Settings := nil;
finalization
  gl_Settings.Free;
  gl_Settings := nil;
end.
