unit UUtils;

interface

uses
  Windows, Classes, StdCtrls, PNGImage, Graphics, Types, GR32, IdHashElf, IdHash;

type
  TCachedBoolean = (cbUnknown, cbFalse, cbTrue);
const
  CCachedBoolean: array[TCachedBoolean] of boolean = (false, false, true);

type
  TMemoDlgBtn = (mdYesNo, mdOK, mdOKCancel);
  TSelectionType = (stSingle, stMulti, stAllowNew, stMultiAllowNew);
  TButtonArray = array of TButton;

type
  TPngImageX = class(TPngImage)
  public
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
  end;

  THashFile = class(TIdHashElf)
  public
    function HashValue( strFileName: string ) : LongWord; overload;
    function HashValueAsHex(strFileName: string): string;
    class function S_HashValueAsHex(strFileName: string): string;
  end;

  THashString = class(TIdHashElf)
  public
    function HashValue( strHashMe: string ) : LongWord; overload;
    function HashValueAsHex(strHashMe: string): string;
    class function S_HashValueAsHex(strHashMe: string): string;
  end;

procedure FindFilesWithExtensions(FilesList: TStrings; blnRecursive: boolean; strStartDir, strCurrentDir, strFileExtensions: string);
procedure ViewPNG(strFileName: string; bmp32: TBitmap32);
procedure ViewPicture(strFileName: string; bmp32: TBitmap32);
procedure SaveBitmap32AsPNG(bmp32: TBitmap32; strFileName: string);

function GetTempDir: string;
function GetTempFile(const strExtension: string): string;

function ReadImageSize(strFileName: string): TRect;

function LoadUnicodeFromFile(strFileName: string): string;
procedure SaveUnicodeToFile(strFileName, strText: string);

implementation

uses
  ActiveX, ComObj,
  SysUtils, UUtilsStrings;

procedure FindFilesWithExtensions(FilesList: TStrings; blnRecursive: boolean; strStartDir, strCurrentDir, strFileExtensions: string);
var
  SR: TSearchRec;
  DirList: TStringList;
  IsFound: Boolean;
  i: integer;
begin
  strStartDir := IncludeTrailingPathDelimiter(strStartDir);
  if strCurrentDir <> '' then
    strCurrentDir := IncludeTrailingPathDelimiter(strCurrentDir);

  // make sure there is a ;
  if Length(strFileExtensions) > 0 then begin
    if strFileExtensions[1] <> ';' then
      strFileExtensions := ';' + strFileExtensions + ';';
      strFileExtensions := stringreplace(strFileExtensions, ';;', ';', [rfReplaceAll]);
  end;

  if blnRecursive then begin
    DirList := TStringList.Create;
  end else
    DirList := nil;
  try
    IsFound := FindFirst(strStartDir + strCurrentDir + '*.*', faAnyFile, SR) = 0;
    while IsFound do begin
      if (SR.Attr and faDirectory) = 0 then begin
        if PosIEx(';' + ExtractFileExt(SR.Name) + ';', strFileExtensions) > 0 then
          FilesList.Add(strCurrentDir + SR.Name);
      end else begin
        if Assigned(DirList) then begin
          if (SR.Name[1] <> '.') then begin
            DirList.Add(strCurrentDir + SR.Name);
          end;
        end;
      end;
      IsFound := FindNext(SR) = 0;
    end;
    FindClose(SR);

    if Assigned(DirList) then begin
      for i := 0 to DirList.Count - 1 do
        FindFilesWithExtensions(FilesList, blnRecursive, strStartDir, DirList[i], strFileExtensions);
    end;
  finally
    FreeAndNil(DirList);
  end;
end;

procedure TPngImageX.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  Chunk: TChunkTRNS;
  Count: Integer;
  I: Integer;
begin
  if Empty then Exit;
  Chunk := TChunkTRNS(Chunks.ItemFromClass(TChunkTRNS));
  if (TransparencyMode = ptmBit) and Assigned(Chunk) and
    (Header.ColorType = COLOR_PALETTE) then
  begin
    Count := 0;
    for I := 0 to High(Chunk.PaletteValues) do
      if Chunk.PaletteValues[I] <> $FF then
      begin
        Inc(Count);
        if Count > 1 then
          Break;
      end;
    if Count = 1 then
    begin
      DrawPartialTrans(ACanvas.Handle, Rect);
      Exit;
    end;
  end;
  inherited;
end;

procedure ViewPNG(strFileName: string; bmp32: TBitmap32);
var
  PngImage: TPngImage;
begin
  bmp32.Clear(clBlack32);
  if FileExists(strFileName) then begin
    PngImage := TPngImageX.Create;
    try
      PngImage.LoadFromFile(strFileName);
      bmp32.SetSizeFrom(PngImage);
      bmp32.Clear(clBlack32);

      bmp32.Canvas.Draw(0, 0, PngImage);
      bmp32.Changed;
    finally
      PngImage.Free;
    end;
  end;
end;

procedure ViewPicture(strFileName: string; bmp32: TBitmap32);
var
  strExtension: string;
begin
  strExtension := ExtractFileExt(strFileName);
  if strExtension = '.png' then begin
    ViewPNG(strFileName, bmp32);
  end else begin
    bmp32.Clear(clBlack32);
    if FileExists(strFileName) then begin
      bmp32.LoadFromFile(strFileName);
    end;
  end;
end;

procedure SaveBitmap32AsPNG(bmp32: TBitmap32;{ var }strFileName: string);
var
  png: TPNGImage;
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  png := TPNGImage.Create;
  try
    bmp.Assign(bmp32);
    png.Assign(bmp);
    strFileName := ChangeFileExt(strFileName, '.png');
    png.SaveToFile(strFileName);
  finally
    png.Free;
    bmp.Free;
  end;
end;

function GetTempDir: string;
var
  iLen: integer;
begin
  SetLength(Result, MAX_PATH);
  iLen := GetTempPath(MAX_PATH - 1, PChar(Result));
  SetLength(Result, iLen);
end;

function GetTempFile(const strExtension: string): string;
var
  Buffer: String;
begin
  SetLength(Buffer,MAX_PATH);
  repeat
    GetTempPath( MAX_PATH, PChar( Buffer) );
    GetTempFileName(PChar( Buffer), '~', 0, PChar(Buffer));
    Result := ChangeFileExt(Buffer, strExtension);
  until not FileExists(Result);
end;

function ReadImageSize(strFileName: string): TRect;
var
  picture: TPicture;
begin
  Result := Rect(0, 0, 0, 0);
  if FileExists(strFileName) then begin
    picture := TPicture.Create;
    try
      picture.LoadFromFile(strFileName);
      Result := Rect(0, 0, picture.Width, picture.Height);
    finally
      picture.Free;
    end;
  end;
end;


{ THashFile }

function THashFile.HashValue(strFileName: string): LongWord;
var
  fileStream: TFileStream;
begin
  if FileExists(strFileName) then begin
    fileStream := TFileStream.Create(strFileName, fmOpenRead);
    try
      Result := HashValue(fileStream);
    finally
      fileStream.Free;
    end;
  end else begin
    Result := $FFFFFFFF;
  end;
end;

function THashFile.HashValueAsHex(strFileName: string): string;
var
  cHash: Cardinal;
begin
  cHash := HashValue(strFileName);
  Result := IntToHex(cHash, 8);
end;

class function THashFile.S_HashValueAsHex(strFileName: string): string;
var
  hash: THashFile;
begin
  hash := THashFile.Create;
  try
    Result := hash.HashValueAsHex(strFileName);
  finally
    hash.Free;
  end;
end;

{ THashString }

function THashString.HashValue(strHashMe: string): LongWord;
var
  stringStream: TStringStream;
begin
  stringStream := TStringStream.Create(strHashMe);
  try
    Result := HashValue(stringStream);
  finally
    stringStream.Free;
  end;
end;

function THashString.HashValueAsHex(strHashMe: string): string;
var
  cHash: Cardinal;
begin
  cHash := HashValue(strHashMe);
  Result := IntToHex(cHash, 8);
end;

class function THashString.S_HashValueAsHex(strHashMe: string): string;
var
  hash: THashString;
begin
  hash := THashString.Create;
  try
    Result := hash.HashValueAsHex(strHashMe);
  finally
    hash.Free;
  end;
end;

function LoadUnicodeFromFile(strFileName: string): string;
var
  ss: TStringStream;
begin
  ss := TStringStream.Create('', TEncoding.Unicode, false);
  try
    ss.LoadFromFile(strFileName);
    Result := ss.DataString;
  finally
    ss.Free;
  end;
end;

procedure SaveUnicodeToFile(strFileName, strText: string);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(strText, TEncoding.Unicode, false);
  try
    ss.SaveToFile(strFileName);
  finally
    ss.Free;
  end;
end;

end.
