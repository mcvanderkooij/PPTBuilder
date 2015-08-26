unit UMRUList;

interface

uses
  Classes;

type
  TMRUList = class(TStringList)
  public
    procedure AddMRU(strFileName: string);
    procedure CheckExists;

    procedure Load;
    procedure Save;
  end;

function GetMRUList: TMRUList;

implementation

uses
  SysUtils,
  USettings;

var
  gl_MRUs: TMRUList;

function GetMRUList: TMRUList;
begin
  if not Assigned(gl_MRUs) then begin
    gl_MRUs := TMRUList.Create;
    gl_MRUs.Load;
  end;
  Result := gl_MRUs;
end;

{ TMRUList }

procedure TMRUList.AddMRU(strFileName: string);
var
  i: integer;
begin
  if strFileName = '' then
    Exit;
  if (Count > 0) and (Strings[0] = strFileName) then
    Exit;
  i := IndexOf(strFileName);
  if i <> -1 then begin
    Delete(i);
  end;
  Insert(0, strFileName);

  while Count > 10 do
    Delete(10);

  Save;
end;

procedure TMRUList.CheckExists;
var
  i: integer;
  blnDoSave: boolean;
begin
  blnDoSave := false;
  i := 0;
  while i < Count do begin
    if not FileExists(Strings[i]) then begin
      Delete(i);
      blnDoSave := true;
    end else begin
      inc(i);
    end;
  end;
  if blnDoSave then begin
    Save;
  end;
end;

procedure TMRUList.Load;
begin
  Assign(GetSettings.MRUs);
  CheckExists;
end;

procedure TMRUList.Save;
begin
  GetSettings.MRUs.Assign(Self);
  GetSettings.Save;
end;

initialization
  gl_MRUs := nil;
finalization
  FreeAndNil(gl_MRUs);
end.
