unit skushell;

{*********************************************************
     Sanjay Kanade's Utility Functions for Delphi
     Shell functions

     SKUUTILS
     Copyright (C) 1999, Sanjay Kanade

***********************************************************}

interface

uses windows, sysutils, dialogs, controls, forms, shlobj, activex, comobj,
     shellapi, classes;

type
  TRootFolder = (rtStartMenu, rtPrograms, rtDrives, rtDesktopDir, rtPersonal,
                 rtAppData, rtFavorites, rtTemplates, rtDesktop, rtCommonAppData,
                 rtCommonDocuments);
  {IMPORTANT: You can easily extend this list by adding your own constants.
  Note that they are not same as CSIDL constants. Rather, we have shortlisted
  our own list of constants. The idea is not to get confused by too many
  CSIDL constants and what each one represents. We only use a few of them.}

  //******************************************************
  function getSpecialFolder(rootType: TRootFolder): string;
  {Gets a special location from the shell. Pass one of the
  roottypes given at the top of this unit.
  Example:
    getSpecialFolder(rtPrograms) would return the location
    of 'C:\Program Files' folder.
  ******************************************************}

  //*****************************************************
  function browseFolderDialog(dlgTitle: string; rootType: TRootFolder;
                                           setInitialDir: string): string;
  {Displays the folder selection dialog and allows you to preselect an existing
  folder. Returns the folder which the user selects.
  Example:
    browseFolderDialog('Select a folder', rtDrives, 'C:\Windows\Start Menu');
  ******************************************************}

  //******************************************************
  function createShortcut(exePath, shortcutPath, descr: string): boolean;
  {Creates a shortcut for you. exePath is the path of the target file which
  can be any file (not necessarily an EXE). Descr is the description that
  appears with the shortcut. shortcutPath is the path name of the .LNK file
  which you want to create. Returns true on success.

  Example: to create a desktop folder for a file, you would use:
    shortcutPath := getSpecialFolder(rtDesktopDir);
    if (shortcutPath <> '') and (AnsiLastChar(shortcutPath) <> '\') then
        shortcutPath := Concat(shortcutPath, '\');
    createShortcut('C:\Program Files\myprog.exe',
                    shortcutPath + 'myprog.lnk',
                    'My program');
  ******************************************************}

var
  SHGetFolderPath:  FUNCTION (hwndOwner: HWND;
                                  nFolder: integer;
                                  hToken: THandle;
                                  dwFlags: DWORD;
                                  lpszPath: PChar): HRESULT;
                                  StDCall = NIL;


implementation

{******************************************************
For internal use
******************************************************}
Procedure FreePidl( pidl: PItemIDList );
  Var
    alloc: IMalloc;
  Begin
    If Succeeded(SHGetMalloc(alloc)) Then Begin
      alloc.Free(pidl);
    End;
  End;

{******************************************************
function getSpecialFolder(rootType: TRootFolder): string;
Gets a special location from the shell. Pass one of the
roottypes given at the top of this unit.
Example:
  getSpecialFolder(rtPrograms) would return the location
  of 'C:\Program Files' folder.
******************************************************}
function getSpecialFolder(rootType: TRootFolder): string;
var
  pidl: pItemIDList;
  nameBuf: Array [0..MAX_PATH] of Char;
  csidlValue: Integer;
begin
  case rootType of
    rtDrives: csidlValue := CSIDL_DRIVES;
    rtPrograms: csidlValue := CSIDL_PROGRAMS;
    rtStartMenu: csidlValue := CSIDL_STARTMENU;
    rtDesktopDir: csidlValue := CSIDL_DESKTOPDIRECTORY;
    rtDesktop: csidlValue := CSIDL_DESKTOP;
    rtPersonal: csidlValue := CSIDL_PERSONAL;
    rtAppData: csidlValue := CSIDL_APPDATA;
    rtFavorites: csidlValue := CSIDL_FAVORITES;
    rtTemplates: csidlValue := CSIDL_TEMPLATES;
    rtCommonAppData: csidlValue := CSIDL_COMMON_APPDATA;
    rtCommonDocuments: csidlValue := CSIDL_COMMON_DOCUMENTS;
  else
    exit;
  end;

  result := '';
  if SUCCEEDED(SHGetSpecialFolderLocation(
		  Application.handle, csidlValue, pidl)) then
  begin
    If pidl <> Nil Then
    Begin
      If SHGetPathFromIDList(pidl, namebuf) Then
        Result := StrPas(namebuf);
      FreePidl( pidl );
    End;
  end
  else
  //Fix:
  if assigned(SHGetFolderPath) and
    ((csidlvalue=CSIDL_APPDATA) or (csidlvalue=CSIDL_PERSONAL) or
     (csidlvalue=CSIDL_COMMON_APPDATA) or (csidlvalue=CSIDL_COMMON_DOCUMENTS))
  then
  begin
    if SUCCEEDED(SHGetFolderPath(
		  Application.handle, csidlValue, 0, 0, namebuf)) then
       Result := StrPas(namebuf);
  end;
end;

{******************************************************
For internal use
******************************************************}
function BrowseCallBack(hwnd: HWND; msg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  if (msg=BFFM_INITIALIZED) and (lpData <> 0)  then
    SendMessage(hwnd, BFFM_SETSELECTION, integer(TRUE), lpData);
  result := 0;
end;

{******************************************************
function browseFolderDialog(dlgTitle: string; rootType: TRootFolder; setInitialDir: string): string;
Displays the folder selection dialog and allows you to preselect an existing
folder. Returns the folder which the user selects.
Example:
  browseFolderDialog('Select a folder', rtDrives, 'C:\Windows\Start Menu');
******************************************************}
function browseFolderDialog(dlgTitle: string; rootType: TRootFolder; setInitialDir: string): string;
var
  csidlValue: Integer;
  nameBuf: Array [0..MAX_PATH] of Char;
  bi: TBrowseInfo;
  pidl, pidlBrowse: pItemIDList;
begin
  Result := '';

  case rootType of
    rtDrives: csidlValue := CSIDL_DRIVES;
    rtPrograms: csidlValue := CSIDL_PROGRAMS;
    rtStartMenu: csidlValue := CSIDL_STARTMENU;
    rtDesktopDir: csidlValue := CSIDL_DESKTOPDIRECTORY;
    rtDesktop: csidlValue := CSIDL_DESKTOP;
    rtPersonal: csidlValue := CSIDL_PERSONAL;
    rtAppData: csidlValue := CSIDL_APPDATA;
    rtFavorites: csidlValue := CSIDL_FAVORITES;
    rtTemplates: csidlValue := CSIDL_TEMPLATES;
    rtCommonAppData: csidlValue := CSIDL_COMMON_APPDATA;
  else
    exit;
  end;

  if SUCCEEDED(SHGetSpecialFolderLocation(
		  Application.handle, csidlValue, pidl)) then
  begin
    If pidl <> Nil Then
    Begin
      With bi Do
      Begin
      	hwndOwner := Application.handle;
	pidlRoot := pidl;
	pszDisplayName := @nameBuf;
	lpszTitle := pchar(dlgTitle);
	ulFlags := BIF_RETURNONLYFSDIRS;
	lpfn := BrowseCallBack;
	lParam := 0;
        if setInitialDir <> '' then
          lParam := integer(pchar(setInitialDir));
	pidlBrowse := SHBrowseForFolder(bi);
        If pidlBrowse <> Nil Then
        begin
          If SHGetPathFromIDList(pidlBrowse, namebuf) Then
            Result := StrPas(namebuf);
          FreePidl(pidlBrowse);
        end;
        FreePidl( pidl );
      end;
    End;
  end;
end;


{******************************************************
function createShortcut(exePath, shortcutPath, descr: string): boolean;
Creates a shortcut for you. exePath is the path of the target file which
can be any file (not necessarily an EXE). Descr is the description that
appears with the shortcut. shortcutPath is the path name of the .LNK file
which you want to create. Returns true on success.

Example: to create a desktop folder for a file, you would use:
  shortcutPath := getSpecialFolder(rtDesktopDir);
  if (shortcutPath <> '') and (AnsiLastChar(shortcutPath) <> '\') then
      shortcutPath := Concat(shortcutPath, '\');
  createShortcut('C:\Program Files\myprog.exe',
                  shortcutPath + 'myprog.lnk',
                  'My program');
******************************************************}
function createShortcut(exePath, shortcutPath, descr: string): boolean;
var
  aComObj: IUnknown;
  aShellLink: IShellLink;
  iFile: IPersistFile;
  wideFileName: wideString;
begin
  result := false;
  try
    aComObj := createComObject(CLSID_ShellLink);
    aShellLink := aComObj as IShellLink;
    iFile := aComObj as IPersistFile;
    aShellLink.setPath(pchar(exePath));
    aShellLink.setDescription(pchar(descr));
    aShellLink.setWorkingDirectory(pchar(ExtractFileDir(exePath)));
    wideFileName := shortCutPath;
    iFile.save(PWChar(wideFileName), false);
    result := true;
  except
  end;
end;

var
  OldError: Longint;
  FLibHandle: THandle;

INITIALIZATION
  OldError := SetErrorMode(SEM_NOOPENFILEERRORBOX);
  try
    FLibHandle := LoadLibrary('shfolder');
    if FLibHandle = 0 then
      exit;

    SHGetFolderPath := GetProcAddress(FLibHandle, 'SHGetFolderPathA');
  finally
    SetErrorMode(OldError);
  end;

finalization
  if FLibHandle <> 0 then FreeLibrary(FLibHandle);
end.


