unit USourceBook;

interface

uses
  System.Generics.Collections, USourceInfo, UUtilsStrings;

type
  TCachedBook = class
  private
    FBookName: string;
    FChaptersAndVerses: TStringTree;
  public
    property BookName: string read FBookName write FBookName;
    property ChaptersAndVerses: TStringTree read FChaptersAndVerses;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure Load;
  end;

  TCachedBooks = class(TObjectList<TCachedBook>)
  public
    destructor Destroy; override;

    function Get(strBookName: string): TCachedBook;
    function FindByName(strBookName: string): TCachedBook;

    function GetChapter(ppt: TSourceInfo): TStringTree;
    function GetVerse(ppt: TSourceInfo): string;
  end;


function GetCachedBooks: TCachedBooks;

implementation

uses
  USettings, SysUtils, Classes, UUtils;

var
  gl_CachedBooks: TCachedBooks;

function GetCachedBooks: TCachedBooks;
begin
  if not Assigned(gl_CachedBooks) then begin
    gl_CachedBooks := TCachedBooks.Create;
  end;
  Result := gl_CachedBooks
end;

{ TCachedBook }

constructor TCachedBook.Create;
begin
  inherited;
  FChaptersAndVerses := TStringTree.Create;
end;

destructor TCachedBook.Destroy;
begin
  FChaptersAndVerses.Free;
  inherited;
end;

procedure TCachedBook.Load;
var
  strDirName: string;
  slChapters, slChapter: TStringList;
  i, j: integer;
  strCurVerseNumber: string;
  strCurVerse: string;

  chapter: TStringTree;

  procedure AddCurVerse;
  var
    verse: TStringTree;
  begin
    if strCurVerseNumber <> '' then begin
      verse := chapter.Add(strCurVerseNumber);
      verse.Add(trim(strCurVerse));

      strCurVerseNumber := '';
      strCurVerse := '';
    end;
  end;

begin
  strDirName := GetSettings.GetDir('content') + 'books\' + FBookName + '\';
  if DirectoryExists(strDirName) then begin
    slChapters := TStringList.Create;
    try
      FindFilesWithExtensions(slChapters, false, strDirName, '', '.txt');
      SortStrings(slChapters);
      for i := 0 to slChapters.Count-1 do begin
        chapter := nil;
        slChapter := TStringList.Create;
        try
          if FileExists(strDirName + slChapters[i]) then begin
            slChapter.LoadFromFile(strDirName + slChapters[i]);
            chapter := FChaptersAndVerses.Add(slChapters[i]);

            strCurVerseNumber := '';
            strCurVerse := '';
            for j := 0 to slChapter.Count -1 do begin
              if copy(slChapter[j], 1, 2) = '::'  then begin
                AddCurVerse;
                strCurVerseNumber := copy(slChapter[j], 3, MaxInt);
              end else begin
                strCurVerse := strCurVerse + #13 + slChapter[j];
              end;
            end;
            AddCurVerse;
          end;
        finally
          slChapter.Free;
        end;
      end;
    finally
      slChapters.Free;
    end;
  end;
end;

{ TCachedBooks }

destructor TCachedBooks.Destroy;
begin
  Clear;
  inherited;
end;

function TCachedBooks.FindByName(strBookName: string): TCachedBook;
var
  i: Integer;
begin
  for i := 0 to Count -1 do begin
    if Items[i].BookName = strBookName then begin
      Result := Items[i];
      Exit;
    end;
  end;
  Result := nil;
end;

function TCachedBooks.Get(strBookName: string): TCachedBook;
begin
  Result := FindByName(strBookName);
  if Result = nil then begin
    Result := TCachedBook.Create;
    Result.BookName := strBookName;
    Result.Load;
    Add(Result);
  end;
end;

function TCachedBooks.GetChapter(ppt: TSourceInfo): TStringTree;
var
  cachedBook: TCachedBook;
  i: Integer;
begin
  Result := nil;
  if ppt.SourceType = sitBook then begin
    cachedBook := Get(ppt.FileName);
    if Assigned(cachedBook) then begin
      for i := 0 to cachedBook.ChaptersAndVerses.Count -1 do begin
        if cachedBook.ChaptersAndVerses.Item[i].Data = ppt.SlideName then begin
          Result := cachedBook.ChaptersAndVerses.Item[i];
          Exit;
        end;
      end;
    end;
  end;
end;

function TCachedBooks.GetVerse(ppt: TSourceInfo): string;
var
  chapter: TStringTree;
  i: Integer;
begin
  Result := '';
  if ppt.SourceType = sitBook then begin
    chapter := GetChapter(ppt);
    if Assigned(chapter) then begin
      for i := 0 to chapter.Count -1 do begin
        if chapter.Item[i].Data = ppt.ShapeName then begin
          Result := chapter.Item[i].Item[0].Data;
          Exit;
        end;
      end;
    end;
  end;
end;

end.
