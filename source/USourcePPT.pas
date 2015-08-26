unit USourcePPT;

interface

uses
  System.Generics.Collections, USourceInfo,
  PowerPoint_TLB;

type
  TCachedPPT = class
  private
    FFileName: string;
    FApp: PowerPointApplication;
    FPresentation: PowerPointPresentation;
    function GetPresentation: PowerPointPresentation;
  public
    property FileName: string read FFileName write FFileName;

    property Presentation: PowerPointPresentation read GetPresentation;

    destructor Destroy; override;

    function SlidesAndShapes: TStringTree;
  end;

  TCachedPPTs = class(TObjectList<TCachedPPT>)
  public
    destructor Destroy; override;

    function Get(strFileName: string): TCachedPPT;
    function FindByName(strFileName: string): TCachedPPT;

    function GetShape(ppt: TSourceInfo): PowerPoint_TLB.Shape;
    function GetSlide(ppt: TSourceInfo): PowerPoint_TLB._Slide;
  end;

function GetCachedPPTs: TCachedPPTs;

implementation

uses
  Office_TLB, SysUtils,
  USettings, UUtilsStrings, UfrmBrowseFTP;

var
  gl_CachedPPTs: TCachedPPTs;

function GetCachedPPTs: TCachedPPTs;
begin
  if not Assigned(gl_CachedPPTs) then begin
    gl_CachedPPTs := TCachedPPTs.Create;
  end;
  Result := gl_CachedPPTs
end;

{ TCachedPPTs }

function TCachedPPTs.Get(strFileName: string): TCachedPPT;
begin
  Result := FindByName(strFileName);
  if Result = nil then begin
    Result := TCachedPPT.Create;
    Result.FileName := strFileName;
    Add(Result);
  end;
end;

destructor TCachedPPTs.Destroy;
begin
  Clear;
  inherited;
end;

function TCachedPPTs.FindByName(strFileName: string): TCachedPPT;
var
  i: Integer;
begin
  for i := 0 to Count -1 do begin
    if Items[i].FileName = strFileName then begin
      Result := Items[i];
      Exit;
    end;
  end;
  Result := nil;
end;

function TCachedPPTs.GetShape(ppt: TSourceInfo): PowerPoint_TLB.Shape;
var
  slide: PowerPoint_TLB._Slide;
  i: Integer;
begin
  Result := nil;
  if ppt.SourceType = sitPPT then begin
    slide := GetSlide(ppt);
    if Assigned(slide) then begin
      for i := 1 to slide.Shapes.Count do begin
        if slide.Shapes.Item(i).Name = ppt.ShapeName then begin
          Result := slide.Shapes.Item(i);
          Exit;
        end;
      end;
    end;
  end;
end;

function TCachedPPTs.GetSlide(ppt: TSourceInfo): PowerPoint_TLB._Slide;
var
  cachedPPT: TCachedPPT;
  i: Integer;
begin
  Result := nil;
  if ppt.SourceType = sitPPT then begin
    cachedPPT := Get(ppt.FileName);
    if Assigned(cachedPPT) then begin
      for i := 1 to cachedPPT.Presentation.Slides.Count do begin
        if cachedPPT.Presentation.Slides.Item(i).Name = ppt.SlideName then begin
          Result := cachedPPT.Presentation.Slides.Item(i);
          Exit;
        end;
      end;
    end;
  end;
end;

{ TCachedPPT }

destructor TCachedPPT.Destroy;
begin
  FPresentation := nil;
  FApp.Quit;
  FApp := nil;
  inherited;
end;

function TCachedPPT.GetPresentation: PowerPointPresentation;
var
  strImplodedDir: string;
begin
  if not Assigned(FApp) then begin
    if not FileExists(FFileName) then begin
      strImplodedDir := GetSettings.DirImplode(FFileName);
      if strImplodedDir <> FFileName then begin
        if pos('<ftp>', strImplodedDir) = 1 then begin
          strImplodedDir := copy(strImplodedDir, 6, MaxInt);

          strImplodedDir := StringReplace(strImplodedDir, '\', '/', [rfReplaceAll]);

          CopyExternalFileToLocal(strImplodedDir)
        end;
      end;
    end;


    FApp := CoPowerPointApplication.Create;
    FPresentation := FApp.Presentations.Open(FFileName, msoFalse, msoFalse, msoFalse);
  end;
  Result := FPresentation
end;

function TCachedPPT.SlidesAndShapes: TStringTree;
var
  pres: PowerPointPresentation;
  slide: PowerPoint_TLB._Slide;
  i,j: integer;
  slideNode: TStringTree;
begin
  Result := TStringTree.Create;
  pres := Presentation;
  if Assigned(pres) then begin
    for i := 1 to pres.Slides.Count do begin
      slide := pres.Slides.Item(i);
      slideNode := Result.Add(slide.Name);
      for j := 1 to slide.Shapes.Count do begin
        if slide.Shapes.Item(j).type_ = msoPicture then begin
          slideNode.Add(slide.Shapes.Item(j).Name);
        end;
      end;
    end;
  end;
end;

initialization
  gl_CachedPPTs := nil;
finalization
  FreeAndNil(gl_CachedPPTs);
end.
