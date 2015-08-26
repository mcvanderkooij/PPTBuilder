unit URibbon;

interface

uses
  Classes, UProject, USlide, GR32;

type
  TRibbon = class
  private
    FProject: TProject;
    FContentDir: string;
  protected
    function CreateAndSaveBitmap(slPictoSlides: TStringList; iCurrent: integer;
      rectSize, incPoint: TPoint; iMaxShow: integer): string;
  public
    constructor create(project: TProject; strContentDir: string); virtual;

    function Build(iCurrentSlide: integer; rectArea: TRect): string;
  end;

implementation

uses
  Math, SysUtils,
  UUtils, USourcePPT, USourceInfo;

{ TRibbon }

function TRibbon.Build(iCurrentSlide: integer; rectArea: TRect): string;
var
  iSlide: integer;
  slide: TSlide;
  slPictoSlides: TStringList;

  iCurrentPicto: integer;

  rectSize: TPoint;
  incPoint: TPoint;
  iMaxShow: integer;
  sourceInfo: TSourceInfo;
begin
  rectSize :=  Point(Min(rectArea.Width, rectArea.Height), Min(rectArea.Width, rectArea.Height));
  if rectArea.Width < rectArea.Height then begin
    incPoint := Point(0, rectSize.Y);
    iMaxShow := rectArea.Height div rectSize.Y;
  end else begin
    incPoint := Point(rectSize.X, 0);
    iMaxShow := rectArea.Width div rectSize.X;
  end;

  iCurrentPicto := -1;

  slPictoSlides := TStringList.Create;
  try
    for iSlide := 0 to FProject.Slides.Count -1 do begin
      slide := TSlide.Create(FProject.Slides[iSlide]);
      try
        if slide.IsSubOverview then begin
          continue;
        end;
        sourceInfo := slide.PictoName;
        if (sourceInfo.SourceType = sitFileName) and FileExists(sourceInfo.FileName) then begin

          if (slPictoSlides.Count = 0) or (CompareText(slPictoSlides[slPictoSlides.Count -1], sourceInfo.FileName) <> 0) then begin
            if (iSlide >= iCurrentSlide) and (iCurrentSlide >= 0) then begin
              if iCurrentPicto = -1 then begin
                iCurrentPicto := slPictoSlides.Count;
              end;
            end;
            slPictoSlides.Add(sourceInfo.FileName);
          end else begin
            if (iSlide >= iCurrentSlide) and (iCurrentSlide >= 0) then begin
              if iCurrentPicto = -1 then begin
                iCurrentPicto := slPictoSlides.Count -1;
              end;
            end;
          end;
        end;
      finally
        slide.Free;
      end;
    end;

    if (iCurrentPicto = -1) and (iCurrentSlide >= 0) then
      iCurrentPicto := slPictoSlides.Count -1;

    while (iMaxShow < slPictoSlides.Count) and (slPictoSlides.Count > 0) do begin
      if (iCurrentPicto = 0) or ((slPictoSlides.Count - iCurrentPicto) > iCurrentPicto) then begin
        slPictoSlides.Delete(slPictoSlides.Count -1);
      end else begin
        slPictoSlides.Delete(0);
        dec(iCurrentPicto);
      end;
    end;
    Result := CreateAndSaveBitmap(slPictoSlides, iCurrentPicto, rectSize, incPoint, iMaxShow);
  finally
    slPictoSlides.Free;
  end;
end;

constructor TRibbon.create(project: TProject; strContentDir: string);
begin
  inherited Create;
  FProject := project;
  FContentDir := strContentDir;
end;

function TRibbon.CreateAndSaveBitmap(slPictoSlides: TStringList;
  iCurrent: integer; rectSize, incPoint: TPoint; iMaxShow: integer): string;
var
  i: integer;
  bmp, bmpPicto, bmpPointing: TBitmap32;
  bmpSize: TPoint;

  rectDestination, rectSource: TRect;
begin
  bmp := TBitmap32.Create;
  bmpPicto := TBitmap32.Create;
  bmpPointing := TBitmap32.Create;
  try
    bmpPointing.SetSize(rectSize.X, rectSize.Y);
    bmpPointing.Clear(SetAlpha(clYellow32, $80));
    bmpPointing.DrawMode := dmBlend;

    bmpSize := rectSize;
    for i := 0 to slPictoSlides.Count -2 do begin
      bmpSize.X := bmpSize.X + incPoint.X;
      bmpSize.Y := bmpSize.Y + incPoint.Y;
    end;
    bmp.SetSize(bmpSize.X, bmpSize.Y);
    bmp.Clear(clBlack32);

    bmpSize := Point(0,0);
    rectDestination := Rect(0, 0, rectSize.X, rectSize.Y);
    for i := 0 to slPictoSlides.Count -1 do begin
      ViewPNG( slPictoSlides[i], bmpPicto );
      rectSource := Rect(0, 0, bmpPicto.Width, bmpPicto.Height);
      bmpPicto.DrawTo(bmp, rectDestination, rectSource );

      if i = iCurrent then begin
        bmpPointing.DrawTo(bmp, rectDestination, bmpPointing.ClipRect );
      end;

      rectDestination.Left := rectDestination.Left + incPoint.X;
      rectDestination.Top := rectDestination.Top + incPoint.Y;
      rectDestination.Right := rectDestination.Right + incPoint.X;
      rectDestination.Bottom := rectDestination.Bottom + incPoint.Y;
    end;

    Result := GetTempDir + 'ribbon.png';

    SaveBitmap32AsPNG(bmp, Result);
  finally
    bmpPointing.Free;
    bmpPicto.Free;
    bmp.Free;
  end;

end;

end.
