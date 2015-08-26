unit UOverview;

interface

uses
  UProject, USlide;

type
  TOverview = class
  private
    FProject: TProject;
  protected
    function BuildMain(iCurrentSlide: integer; slideOverview: TSlide;
      out iFontSizeNeeded: integer): string;
    function BuildSUBS(iCurrentSlide: integer; slideOverview: TSlide;
      out iFontSizeNeeded: integer): string;
  public
    constructor create(project: TProject); virtual;

    function Build(iCurrentSlide: integer; slideOverview: TSlide;
      blnSubOverview: boolean;
      out iFontSizeNeeded: integer): string;
  end;

implementation

uses
  Classes, SysUtils, Math,
  GnuGetText, UUtilsStrings, USlideTemplate;

{ TOverview }

function TOverview.Build(iCurrentSlide: integer; slideOverview: TSlide;
  blnSubOverview: boolean;
  out iFontSizeNeeded: integer): string;
begin
  if blnSubOverview then
    Result := BuildSUBS(iCurrentSlide, slideOverview, iFontSizeNeeded)
  else
    Result := BuildMain(iCurrentSlide, slideOverview, iFontSizeNeeded);
end;

function TOverview.BuildMain(iCurrentSlide: integer; slideOverview: TSlide;
  out iFontSizeNeeded: integer): string;
var
  iSlide: Integer;
  slide: TSlide;

  slOverView: array [TOverviewType] of TStringList;
  strOverviewLine: string;
  strColor, strSize: string;
  overviewIndex: TOverviewType;
  strPrefix: string;
  aOverviewLine: TArrayOfString;
  iMaxRows, iRowsNeeded, iLoop, i: integer;
begin
  Result := '';

  for overviewIndex := low(TOverviewType) to High(TOverviewType) do begin
    slOverView[overviewIndex] := TStringList.Create;
  end;

  try
    for iSlide := 0 to FProject.Slides.Count -1 do begin
      slide := TSlide.Create(FProject.Slides[iSlide]);
      try
        if slide.IsSubOverview then begin
          continue;
        end;

        if slide.SlideTemplateName = CTEMPLATE_COLLECTE then begin
          FProject.AfterCollecte := '$$';
        end;

        if not slide.ShowInOverview then begin
          continue;
        end;

        case slide.OverviewType of
          otReading:
            if slOverView[otReading].Count = 0 then
              strPrefix := _('Reading') + ':'#9                                 // todo content translation
            else
              strPrefix := #9;
          otText: if slOverView[otText].Count = 0 then                          // todo content translation
              strPrefix := _('Text') + ':'#9
            else
              strPrefix := #9;
        else
          strPrefix := '';
        end;
        if slide.OverviewType in [otSong, otReading, otText] then begin
          if FProject.AfterCollecte = '$$' then begin
            FProject.AfterCollecte := RespaceOverviewName(slide.OverviewName, false );
          end;

          if (iSlide < iCurrentSlide) and (slide.OverviewType in [otSong]) then begin
            strColor := '<grey>';
          end else begin
            strColor := '<white>';
          end;
          strOverviewLine := RespaceOverviewName(slide.OverviewName, slide.OverviewType = otSong );

          // add extra spaces for 2 and 1 digit song numbers
          aOverviewLine := split(strOverviewLine, #9);
          if Length(aOverviewLine) >= 2 then begin
            if Length(aOverviewLine[1]) = 2 then begin
              aOverviewLine[1] := '  ' + aOverviewLine[1];
            end else if Length(aOverviewLine[1]) = 1 then begin
              aOverviewLine[1] := '    ' + aOverviewLine[1];
            end;
            strOverviewLine := join(aOverviewLine, #9);
          end;
          strOverviewLine := strColor + strPrefix + strOverviewLine + strColor;

          slOverView[slide.OverviewType].Add(strOverviewLine);
        end;
      finally
        slide.Free;
      end;
    end;

    iFontSizeNeeded := 32;
    strSize := '';//'<b>';
    iMaxRows := slideOverview.Layout.Values['content'].GetMaxRows(iFontSizeNeeded);
    iRowsNeeded := slOverView[otSong].Count + slOverView[otReading].Count + slOverView[otText].Count + 1;

    while iRowsNeeded > iMaxRows do begin
      dec(iFontSizeNeeded, 4);
      iMaxRows := slideOverview.Layout.Values['content'].GetMaxRows(iFontSizeNeeded);
    end;

    // just add them, it will fit
    i := 0;
    while i < slOverView[otSong].Count do begin
      slOverView[otIgnore].Add(strSize + slOverView[otSong][i]+strSize);
      inc(i);
    end;

    // add empty spaces
    for iLoop := iRowsNeeded to iMaxRows -1 do begin
      slOverView[otIgnore].Add('');
    end;

    i := 0;
    while i < slOverView[otReading].Count do begin
      slOverView[otIgnore].Add(strSize + slOverView[otReading][i]+strSize);
      inc(i);
    end;

    i := 0;
    while i < slOverView[otText].Count do begin
      slOverView[otIgnore].Add(strSize + slOverView[otText][i]+strSize);
      inc(i);
    end;

    Result := slOverView[otIgnore].Text;
  finally
    for overviewIndex := low(TOverviewType) to High(TOverviewType) do begin
      slOverView[overviewIndex].Free;
    end;
  end;
end;

function TOverview.BuildSUBS(iCurrentSlide: integer; slideOverview: TSlide;
  out iFontSizeNeeded: integer): string;
var
  slOverView: TStringList;
  iSlide: integer;
  slide: TSlide;
  strPrefix, strColor, strOverviewLine: string;
begin
  slOverView := TStringList.Create;
  try
    slOverView.Add('');
    slOverView.Add('');
    for iSlide := 0 to FProject.Slides.Count -1 do begin
      slide := TSlide.Create(FProject.Slides[iSlide]);
      try
        if not slide.ShowInOverview then begin
          continue;
        end;
        if not slide.IsSubOverview then begin
          continue;
        end;

        case slide.OverviewType of
          otReading: strPrefix := _('Reading') + ' ';                           // todo content translation
          otText: strPrefix := _('Text') + ' ';                                 // todo content translation
        else
          strPrefix := _('Singing') + ' ';                                      // todo content translation
        end;
        if slide.OverviewType in [otSong, otReading, otText] then begin
          if (iSlide < iCurrentSlide) then begin
            strColor := '<grey><center>';
          end else begin
            strColor := '<white><center>';
          end;
          strOverviewLine := RespaceOverviewName(slide.OverviewName, false );
          strOverviewLine := strColor + strPrefix + strOverviewLine + strColor;
          slOverView.Add(strOverviewLine);

          if slide.OverviewType = otSong then
            slOverView.Add('');
        end;
      finally
        slide.Free;
      end;
    end;
    Result := slOverView.Text;
  finally
    slOverView.Free;
  end;
end;

constructor TOverview.create(project: TProject);
begin
  inherited Create;
  FProject := project;
end;

end.
