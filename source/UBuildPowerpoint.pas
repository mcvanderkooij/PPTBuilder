unit UBuildPowerpoint;

interface

uses
  Windows, Classes, SysUtils, UITypes, PowerPoint_TLB,
  UProject, USlide;

procedure BuildPowerpointSUBS(presentation: PowerPointPresentation; project: TProject);
procedure BuildPowerpoint(strFileName: string; project: TProject);
function ReplaceSpecials(strText: string; project: TProject; slide: TSlide): string;

implementation

uses
  Office_TLB, Types, RegularExpressionsCore, Forms, ShellApi,
  USourceInfo, GNUGetText,
  UUtils, UUtilsStrings, USlideLayout, USettings, USourcePPT, UTempActions,
  UUtilsFeedback,
  UOverview, USlideTemplate, URibbon, URegexReplaceProperties;

type
  TTagAction = (taBold, taItalic, taUnderline, taSuperscript, taSubscript, taColor, taSize, taBullet, taCenter);

var
  gl_StartColorScheme: ColorScheme;
  gl_StartDesign: Design;

function ReplaceSpecials(strText: string; project: TProject; slide: TSlide): string;
var
  regex: TRegexReplaceProperties;
begin
  regex := TRegexReplaceProperties.Create(project.Properties);
  try
    Result := regex.ReplaceWithProperties(strText);
  finally
    regex.Free;
  end;
  if Assigned(slide) then begin
    regex := TRegexReplaceProperties.Create(slide.Variables);
    try
      Result := regex.ReplaceWithProperties(Result);
    finally
      regex.Free;
    end;
  end;
  Result := StringReplace(Result, '%aftercollecte%', project.AfterCollecte, [rfReplaceAll, rfIgnoreCase]);
end;

function ReplaceAutomaticSmallNumbers(strText: string): string;
var
  regex: TPerlRegEx;
begin
  regex := TPerlRegEx.Create();
  try
    regex.Subject := strText;
    regex.Options := [preMultiLine];
    regex.RegEx := '\d+';
    regex.Replacement := '<14>$&<14>';
    regex.ReplaceAll;
    Result := regex.Subject;
  finally
    regex.Free;
  end;
end;

// each row
procedure LayoutTextRange( txtrange: PowerPoint_TLB.TextRange; strTag: string;
  action: TTagAction; clFontColor: TColor; iFontSize: integer);
var
  startRange, endRange, withRange: PowerPoint_TLB.TextRange;
  iStart: integer;
  iStartRange, iEndRange: integer;
begin
  iStart := 0;
  while iStart >= 0 do begin

    startRange := txtrange.Find(strTag, iStart, msoFalse, msoFalse);
    if Assigned(startRange) and (startRange.Length > 0) then begin
      iStartRange := startRange.Start - txtrange.Start + 1 + 3;
      endRange := txtrange.Find(strTag, iStartRange, msoFalse, msoFalse);
      if assigned(endRange) and (endRange.Length > 0) then begin
        iEndRange := endRange.Start - txtrange.Start;
        iStart := endRange.Start - txtrange.Start + 3;
      end else begin
        iEndRange := txtrange.Length;
        iStart := -1; // stop
      end;
      withRange := txtrange.Characters(iStartRange, iEndRange - iStartRange + 1 );
      case action of
        taBold: withRange.Font.Bold := msoTrue;
        taItalic: withRange.Font.Italic := msoTrue;
        taUnderline: withRange.Font.Underline := msoTrue;
        taSuperscript: withRange.Font.Superscript := msoTrue;
        taSubscript: withRange.Font.Subscript := msoTrue;
        taColor: withRange.Font.Color.RGB := clFontColor;
        taSize: withRange.Font.Size := iFontSize;
        taBullet: begin
          withRange.ParagraphFormat.Bullet.type_ := ppBulletUnnumbered;
          withRange.ParagraphFormat.Bullet.Visible := msoTrue;
        end;
        taCenter: withRange.ParagraphFormat.Alignment := ppAlignCenter;
      end;

    end else begin
      iStart := -1; // stop
    end;
  end;
  while txtrange.Replace(strTag, '', 0, msoFalse, msoFalse) <> nil do ;
end;

procedure LayoutTextFrame( txtframe: PowerPoint_TLB.TextFrame);
var
  txtRange: TextRange;
//  i: integer;
begin
//  i := 1;
//  txtRange := txtframe.TextRange.Lines(i, 1);
//  txtRange := txtframe.TextRange.Paragraphs(i, 1);
  txtRange := txtframe.TextRange;
//  while assigned(txtRange) and (txtRange.Start <= txtframe.TextRange.Length) do begin
    LayoutTextRange( txtRange, '<b>', taBold, 0, 0 );
    LayoutTextRange( txtRange, '<i>', taItalic, 0, 0 );
    LayoutTextRange( txtRange, '<u>', taUnderline, 0, 0 );
    LayoutTextRange( txtRange, '<^>', taSuperscript, 0, 0 );
    LayoutTextRange( txtRange, '<v>', taSubscript, 0, 0 );
    LayoutTextRange( txtRange, '<center>', taCenter, 0, 0 );
    LayoutTextRange( txtRange, '<red>', taColor, TColors.Red, 0 );
    LayoutTextRange( txtRange, '<blue>', taColor, TColors.Blue, 0 );
    LayoutTextRange( txtRange, '<black>', taColor, TColors.Black, 0 );
    LayoutTextRange( txtRange, '<white>', taColor, TColors.White, 0 );
    LayoutTextRange( txtRange, '<grey>', taColor, TColors.Grey, 0 );
    LayoutTextRange( txtRange, '<green>', taColor, TColors.Green, 0 );
    LayoutTextRange( txtRange, '<14>', taSize, 0, 14 );
    LayoutTextRange( txtRange, '<28>', taSize, 0, 28 );
    LayoutTextRange( txtRange, '<32>', taSize, 0, 32 );
    LayoutTextRange( txtRange, '<36>', taSize, 0, 36 );
    LayoutTextRange( txtRange, '<40>', taSize, 0, 40 );
    LayoutTextRange( txtRange, '<44>', taSize, 0, 44 );
    LayoutTextRange( txtRange, '<48>', taSize, 0, 48 );
    LayoutTextRange( txtRange, '<52>', taSize, 0, 52 );
    LayoutTextRange( txtRange, '<+>', taBullet, 0, 0 );
//    inc(i);
    //txtRange := txtframe.TextRange.Lines(i, 1);
//    txtRange := txtframe.TextRange.Paragraphs(i, 1);
//  end;
end;


procedure BuildSlide(presentation: PowerPointPresentation;
  slide: TSlide; project: TProject; iCurrentSlide: integer);
var
  iNewPPTSlideCount, iPPTSlide: integer;
  pptSlide, pptSourceSlide: PowerPointSlide;
  pptShape: PowerPoint_TLB.Shape;

  strContentDir, strFileName: string;
  strContentText: string;

  slideItemContent, slideItemArea: TSlideItem;
  layoutItem: TLayoutItem;

  iTextWidth: integer;
  rcArea: TRect;

  iAreaIndex: integer;
  strArea: string;
  rectDestination, rectSource: TRect;
  dScaleWidth, dScaleHeight: single;

  overview: TOverview;
  iFontSizeNeeded: integer;

  ribbon: TRibbon;
  strRibbonFileName: string;
  sourceInfo, sourceInfoContent: TSourceInfo;

  template, templateSub: TSlideTemplate;
  slideSub: TSlide;

begin
  strContentDir := GetSettings.GetContentDir;

  template := GetSlideTemplates.FindByName(slide.SlideTemplateName);
  if not Assigned(template) then
    Exit;

  // calculate nr of PPT slides
  slideItemContent := slide['content'];
  if not Assigned(slideItemContent) then begin
    beep;
    Exit;
  end;
  iNewPPTSlideCount := slideItemContent.ContentSources.Count;

  if iNewPPTSlideCount = 0 then begin
    beep;
    Exit;
  end;

  ribbon := TRibbon.create(project, strContentDir);
  try
    for iPPTSlide := 1 to iNewPPTSlideCount do begin
      sourceInfoContent := slideItemContent.ContentSources[iPPTSlide-1];
      if ((slideItemContent.ContentType in [ctExtSlide])
          or (sourceInfoContent.ContentTypeOverride and (sourceInfoContent.SourceType = sitPPT))
          or ((epAllowExternalPPTs in template.EditPossibilities) and (iPPTSlide > 1))) then begin
        if iPPTSlide <= slideItemContent.ContentSources.Count then begin
          if (sourceInfoContent.FileName <> '') and (sourceInfoContent.SourceType = sitPPT) then begin
            pptSourceSlide := GetCachedPPTs.GetSlide(sourceInfoContent);
            if Assigned(pptSourceSlide) then begin
              pptSourceSlide.Copy;
              presentation.Slides.Paste(presentation.Slides.Count + 1);
              pptSlide := presentation.Slides.Item(presentation.Slides.Count);
              pptSlide.Design := pptSourceSlide.Design;
              pptSlide.ColorScheme := pptSourceSlide.ColorScheme;
            end;
          end;
        end;
      end else if (sourceInfoContent.SourceType = sitTemplate) and (sourceInfoContent.Text = 'SUBS') then begin
        BuildPowerpointSUBS(presentation, project);
      end else if (sourceInfoContent.SourceType = sitTemplate) then begin
        templateSub := GetSlideTemplates.FindByName(ReplaceSpecials(sourceInfoContent.Text, project, slide));
        if Assigned(templateSub) then begin
          slideSub := templateSub.DoOnAdd(false);
          try
            if Assigned(slideSub) then begin
              slideSub.InternalHideRibbon := true;
              BuildSlide(presentation, slideSub, project, iCurrentSlide );
            end;
          finally
            slideSub.Free;
          end;
        end;
      end else begin
        pptSlide := presentation.Slides.Add(presentation.Slides.Count + 1, ppLayoutBlank);

        pptSlide.FollowMasterBackground := msoFalse;
        pptSlide.Background.Fill.Solid;
        pptSlide.Background.Fill.ForeColor.RGB := 0;

        if gl_StartColorScheme = nil then
          gl_StartColorScheme := pptSlide.ColorScheme
        else
          pptSlide.ColorScheme := gl_StartColorScheme;

        if gl_StartDesign = nil then
          gl_StartDesign := pptSlide.Design
        else
          pptSlide.Design := gl_StartDesign;

        layoutItem := slide.Layout['ribbon'];
        if Assigned(layoutItem) and not slide.InternalHideRibbon then begin
          strRibbonFileName := ribbon.Build(iCurrentSlide, layoutItem.Area);
        end;

        for iAreaIndex := 0 to slide.Count -1 do begin
          strArea := slide.KeyOfIndex[iAreaIndex];

          slideItemArea := slide[strArea];
          layoutItem := slide.Layout[strArea];
          if Assigned(slideItemArea) and (slideItemArea.ContentSources.Count > 0) and Assigned(layoutItem) and layoutItem.Visible then begin
             // use last available ContentText item
             if iPPTSlide <= slideItemArea.ContentSources.Count then
               sourceInfo := slideItemArea.ContentSources[iPPTSlide -1]
             else
               sourceInfo := slideItemArea.ContentSources[slideItemArea.ContentSources.Count -1];

            if slideItemArea.ContentType in [ctPicture, ctPictureFit, ctRibbon] then begin

              if slideItemArea.ContentType in [ctRibbon] then begin
                strFileName := strRibbonFileName;
              end else begin
                if sourceInfo.SourceType = sitPPT then begin
                  strFileName := GetTempDir + 'shape-5A257AC3-73C8-472E-A424-85D08C0A43FD.png';
                  pptShape := GetCachedPPTs.GetShape(sourceInfo);
                  if Assigned(pptShape) then begin
                    pptShape.Export(strFileName, ppShapeFormatPNG, round(pptShape.Width), round(pptShape.Height), ppScaleToFit);
                  end else begin
                    continue;
                  end;
                end else begin
                  strFileName := Getsettings.DirExplode(sourceInfo.FileName);
                end;
              end;

              rectDestination := layoutItem.Area;
              if slideItemArea.ContentType  in [ctPictureFit, ctRibbon] then begin
                rectSource := ReadImageSize(strFileName);

                dScaleWidth := rectSource.Width / rectDestination.Width;
                dScaleHeight := rectSource.Height / rectDestination.Height;

                if (dScaleWidth < 1) and (dScaleHeight < 1) then begin
                  rectDestination.Left := layoutItem.Area.CenterPoint.X - trunc((rectSource.Width) / 2);
                  rectDestination.Right := rectDestination.Left + trunc((rectSource.Width) );
                  rectDestination.Top := layoutItem.Area.CenterPoint.Y - trunc((rectSource.Height) / 2);
                  rectDestination.Bottom := rectDestination.Top + trunc((rectSource.Height) );
                end else if dScaleWidth < dScaleHeight then begin
                  rectDestination.Left := layoutItem.Area.CenterPoint.X - trunc((rectSource.Width / dScaleHeight) / 2);
                  rectDestination.Right := rectDestination.Left + trunc((rectSource.Width / dScaleHeight) );
                end else if dScaleWidth > dScaleHeight then begin
                  rectDestination.Top := layoutItem.Area.CenterPoint.Y - trunc((rectSource.Height / dScaleWidth) / 2);
                  rectDestination.Bottom := rectDestination.Top + trunc((rectSource.Height / dScaleWidth) );
                end;
              end;
              if FileExists(strFileName) then begin
                pptShape := pptSlide.Shapes.AddPicture(strFileName, msoFalse, msoTrue,
                  rectDestination.Left, rectDestination.Top,
                  rectDestination.Width, rectDestination.Height);
              end;
            end else if slideItemArea.ContentType in [ctText, ctTextMemo, ctOverview, ctOverviewSubs] then begin

              strContentText := sourceInfo.Text;

              if slide.AutomaticSmallNumbers and (strArea = 'content') then begin
                strContentText := ReplaceAutomaticSmallNumbers(strContentText);
              end;

              iFontSizeNeeded := layoutItem.FontSize;
              rcArea := layoutItem.Area;
              if layoutItem.Autosize then begin
                iTextWidth := layoutItem.TextWidth(strContentText);
                case layoutItem.AreaAlignment of
                  alCenter: begin
                      rcArea.Left := layoutItem.Area.Left + iTextWidth div 2;
                      rcArea.Width := iTextWidth;
                    end;
                  alRight: begin
                      rcArea.Left := layoutItem.Area.Right - iTextWidth;
                    end;
                end;
                rcArea.Width := iTextWidth;
              end;
              if layoutItem.UseBackground then begin
                pptShape := pptSlide.Shapes.AddShape(msoShapeRectangle,
                  rcArea.Left, rcArea.Top,
                  rcArea.Width, rcArea.Height);
                pptShape.Fill.Solid;
                pptShape.Fill.ForeColor.RGB := layoutItem.BackgroundColor;
                pptShape.Line.Visible := msoFalse;
              end else begin
                pptShape := pptSlide.Shapes.AddTextbox(msoTextOrientationHorizontal,
                  rcArea.Left, rcArea.Top,
                  rcArea.Width, rcArea.Height);
                if layoutItem.Autosize then begin
                  Assert(false, 'not tested');
                  pptShape.TextFrame.AutoSize := msoTrue
                end else
                  pptShape.TextFrame.AutoSize := msoFalse;
              end;
              if slideItemArea.ContentType in [ctOverview, ctOverviewSubs] then begin

                overview := TOverview.create(project);
                try
                  strContentText := overview.Build(iCurrentSlide, slide, slideItemArea.ContentType = ctOverviewSubs, iFontSizeNeeded);
                finally
                  overview.Free;
                end;
                pptShape.TextFrame.Ruler.TabStops.Add(ppTabStopLeft, 100);
                pptShape.TextFrame.Ruler.TabStops.Add(ppTabStopLeft, 170);
              end;
              pptShape.TextFrame.TextRange.Text := ReplaceSpecials(strContentText, project, slide);
              pptShape.TextFrame.TextRange.Font.Color.RGB := layoutItem.FontColor;
              pptShape.TextFrame.TextRange.Font.Name := GetSettings.FontName;
              pptShape.TextFrame.TextRange.Font.Size := iFontSizeNeeded;
              if layoutItem.Autosize then begin
                pptShape.TextFrame.TextRange.ParagraphFormat.Alignment := ppAlignCenter;
              end else begin
                case layoutItem.AreaAlignment of
                  alLeft: pptShape.TextFrame.TextRange.ParagraphFormat.Alignment := ppAlignLeft;
                  alCenter: pptShape.TextFrame.TextRange.ParagraphFormat.Alignment := ppAlignCenter;
                  alRight: pptShape.TextFrame.TextRange.ParagraphFormat.Alignment := ppAlignRight;
                end;
              end;
//              if (slideItem.ContentType <> ctOverview) and (slide.OverviewType in [otIgnore]) then begin
//                pptShape.TextFrame.Ruler.Levels.Item(1).LeftMargin := 20;
//                pptShape.TextFrame.Ruler.Levels.Item(1).FirstMargin := 0;
//              end;

              LayoutTextFrame(pptShape.TextFrame);
            end;
          end;
        end;
      end;
    end;
  finally
    ribbon.Free;
  end;
end;

procedure BuildPowerpointSUBS(presentation: PowerPointPresentation; project: TProject);
var
  i: integer;
  slide: TSlide;

  slideOverview: TSlide;
  template: TSlideTemplate;
begin

  template := GetSlideTemplates.FindByName(CTEMPLATE_OVERVIEW_SUBS);
  slideOverview := template.DoOnAdd(false);
  try
    for i := 0 to project.Slides.Count -1 do begin
      slide := TSlide.Create(project.Slides[i]);
      try
        if slide.OverviewType in [otSong, otReading, otText] then begin
          if slide.IsSubOverview then begin
            slide.InternalHideRibbon := true;
            BuildSlide(presentation, slide, project, i);
            if slide.OverviewType in [otSong] then begin
              BuildSlide(presentation, slideOverview, project, i+1);
            end;
          end;
        end;
      finally
        slide.Free;
      end;
    end;
  finally
    slideOverview.Free;
  end;
end;

procedure BuildPowerpoint(strFileName: string; project: TProject);
var
  app: PowerPointApplication;
  presentation: PowerPointPresentation;
  i: Integer;
  slide, slideNext: TSlide;

  slideOverview: TSlide;
  template, templateNext: TSlideTemplate;

  feedback: TFeedbackForm;
begin
  SetWaitCursor();
  feedback := TFeedbackForm.CreateNew(Application.MainForm);
  try
    feedback.PB_Max := project.Slides.Count;
    feedback.Show;
    feedback.MessageString := _('Create Powerpoint...');
    app := CoPowerPointApplication.Create;
    presentation := app.Presentations.Add(msoFalse);

    gl_StartColorScheme := nil;
    gl_StartDesign := nil;

    project.AfterCollecte := '$$$';

  //  presentation.PageSetup.SlideSize := ppSlideSizeCustom;
  //  presentation.PageSetup.SlideWidth := 800;
  //  presentation.PageSetup.SlideHeight := 600;
    presentation.PageSetup.SlideSize := ppSlideSizeOnScreen; // 720x540

    presentation.PageSetup.FirstSlideNumber := 1;
    presentation.PageSetup.SlideOrientation := msoOrientationHorizontal;
    presentation.PageSetup.NotesOrientation := msoOrientationVertical;

    template := GetSlideTemplates.FindByName(CTEMPLATE_OVERVIEW);
    slideOverview := template.DoOnAdd(false);
    try
      BuildSlide(presentation, slideOverview, project, -1);
      for i := 0 to project.Slides.Count -1 do begin
        feedback.PB_Position := i;
        slide := TSlide.Create(project.Slides[i]);
        try
          if slide.IsSubOverview then begin
            continue;
          end;
          templateNext := nil;
          if (i+1) < project.Slides.Count -1 then begin
            slideNext := TSlide.Create(project.Slides[i+1]);
            try
              templateNext := nil;
              if Assigned(slideNext) then begin
                templateNext := GetSlideTemplates.FindByName(slideNext.SlideTemplateName);
              end;
            finally
              slideNext.Free;
            end;
          end;

          BuildSlide(presentation, slide, project, i);

          template := GetSlideTemplates.FindByName(slide.SlideTemplateName);
          if not (Assigned(templateNext) and templateNext.NoPreviousOverview) and Assigned(template) and template.FollowedByOverview then begin
            BuildSlide(presentation, slideOverview, project, i+1);
          end;
        finally
          slide.Free;
        end;
      end;
    finally
      slideOverview.Free;
    end;
  finally
    feedback.Free;
  end;

  presentation.SaveAs(strFileName, ppSaveAsPresentation, msoFalse );
  presentation.Close;
  presentation := nil;
  app.Quit;
  app := nil;
  Application.ProcessMessages;
  if FileExists(strFileName) then
    ShellExecute(0, '', PChar(strFileName), '', '', SW_SHOWNORMAL);
end;

end.
