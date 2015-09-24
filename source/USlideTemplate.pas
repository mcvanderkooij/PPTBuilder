unit USlideTemplate;

interface

uses
  System.Generics.Collections, System.Generics.Defaults, Data.DBXJSON,
  USlide, UFastKeysSS, USourceInfo, UUtilsJSon;

const
  CTEMPLATE_COLLECTE = 'Collecte';
  CTEMPLATE_OVERVIEW = 'Overzicht';
  CTEMPLATE_OVERVIEW_SUBS = 'Overzicht Formulier';
  CTEMPLATE_READING_AGB = 'Lezen Apostolische Geloofsbelijdenis';
  CTEMPLATE_READING_DL = 'Lezen Dortse Leerregels';
  CTEMPLATE_READING_GBA = 'Lezen Geloofsbelijdenis van Athanasius';
  CTEMPLATE_READING_HC = 'Lezen Heidelbergse Catechismus';
  CTEMPLATE_TEXT_AGB = 'Tekst Apostolische Geloofsbelijdenis';
  CTEMPLATE_TEXT_DL = 'Tekst Dortse Leerregels';
  CTEMPLATE_TEXT_GBA = 'Tekst Geloofsbelijdenis van Athanasius';
  CTEMPLATE_TEXT_HC = 'Tekst Heidelbergse Catechismus';

  CCONTENT_COLLECTE = #13#13#13'<40>1<^>e<^> collecte:'#13'%collecte1%'#13#13'2<^>e<^> collecte:'#13'%collecte2%';

type
  TEditPossibility = (epFixed, epAllowExternalPPTs, epSinglePage,
    epIsSong, epSongIsMemo, epSongIsPPT, epSongIsPicture,
    epIsText, epIsBook, epBookMulti,
    epMemoMulti, epSlideVariables);
  TEditPossibilities = set of TEditPossibility;

  TSlideTemplate = class(TJSONPersistent)
  private
    FName: string;
    FSlideLayoutName: string;
    FAreaData: TSlideItems;
    FSelectContentSubDir: string;
    FOverviewType: TOverviewType;
    FPictoName: TSourceInfo;
    FFollowedByOverview: boolean;
    FOverviewName: string;
    FEditPossibilities: TEditPossibilities;
    FCategoryName: string;
    FNoPreviousOverview: boolean;
    FVariableOptions: TFastKeyValuesSS;
    FVariableDefaults: TFastKeyValuesSS;
    FMenuOrder: integer;
    FIsSubOverview: boolean;
    function GetName: string;
    function GetSlideLayoutName: string;
    function GetVariableOptions: TFastKeyValuesSS;
    function GetVariableDefaults: TFastKeyValuesSS;
    procedure SetPictoName(const Value: TSourceInfo);
  protected
    function GetAsJSonObject: TJSONObject; override;
    procedure SetAsJSonObject(const Value: TJSONObject); override;
  public
    constructor Create(strName, strCategoryName: string; strSlideLayoutName: string);
    destructor Destroy; override;
    function DoOnAdd(blnDoEdit: boolean): TSlide;
    function DoOnEdit(slide: TSlide): boolean;

    property Name: string read GetName;
    property MenuOrder: integer read FMenuOrder write FMenuOrder;
    property CategoryName: string read FCategoryName;
    property SlideLayoutName: string read GetSlideLayoutName;
    property EditPossibilities: TEditPossibilities read FEditPossibilities write FEditPossibilities;
    property OverviewType: TOverviewType read FOverviewType write FOverviewType;
    property OverviewName: string read FOverviewName write FOverviewName;
    property IsSubOverview: boolean read FIsSubOverview write FIsSubOverview;
    property FollowedByOverview: boolean read FFollowedByOverview write FFollowedByOverview;
    property NoPreviousOverview: boolean read FNoPreviousOverview write FNoPreviousOverview;
    property PictoName: TSourceInfo read FPictoName write SetPictoName;
    property SelectContentSubDir: string read FSelectContentSubDir write FSelectContentSubDir;

    property VariableOptions: TFastKeyValuesSS read GetVariableOptions;
    property VariableDefaults: TFastKeyValuesSS read GetVariableDefaults;

    property AreaData: TSlideItems read FAreaData write FAreaData;
  end;

  TSlideTemplateComparer = class(TComparer<TSlideTemplate>)
  public
    function Compare(const Left, Right: TSlideTemplate): Integer; override;
  end;

  TSlideTemplates = class(TObjectList<TSlideTemplate>)
  public
    function Add(strName, strCategoryName, strSlideLayoutName: string; iMenuOrder: integer): TSlideTemplate;
    function FindByName(strName: string): TSlideTemplate;

    function GetTemplateNames(strCategory: string; blnIncludeNone: boolean): string;

    procedure SaveSlideTemplates;
    procedure LoadSlideTemplates;
  end;

function GetSlideTemplates: TSlideTemplates;
procedure FillSlideTemplates;

implementation

uses
  Forms, Dialogs, SysUtils, Controls, GNUGetText, Classes,
  USlideLayout, UfrmPictureSelector, UUtilsForms, UUtilsStrings,
  UfrmEditSong, UfrmEditText, UfrmEditExtSlide, UfrmEditSinglePage,
  UfrmEditBook,
  UStringLogicalComparer, USourcePPT, USettings, UUtils,
  UfrmEditVariables;

var
  gl_SlideTemplates: TSlideTemplates = nil;

function GetSlideTemplates: TSlideTemplates;
begin
  if not Assigned(gl_SlideTemplates) then begin
    gl_SlideTemplates := TSlideTemplates.Create(TSlideTemplateComparer.Create);

    FillSlideTemplates;
//    gl_SlideTemplates.SaveSlideTemplates;

//    gl_SlideTemplates.LoadSlideTemplates;

    gl_SlideTemplates.Sort;
  end;
  Result := gl_SlideTemplates;
end;

procedure FillSlideTemplates;
var
  template: TSlideTemplate;
  source: TSourceInfo;
  i: integer;
  strI: string;
  oSources: TSourceInfos;
  iMenuOrder: integer;
begin
  // geen
  iMenuOrder := 0;
  template := gl_SlideTemplates.Add(CTEMPLATE_OVERVIEW, '', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, '%speaker%');
  template.AreaData.AddSlideItemString('content', ctOverview, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('');
  template.EditPossibilities := [epFixed];

  template := gl_SlideTemplates.Add(CTEMPLATE_OVERVIEW_SUBS, '', 'Content-layout', iMenuOrder);
  //template.AreaData.AddSlideItemString('footer', ctText, '%speaker%');
  template.AreaData.AddSlideItemString('content', ctOverviewSubs, '');
  template.PictoName := TSourceInfo.CreateAsFileName('');
  template.EditPossibilities := [epFixed];

  //Algemeen
  iMenuOrder := 10000;
  template := gl_SlideTemplates.Add('Welkom', 'Algemeen', 'Titlepage-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Welkom in de Kandelaarkerk te Stadskanaal');
  template.AreaData.AddSlideItemFileName('content', ctPicture, '<content>pictures\kerk-zon.jpg');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kerkenraad.png');
  template.SelectContentSubDir := 'pictures';
  template.EditPossibilities := [epSinglePage];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Afscheid - week', 'Algemeen', 'Titlepage-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Tot volgende week D.V.');
  template.AreaData.AddSlideItemFileName('content', ctPicture, '<content>pictures\kerk-zon.jpg');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kerkenraad.png');
  template.SelectContentSubDir := 'pictures';
  template.FollowedByOverview := False;
  template.EditPossibilities := [epSinglePage];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Afscheid - 16.30', 'Algemeen', 'Titlepage-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Tot vanmiddag 16.30 uur D.V.');
  template.AreaData.AddSlideItemFileName('content', ctPicture, '<content>pictures\kerk-zon.jpg');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kerkenraad.png');
  template.SelectContentSubDir := 'pictures';
  template.FollowedByOverview := False;
  template.EditPossibilities := [epSinglePage];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Afscheid - 19.00', 'Algemeen', 'Titlepage-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Tot vanmiddag 19.00 uur D.V.');
  template.AreaData.AddSlideItemFileName('content', ctPicture, '<content>pictures\kerk-zon.jpg');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kerkenraad.png');
  template.SelectContentSubDir := 'pictures';
  template.FollowedByOverview := False;
  template.EditPossibilities := [epSinglePage];

  inc(iMenuOrder, 10);
  oSources := TSourceInfos.Create;
  for i := 0 to 2 do begin
    source := TSourceInfo.Create;
    source.SourceType := sitPPT;
    source.Description := '';
    source.Text := '';
    source.FileName := '<content>ppts\AED in geval van nood.ppt';
    source.SlideName := 'Slide'+intToStr(i+2);
    source.ShapeName := '';
    oSources.Add(source);
  end;
  template := gl_SlideTemplates.Add('AED in geval van nood', 'Algemeen', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItem('content', ctExtSlide, oSources);
  template.PictoName := TSourceInfo.CreateAsFileName('');
  template.FollowedByOverview := False;
  template.NoPreviousOverview := true;
  template.EditPossibilities := [epFixed];
  template.SelectContentSubDir := 'ppts';

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Mededelingen', 'Algemeen', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Mededelingen');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\mededelingen.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\mededelingen.png');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epFixed];
  template.FollowedByOverview := False;

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Mededelingen Kerkenraad', 'Algemeen', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Mededelingen');
  template.AreaData.AddSlideItemPPT('content', ctExtSlide, '<content>ppts\Mededelingen Kerkenraad.ppt', 'Slide3', '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\mededelingen.png');
  template.SelectContentSubDir := 'ppts';
  template.EditPossibilities := [epFixed];
  template.FollowedByOverview := False;
  template.NoPreviousOverview := true;

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Aangepast - tekstblad', 'Algemeen', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, '');
  template.AreaData.AddSlideItemString('content', ctTextMemo, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Bijbel.png');
  template.EditPossibilities := [epMemoMulti];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Aangepast - pictoblad', 'Algemeen', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, '');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epSinglePage];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Externe Powerpoint', 'Algemeen', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItemPPT('content', ctExtSlide, '', '', '');
  template.PictoName := TSourceInfo.CreateAsFileName('');
  template.OverviewType := otIgnore;
  template.EditPossibilities := [epAllowExternalPPTs];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Zwart beeld', 'Algemeen', 'Form-layout', iMenuOrder);
  //template.AreaData.AddSlideItemString('footer', ctText, 'Mededelingen');
  template.AreaData.AddSlideItemString('content', ctText, '');
  //template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
//  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\mededelingen.png');
  template.SelectContentSubDir := 'pictos';
//  template.EditPossibilities := [epFixed];
  template.EditPossibilities := [epFixed];
  template.FollowedByOverview := False;



  // Liturgisch
  iMenuOrder := 20000;
  template := gl_SlideTemplates.Add('Votum en zegengroet', 'Liturgisch', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Votum en zegengroet');
  template.AreaData.AddSlideItemFileName('content1', ctPictureFit, '<content>songs\Votum.png');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\zegening.png');
  template.AreaData.AddSlideItemFileName('content3', ctPictureFit, '<content>songs\Amen.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\zegening.png');
  template.SelectContentSubDir := 'songs';
  template.EditPossibilities := [epFixed];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Preek', 'Liturgisch', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, '');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\preek.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\preek.png');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epAllowExternalPPTs];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Schuldbelijdenis en Genadeverkondiging', 'Liturgisch', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Schuldbelijdenis en Genadeverkondiging');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\verootmoediging.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\verootmoediging.png');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epFixed];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Schuldbelijdenis', 'Liturgisch', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Schuldbelijdenis');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\verootmoediging.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\verootmoediging.png');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epSinglePage];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Genadeverkondiging', 'Liturgisch', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Genadeverkondiging');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\verootmoediging.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\verootmoediging.png');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epSinglePage];

  // collecte
  iMenuOrder := 30000;
  oSources := TSourceInfos.Create;
  oSources.Add(TSourceInfo.CreateAsString(CCONTENT_COLLECTE));
  oSources.Add(TSourceInfo.CreateAsPPT('<content>ppts\Mededelingen Kerkenraad.ppt', 'Slide3', '', true));
  oSources.Add(TSourceInfo.CreateAsString(CCONTENT_COLLECTE));
  template := gl_SlideTemplates.Add(CTEMPLATE_COLLECTE, 'Liturgisch', 'Center-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Na de collecten %aftercollecte%');
  template.AreaData.AddSlideItem('content', ctTextMemo, oSources);
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\collecte 3.png');
  template.FollowedByOverview := False;
  template.EditPossibilities := [epFixed];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Zegen Gezongen Amen', 'Liturgisch', 'Content2-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Gezongen Amen');
  template.AreaData.AddSlideItemFileName('content1', ctPictureFit, '<content>pictos\zegening.png');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>songs\Amen-amen-amen.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\zegening.png');
  template.FollowedByOverview := False;
  template.NoPreviousOverview := true;
  template.EditPossibilities := [epFixed];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Zegen Gezongen Amen Lb 456 vs 3', 'Liturgisch', 'Content2-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Lb 456 : 3');
  template.AreaData.AddSlideItemFileName('content1', ctPictureFit, '<content>pictos\zegening.png');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>songs\Lb-456-vs-3-Amen.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\zegening.png');
  template.FollowedByOverview := False;
  template.NoPreviousOverview := true;
  template.OverviewType := otIgnore;
  template.OverviewName := 'Lb 456:3';
  template.EditPossibilities := [epFixed];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Zegen Gezongen Amen Ps 89 vs 18 laatste regel', 'Liturgisch', 'Content2-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Ps 89 : 18 laatste regel');
  template.AreaData.AddSlideItemFileName('content1', ctPictureFit, '<content>pictos\zegening.png');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>songs\Ps-89-vs-18-laatste-regel.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\zegening.png');
  template.FollowedByOverview := False;
  template.NoPreviousOverview := true;
  template.OverviewType := otIgnore;
  template.OverviewName := 'Ps 89:18(slot)';
  template.EditPossibilities := [epFixed];

  // Zingen
  iMenuOrder := 40000;
  template := gl_SlideTemplates.Add('Zingen-zit-PPT', 'Zingen', 'Songs-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, '');
  template.AreaData.AddSlideItemString('footer-left', ctText, '');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.AreaData.AddSlideItemString('remark', ctText, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\zingen uit kerkboek zittend.png');
  template.OverviewType := otSong;
  template.EditPossibilities := [epSongIsPPT, epIsSong];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Zingen-staan-PPT', 'Zingen', 'Songs-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, '');
  template.AreaData.AddSlideItemString('footer-left', ctText, '');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.AreaData.AddSlideItemString('remark', ctText, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\zingen uit kerkboek staand.png');
  template.OverviewType := otSong;
  template.EditPossibilities := [epSongIsPPT, epIsSong];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Zingen-bidden-PPT', 'Zingen', 'Songs-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, '');
  template.AreaData.AddSlideItemString('footer-left', ctText, '');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.AreaData.AddSlideItemString('remark', ctText, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\zingend bidden.png');
  template.OverviewType := otSong;
  template.EditPossibilities := [epSongIsPPT, epIsSong];

//  inc(iMenuOrder, 10);
//  template := gl_SlideTemplates.Add('Zingen-plaatjes', 'Zingen', 'Songs-layout', iMenuOrder);
//  template.AreaData.AddSlideItemString('footer', ctText, '');
//  template.AreaData.AddSlideItemString('footer-left', ctText, '');
//  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '');
//  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
//  template.AreaData.AddSlideItemString('remark', ctText, '');
//  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\zingen uit kerkboek zittend.png');
//  template.SelectContentSubDir := 'songs';
//  template.OverviewType := otSong;
//  template.EditPossibilities := [epSongIsPicture, epIsSong];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Zingen-tekst', 'Zingen', 'Songs-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, '');
  template.AreaData.AddSlideItemString('footer-left', ctText, '');
  template.AreaData.AddSlideItemString('content', ctTextMemo, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.AreaData.AddSlideItemString('remark', ctText, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\zingen uit kerkboek zittend.png');
  template.OverviewType := otSong;
  template.EditPossibilities := [epSongIsMemo, epIsSong];

  // Bijbel
  iMenuOrder := 50000;
  template := gl_SlideTemplates.Add('Lezen', 'Schriftlezing en tekst', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Lezen');
  template.AreaData.AddSlideItemString('content', ctTextMemo, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Bijbel.png');
  template.OverviewType := otReading;
  template.EditPossibilities := [epMemoMulti, epIsText];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add(CTEMPLATE_READING_AGB, 'Schriftlezing en tekst', 'Songs-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Lezen');
  template.AreaData.AddSlideItemString('footer-left', ctText, '');
  template.AreaData.AddSlideItemString('content', ctTextMemo, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Bijbel.png');
  template.SelectContentSubDir := 'Apostolische Geloofsbelijdenis';
  template.VariableDefaults['FooterLeftPrefix'] := '';
  template.OverviewType := otReading;
  template.EditPossibilities := [epBookMulti, epIsBook];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add(CTEMPLATE_READING_DL, 'Schriftlezing en tekst', 'Songs-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Lezen');
  template.AreaData.AddSlideItemString('footer-left', ctText, '');
  template.AreaData.AddSlideItemString('content', ctTextMemo, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Bijbel.png');
  template.SelectContentSubDir := 'Dortse Leerregels';
  template.VariableDefaults['FooterLeftPrefix'] := '';
  template.OverviewType := otReading;
  template.EditPossibilities := [epBookMulti, epIsBook];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add(CTEMPLATE_READING_GBA, 'Schriftlezing en tekst', 'Songs-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Lezen');
  template.AreaData.AddSlideItemString('footer-left', ctText, '');
  template.AreaData.AddSlideItemString('content', ctTextMemo, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Bijbel.png');
  template.SelectContentSubDir := 'Geloofsbel. van Athanasius';
  template.VariableDefaults['FooterLeftPrefix'] := '';
  template.OverviewType := otReading;
  template.EditPossibilities := [epBookMulti, epIsBook];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add(CTEMPLATE_READING_HC, 'Schriftlezing en tekst', 'Songs-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Lezen');
  template.AreaData.AddSlideItemString('footer-left', ctText, '');
  template.AreaData.AddSlideItemString('content', ctTextMemo, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Bijbel.png');
  template.SelectContentSubDir := 'Heidelbergse Catechismus';
  template.VariableDefaults['FooterLeftPrefix'] := 'Vraag';
  template.OverviewType := otReading;
  template.EditPossibilities := [epBookMulti, epIsBook];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Tekst', 'Schriftlezing en tekst', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Tekst');
  template.AreaData.AddSlideItemString('footer-left', ctText, '');
  template.AreaData.AddSlideItemString('content', ctTextMemo, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Bijbel.png');
  template.OverviewType := otText;
  template.EditPossibilities := [epMemoMulti, epIsText];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add(CTEMPLATE_TEXT_AGB, 'Schriftlezing en tekst', 'Songs-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Lezen');
  template.AreaData.AddSlideItemString('footer-left', ctText, '');
  template.AreaData.AddSlideItemString('content', ctTextMemo, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Bijbel.png');
  template.SelectContentSubDir := 'Apostolische Geloofsbelijdenis';
  template.VariableDefaults['FooterLeftPrefix'] := '';
  template.OverviewType := otText;
  template.EditPossibilities := [epBookMulti, epIsBook];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add(CTEMPLATE_TEXT_DL, 'Schriftlezing en tekst', 'Songs-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Lezen');
  template.AreaData.AddSlideItemString('footer-left', ctText, '');
  template.AreaData.AddSlideItemString('content', ctTextMemo, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Bijbel.png');
  template.SelectContentSubDir := 'Dortse Leerregels';
  template.VariableDefaults['FooterLeftPrefix'] := '';
  template.OverviewType := otText;
  template.EditPossibilities := [epBookMulti, epIsBook];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add(CTEMPLATE_TEXT_GBA, 'Schriftlezing en tekst', 'Songs-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Lezen');
  template.AreaData.AddSlideItemString('footer-left', ctText, '');
  template.AreaData.AddSlideItemString('content', ctTextMemo, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Bijbel.png');
  template.SelectContentSubDir := 'Geloofsbel. van Athanasius';
  template.VariableDefaults['FooterLeftPrefix'] := '';
  template.OverviewType := otText;
  template.EditPossibilities := [epBookMulti, epIsBook];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add(CTEMPLATE_TEXT_HC, 'Schriftlezing en tekst', 'Songs-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Tekst');
  template.AreaData.AddSlideItemString('footer-left', ctText, '');
  template.AreaData.AddSlideItemString('content', ctTextMemo, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Bijbel.png');
  template.SelectContentSubDir := 'Heidelbergse Catechismus';
  template.VariableDefaults['FooterLeftPrefix'] := 'Vraag';
  template.OverviewType := otText;
  template.EditPossibilities := [epBookMulti, epIsBook];

  // Bidden
  iMenuOrder := 60000;
  template := gl_SlideTemplates.Add('Gebed', 'Bidden', 'Content3-layout', iMenuOrder);
  //template.AreaData.AddSlideItemString('footer', ctText, 'Gebed');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\bidden Christelijk.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\bidden Christelijk.png');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epFixed];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Gebed en gezongen Onze Vader', 'Bidden', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Gebed en gezongen Onze Vader');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\Gebed en gezongen Onze Vader.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Gebed en gezongen Onze Vader.png');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epFixed];

  // Wet
  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Wet - 10 geboden', 'Wet', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'De wet');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\geboden.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\geboden.png');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epFixed];

  inc(iMenuOrder, 10);
  oSources := TSourceInfos.Create;
  for i := 1 to 10 do begin
    source := TSourceInfo.Create;
    source.SourceType := sitPPT;
    source.Description := 'vers ' + intToStr(i);
    source.Text := '';
    source.FileName := '<content>ppts\Gz 176b.ppt';
    source.SlideName := 'Slide'+intToStr(i+1);
    source.ShapeName := '';
    oSources.Add(source);
  end;
  template := gl_SlideTemplates.Add('Wet Gezongen Gz 176b Mannen en vrouwen', 'Wet', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, '');
  template.AreaData.AddSlideItem('content', ctExtSlide, oSources);
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\wet zingen.png');
  template.OverviewType := otSong;
  template.OverviewName := 'Gz 176 b';
  template.EditPossibilities := [epSongIsPPT, epIsSong];
  template.SelectContentSubDir := 'ppts';

  inc(iMenuOrder, 10);
  oSources := TSourceInfos.Create;
  for i := 2 to 12 do begin
    source := TSourceInfo.Create;
    source.SourceType := sitPPT;
    source.Description := 'vers ' + intToStr(i-1);
    source.Text := '';
    source.FileName := '<content>ppts\10_geboden_in_beeld_en_lied.ppt';
    source.SlideName := 'Slide'+intToStr(i);
    source.ShapeName := '';
    oSources.Add(source);
  end;
  template := gl_SlideTemplates.Add('Wet Gezongen Het lied van de tien woorden', 'Wet', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, '');
  template.AreaData.AddSlideItem('content', ctExtSlide, oSources);
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\wet zingen.png');
  template.OverviewType := otSong;
  template.OverviewName := 'Het lied van de tien woorden';
  template.EditPossibilities := [epSongIsPPT, epIsSong];
  template.SelectContentSubDir := 'ppts';


  // Kinderen
  iMenuOrder := 70000;
  source := TSourceInfo.Create;
  source.SourceType := sitPPT;
  source.Description := '';
  source.Text := '';
  source.FileName := '<content>ppts\Kindmoment_Groep_0-6.ppt';
  source.SlideName := 'Slide2';
  source.ShapeName := '';
  template := gl_SlideTemplates.Add('Kidsbijbelclub 0-6 vanmorgen', 'Kinderen', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItem('content', ctExtSlide, source);
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kinderen.png');
  template.OverviewType := otIgnore;
  template.EditPossibilities := [epFixed];
  template.SelectContentSubDir := 'ppts';

  inc(iMenuOrder, 10);
  source := source.DeepCopy;
  source.SlideName := 'Slide4';
  template := gl_SlideTemplates.Add('Kidsbijbelclub 0-6 naar de zalen', 'Kinderen', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItem('content', ctExtSlide, source);
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kinderen.png');
  template.OverviewType := otIgnore;
  template.EditPossibilities := [epFixed];
  template.SelectContentSubDir := 'ppts';

  inc(iMenuOrder, 10);
  source := source.DeepCopy;
  source.SlideName := 'Slide3';
  template := gl_SlideTemplates.Add('Kidsbijbelclub 0-6 terug', 'Kinderen', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItem('content', ctExtSlide, source);
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kinderen.png');
  template.OverviewType := otIgnore;
  template.EditPossibilities := [epFixed];
  template.SelectContentSubDir := 'ppts';

  inc(iMenuOrder, 10);
  source := source.DeepCopy;
  source.SlideName := 'Slide3';
  template := gl_SlideTemplates.Add('Kidsbijbelclub 0-6 terug (avondmaal)', 'Kinderen', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItem('content', ctExtSlide, source);
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kinderen.png');
  template.OverviewType := otIgnore;
  template.EditPossibilities := [epFixed];
  template.SelectContentSubDir := 'ppts';
  template.IsSubOverview := true;


  // Geloofsbelijdenis
  iMenuOrder := 80000;
  template := gl_SlideTemplates.Add('Geloofsbelijdenis Zingen Gz 179a', 'Geloofsbelijdenis', 'Songs-layout', iMenuOrder);
  oSources := TSourceInfos.Create;
  oSources.Add(TSourceInfo.CreateAsString('Geloofsbelijdenis Gezang 179a'));
  oSources.Add(TSourceInfo.CreateAsString('Geloofsbelijdenis Gezang 179a'));
  oSources.Add(TSourceInfo.CreateAsString('Geloofsbelijdenis Gezang 179a'));
  template.AreaData.AddSlideItem('footer', ctText, oSources);
  oSources := TSourceInfos.Create;
  oSources.Add(TSourceInfo.CreateAsString('deel a'));
  oSources.Add(TSourceInfo.CreateAsString('deel b'));
  oSources.Add(TSourceInfo.CreateAsString('deel c'));
  template.AreaData.AddSlideItem('footer-left', ctText, oSources);
  oSources := TSourceInfos.Create;
  oSources.Add(TSourceInfo.CreateAsFileName('<content>songs\Gereformeerd Kerkboek\Gezang 179a deel 1.png'));
  oSources.Add(TSourceInfo.CreateAsFileName('<content>songs\Gereformeerd Kerkboek\Gezang 179a deel 2.png'));
  oSources.Add(TSourceInfo.CreateAsFileName('<content>songs\Gereformeerd Kerkboek\Gezang 179a deel 3.png'));
  template.AreaData.AddSlideItem('content', ctPictureFit, oSources);
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Geloofsbelijdenis zingen.png');
  template.SelectContentSubDir := 'songs';
  template.OverviewType := otSong;
  template.OverviewName := 'Gz 179 a';
  template.EditPossibilities := [epFixed, epIsSong];

  inc(iMenuOrder, 10);
  oSources := TSourceInfos.Create;
  for i := 0 to 2 do begin
    source := TSourceInfo.Create;
    source.SourceType := sitPPT;
    source.Description := '';
    source.Text := '';
    source.FileName := '<content>ppts\Gz 179a beurtzang.ppt';
    source.SlideName := 'Slide'+intToStr(i+2);
    source.ShapeName := '';
    oSources.Add(source);
  end;
  template := gl_SlideTemplates.Add('Geloofsbelijdenis Zingen Gz 179a beurtzang', 'Geloofsbelijdenis', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItem('content', ctExtSlide, oSources);
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Geloofsbelijdenis zingen.png');
  template.OverviewType := otSong;
  template.OverviewName := 'Gz 179 a';
  template.EditPossibilities := [epFixed, epIsSong];
  template.SelectContentSubDir := 'ppts';

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Geloofsbelijdenis Zingen Gz 179b', 'Geloofsbelijdenis', 'Songs-layout', iMenuOrder);
  oSources := TSourceInfos.Create;
  oSources.Add(TSourceInfo.CreateAsString('Geloofsbelijdenis Gezang 179b'));
  oSources.Add(TSourceInfo.CreateAsString('Geloofsbelijdenis Gezang 179b'));
  oSources.Add(TSourceInfo.CreateAsString('Geloofsbelijdenis Gezang 179b'));
  template.AreaData.AddSlideItem('footer', ctText, oSources);
  oSources := TSourceInfos.Create;
  oSources.Add(TSourceInfo.CreateAsString('deel a'));
  oSources.Add(TSourceInfo.CreateAsString('deel b'));
  oSources.Add(TSourceInfo.CreateAsString('deel c'));
  template.AreaData.AddSlideItem('footer-left', ctText, oSources);
  oSources := TSourceInfos.Create;
  oSources.Add(TSourceInfo.CreateAsFileName('<content>songs\Gereformeerd Kerkboek\Gezang 179b deel 1.png'));
  oSources.Add(TSourceInfo.CreateAsFileName('<content>songs\Gereformeerd Kerkboek\Gezang 179b deel 2.png'));
  oSources.Add(TSourceInfo.CreateAsFileName('<content>songs\Gereformeerd Kerkboek\Gezang 179b deel 3.png'));
  template.AreaData.AddSlideItem('content', ctPictureFit, oSources);
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Geloofsbelijdenis zingen.png');
  template.SelectContentSubDir := 'songs';
  template.OverviewType := otSong;
  template.OverviewName := 'Gz 179 b';
  template.EditPossibilities := [epFixed, epIsSong];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Geloofsbelijdenis lezen', 'Geloofsbelijdenis', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Geloofsbelijdenis');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\Geloofsbelijdenis lezen.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Geloofsbelijdenis lezen.png');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epSinglePage];


  // Formulieren
  iMenuOrder := 90000;
  for i := 1 to 5 do begin
    strI := IntToStr(i);
    template := gl_SlideTemplates.Add('Formulier Avondmaal ' + strI, 'Formulieren', 'Form-layout', iMenuOrder);
    template.AreaData.AddSlideItemString('footer', ctText, 'Avondmaalsformulier ' + strI);
    template.AreaData.AddSlideItem('content', ctTextMemo, LoadAndSplitMemoFile(GetSettings.GetDir('content') + 'forms\Avondmaalsformulier ' + strI + '.txt' ));
    template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\Avondmaal.png');
    template.OverviewName := 'Avondmaalsformulier ' + strI;
    template.FollowedByOverview := false;
    template.EditPossibilities := [epFixed];
    inc(iMenuOrder, 10);
  end;

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Avondmaal viering (meerdere tafels)', 'Formulieren', 'Form-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Avondmaal');
  template.AreaData.AddSlideItem('content', ctTextMemo, LoadAndSplitMemoFile(GetSettings.GetDir('content') + 'forms\Avondmaal viering.txt' ));
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\avondmaal.png');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epFixed];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Avondmaal picto', 'Formulieren', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Avondmaal');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\avondmaal.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\avondmaal.png');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epFixed];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Avondmaal foto', 'Formulieren', 'Content-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Avondmaal');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictures\avondmaal5.jpg');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\avondmaal.png');
  template.SelectContentSubDir := 'pictures';
  template.EditPossibilities := [epFixed];

  for i := 1 to 3 do begin
    strI := IntToStr(i);
    template := gl_SlideTemplates.Add('Formulier Doop ' + strI, 'Formulieren', 'Form-layout', iMenuOrder);
    template.AreaData.AddSlideItemString('footer', ctText, 'Doopformulier ' + strI);
    template.AreaData.AddSlideItem('content', ctTextMemo, LoadAndSplitMemoFile(GetSettings.GetDir('content') + 'forms\Doopformulier ' + strI + '.txt' ));
    template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kinderdoop.png');
    template.OverviewName := 'Doopformulier ' + strI;
    template.EditPossibilities := [epSlideVariables];
    template.VariableOptions['1 Bidden voor doop'] := gl_SlideTemplates.GetTemplateNames('Bidden', true);
    template.VariableDefaults['1 Bidden voor doop'] := 'Gebed';
    template.VariableOptions['2 Bidden na doop'] := gl_SlideTemplates.GetTemplateNames('Bidden', true);
    template.VariableDefaults['2 Bidden na doop'] := 'Gebed';
    if i = 3 then begin
      template.VariableOptions['Geloofsbelijdenis'] := gl_SlideTemplates.GetTemplateNames('Geloofsbelijdenis', true);
      template.VariableDefaults['Geloofsbelijdenis'] := 'Geloofsbelijdenis lezen';
    end;
    template.VariableDefaults['hij of zij'] := 'hij zij';
    template.VariableDefaults['zijn of haar'] := 'zijn haar';
    template.VariableDefaults['zoon of dochter'] := 'zoon dochter';
    template.VariableDefaults['hem of haar'] := 'hem haar';
    template.VariableDefaults['Vader en moeder'] := 'Vader en moeder';
    inc(iMenuOrder, 10);
  end;

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Doop picto', 'Formulieren', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Doop');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\kinderdoop.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kinderdoop.png');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epFixed];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Formulier Doop Volwassenen', 'Formulieren', 'Form-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Doopformulier Volwassenen');
  template.AreaData.AddSlideItem('content', ctTextMemo, LoadAndSplitMemoFile(GetSettings.GetDir('content') + 'forms\Doopformulier Volwassenen.txt' ));
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\volwassendoop.png');
  template.OverviewName := 'Doopformulier Volwassenen';
  template.EditPossibilities := [epSlideVariables];
  template.VariableOptions['1 Bidden voor doop'] := gl_SlideTemplates.GetTemplateNames('Bidden', true);
  template.VariableDefaults['1 Bidden voor doop'] := 'Gebed';
  template.VariableOptions['2 Bidden na doop'] := gl_SlideTemplates.GetTemplateNames('Bidden', true);
  template.VariableDefaults['2 Bidden na doop'] := 'Gebed';
  template.VariableDefaults['broeder/zuster'] := 'broeder zuster';

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Doop picto volwassen', 'Formulieren', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Doop');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\volwassendoop.png');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\volwassendoop.png');
  template.SelectContentSubDir := 'pictos';
  template.EditPossibilities := [epFixed];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Formulier Openbare geloofsbelijdenis', 'Formulieren', 'Form-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Openbare geloofsbelijdenis');
  template.AreaData.AddSlideItem('content', ctTextMemo, LoadAndSplitMemoFile(GetSettings.GetDir('content') + 'forms\Openbare geloofsbelijdenis.txt' ));
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\belijdenis doen.png');
  template.OverviewName := 'Openbare geloofsbelijdenis';
  template.EditPossibilities := [epSlideVariables];
  template.VariableDefaults['broeder(s)/zuster(s)'] := 'broeder(s)/zuster(s)';

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Formulier Bevestiging ouderlingen en diakenen', 'Formulieren', 'Form-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Bevestiging ouderlingen en diakenen');
  template.AreaData.AddSlideItem('content', ctTextMemo, LoadAndSplitMemoFile(GetSettings.GetDir('content') + 'forms\Bevestiging ouderlingen en diakenen.txt' ));
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kerkenraad.png');
  template.OverviewName := 'Bevestiging ouderlingen en diakenen';
  template.EditPossibilities := [epSlideVariables];
  template.VariableOptions['Gebed'] := gl_SlideTemplates.GetTemplateNames('Bidden', true);
  template.VariableDefaults['Gebed'] := 'Gebed';
  template.VariableDefaults['ouderling en diaken'] := 'ouderling en diaken';

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Formulier Bevestiging van dienaren van het woord', 'Formulieren', 'Form-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Bevestiging van dienaren van het woord');
  template.AreaData.AddSlideItem('content', ctTextMemo, LoadAndSplitMemoFile(GetSettings.GetDir('content') + 'forms\Bevestiging van dienaren van het woord.txt' ));
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kerkenraad.png');
  template.OverviewName := 'Bevestiging van dienaren van het woord';
  template.EditPossibilities := [epSlideVariables];
  template.VariableOptions['Gebed'] := gl_SlideTemplates.GetTemplateNames('Bidden', true);
  template.VariableDefaults['Gebed'] := 'Gebed';

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Formulier Huwelijk (1999)', 'Formulieren', 'Form-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Huwelijksformulier');
  template.AreaData.AddSlideItem('content', ctTextMemo, LoadAndSplitMemoFile(GetSettings.GetDir('content') + 'forms\Huwelijksformulier.txt' ));
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\huwelijksdienst.png');
  template.OverviewName := 'Huwelijksformulier';
  template.EditPossibilities := [epSlideVariables];
  template.VariableOptions['Gebed'] := gl_SlideTemplates.GetTemplateNames('Bidden', true);
  template.VariableDefaults['Gebed'] := 'Gebed';
  template.VariableOptions['uitspreken of antwoorden'] := 'je beloften uitspreken? (2 slides verwijderen)'#13'antwoorden op de volgende vragen:';

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Zegenbede picto', 'Formulieren', 'Content3-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Zegenbede');
  template.AreaData.AddSlideItemFileName('content1', ctPictureFit, '');
  template.AreaData.AddSlideItemFileName('content', ctPictureFit, '<content>pictos\zegening.png');
  template.AreaData.AddSlideItemFileName('content3', ctPictureFit, '');
  template.AreaData.AddSlideItemString('ribbon', ctRibbon, '');
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\zegening.png');
  template.SelectContentSubDir := 'songs';
  template.EditPossibilities := [epFixed];

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Formulier Kerkelijke tucht uitsluiting', 'Formulieren', 'Form-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Formulier voor de openbare kerkelijke tucht');
  template.AreaData.AddSlideItem('content', ctTextMemo, LoadAndSplitMemoFile(GetSettings.GetDir('content') + 'forms\Kerkelijke tucht 1 uitsluiting.txt' ));
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kerk.png');
  template.OverviewName := 'Formulier voor de openbare kerkelijke tucht';
  template.EditPossibilities := [epSlideVariables];
  template.VariableOptions['Gebed'] := gl_SlideTemplates.GetTemplateNames('Bidden', true);
  template.VariableDefaults['Gebed'] := 'Gebed';
  template.VariableDefaults['broeder zuster'] := 'broeder zuster';
  template.VariableDefaults['Hij Zij'] := 'Hij Zij';
  template.VariableDefaults['hij zij'] := 'hij zij';
  template.VariableDefaults['Zijn Haar'] := 'Zijn Haar';
  template.VariableDefaults['zijn haar'] := 'zijn haar';
  template.VariableDefaults['hem haar'] := 'hem haar';

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Formulier Kerkelijke tucht terugkeer', 'Formulieren', 'Form-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Formulier voor de openbare kerkelijke tucht');
  template.AreaData.AddSlideItem('content', ctTextMemo, LoadAndSplitMemoFile(GetSettings.GetDir('content') + 'forms\Kerkelijke tucht 2 terugkeer.txt' ));
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kerk.png');
  template.OverviewName := 'Formulier voor de openbare kerkelijke tucht';
  template.EditPossibilities := [epSlideVariables];
  template.VariableOptions['Gebed'] := gl_SlideTemplates.GetTemplateNames('Bidden', true);
  template.VariableDefaults['Gebed'] := 'Gebed';
  template.VariableDefaults['Broeder Zuster'] := 'Broeder Zuster';
  template.VariableDefaults['broeder zuster'] := 'broeder zuster';
  template.VariableDefaults['hij zij'] := 'hij zij';
  template.VariableDefaults['zijn haar'] := 'zijn haar';
  template.VariableDefaults['hem haar'] := 'hem haar';

  inc(iMenuOrder, 10);
  template := gl_SlideTemplates.Add('Formulier Kerkelijke tucht uitsluiting dooplid', 'Formulieren', 'Form-layout', iMenuOrder);
  template.AreaData.AddSlideItemString('footer', ctText, 'Formulier voor de openbare kerkelijke tucht');
  template.AreaData.AddSlideItem('content', ctTextMemo, LoadAndSplitMemoFile(GetSettings.GetDir('content') + 'forms\Kerkelijke tucht 3 uitsluiting dooplid.txt' ));
  template.PictoName := TSourceInfo.CreateAsFileName('<content>pictos\kerk.png');
  template.OverviewName := 'Formulier voor de openbare kerkelijke tucht';
  template.EditPossibilities := [epSlideVariables];
  template.VariableOptions['Gebed'] := gl_SlideTemplates.GetTemplateNames('Bidden', true);
  template.VariableDefaults['Gebed'] := 'Gebed';
  template.VariableDefaults['hij zij'] := 'hij zij';
  template.VariableDefaults['zijn haar'] := 'zijn haar';
  template.VariableDefaults['hem haar'] := 'hem haar';
end;


{ TSlideTemplate }

constructor TSlideTemplate.Create(strName, strCategoryName, strSlideLayoutName: string);
begin
  inherited Create;
  FName := strName;
  FCategoryName := strCategoryName;
  FSlideLayoutName := strSlideLayoutName;
  FEditPossibilities := [epFixed];
  FAreaData := TSlideItems.Create;
  FSelectContentSubDir := '';
  FOverviewType := otIgnore;
  FFollowedByOverview := true;
  FNoPreviousOverview := false;
  FIsSubOverview := false;
  FVariableOptions := TFastKeyValuesSS.Create;
  FVariableDefaults := TFastKeyValuesSS.Create;
  FPictoName := TSourceInfo.Create;
end;

destructor TSlideTemplate.Destroy;
begin
  FPictoName.Free;
  FVariableDefaults.Free;
  FVariableOptions.Free;
  FAreaData.Free;
  inherited;
end;

function TSlideTemplate.DoOnAdd(blnDoEdit: boolean): TSlide;
var
  bContinue: boolean;

  iArea, iVariable: integer;
  strArea: string;
begin
  Result := TSlide.Create(GetSlideLayouts.FindByName(SlideLayoutName));
  Result.SlideTemplateName := FName;
  Result.SlideName := FName;
  Result.PictoName := FPictoName.DeepCopy;
  Result.OverviewType := FOverviewType;
  Result.OverviewName := FOverviewName;
  Result.IsSubOverview := FIsSubOverview;
  Result.AutomaticSmallNumbers := (FOverviewType in [otReading, otText]) and not (epIsBook in FEditPossibilities);

  for iArea := 0 to AreaData.Count -1 do begin
    strArea := AreaData.KeyOfIndex[iArea];
    Result[strArea] := TSlideItem.Create(AreaData.ValueOfIndex[iArea].ContentType, AreaData.ValueOfIndex[iArea].ContentSources.DeepCopy);
  end;

  for iVariable := 0 to VariableDefaults.Count -1 do begin
    Result.Variables[VariableDefaults.KeyOfIndex[iVariable]] := VariableDefaults.ValueOfIndex[iVariable];
  end;

  if blnDoEdit then begin
    bContinue := DoOnEdit(Result);
    if not bContinue then begin
      FreeAndNil(Result);
    end;
  end;
end;

function TSlideTemplate.DoOnEdit(slide: TSlide): boolean;
var
  strText: string;
  iContentIndex: integer;
  areaTemplate: TSlideItem;

  strAreaName: string;
  iArea: integer;

  editFormInterface: ISlideEditForm;
  editForm: TForm;
  frmPictureSelector: TfrmPictureSelector;

begin
  Result := True;
  if epFixed in EditPossibilities then
    Exit;

  editForm := nil;

  if epIsSong in EditPossibilities then begin
    editForm := TfrmEditSong.Create(Application.MainForm);
  end else if epIsText in EditPossibilities then begin
    editForm := TfrmEditText.Create(Application.MainForm);

//  if slide.OverviewType in [otSong, otReading, otText] then begin
//    case slide.OverviewType of
//      otSong: editForm := TfrmEditSong.Create(Application.MainForm);
//      otReading,
//      otText: editForm := TfrmEditText.Create(Application.MainForm);
//    end;
  end else if epAllowExternalPPTs in EditPossibilities then begin
    editForm := TfrmEditExtSlide.Create(Application.MainForm);
  end else if epSinglePage in EditPossibilities then begin
    editForm := TfrmEditSinglePage.Create(Application.MainForm);
  end else if epMemoMulti in EditPossibilities then begin
    editForm := TfrmEditText.Create(Application.MainForm);
  end else if epBookMulti in EditPossibilities then begin
    editForm := TfrmEditBook.Create(Application.MainForm, FSelectContentSubDir);
  end else if epSlideVariables in EditPossibilities then begin
    editForm := TfrmEditVariables.Create(Application.MainForm);
  end;


  if Assigned(editForm) then begin
    try
      if Supports(editForm, ISlideEditForm, editFormInterface) then begin
        editFormInterface.SetSlideAsString(slide.AsJSon);
        Result := editFormInterface.Edit;
        if Result then begin
          slide.AsJSon := editFormInterface.GetSlideAsString;
        end;
      end;
    finally
      editFormInterface := nil;
      editForm.Free;
    end;
  end else begin

    // fallback
    if Result then begin
      strText := slide.SlideName;
      Result := InputQuery('Slidename ', 'Enter text', strText);
      if Result then begin
        slide.SlideName := strText;
      end;
    end;

    if Result and (slide.OverviewType <> otIgnore) then begin
      strText := slide.OverviewName;
      Result := InputQuery('Overzicht name', 'Enter text', strText);
      if Result then begin
        slide.OverviewName := strText;
      end;
    end;

    areaTemplate := AreaData['footer'];
    if Result and Assigned(areaTemplate) and (areaTemplate.ContentType = ctText) then begin
      for iContentIndex := 0 to slide['footer'].ContentSources.Count -1 do begin
        strText := slide['footer'].ContentSources[iContentIndex].Text;
        Result := InputQuery('Voettekst ' + IntToStr(iContentIndex), 'Enter text', strText);
        if Result then begin
          slide['footer'].ContentSources[iContentIndex] := TSourceInfo.CreateAsString(strText);
        end;
      end;
    end;

    for iArea := 0 to slide.Count -1 do begin
      strAreaName := slide.KeyOfIndex[iArea];

      areaTemplate := AreaData[strAreaName];
      if Result and Assigned(areaTemplate) and (areaTemplate.ContentType in [ctPicture, ctPictureFit]) then begin
        for iContentIndex := 0 to slide[strAreaName].ContentSources.Count -1 do begin
          frmPictureSelector := TfrmPictureSelector.Create(Application.MainForm);
          try
            frmPictureSelector.Caption := 'Select Picture for ' + strAreaName;
            frmPictureSelector.Selection := slide[strAreaName].ContentSources[iContentIndex];
            frmPictureSelector.SubDir := SelectContentSubDir;
            frmPictureSelector.ShowFooterText := false;
            Result := frmPictureSelector.ShowModal = mrOK;
            if Result then begin
              slide[strAreaName].ContentSources[iContentIndex] := frmPictureSelector.Selection;
            end;
          finally
            frmPictureSelector.Free;
          end;
        end;
      end;
      if Result and Assigned(areaTemplate) and (areaTemplate.ContentType = ctTextMemo) then begin
        for iContentIndex := 0 to slide[strAreaName].ContentSources.Count -1 do begin
          strText := slide[strAreaName].ContentSources[iContentIndex].Text;
          Result := ShowMemoDlg(strAreaName, strText, 'Enter text', '', '');
          if Result then begin
            slide[strAreaName].ContentSources[iContentIndex] := TSourceInfo.CreateAsString(strText);
          end;
        end;
      end;
    end;
  end;
end;

function TSlideTemplate.GetAsJSonObject: TJSONObject;
var
  strEditPossibilities: string;
begin
  Result := inherited GetAsJSonObject;
  Result.AddPair('Name', EscapeString(FName));
  Result.AddPair('MenuOrder', TJSONNumber.Create(FMenuOrder));
  Result.AddPair('CategoryName', EscapeString(FCategoryName));
  Result.AddPair('SlideLayoutName', EscapeString(FSlideLayoutName));
  Result.AddPair('AreaData', FAreaData.AsJSonObject);
  Result.AddPair('SelectContentSubDir', EscapeString(FSelectContentSubDir));
  Result.AddPair('OverviewType', TJSONNumber.Create(ord(FOverviewType)));
  Result.AddPair('PictoName', FPictoName.AsJSonObject);
  Result.AddPair(CreateBoolPair('FollowedByOverview', FFollowedByOverview));
  Result.AddPair(CreateBoolPair('NoPreviousOverview', FNoPreviousOverview));
  Result.AddPair('OverviewName', EscapeString(FOverviewName));
  Result.AddPair(CreateBoolPair('IsSubOverview', FIsSubOverview));

  strEditPossibilities := SetToString(TypeInfo(TEditPossibilities), FEditPossibilities, false);
  Result.AddPair('EditPossibilities', EscapeString(strEditPossibilities));
  Result.AddPair('VariableOptions', FVariableOptions.AsJSonObject);
  Result.AddPair('VariableDefaults', FVariableDefaults.AsJSonObject);
end;

function TSlideTemplate.GetName: string;
begin
  Result := FName;
end;

function TSlideTemplate.GetSlideLayoutName: string;
begin
  Result := FSlideLayoutName;
end;

function TSlideTemplate.GetVariableDefaults: TFastKeyValuesSS;
begin
  Result := FVariableDefaults;
end;

function TSlideTemplate.GetVariableOptions: TFastKeyValuesSS;
begin
  Result := FVariableOptions;
end;

procedure TSlideTemplate.SetAsJSonObject(const Value: TJSONObject);
var
  strEditPossibilities: string;
begin
  inherited;
  FName := UUtilsJSON.GetAsString(Value, 'Name');
  FMenuOrder := UUtilsJSON.GetAsInteger(Value, 'MenuOrder');
  FCategoryName := UUtilsJSON.GetAsString(Value, 'CategoryName');

  FSlideLayoutName := UUtilsJSON.GetAsString(Value, 'SlideLayoutName');
  FAreaData.AsJSonObject := UUtilsJSon.GetAsObject(Value, 'AreaData');
  FSelectContentSubDir := UUtilsJSON.GetAsString(Value, 'SelectContentSubDir');
  FOverviewType := TOverviewType(UUtilsJSON.GetAsInteger(Value, 'OverviewType'));
  FPictoName.AsJSonObject := UUtilsJSON.GetAsObject(Value, 'PictoName');
  FFollowedByOverview := UUtilsJSON.GetAsBoolean(Value, 'FollowedByOverview');
  FNoPreviousOverview := UUtilsJSON.GetAsBoolean(Value, 'NoPreviousOverview');
  FOverviewName := UUtilsJSON.GetAsString(Value, 'OverviewName');
  FIsSubOverview := UUtilsJSON.GetAsBoolean(Value, 'IsSubOverview');

  strEditPossibilities := UUtilsJSON.GetAsString(Value, 'EditPossibilities');
  StringToSet(TypeInfo(TEditPossibilities), FEditPossibilities, strEditPossibilities);

  FVariableOptions.AsJSonObject := UUtilsJSON.GetAsObject(Value, 'VariableOptions');
  FVariableDefaults.AsJSonObject := UUtilsJSON.GetAsObject(Value, 'VariableDefaults');
end;

procedure TSlideTemplate.SetPictoName(const Value: TSourceInfo);
begin
  FreeAndNil(FPictoName);
  FPictoName := Value;
end;

{ TSlideTemplates }

function TSlideTemplates.Add(strName, strCategoryName, strSlideLayoutName: string; iMenuOrder: integer): TSlideTemplate;
begin
  Result := FindByName(strName);
  if Result = nil then begin
    Result := TSlideTemplate.Create(strName, strCategoryName, strSlideLayoutName);
    inherited Add(Result);
  end;
  Result.MenuOrder := iMenuOrder;
end;

function TSlideTemplates.FindByName(strName: string): TSlideTemplate;
var
  i: Integer;
begin
  for i := 0 to Count -1 do begin
    if Items[i].Name = strName then begin
      Result := Items[i];
      Exit;
    end;
  end;
  Result := nil;
end;

function TSlideTemplates.GetTemplateNames(strCategory: string;
  blnIncludeNone: boolean): string;
var
  i: Integer;
begin
  Result := '';
  if blnIncludeNone then begin
    Result := _('None');
  end;
  for i := 0 to Count -1 do begin
    if CompareText(Items[i].CategoryName, strCategory) = 0 then begin
      if Result <> '' then begin
        Result := Result + #13;
      end;
      Result := Result + Items[i].Name;
    end;
  end;
end;

procedure TSlideTemplates.LoadSlideTemplates;
var
  slFiles: TStringList;
  strDir,  strCategory, strName: string;
  i: Integer;
  slideTemplate: TSlideTemplate;
begin
  strDir := GetSettings.GetContentDir + 'slidetemplates\';
  slFiles := TStringList.Create;
  try
    FindFilesWithExtensions(slFiles, true, strDir, '', '.json');

    for i := 0 to slFiles.Count -1 do begin
      strCategory := ExtractFilePath(slFiles[i]);
      strName := ExtractFileName(slFiles[i]);
      slideTemplate := Add(ChangeFileExt(strName, ''), strCategory, '', -1);
      slideTemplate.AsJSon := LoadUnicodeFromFile( strDir + slFiles[i] );
    end;
  finally
    slFiles.Free;
  end;

end;

procedure TSlideTemplates.SaveSlideTemplates;
var
  strDir: string;
  i: Integer;
begin
  strDir := GetSettings.GetContentDir + 'slidetemplates\';

  for i := 0 to Count -1 do begin
    ForceDirectories(strDir + Items[i].CategoryName + '\');
    SaveUnicodeToFile(strDir + Items[i].CategoryName + '\' + Items[i].Name + '.json', Items[i].AsJSon);
  end;
end;

{ TSlideTemplateComparer }

function TSlideTemplateComparer.Compare(const Left, Right: TSlideTemplate): Integer;
begin
  if Left.MenuOrder < Right.MenuOrder then
    Result := -1
  else if Left.MenuOrder > Right.MenuOrder then
    Result := 1
  else begin
    Result := CompareNatural(Left.CategoryName, Right.CategoryName);
    if Result = 0 then begin
      Result := CompareNatural(Left.Name, Right.Name);
    end;
  end;
end;

initialization

finalization
  gl_SlideTemplates.Clear;
  gl_SlideTemplates.Free;
end.
