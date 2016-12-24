unit ULiturgy;

interface

uses
  System.Generics.Collections, System.Generics.Defaults, Data.DBXJSON, System.JSON, Classes,
  UFastKeysSS, UProject, UUtilsJSON, USlideTemplate;

type
  TLiturgy = class(TJSONPersistent)
  private
    FName: string;
    FProjectProperties: TFastKeyValuesSS;
    FSlideTemplates: TStringList;
    FMenuOrder: integer;
  protected
    function GetAsJSonObject: TJSONObject; override;
    procedure SetAsJSonObject(const Value: TJSONObject); override;
  public
    constructor Create(strName: string); virtual;
    destructor Destroy; override;

    procedure FillProjectProperties(project: TProject);
    procedure FillProjectSlides(project: TProject; AFilterSlideTypeOptions: TSlideTypeOptions);
    function GetSlideTypeOptions: TSlideTypeOptions;

    property Name: string read FName;
    property MenuOrder: integer read FMenuOrder write FMenuOrder;
    property ProjectProperties: TFastKeyValuesSS read FProjectProperties;
    property SlideTemplates: TStringList read FSlideTemplates;
  end;

  ILiturgyComparer = interface(IComparer<TLiturgy>)
  end;

  TLiturgyComparer = class(TComparer<TLiturgy>, ILiturgyComparer)
  public
    function Compare(const Left, Right: TLiturgy): Integer; override;
  end;

  TLiturgies = class(TObjectList<TLiturgy>)
  public
    function Add(strName: string; iMenuOrder: integer): TLiturgy;
    function FindByName(strName: string): TLiturgy;

    procedure SaveLiturgies;
    procedure LoadLiturgies;
  end;

function GetLiturgies: TLiturgies;
procedure FillLiturgies;

implementation

uses
  SysUtils,
  USlide, USettings, UUtils;

var
  gl_Liturgies: TLiturgies = nil;

function GetLiturgies: TLiturgies;
var
  comparer: ILiturgyComparer;
begin
  if not Assigned(gl_Liturgies) then begin
    gl_Liturgies := TLiturgies.Create();
    FillLiturgies;
//    gl_Liturgies.SaveLiturgies;

//    gl_Liturgies.LoadLiturgies;
    comparer := TLiturgyComparer.Create;
    gl_Liturgies.Sort(comparer);
  end;
  Result := gl_Liturgies;
end;

procedure FillLiturgies;
var
  liturgy: TLiturgy;
begin
  //// Leeg
  liturgy := GetLiturgies.Add('Leeg', 0);
  liturgy.ProjectProperties['speaker'] := '';
  liturgy.ProjectProperties['collecte1'] := '';
  liturgy.ProjectProperties['collecte2'] := '';

  //// Orde van dienst A morgendienst
  liturgy := GetLiturgies.Add('Orde van dienst A morgendienst', 10);
  liturgy.ProjectProperties['speaker'] := 'Voorganger: ds A.J. van Zuijlekom';
  liturgy.ProjectProperties['collecte1'] := 'Eredienst';
  liturgy.ProjectProperties['collecte2'] := 'Diaconaal doel';
  // aanvang

  liturgy.SlideTemplates.Add('AED in geval van nood');
  liturgy.SlideTemplates.Add('Welkom');
  liturgy.SlideTemplates.Add('Mededelingen');
  liturgy.SlideTemplates.Add('Stilte moment');
  liturgy.SlideTemplates.Add('Votum en zegengroet Lb 416');
  liturgy.SlideTemplates.Add('Zingen-staan-PPT');
  liturgy.SlideTemplates.Add('Wet - 10 geboden');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Lezen');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Gebed');
  liturgy.SlideTemplates.Add(CTEMPLATE_COLLECTE);
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Tekst');
  liturgy.SlideTemplates.Add('Preek');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Gebed');
  liturgy.SlideTemplates.Add('Zingen-staan-PPT');
  liturgy.SlideTemplates.Add('Zegen Amen Lb 416');
  liturgy.SlideTemplates.Add('Afscheid - 16.30');

  //// Orde van dienst A middagdienst
  liturgy := GetLiturgies.Add('Orde van dienst A middagdienst', 20);
  liturgy.ProjectProperties['speaker'] := 'Voorganger: ds A.J. van Zuijlekom';
  liturgy.ProjectProperties['collecte1'] := 'Eredienst';
  liturgy.ProjectProperties['collecte2'] := 'Diaconaal doel';
  // aanvang
  liturgy.SlideTemplates.Add('AED in geval van nood');
  liturgy.SlideTemplates.Add('Welkom');
  liturgy.SlideTemplates.Add('Mededelingen');
  liturgy.SlideTemplates.Add('Stilte moment');
  liturgy.SlideTemplates.Add('Votum en zegengroet Lb 416');
  liturgy.SlideTemplates.Add('Zingen-staan-PPT');
  liturgy.SlideTemplates.Add('Geloofsbelijdenis lezen');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Lezen');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Gebed');
  liturgy.SlideTemplates.Add(CTEMPLATE_COLLECTE_CHILDREN);
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Tekst');
  liturgy.SlideTemplates.Add('Preek');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Gebed');
  liturgy.SlideTemplates.Add('Zingen-staan-PPT');
  liturgy.SlideTemplates.Add('Zegen Amen Lb 416');
  liturgy.SlideTemplates.Add('Afscheid - week');

  //// Orde van dienst B morgendienst
  liturgy := GetLiturgies.Add('Orde van dienst B morgendienst', 30);
  liturgy.ProjectProperties['speaker'] := 'Voorganger: ds A.J. van Zuijlekom';
  liturgy.ProjectProperties['collecte1'] := 'Eredienst';
  liturgy.ProjectProperties['collecte2'] := 'Diaconaal doel';
  // aanvang
  liturgy.SlideTemplates.Add('AED in geval van nood');
  liturgy.SlideTemplates.Add('Kidsbijbelclub 0-6 vanmorgen');
  liturgy.SlideTemplates.Add('Kompas 7-8 vanmorgen');
  liturgy.SlideTemplates.Add('Kidsbijbelclub en Kompas vanmorgen');

  liturgy.SlideTemplates.Add('Welkom');
  liturgy.SlideTemplates.Add('Mededelingen');
  liturgy.SlideTemplates.Add('Stilte moment');
  liturgy.SlideTemplates.Add('Votum en zegengroet Lb 416');
  liturgy.SlideTemplates.Add('Zingen-staan-PPT');
  liturgy.SlideTemplates.Add('Wet - 10 geboden');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  // dienst van het woord
  liturgy.SlideTemplates.Add('Gebed');
  liturgy.SlideTemplates.Add('Kidsbijbelclub 0-6 naar de zalen');
  liturgy.SlideTemplates.Add('Kompas 7-8 naar de zalen');
  liturgy.SlideTemplates.Add('Kidsbijbelclub en Kompas naar de zalen');

  liturgy.SlideTemplates.Add('Lezen');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Tekst');
  liturgy.SlideTemplates.Add('Preek');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Kidsbijbelclub 0-6 terug');
  liturgy.SlideTemplates.Add('Kompas 7-8 terug');
  liturgy.SlideTemplates.Add('Kidsbijbelclub en Kompas terug');

  // Dienst van de Tafel / Dienst van de Dankbaarheid
//  liturgy.SlideTemplates.Add('Doop picto');
  liturgy.SlideTemplates.Add('Gebed');
  liturgy.SlideTemplates.Add(CTEMPLATE_COLLECTE);
//  liturgy.SlideTemplates.Add('Avondmaal picto');
  // Zending en zege
  liturgy.SlideTemplates.Add('Zingen-staan-PPT');
  liturgy.SlideTemplates.Add('Zegen Amen Lb 416');
  liturgy.SlideTemplates.Add('Afscheid - 16.30');

  //// Orde van dienst B middagdienst
  liturgy := GetLiturgies.Add('Orde van dienst B middagdienst', 40);
  liturgy.ProjectProperties['speaker'] := 'Voorganger: ds A.J. van Zuijlekom';
  liturgy.ProjectProperties['collecte1'] := 'Eredienst';
  liturgy.ProjectProperties['collecte2'] := 'Diaconaal doel';
  // aanvang
  liturgy.SlideTemplates.Add('AED in geval van nood');
  liturgy.SlideTemplates.Add('Welkom');
  liturgy.SlideTemplates.Add('Mededelingen');
  liturgy.SlideTemplates.Add('Stilte moment');
  liturgy.SlideTemplates.Add('Votum en zegengroet Lb 416');
  liturgy.SlideTemplates.Add('Zingen-staan-PPT');
  liturgy.SlideTemplates.Add('Gebed');
  liturgy.SlideTemplates.Add('Lezen');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Tekst');
  liturgy.SlideTemplates.Add('Preek');
  liturgy.SlideTemplates.Add('Geloofsbelijdenis lezen');
//  liturgy.SlideTemplates.Add('Doop picto');
  liturgy.SlideTemplates.Add('Gebed');
  liturgy.SlideTemplates.Add(CTEMPLATE_COLLECTE_CHILDREN);
//  liturgy.SlideTemplates.Add('Avondmaal picto');
  liturgy.SlideTemplates.Add('Zingen-staan-PPT');
  liturgy.SlideTemplates.Add('Zegen Amen Lb 416');
  liturgy.SlideTemplates.Add('Afscheid - week');

  //// Orde van dienst C morgendienst
  liturgy := GetLiturgies.Add('Orde van dienst C morgendienst', 50);
  liturgy.ProjectProperties['speaker'] := 'Voorganger: ds A.J. van Zuijlekom';
  liturgy.ProjectProperties['collecte1'] := 'Eredienst';
  liturgy.ProjectProperties['collecte2'] := 'Diaconaal doel';
  // aanvang
  liturgy.SlideTemplates.Add('AED in geval van nood');
  liturgy.SlideTemplates.Add('Kidsbijbelclub 0-6 vanmorgen');
  liturgy.SlideTemplates.Add('Kompas 7-8 vanmorgen');
  liturgy.SlideTemplates.Add('Kidsbijbelclub en Kompas vanmorgen');

  liturgy.SlideTemplates.Add('Welkom');
  liturgy.SlideTemplates.Add('Mededelingen');
  liturgy.SlideTemplates.Add('Stilte moment');
  liturgy.SlideTemplates.Add('Votum en zegengroet Lb 416');
  liturgy.SlideTemplates.Add('Schuldbelijdenis en Genadeverkondiging');
  liturgy.SlideTemplates.Add('Zingen-staan-PPT');
  // dienst van het woord
  liturgy.SlideTemplates.Add('Gebed');
  liturgy.SlideTemplates.Add('Kidsbijbelclub 0-6 naar de zalen');
  liturgy.SlideTemplates.Add('Kompas 7-8 naar de zalen');
  liturgy.SlideTemplates.Add('Kidsbijbelclub en Kompas naar de zalen');

  liturgy.SlideTemplates.Add('Lezen');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Tekst');
  liturgy.SlideTemplates.Add('Preek');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Kidsbijbelclub 0-6 terug');
  liturgy.SlideTemplates.Add('Kompas 7-8 terug');
  liturgy.SlideTemplates.Add('Kidsbijbelclub en Kompas terug');

  // Dienst van de Tafel / Dienst van de Dankbaarheid
  liturgy.SlideTemplates.Add('Wet - 10 geboden');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
//  liturgy.SlideTemplates.Add('Doop picto');
  liturgy.SlideTemplates.Add('Gebed');
//  liturgy.SlideTemplates.Add('Geloofsbelijdenis lezen');
  liturgy.SlideTemplates.Add(CTEMPLATE_COLLECTE);
//  liturgy.SlideTemplates.Add('Avondmaal picto');
  // Zending en zege
  liturgy.SlideTemplates.Add('Zingen-staan-PPT');
  liturgy.SlideTemplates.Add('Zegen Amen Lb 416');
  liturgy.SlideTemplates.Add('Afscheid - 16.30');

  //// Orde van dienst C middagdienst
  liturgy := GetLiturgies.Add('Orde van dienst C middagdienst', 60);
  liturgy.ProjectProperties['speaker'] := 'Voorganger: ds A.J. van Zuijlekom';
  liturgy.ProjectProperties['collecte1'] := 'Eredienst';
  liturgy.ProjectProperties['collecte2'] := 'Diaconaal doel';
  // aanvang
  liturgy.SlideTemplates.Add('AED in geval van nood');
  liturgy.SlideTemplates.Add('Welkom');
  liturgy.SlideTemplates.Add('Mededelingen');
  liturgy.SlideTemplates.Add('Stilte moment');
  liturgy.SlideTemplates.Add('Votum en zegengroet Lb 416');
  liturgy.SlideTemplates.Add('Zingen-staan-PPT');
  // dienst van het woord
  liturgy.SlideTemplates.Add('Gebed');
  liturgy.SlideTemplates.Add('Lezen');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Tekst');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  liturgy.SlideTemplates.Add('Preek');
  liturgy.SlideTemplates.Add('Zingen-zit-PPT');
  // Dienst van de Tafel / Dienst van de Dankbaarheid
//  liturgy.SlideTemplates.Add('Doop picto');
  liturgy.SlideTemplates.Add('Geloofsbelijdenis lezen');
  liturgy.SlideTemplates.Add('Gebed');
  liturgy.SlideTemplates.Add(CTEMPLATE_COLLECTE_CHILDREN);
  // Zending en zege
  liturgy.SlideTemplates.Add('Zingen-staan-PPT');
  liturgy.SlideTemplates.Add('Zegen Amen Lb 416');
  liturgy.SlideTemplates.Add('Afscheid - week');
end;

{ TLiturgies }

function TLiturgies.Add(strName: string; iMenuOrder: integer): TLiturgy;
begin
  Result := FindByName(strName);
  if Result = nil then begin
    Result := TLiturgy.Create(strName);
    inherited Add(Result);
  end;
  Result.MenuOrder := iMenuOrder;
end;

function TLiturgies.FindByName(strName: string): TLiturgy;
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

procedure TLiturgies.LoadLiturgies;
var
  slFiles: TStringList;
  strDir: string;
  i: Integer;
  liturgy: TLiturgy;
begin
  strDir := GetSettings.GetContentDir + 'liturgies\';
  slFiles := TStringList.Create;
  try
    FindFilesWithExtensions(slFiles, false, strDir, '', '.json');

    for i := 0 to slFiles.Count -1 do begin
      liturgy := Add(ChangeFileExt(slFiles[i], ''), -1);
      liturgy.AsJSon := LoadUnicodeFromFile( strDir + slFiles[i] );
    end;
  finally
    slFiles.Free;
  end;
end;

procedure TLiturgies.SaveLiturgies;
var
  strDir: string;
  i: Integer;
begin
  strDir := GetSettings.GetContentDir + 'liturgies\';
  ForceDirectories(strDir);
  for i := 0 to Count -1 do begin
    SaveUnicodeToFile(strDir + Items[i].Name + '.json', Items[i].AsJSon);
  end;
end;

{ TLiturgy }

constructor TLiturgy.Create(strName: string);
begin
  inherited Create;
  FName := strName;
  FProjectProperties := TFastKeyValuesSS.Create;
  FSlideTemplates := TStringList.Create;
end;

destructor TLiturgy.Destroy;
begin
  FSlideTemplates.Free;
  FProjectProperties.Free;
  inherited;
end;

procedure TLiturgy.FillProjectProperties(project: TProject);
var
  i: integer;
begin
  for i := 0 to ProjectProperties.Count -1 do begin
    project.Properties[ProjectProperties.KeyOfIndex[i]] := ProjectProperties.ValueOfIndex[i];
  end;
end;

procedure TLiturgy.FillProjectSlides(project: TProject; AFilterSlideTypeOptions: TSlideTypeOptions);
var
  i: integer;
  template: TSlideTemplate;
  slide: TSlide;
begin
  for i := 0 to SlideTemplates.Count -1 do begin
    template := GetSlideTemplates.FindByName(SlideTemplates[i]);
    if Assigned(template) and (template.TypeOption in AFilterSlideTypeOptions) then begin
      slide := template.DoOnAdd(false);
      try
        if Assigned(slide) then begin
          project.Slides.Add(slide.AsJSon);
        end;
      finally
        slide.Free;
      end;
    end;
  end;
end;

function TLiturgy.GetAsJSonObject: TJSONObject;
begin
  Result := inherited GetAsJSonObject;
  Result.AddPair('Name', EscapeString(FName));
  Result.AddPair('MenuOrder', TJSONNumber.Create(FMenuOrder));
  Result.AddPair('ProjectProperties', FProjectProperties.AsJSonObject);
  Result.AddPair(CreateStringListPair('SlideTemplates', FSlideTemplates));
end;

function TLiturgy.GetSlideTypeOptions: TSlideTypeOptions;
var
  i: integer;
  template: TSlideTemplate;
begin
  Result := [];
  for i := 0 to SlideTemplates.Count -1 do
  begin
    template := GetSlideTemplates.FindByName(SlideTemplates[i]);
    if Assigned(template) then
    begin
      include(Result, template.TypeOption);
    end;
  end;
end;

procedure TLiturgy.SetAsJSonObject(const Value: TJSONObject);
begin
  inherited;
  FName := UUtilsJSON.GetAsString(Value, 'Name');
  FMenuOrder := UUtilsJSON.GetAsInteger(Value, 'MenuOrder');
  FProjectProperties.AsJSonObject := UUtilsJSON.GetAsObject(Value, 'ProjectProperties');
  UUtilsJSON.GetAsStringList(Value, 'SlideTemplates', FSlideTemplates);
end;

{ TLiturgyComparer }

function TLiturgyComparer.Compare(const Left, Right: TLiturgy): Integer;
begin
  if Left.MenuOrder < Right.MenuOrder then
    Result := -1
  else if Left.MenuOrder > Right.MenuOrder then
    Result := 1
  else
    Result := 0;
end;

initialization
  gl_Liturgies := nil;
finalization
  gl_Liturgies.Clear;
  FreeAndNil(gl_Liturgies);
end.
