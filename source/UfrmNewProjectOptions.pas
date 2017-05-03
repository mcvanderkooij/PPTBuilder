unit UfrmNewProjectOptions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  UProject, USlideTemplate, Vcl.StdCtrls, UframeProjectProperties, Vcl.CheckLst;

type
  TfrmNewProjectOptions = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    FrameProjectProperties1: TFrameProjectProperties;
    lbSlideOptions: TCheckListBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    FNonOptionalSlideTypeOptions: TSlideTypeOptions;
  public
    procedure ProjectToForm(project: TProject);
    procedure FormToProject(project: TProject);
    procedure OptionsToForm(ASlideTypeOptions: TSlideTypeOptions);
    function FormToOptions: TSlideTypeOptions;
  end;

function GetProjectInfo(project: TProject; var ASlideTypeOptions: TSlideTypeOptions): boolean;

implementation

{$R *.dfm}

uses
  GNUGetText;

function GetProjectInfo(project: TProject; var ASlideTypeOptions: TSlideTypeOptions): boolean;
var
  frmProjectOptions: TfrmNewProjectOptions;
begin
  frmProjectOptions := TfrmNewProjectOptions.Create(Application);
  try
    frmProjectOptions.ProjectToForm(project);
    frmProjectOptions.OptionsToForm(ASlideTypeOptions);
    Result := frmProjectOptions.ShowModal = mrOK;
    if Result then
    begin
      frmProjectOptions.FormToProject(project);
      ASlideTypeOptions := frmProjectOptions.FormToOptions;
    end;
  finally
    frmProjectOptions.Free;
  end;
end;

{ TfrmProjectOptions }

procedure TfrmNewProjectOptions.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
end;

function TfrmNewProjectOptions.FormToOptions: TSlideTypeOptions;
var
  i: Integer;
begin
  Result := FNonOptionalSlideTypeOptions;
  for i := 0 to lbSlideOptions.Items.Count -1 do
  begin
    if lbSlideOptions.Checked[i] then
      include(Result, TSlideTypeOption(lbSlideOptions.Items.Objects[i]));
  end;

  // combine
  if (stKidsclub06 in Result) and (stKompas78 in Result) then
  begin
    Exclude(Result, stKidsclub06);
    Exclude(Result, stKompas78);
    Include(Result, stKidsclub06AndKompas78);
  end;
end;

procedure TfrmNewProjectOptions.FormToProject(project: TProject);
begin
  project.Properties['speaker'] := FrameProjectProperties1.Speaker;
  project.Properties['collecte1'] := FrameProjectProperties1.Collecte1;
  project.Properties['collecte2'] := FrameProjectProperties1.Collecte2;
end;

procedure TfrmNewProjectOptions.OptionsToForm(
  ASlideTypeOptions: TSlideTypeOptions);
begin
  FNonOptionalSlideTypeOptions := [];
  if stNone in ASlideTypeOptions then
    Include(FNonOptionalSlideTypeOptions, stNone);

  if stEAD in ASlideTypeOptions then
    lbSlideOptions.Items.AddObject(_('AED'), TObject(ord(stEAD)));

  if (stKidsclub06 in ASlideTypeOptions) or (stKidsclub06AndKompas78 in ASlideTypeOptions) then
    lbSlideOptions.Items.AddObject(_('Kidsclub groep 0-6'), TObject(ord(stKidsclub06)));
  if (stKompas78 in ASlideTypeOptions) or (stKidsclub06AndKompas78 in ASlideTypeOptions) then
    lbSlideOptions.Items.AddObject(_('Kompas groep 7-8'), TObject(ord(stKompas78)));

  if (stAfscheid1500 in ASlideTypeOptions) then
    lbSlideOptions.Items.AddObject(_('Afscheid 1500 uur'), TObject(ord(stAfscheid1500)));
  if (stAfscheid1500 in ASlideTypeOptions) then
    lbSlideOptions.Items.AddObject(_('Afscheid 1630 uur'), TObject(ord(stAfscheid1630)));
  if (stAfscheid1500 in ASlideTypeOptions) then
    lbSlideOptions.Items.AddObject(_('Afscheid 1900 uur'), TObject(ord(stAfscheid1900)));

end;

procedure TfrmNewProjectOptions.ProjectToForm(project: TProject);
begin
  FrameProjectProperties1.Speaker := project.Properties['speaker'];
  FrameProjectProperties1.Collecte1 := project.Properties['collecte1'];
  FrameProjectProperties1.Collecte2 := project.Properties['collecte2'];
end;

end.
