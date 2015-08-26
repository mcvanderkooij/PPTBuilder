unit UfrmEditVariables;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UFastKeysSS, Vcl.StdCtrls, Vcl.ExtCtrls,
  USlide;

type
  TfrmEditVariables = class(TForm, ISlideEditForm)
    Panel1: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    procedure FormCreate(Sender: TObject);
  private
    FLabels: Array of TLabel;
    FValues: Array of TWinControl;
    { Private declarations }
  protected
    FStartSlide: string;
    function GetSlideAsString: string;
    procedure SetSlideAsString(const Value: string);
    function Edit: boolean;
  public
    procedure FastKeysToForm(vars, options: TFastKeyValuesSS);
    procedure FastKeysFromForm(vars: TFastKeyValuesSS);
    { Public declarations }
  end;

function EditFastKeySSVariables(vars, options: TFastKeyValuesSS): boolean;

implementation

uses
  Math,
  GNUGetText, USlideTemplate, USlideVariables, USourceInfo;

{$R *.dfm}

function EditFastKeySSVariables(vars, options: TFastKeyValuesSS): boolean;
var
  frmEditVariables: TfrmEditVariables;
begin
  if vars.Count = 0 then begin
    Result := true;
    Exit;
  end;
  frmEditVariables := TfrmEditVariables.Create(Application.MainForm);
  try
    frmEditVariables.FastKeysToForm(vars, options);
    Result := frmEditVariables.ShowModal = mrOK;
    if Result then begin
      frmEditVariables.FastKeysFromForm(vars);
    end;
  finally
    frmEditVariables.Free;
  end;
end;

{ TfrmEditVariables }

function TfrmEditVariables.Edit: boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TfrmEditVariables.FastKeysFromForm(vars: TFastKeyValuesSS);
var
  i: Integer;
  strKey, strValue: string;
begin
  for i := 0 to Length(FValues) -1 do begin
    if FValues[i] is TEdit then begin
      strValue := TEdit(FValues[i]).Text;
    end else if FValues[i] is TComboBox then begin
      strValue := TComboBox(FValues[i]).Text;
    end;
    strKey := FLabels[i].Caption;

    vars[strKey] := strValue;
  end;
end;

procedure TfrmEditVariables.FastKeysToForm(vars, options: TFastKeyValuesSS);
var
  i, index: Integer;
  edit: TEdit;
  combo: TComboBox;
begin
  SetLength(FLabels, vars.Count);
  SetLength(FValues, vars.Count);
  for i := 0 to vars.Count -1 do begin
    FLabels[i] := TLabel.Create(Self);
    FLabels[i].Parent := Self;
    FLabels[i].Caption := vars.KeyOfIndex[i];
    FLabels[i].Top := i*25 + 16;
    FLabels[i].Left := 8;
    FLabels[i].ShowAccelChar := false;

    index := options.IndexOfKey(vars.KeyOfIndex[i]);
    if index = -1 then begin
      edit := TEdit.Create(Self);
      edit.Parent := Self;
      edit.Text := vars.ValueOfIndex[i];
      edit.Top := i*25 + 12;
      edit.Left := ClientWidth div 3;
      edit.Width := (ClientWidth div 3 * 2) -8;
      edit.Anchors := [akLeft, akTop, akRight];
      FValues[i] := edit;
    end else begin
      combo := TComboBox.Create(Self);
      combo.Parent := Self;
      combo.Style := csDropDownList;
      combo.Items.Text := options.ValueOfIndex[index];
      combo.ItemIndex := combo.Items.IndexOf(vars.ValueOfIndex[i]);
      combo.Top := i*25 + 12;
      combo.Left := ClientWidth div 3;
      combo.Width := (ClientWidth div 3 * 2) -8;
      combo.Anchors := [akLeft, akTop, akRight];
      FValues[i] := combo;
    end;
  end;
  ClientHeight := Panel1.Height + vars.Count * 25 + 16;
  Constraints.MinHeight := Min(Height, 400);
  Constraints.MaxHeight := Max(Height, 400);
  Constraints.MinWidth := Min(Width, 400);
  Constraints.MaxWidth := Max(Width, 800);
end;

procedure TfrmEditVariables.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
end;

function TfrmEditVariables.GetSlideAsString: string;
var
  slide: TSlide;
begin
  slide := TSlide.Create(FStartSlide);
  try
    FastKeysFromForm(slide.Variables);
    Result := slide.AsJSon;
  finally
    slide.Free;
  end;
end;

procedure TfrmEditVariables.SetSlideAsString(const Value: string);
var
  slide: TSlide;
  template: TSlideTemplate;

  slVariables: TStringList;
  sourceInfo: TSourceInfo;
  slideItemContent: TSlideItem;
  i: integer;
begin
  FStartSlide := Value;
  slide := TSlide.Create(FStartSlide);
  if Assigned(slide) then begin
    slVariables := TStringList.Create;
    try
      slVariables.Duplicates := dupIgnore;
      slVariables.Sorted := true;
      slVariables.CaseSensitive := True;
      slideItemContent := slide['content'];

      for i := 0 to slideItemContent.ContentSources.Count -1 do begin
        sourceInfo := slideItemContent.ContentSources[i];
        if sourceInfo.SourceType in [sitString, sitTemplate] then
          DetectSlideVariables(sourceInfo.Text, slVariables);
      end;
      CleanupSlideVariables(slide.Variables, slVariables);
    finally
      slVariables.Free;
    end;

    template := GetSlideTemplates.FindByName(slide.SlideTemplateName);
    try
      FastKeysToForm(slide.Variables, template.VariableOptions);
    finally
      slide.Free;
    end;
  end;
end;

end.
