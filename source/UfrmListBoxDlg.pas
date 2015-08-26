unit UfrmListBoxDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, UUtils, Vcl.AppEvnts;

type
  TfrmListBoxDlg = class(TForm)
    pnlHeaderMessage: TPanel;
    pnlButtons: TPanel;
    pnlFooterMessage: TPanel;
    btnYes: TButton;
    btnNo: TButton;
    btnOk: TButton;
    lblHeader: TLabel;
    lblFooter: TLabel;
    lbItems: TListBox;
    pnlEdit: TPanel;
    edtItem: TEdit;
    btnCancel: TButton;
    ApplicationEvents1: TApplicationEvents;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure lbItemsDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtItemChange(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
  private
    FHeader: string;
    FTitle: string;
    FFooter: string;
    FButtons: TMemoDlgBtn;
    FSelectionType: TSelectionType;
    FStorageName: string;
    FMessageText: TStringList;
    procedure SetButtons(const Value: TMemoDlgBtn);
    procedure SetFooter(const Value: string);
    procedure SetHeader(const Value: string);
    procedure SetTitle(const Value: string);
    function GetMessageText: TStrings;
    function GetSelectedItem: string;
    procedure SetSelectedItem(const Value: string);
    procedure SetSelectionType(const Value: TSelectionType);
    { Private declarations }
  protected
    procedure Refilter;
    procedure ReallignButtons;
    procedure OnChangeMessageText(Sender: TObject);
  public
    constructor CreateWithGoal(AOwner: TComponent; strStorageName: string); virtual;
    property StorageName: string read FStorageName;
    property SelectionType: TSelectionType read FSelectionType write SetSelectionType;
    property Title: string read FTitle write SetTitle;
    property Header: string read FHeader write SetHeader;
    property Footer: string read FFooter write SetFooter;
    property MessageText: TStrings read GetMessageText;
    property Buttons: TMemoDlgBtn read FButtons write SetButtons;
    property SelectedItem: string read GetSelectedItem write SetSelectedItem;

    { Public declarations }
  end;

implementation

uses
  StrUtils,
  GNUGetText
{$IFNDEF SIMPLE}
  , USettings
{$ENDIF}
  ;

{$R *.dfm}

procedure TfrmListBoxDlg.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  if FSelectionType in [stSingle] then begin
    btnYes.Enabled := lbItems.ItemIndex <> -1;
    btnOk.Enabled := lbItems.ItemIndex <> -1;
  end;
  if FSelectionType in [stAllowNew] then begin
    btnYes.Enabled := (lbItems.ItemIndex <> -1) or (edtItem.Text <> '');
    btnOk.Enabled := (lbItems.ItemIndex <> -1) or (edtItem.Text <> '');
  end;
  Done := true;
end;

constructor TfrmListBoxDlg.CreateWithGoal(AOwner: TComponent;
  strStorageName: string);
begin
  Create(AOwner);
  FStorageName := strStorageName;
end;

procedure TfrmListBoxDlg.edtItemChange(Sender: TObject);
begin
  Refilter;
end;

procedure TfrmListBoxDlg.FormCreate(Sender: TObject);
begin
  //ChangeBidiAsNeeded(Self);
  TranslateComponent(self);
  //AutoFitButtonCaptions(Self);
  FHeader := '';
  FTitle := Application.Title;
  FFooter := '';
  FButtons := mdOK;
  FSelectionType := stSingle;
  ReallignButtons;
  FMessageText := TStringList.Create;
  FMessageText.OnChange := OnChangeMessageText;
{$IFNDEF SIMPLE}
  CreateFormState(Self, 'TfrmListBoxDlg' + StorageName);
{$ENDIF}  
end;

procedure TfrmListBoxDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMessageText);
end;

procedure TfrmListBoxDlg.FormResize(Sender: TObject);
begin
  ReallignButtons;
end;

procedure TfrmListBoxDlg.FormShow(Sender: TObject);
begin
  pnlHeaderMessage.Visible := FHeader <> '';
  pnlFooterMessage.Visible := FFooter <> '';
  pnlEdit.Visible := FSelectionType in [stSingle, stAllowNew, stMultiAllowNew];
  if FSelectionType in [stSingle, stAllowNew, stMultiAllowNew] then
    edtItem.SetFocus
  else
    lbItems.SetFocus;
  ReallignButtons;
end;

function TfrmListBoxDlg.GetMessageText: TStrings;
begin
  Result := FMessageText;
end;

function TfrmListBoxDlg.GetSelectedItem: string;
var
  slSelected: TStringList;
  i: integer;
begin
  if FSelectionType in [stMulti, stMultiAllowNew] then begin
    slSelected := TStringList.Create;
    try
      slSelected.Delimiter := ',';
      slSelected.StrictDelimiter := True;
      lbItems.ItemIndex := -1;
      for i := 0 to lbItems.Items.Count - 1 do begin
        if lbItems.Selected[i] then begin
          slSelected.Add(lbItems.Items[i]);
        end;
      end;
      if FSelectionType = stMultiAllowNew then begin
        if (edtItem.Text <> '') and (lbItems.Items.IndexOf(edtItem.Text) = -1)  then
          slSelected.Add(edtItem.Text);
      end;
      Result := slSelected.DelimitedText;
    finally
      FreeAndNil(slSelected);
    end;
  end else begin
    if (FSelectionType = stAllowNew) and (edtItem.Focused or (lbItems.ItemIndex = -1)) then begin
      Result := edtItem.Text;
    end else begin
      if lbItems.ItemIndex <> -1 then
        Result := lbItems.Items[lbItems.ItemIndex]
      else
        Result := '';
    end;
  end;
end;

procedure TfrmListBoxDlg.lbItemsDblClick(Sender: TObject);
begin
  case Buttons of
    mdYesNo: ModalResult := mrYes;
    mdOK,
    mdOKCancel: ModalResult := mrOk;
  else
    ModalResult := mrNone;
  end;
end;

procedure TfrmListBoxDlg.OnChangeMessageText(Sender: TObject);
begin
  lbItems.Items.Assign(FMessageText);
  edtItem.Text := '';
end;

procedure TfrmListBoxDlg.ReallignButtons;
const
  CButtonWidth = 75;
  CButtonSpace = 5;
var
  aButtonArray: TButtonArray;
  i: integer;
  iTotalButtonWidth: integer;
begin
  case FButtons of
    mdYesNo: begin
        SetLength(aButtonArray, 2);
        aButtonArray[0] := btnYes;
        aButtonArray[1] := btnNo;
      end;
    mdOK: begin
        SetLength(aButtonArray, 1);
        aButtonArray[0] := btnOk;
      end;
    mdOKCancel: begin
        SetLength(aButtonArray, 2);
        aButtonArray[0] := btnOk;
        aButtonArray[1] := btnCancel;
      end;
  end;
  btnYes.Visible := FButtons = mdYesNo;
  btnNo.Visible := FButtons = mdYesNo;
  btnOk.Visible := FButtons in [mdOK, mdOKCancel];
  btnCancel.Visible := FButtons in [mdOK, mdOKCancel];

  iTotalButtonWidth := Length(aButtonArray) * (CButtonWidth + CButtonSpace) - CButtonSpace;
  for i := 0 to Length(aButtonArray) - 1 do begin
    if BiDiMode = bdRightToLeft then
      aButtonArray[i].Left := (Self.Width div 2) + (iTotalButtonWidth div 2) - (i+1) * (CButtonWidth + CButtonSpace)
    else
      aButtonArray[i].Left := (Self.Width div 2) - (iTotalButtonWidth div 2) + i * (CButtonWidth + CButtonSpace);
    aButtonArray[i].Width := CButtonWidth;
    if i = 0 then
      aButtonArray[i].Default := True
    else if i = (Length(aButtonArray) - 1) then
      aButtonArray[i].Cancel := True;
  end;
end;

procedure TfrmListBoxDlg.Refilter;
var
  i: integer;
  strSelected, strItem: string;
  strFilter: string;
begin
  if FSelectionType in [stSingle, stAllowNew] then begin
    if lbItems.ItemIndex <> -1 then
      strSelected := lbItems.Items[lbItems.ItemIndex]
    else
      strSelected := '';
    strFilter := edtItem.Text;
    if strFilter = '' then begin
      lbItems.Items.Assign(FMessageText);
    end else begin
      lbItems.Items.Clear;
      for i := 0 to FMessageText.Count -1 do begin
        strItem := FMessageText[i];
        if ContainsText(strItem, strFilter) then
          lbItems.Items.Add(strItem)
      end;
    end;
    lbItems.ItemIndex := lbItems.Items.IndexOf(strSelected);
  end;
end;

procedure TfrmListBoxDlg.SetButtons(const Value: TMemoDlgBtn);
begin
  if FButtons <> Value then begin
    FButtons := Value;
    ReallignButtons;
  end;
end;

procedure TfrmListBoxDlg.SetFooter(const Value: string);
begin
  if (FFooter <> Value) then begin
    FFooter := Value;
    lblFooter.Caption := FFooter;
  end;
end;

procedure TfrmListBoxDlg.SetHeader(const Value: string);
begin
  if (FHeader <> Value) then begin
    FHeader := Value;
    lblHeader.Caption := FHeader;
  end;
end;

procedure TfrmListBoxDlg.SetSelectedItem(const Value: string);
var
  index,i: integer;
  slSelected: TStringList;
begin
  if FSelectionType in [stMulti, stMultiAllowNew] then begin
    slSelected := TStringList.Create;
    try
      slSelected.StrictDelimiter := True;
      lbItems.ItemIndex := -1;
      slSelected.CommaText := Value;
      for i := 0 to slSelected.Count - 1 do begin
        index := lbItems.Items.IndexOf(slSelected[i]);
        if index <> -1 then begin
          lbItems.Selected[index] := true;
          if lbItems.ItemIndex = -1 then
            lbItems.ItemIndex := index;
        end;
      end;
    finally
      FreeAndNil(slSelected);
    end;
  end else begin
    index := lbItems.Items.IndexOf(Value);
    if index <> -1 then begin
      lbItems.ItemIndex := index;
    end else begin
      if FSelectionType in [stAllowNew, stMultiAllowNew] then
        edtItem.Text := Value;
      lbItems.ItemIndex := index;
    end;
  end;
end;

procedure TfrmListBoxDlg.SetSelectionType(const Value: TSelectionType);
begin
  FSelectionType := Value;
  lbItems.MultiSelect := FSelectionType in [stMulti, stMultiAllowNew];
  ReallignButtons;
end;

procedure TfrmListBoxDlg.SetTitle(const Value: string);
begin
  if (FTitle <> Value) then begin
    FTitle := Value;
    Caption := FTitle;
  end;
end;

end.
