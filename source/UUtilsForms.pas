unit UUtilsForms;

interface

Uses
  Windows, Classes, Controls, Dialogs, Forms, StdCtrls, ExtCtrls,
  UUtils, USourceInfo;

function MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  HelpCtx: Longint): Integer;

function Question(strQuestion: string): boolean;
function QuestionYNC(strQuestion: string): TCachedBoolean;
function ShowMemoDlg(strCaption, strMemoText, strMemoTextFileName, strHeader, strFooter: string; buttons: TMemoDlgBtn): integer; overload;
function ShowMemoDlg(strCaption: string; var strMemoText: string; strHeader, strFooter: string): boolean; overload;
function ShowMemoDlg(strCaption: string; var strMemoText: string; var strFooterText: string; strHeader, strFooter: string): boolean; overload;
function ShowListBoxDlg(strCaption, strHeader, strFooter: string;
  buttons: TMemoDlgBtn; selectionType: TSelectionType; slItems: TStrings;
  strStorageName: string; var strSelectedItem: string): integer;
function SelectPicto(pictoName: TSourceInfo): TSourceInfo;
function SelectPicture(pictoName: TSourceInfo; strSubdir: string): TSourceInfo;
function SelectPictureWithFooter(var pictoName: TSourceInfo; var strFooterText: string; strSubdir, strFooter: string): boolean;

implementation

uses
  SysUtils, UfrmMemoDlg, UfrmListBoxDlg, gnuGetText, UfrmPictureSelector, USettings;


function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;

var
  I,J : Integer;
  MsgForm : TForm;
  aButtons: array of TButton;
  oSwapButton: TButton;
  iSwapLeft: integer;

const
  BtnNames : Array [0..10] of String = ('Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
     'YesToAll', 'Help');
  BtnCaptions : Array [0..10] of String = ('Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
     'YesToAll', 'Help');
begin
  MsgForm := CreateMessageDialog(Msg, DlgType, Buttons);
  with MsgForm do begin
    try
      HelpContext := HelpCtx;
      Case DlgType Of
        mtWarning : Caption := _('Warning');
        mtError   : Caption := _('Error');
        mtInformation : Caption := _('Information');
        mtConfirmation : Caption := _('Confirmation');
      End;

//     BiDiMode := bdRightToLeft;
      for I:=0 to ComponentCount-1 do begin
        if BiDiMode = bdRightToLeft then begin
//       If (Components[I] is TLabel) Then Begin
//          (Components[I] as TLabel).Left := 23;
//       End;
//
          If (Components[I] is TImage) Then Begin
            (Components[I] as TImage).Left := MsgForm.Width-50;
          end;
        end;


        If (Components[I] is TButton) then Begin
          For J:=Low(BtnNames) to High(BtnNames) do Begin
            if TButton(Components[I]).Name = BtnNames[J] then begin
              TButton(Components[I]).Caption := _(BtnCaptions[J]);
              SetLength(aButtons, length(aButtons) +1);
              aButtons[high(aButtons)] := TButton(Components[I]);
            end;
          End;
        End;
      End;
      if BiDiMode = bdRightToLeft then begin
        // make sure the buttons are sorted in the left to right order
        for i := 0 to high(aButtons) do begin
          for j := (i+1) to high(aButtons) do begin
            if aButtons[j].Left < aButtons[i].Left then begin
              oSwapButton := aButtons[j];
              aButtons[j] := aButtons[i];
              aButtons[i] := oSwapButton;
            end;
          end;
        end;

        for i := 0 to high(aButtons) div 2 do begin
          iSwapLeft := aButtons[i].Left;
          aButtons[i].Left := aButtons[high(aButtons) -i].Left;
          aButtons[high(aButtons) -i].Left := iSwapLeft;
        end;

      end;
      Result := ShowModal;
    finally
      free ;
    end;
  end;
end;

function Question(strQuestion: string): boolean;
begin
  if MessageDlg(strQuestion, mtConfirmation, mbYesNo, -1) = mrYes then
    Result := True
  else
    Result := False;
end;

function QuestionYNC(strQuestion: string): TCachedBoolean;
begin
  case MessageDlg(strQuestion, mtConfirmation, mbYesNoCancel, -1) of
    mrYes: Result := cbTrue;
    mrNo: Result := cbFalse;
  else
    Result := cbUnknown;
  end;
end;

function ShowMemoDlg(strCaption, strMemoText, strMemoTextFileName, strHeader, strFooter: string; buttons: TMemoDlgBtn): integer;
var
  frmMemoDlg: TfrmMemoDlg;
begin
  if strCaption = '' then
    strCaption := Application.Title;
  frmMemoDlg := TfrmMemoDlg.Create(Application);
  try
    frmMemoDlg.Title := strCaption;
    if (strMemoTextFileName <> '') and FileExists(strMemoTextFileName) then
      frmMemoDlg.mmoMessage.Lines.LoadFromFile(strMemoTextFileName)
    else
      frmMemoDlg.MessageText.Text := strMemoText;
    frmMemoDlg.Header := strHeader;
    frmMemoDlg.Footer := strFooter;
    frmMemoDlg.Buttons := buttons;
    frmMemoDlg.Messagereadonly := true;
    frmMemoDlg.ShowFooterText := false;
    Result := frmMemoDlg.ShowModal;
  finally
    FreeAndNil(frmMemoDlg);
  end;
end;

function ShowMemoDlg(strCaption: string; var strMemoText: string; strHeader, strFooter: string): boolean;
var
  frmMemoDlg: TfrmMemoDlg;
begin
  if strCaption = '' then
    strCaption := Application.Title;
  frmMemoDlg := TfrmMemoDlg.Create(Application);
  try
    frmMemoDlg.Title := strCaption;
    frmMemoDlg.MessageText.Text := strMemoText;
    frmMemoDlg.Header := strHeader;
    frmMemoDlg.Footer := strFooter;
    frmMemoDlg.Buttons := mdOKCancel;
    frmMemoDlg.ShowFooterText := false;
    frmMemoDlg.Messagereadonly := false;
    Result := frmMemoDlg.ShowModal = mrOk;
    if Result then
      strMemoText := frmMemoDlg.MessageText.Text;
  finally
    FreeAndNil(frmMemoDlg);
  end;
end;

function ShowMemoDlg(strCaption: string; var strMemoText: string; var strFooterText: string; strHeader, strFooter: string): boolean;
var
  frmMemoDlg: TfrmMemoDlg;
begin
  if strCaption = '' then
    strCaption := Application.Title;
  frmMemoDlg := TfrmMemoDlg.Create(Application);
  try
    frmMemoDlg.Title := strCaption;
    frmMemoDlg.MessageText.Text := strMemoText;
    frmMemoDlg.FooterText := strFooterText;
    frmMemoDlg.Header := strHeader;
    frmMemoDlg.Footer := strFooter;
    frmMemoDlg.Buttons := mdOKCancel;
    frmMemoDlg.Messagereadonly := false;
    frmMemoDlg.ShowFooterText := true;
    Result := frmMemoDlg.ShowModal = mrOk;
    if Result then
      strMemoText := frmMemoDlg.MessageText.Text;
      strFooterText := frmMemoDlg.FooterText;
  finally
    FreeAndNil(frmMemoDlg);
  end;
end;


function ShowListBoxDlg(strCaption, strHeader, strFooter: string;
  buttons: TMemoDlgBtn; selectionType: TSelectionType; slItems: TStrings;
  strStorageName: string; var strSelectedItem: string): integer;
var
  frmListBoxDlg: TfrmListBoxDlg;
begin
  if strCaption = '' then
    strCaption := Application.Title;
  frmListBoxDlg := TfrmListBoxDlg.CreateWithGoal(Application, strStorageName);
  try
    frmListBoxDlg.Title := strCaption;
    frmListBoxDlg.MessageText.Assign(slItems);
    frmListBoxDlg.Header := strHeader;
    frmListBoxDlg.Footer := strFooter;
    frmListBoxDlg.Buttons := buttons;
    frmListBoxDlg.SelectionType := selectionType;
    frmListBoxDlg.SelectedItem := strSelectedItem;
    Result := frmListBoxDlg.ShowModal;
    case Result of
      mrYes,
      mrOk: strSelectedItem := frmListBoxDlg.SelectedItem;
    end;
  finally
    FreeAndNil(frmListBoxDlg);
  end;
end;


function SelectPicto(pictoName: TSourceInfo): TSourceInfo;
begin
  Result := SelectPicture(pictoName, 'pictos');
end;

function SelectPicture(pictoName: TSourceInfo; strSubdir: string): TSourceInfo;
var
  frmPictureSelector: TfrmPictureSelector;
begin
  Result := pictoName;
  frmPictureSelector := TfrmPictureSelector.Create(Application.MainForm);
  try
    frmPictureSelector.Selection := pictoName;
    frmPictureSelector.SubDir := strSubdir;
    frmPictureSelector.StartDir := GetSettings.GetContentDir;
    frmPictureSelector.ShowFooterText := false;
    if frmPictureSelector.ShowModal = mrOk then begin
      pictoName.Free;
      Result := frmPictureSelector.Selection;
    end;
  finally
    frmPictureSelector.Free;
  end;
end;

function SelectPictureWithFooter(var pictoName: TSourceInfo; var strFooterText: string; strSubdir, strFooter: string): boolean;
var
  frmPictureSelector: TfrmPictureSelector;
begin
  frmPictureSelector := TfrmPictureSelector.Create(Application.MainForm);
  try
    frmPictureSelector.Selection := pictoName;
    frmPictureSelector.SubDir := strSubdir;
    frmPictureSelector.StartDir := GetSettings.GetContentDir;
    frmPictureSelector.ShowFooterText := true;
    frmPictureSelector.Footer := strFooter;
    frmPictureSelector.FooterText := strFooterText;
    Result := frmPictureSelector.ShowModal = mrOk;
    if Result then begin
      pictoName.Free;
      pictoName := frmPictureSelector.Selection;
      strFooterText := frmPictureSelector.FooterText;
    end;
  finally
    frmPictureSelector.Free;
  end;
end;

end.
