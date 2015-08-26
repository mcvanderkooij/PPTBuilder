unit UfrmSelectString;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.AppEvnts;

type
  TfrmSelectString = class(TForm)
    lbStrings: TListBox;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    edtFilter: TEdit;
    Label1: TLabel;
    ApplicationEvents1: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure edtFilterChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure lbStringsClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lbStringsDblClick(Sender: TObject);
  private
    FStrings: TStringList;
    FSelected: string;
    function GetSelected: string;
    { Private declarations }
  protected
    procedure DoFilter;
  public
    property Strings: TStringList read FStrings;
    property Selected: string read GetSelected write FSelected;
    { Public declarations }
  end;

function SelectString(slStrings: TStringlist; strTitle: string; var strSelected: string; out blnStringsChanged: boolean): boolean;

implementation

{$R *.dfm}

uses
  GNUGetText, UUtilsStrings;

function SelectString(slStrings: TStringlist; strTitle: string; var strSelected: string; out blnStringsChanged: boolean): boolean;
var
  frmSelectString: TfrmSelectString;
begin
  frmSelectString := TfrmSelectString.Create(Application.MainForm);
  try
    frmSelectString.Caption := strTitle;
    frmSelectString.Selected := strSelected;
    frmSelectString.Strings.Assign(slStrings);
    Result := frmSelectString.ShowModal = mrOk;
    if Result then begin
      strSelected := frmSelectString.Selected;
      blnStringsChanged := frmSelectString.Strings.Text <> slStrings.Text;
      if blnStringsChanged then begin
        slStrings.Assign(frmSelectString.Strings);
      end;
    end;
  finally
    frmSelectString.Free;
  end;
end;

procedure TfrmSelectString.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  btnEdit.Enabled := lbStrings.ItemIndex <> -1;
  btnDelete.Enabled := lbStrings.ItemIndex <> -1;
  btnOK.Enabled := lbStrings.ItemIndex <> -1;
end;

procedure TfrmSelectString.btnAddClick(Sender: TObject);
var
  strValue: string;
begin
  strValue := '';
  if InputQuery(_('New'), _('Enter text'), strValue) then begin
    FStrings.Add(strValue);
    FSelected := strValue;
    DoFilter;
  end;
end;

procedure TfrmSelectString.btnDeleteClick(Sender: TObject);
var
  strValue: string;
  index: integer;
begin
  strValue := lbStrings.Items[lbStrings.ItemIndex ];
  index := FStrings.IndexOf(strValue);
  if index <> -1 then begin
    FStrings.Delete(index);
  end;
  FSelected := '';
  DoFilter;
end;

procedure TfrmSelectString.btnEditClick(Sender: TObject);
var
  strOrgtext: string;
  strValue: string;
  index: integer;
begin
  strValue := lbStrings.Items[lbStrings.ItemIndex ];
  strOrgtext := strValue;
  if InputQuery(_('Edit'), _('Enter text'), strValue) then begin
    index := FStrings.IndexOf(strOrgtext);
    if index <> -1 then begin
      FStrings.Delete(index);
    end;
    FStrings.Add(strValue);
    FSelected := strValue;
    DoFilter;
  end;
end;

procedure TfrmSelectString.DoFilter;
var
  i: Integer;
begin
  if edtFilter.Text <> '' then begin
    lbStrings.Items.Clear;
    for i := 0 to FStrings.Count -1 do begin
      if PosIEx(edtFilter.Text, FStrings[i]) <> 0 then
        lbStrings.Items.Add(FStrings[i]);
    end;
  end else begin
    lbStrings.Items.Assign(FStrings);
  end;

  lbStrings.ItemIndex := lbStrings.Items.IndexOf(FSelected);
end;

procedure TfrmSelectString.edtFilterChange(Sender: TObject);
begin
  DoFilter;
end;

procedure TfrmSelectString.FormCreate(Sender: TObject);
begin
  FStrings := TStringList.Create;
end;

procedure TfrmSelectString.FormDestroy(Sender: TObject);
begin
  FStrings.Free;
end;

procedure TfrmSelectString.FormShow(Sender: TObject);
begin
  DoFilter;
end;

function TfrmSelectString.GetSelected: string;
begin
  Result := FSelected;
end;

procedure TfrmSelectString.lbStringsClick(Sender: TObject);
begin
  if lbStrings.ItemIndex <> -1 then begin
    FSelected := lbStrings.Items[lbStrings.ItemIndex];
  end;
end;

procedure TfrmSelectString.lbStringsDblClick(Sender: TObject);
begin
  if lbStrings.ItemIndex <> -1 then begin
    FSelected := lbStrings.Items[lbStrings.ItemIndex];
  end;
  ModalResult := mrOk;
end;

end.
