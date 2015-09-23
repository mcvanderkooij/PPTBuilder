unit UfrmBrowseBook;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  USourceInfo, Vcl.AppEvnts;

type
  TfrmBrowseBook = class(TForm)
    tvBooks: TTreeView;
    Panel1: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    Splitter1: TSplitter;
    mmoVerse: TMemo;
    ApplicationEvents1: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure tvBooksClick(Sender: TObject);
  private
    FSelectionValid: boolean;
    FSelection: TSourceInfo;
    FBook: string;
    function GetSelection: TSourceInfo;
    function GetSelectionMulti: TSourceInfos;
    procedure SetSelection(const Value: TSourceInfo);
    procedure SetSelectionMulti(const Value: TSourceInfos);
    function GetDoMultiSelection: boolean;
    procedure SetDoMultiSelection(const Value: boolean);
    { Private declarations }
  public
    property DoMultiSelection: boolean read GetDoMultiSelection write SetDoMultiSelection;
    property Selection: TSourceInfo read GetSelection write SetSelection;
    property SelectionMulti: TSourceInfos read GetSelectionMulti write SetSelectionMulti;
    property SelectionValid: boolean read FSelectionValid;

    procedure ClearSelection;
    procedure ValidateSelection;
    procedure OpenBook(strBook: string);
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  GNUGetText, USourceBook;

{ TfrmBrowseBook }

procedure TfrmBrowseBook.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
begin
  btnOK.Enabled := SelectionValid;
end;

procedure TfrmBrowseBook.ClearSelection;
begin
  mmoVerse.Lines.Clear;
  FreeAndNil(FSelection);
  FSelection := TSourceInfo.CreateAsBook('' ,'', '');
  FSelectionValid := false;
end;

procedure TfrmBrowseBook.FormCreate(Sender: TObject);
begin
  TranslateComponent(self);
  FSelection := nil;
  ClearSelection;
end;

procedure TfrmBrowseBook.FormDestroy(Sender: TObject);
begin
  FSelection.Free;
end;

function TfrmBrowseBook.GetDoMultiSelection: boolean;
begin
  Result := tvBooks.MultiSelect;
end;

function TfrmBrowseBook.GetSelection: TSourceInfo;
begin
  Result := FSelection.DeepCopy;
end;

function TfrmBrowseBook.GetSelectionMulti: TSourceInfos;
var
  node, nodeChild: TTreeNode;
  iShape: integer;
  source: TSourceInfo;
begin
  Result := TSourceInfos.Create;
  node := tvBooks.Items.GetFirstNode;
  while Assigned(node) do begin
    if node.Selected then begin
      if (node.Level = 1) then begin
        source := TSourceInfo.Create;
        Result.Add(source);
        source.SourceType := sitBook;
        source.FileName := FBook;
        source.SlideName := node.Parent.Text;
        source.ShapeName := node.Text;
      end else if (node.Level = 0) then begin
        nodeChild := node.getFirstChild;
        while Assigned(nodeChild) do begin
          source := TSourceInfo.Create;
          Result.Add(source);
          source.SourceType := sitBook;
          source.FileName := FBook;
          source.SlideName := nodeChild.Parent.Text;
          source.ShapeName := nodeChild.Text;
          nodeChild := nodeChild.getNextSibling;
        end;
      end;
    end;
    node := node.GetNext;
  end;
end;

procedure TfrmBrowseBook.OpenBook(strBook: string);
var
  cachedPPT: TCachedBook;
  i,j: integer;
  slideNode: TTreeNode;
  tree: TStringTree;
begin
  ClearSelection;

  Caption := _('Browse') + ' ' + strBook;
  cachedPPT := GetCachedBooks.Get(strBook);
  if Assigned(cachedPPT) then begin
    FBook := strBook;
    FSelection.FileName := strBook;

    tree := cachedPPT.ChaptersAndVerses;
    for i := 0 to tree.Count -1 do begin
      slideNode := tvBooks.Items.AddChild(nil, tree.Item[i].Data);
      for j := 0 to tree.Item[i].Count -1 do begin
        tvBooks.Items.AddChild(slideNode, tree.Item[i].Item[j].Data );
      end;
    end;

  end;
end;

procedure TfrmBrowseBook.SetDoMultiSelection(const Value: boolean);
begin
  tvBooks.MultiSelect := Value;
end;

procedure TfrmBrowseBook.SetSelection(const Value: TSourceInfo);
var
  node: TTreeNode;
begin
  if Value.FileName = '' then Exit;
  OpenBook(Value.FileName);
  if tvBooks.Items.Count = 0 then Exit;
  if Value.SlideName <> '' then begin
    node := tvBooks.Items.GetFirstNode;
    while Assigned(node) do begin
      if (node.Level = 0) and (node.Text = Value.SlideName) then begin
        node.MakeVisible;
        tvBooks.Selected := node;

        if Value.ShapeName <> '' then begin
          node := node.getFirstChild;
          while Assigned(node) do begin
            if (node.Level = 1) and (node.Text = Value.ShapeName) then begin
              node.MakeVisible;
              tvBooks.Selected := node;
              node := nil;
            end else begin
              node := node.getNextSibling;
            end;
          end;
        end;
        node := nil;
      end else begin
        node := node.getNextSibling;
      end;
    end;
  end;
  ValidateSelection;
end;

procedure TfrmBrowseBook.SetSelectionMulti(const Value: TSourceInfos);
begin
  if Value.Count > 0 then begin
    if Value[0].FileName <> FSelection.FileName then begin
      OpenBook(Value[0].FileName);
    end;
  end;
end;

procedure TfrmBrowseBook.tvBooksClick(Sender: TObject);
begin
  ValidateSelection;
end;

procedure TfrmBrowseBook.ValidateSelection;
var
  iShape: integer;
  source: TSourceInfo;
  nodeChild: TTreeNode;
begin
  FSelectionValid := false;
  if Assigned(tvBooks.Selected) then begin
    if (tvBooks.Selected.Level = 0) then begin
      FSelection.FileName := FBook;
      FSelection.ShapeName := tvBooks.Selected.Text;
      FSelectionValid := true;

      source := TSourceInfo.Create;
      try
        source.SourceType := sitBook;
        source.FileName := FBook;
        mmoVerse.Lines.Text := '';

        nodeChild := tvBooks.Selected.getFirstChild;
        while Assigned(nodeChild) do begin
          source.SlideName := nodeChild.Parent.Text;
          source.ShapeName := nodeChild.Text;
          mmoVerse.Lines.Text := mmoVerse.Lines.Text + #13 + GetCachedBooks.GetVerse(source);
          nodeChild := nodeChild.getNextSibling;
        end;
      finally
        source.Free;
      end;
    end;
    if (tvBooks.Selected.Level = 1) then begin
      FSelection.FileName := FBook;
      FSelection.SlideName := tvBooks.Selected.Parent.Text;
      FSelection.ShapeName := tvBooks.Selected.Text;
      FSelectionValid := true;

      mmoVerse.Lines.Text := GetCachedBooks.GetVerse(FSelection);
    end;
  end;
  if not FSelectionValid then
    ClearSelection;
end;

end.
