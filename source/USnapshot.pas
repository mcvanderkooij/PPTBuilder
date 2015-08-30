unit USnapshot;

interface

uses
  Classes, contnrs{, UObservers};

type
  IUndoRedo = interface
    ['{596D4B08-CCF2-47E1-A901-F69E4EF800ED}']
    function GetHasUndo: boolean;
    function GetHasRedo: boolean;
    procedure DoUndo;
    procedure DoRedo;
  end;

//  ISubjectAddSnapshot = interface(ISubject)
//    ['{BD66D863-E0EE-4113-B287-BEA322FDC480}']
//    function GetSheetID: TGuoidString;
//    procedure SetSheetID(const value: TGuoidString);
//    property SheetID: TGuoidString read GetSheetID write SetSheetID;
//  end;

  TSnapshot = class
  protected
    function GetAsString: string; virtual; abstract;
    procedure SetAsString(const Value: string); virtual; abstract;
  public
    function Equals(oWith: TSnapshot): boolean; virtual;
    property AsString: string read GetAsString write SetAsString;
  end;

  TSnapshotList = class(TObjectList)
  private
    FCurrentSnapshotIndex: integer;
    FActive: boolean;
    function GetCurrentSnapshotIndex: integer;
    procedure SetCurrentSnapshotIndex(const Value: integer);
    function GetItems(index: integer): TSnapshot;
    procedure SetItems(index: integer; const Value: TSnapshot);
    function GetHasUndo: boolean;
    function GetHasRedo: boolean;
  public
    property Active: boolean read FActive write FActive;
    property CurrentSnapshotIndex: integer read GetCurrentSnapshotIndex write SetCurrentSnapshotIndex;
    property Items[index: integer]: TSnapshot read GetItems write SetItems;
    property HasUndo: boolean read GetHasUndo;
    property HasRedo: boolean read GetHasRedo;
    procedure ActionDo(oNewSnapshot: TSnapshot);
    function ActionUndo: TSnapshot;
    function ActionRedo: TSnapshot;
    procedure Clear; override;
  end;

//function GetSubjectAddSnapshot: ISubjectAddSnapshot;

implementation

uses
  SysUtils;

//type
//  TSubjectAddSnapshot = class(TSubject, ISubjectAddSnapshot)
//  private
//    FSheetID: TGuoidString;
//  protected
//    function GetSheetID: TGuoidString;
//    procedure SetSheetID(const value: TGuoidString);
//  end;
//
//var
//  gl_SubjectAddSnapshot: ISubjectAddSnapshot;
//
//function GetSubjectAddSnapshot: ISubjectAddSnapshot;
//begin
//   if not Assigned(gl_SubjectAddSnapshot) then
//    gl_SubjectAddSnapshot := TSubjectAddSnapshot.Create;
//  Result := gl_SubjectAddSnapshot;
//end;
//
//function MakeOneLine(strLine: string): string;
//var
//  i: integer;
//begin
//  Result := strLine;
//  for i := 1 to Length(strLine) do
//    if Result[i] = #$a then
//      Result[i] := '_';
//end;
//

{ TSnapshotList }

procedure TSnapshotList.ActionDo(oNewSnapshot: TSnapshot);
begin
  if FActive then begin
    if FCurrentSnapshotIndex <> -1 then begin
      if Items[FCurrentSnapshotIndex].Equals(oNewSnapshot) then begin
        oNewSnapshot.Free;
        exit;
      end;
      while (FCurrentSnapshotIndex + 1) < Count do begin
        Delete(FCurrentSnapshotIndex + 1);
      end;
    end;
    FCurrentSnapshotIndex := Add(oNewSnapshot);
//gl_DebugSnapshots.Add(MakeOneLine(oNewSnapshot.AsString ));
//gl_DebugSnapshots.Add('ActionDo - FCurrentSnapshotIndex=' + IntToStr(FCurrentSnapshotIndex));
//gl_DebugSnapshots.SaveToFile('D:\temp\snapshots.txt');
    while Count > 30  do begin
      Delete(0);
      dec(FCurrentSnapshotIndex);
    end;
  end;
end;

function TSnapshotList.GetCurrentSnapshotIndex: integer;
begin
  Result := FCurrentSnapshotIndex;
end;

function TSnapshotList.GetHasRedo: boolean;
begin
  Result := FActive and ((FCurrentSnapshotIndex + 1) < Count);
end;

function TSnapshotList.GetHasUndo: boolean;
begin
  Result := FActive and (FCurrentSnapshotIndex > 0);
end;

function TSnapshotList.GetItems(index: integer): TSnapshot;
begin
  Result := inherited Items[index] as TSnapshot;
end;

function TSnapshotList.ActionRedo: TSnapshot;
begin
  if FActive then begin
    if HasRedo then begin
      inc(FCurrentSnapshotIndex);
    end;
    Result := Items[FCurrentSnapshotIndex];
  end;
end;

function TSnapshotList.ActionUndo: TSnapshot;
begin
  if FActive then begin
    if HasUndo then begin
      dec(FCurrentSnapshotIndex);
    end;
    Result := Items[FCurrentSnapshotIndex];
  end;
end;

procedure TSnapshotList.Clear;
begin
  inherited;
  FCurrentSnapshotIndex := -1;
  FActive := false;
end;

procedure TSnapshotList.SetCurrentSnapshotIndex(const Value: integer);
begin
  if (Value >= 0) and (Value < Count) then begin
    FCurrentSnapshotIndex := Value;
  end;
end;

procedure TSnapshotList.SetItems(index: integer; const Value: TSnapshot);
begin
  inherited Items[index] := Value;
end;

{ TSnapshot }

function TSnapshot.Equals(oWith: TSnapshot): boolean;
begin
  Result := AsString = oWith.AsString;
end;

//initialization
//  gl_DebugSnapshots := TStringList.Create;
//finalization
//  FreeAndNil(gl_DebugSnapshots);
{ TSubjectAddSnapshot }

//function TSubjectAddSnapshot.GetSheetID: TGuoidString;
//begin
//  Result := FSheetID;
//end;
//
//procedure TSubjectAddSnapshot.SetSheetID(const value: TGuoidString);
//begin
//  FSheetID := Value;
//end;

end.
