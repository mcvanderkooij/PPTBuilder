unit UFastKeys;

interface

uses
  Classes, SysUtils, UUtilsJSON;

type
  TFastKeyMergeOption = (moAdd, moUpdate, moIncrement, moDelete);
  TFastKeyMergeOptions = set of TFastKeyMergeOption;

  TFastKeyValuesBase = class(TJSONPersistent)
  public
    destructor Destroy; override;
    procedure Clear; virtual; abstract;
    procedure Delete(index: integer); virtual; abstract;

    procedure LoadFromFile(strFileName: string); virtual;
    procedure LoadFromString(strString: string); virtual;
    procedure LoadFromStream(oStream: TStream); virtual;
    procedure LoadFromStrings(oStrings: TStrings); virtual;

    procedure MergeFromFile(strFileName: string; options: TFastKeyMergeOptions = [moAdd, moUpdate]); virtual;
    procedure Merge(strString: string; options: TFastKeyMergeOptions = [moAdd, moUpdate]); overload; virtual;
    procedure Merge(oStream: TStream; options: TFastKeyMergeOptions = [moAdd, moUpdate]); overload; virtual;
    procedure Merge(oStrings: TStrings; options: TFastKeyMergeOptions = [moAdd, moUpdate]); overload; virtual; abstract;
    procedure Merge(oFastKeyValues: TFastKeyValuesBase; options: TFastKeyMergeOptions = [moAdd, moUpdate]); overload; virtual;

    procedure SaveToFile(strFileName: string); virtual;
    function SaveToString: string; virtual;
    procedure SaveToStream(oStream: TStream); virtual;
    procedure SaveToStrings(oStrings: TStrings); virtual; abstract;
  end;

implementation

uses
  UUtilsStrings;

destructor TFastKeyValuesBase.Destroy;
begin
  Clear;
  inherited;
end;

procedure TFastKeyValuesBase.LoadFromFile(strFileName: string);
begin
  Clear;
  MergeFromFile(strFileName);
end;

procedure TFastKeyValuesBase.LoadFromStream(oStream: TStream);
begin
  Clear;
  Merge(oStream);
end;

procedure TFastKeyValuesBase.LoadFromString(strString: string);
begin
  Clear;
  Merge(strString);
end;

procedure TFastKeyValuesBase.LoadFromStrings(oStrings: TStrings);
begin
  Clear;
  Merge(oStrings);
end;

procedure TFastKeyValuesBase.Merge(oFastKeyValues: TFastKeyValuesBase; options: TFastKeyMergeOptions);
var
  slLines: TStringList;
begin
  if options = [] then
    Exit;
  slLines := TStringList.Create;
  try
    oFastKeyValues.SaveToStrings(slLines);
    Merge(slLines, options);
  finally
    slLines.Free;
  end;
end;

procedure TFastKeyValuesBase.Merge(oStream: TStream;
  options: TFastKeyMergeOptions);
var
  slLines: TStringList;
begin
  if options = [] then
    Exit;
  slLines := TStringList.Create;
  try
    slLines.LoadFromStream(oStream);
    Merge(slLines, options);
  finally
    slLines.Free;
  end;
end;

procedure TFastKeyValuesBase.Merge(strString: string;
  options: TFastKeyMergeOptions);
var
  slLines: TStringList;
begin
  if options = [] then
    Exit;
  slLines := TStringList.Create;
  try
    slLines.Delimiter := #8;
    slLines.StrictDelimiter := True;
    slLines.DelimitedText := strString;
    Merge(slLines, options);
  finally
    slLines.Free;
  end;
end;

procedure TFastKeyValuesBase.MergeFromFile(strFileName: string;
  options: TFastKeyMergeOptions);
var
  slLines: TStringList;
begin
  if options = [] then
    Exit;
  slLines := TStringList.Create;
  try
    slLines.LoadFromFile(strFileName, TEncoding.UTF8);
    Merge(slLines, options);
  finally
    slLines.Free;
  end;
end;

procedure TFastKeyValuesBase.SaveToFile(strFileName: string);
var
  slLines: TStringList;
begin
  slLines := TStringList.Create;
  try
    SaveToStrings(slLines);
    slLines.SaveToFile(strFileName, TEncoding.UTF8);
  finally
    slLines.Free;
  end;
end;

procedure TFastKeyValuesBase.SaveToStream(oStream: TStream);
var
  slLines: TStringList;
begin
  slLines := TStringList.Create;
  try
    slLines.Delimiter := #8;
    slLines.StrictDelimiter := True;
    SaveToStrings(slLines);
    slLines.SaveToStream(oStream);
  finally
    slLines.Free;
  end;
end;

function TFastKeyValuesBase.SaveToString: string;
var
  slLines: TStringList;
begin
  slLines := TStringList.Create;
  try
    slLines.Delimiter := #8;
    slLines.StrictDelimiter := True;
    SaveToStrings(slLines);
    Result := slLines.DelimitedText;
  finally
    slLines.Free;
  end;
end;

end.
