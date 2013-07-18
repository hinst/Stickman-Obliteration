unit StickStructureJsonLoaderUnit;

interface

uses
  SysUtils,
  Classes,
  LazUTF8Classes,
  LazFileUtils,
  fpJSON, jsonparser,
  LoggerUnit,
  CommonLog,
  StickStructureUnit;

type
  EStickStructureLoaderJson = class(Exception);

function CreateStickStructureFromJsonFile(const aFileName: string): TStickStructure;
function CreateStickStructureFromJsonStream(const aStream: TStream): TStickStructure;
function CreateStickStructure(const aData: TJSONData): TStickStructure;

implementation

var
  GlobalLog: TLogger;

function GetLog: TLogger;
begin
  if
    nil = GlobalLog
  then
    GlobalLog := TLogger.Create(GetGlobalLog);
  result := GlobalLog;
end;

function CreateStickStructureFromJsonFile(const aFileName: string): TStickStructure;
var
  stream: TFileStreamUTF8;
begin
  if
    FileExistsUTF8(aFileName)
  then
  begin
    stream := TFileStreamUTF8.Create(aFileName, fmOpenRead);
    result := CreateStickStructureFromJsonStream(stream);
    stream.Free;
  end
  else
  begin
    GetLog.Write(
      'Error',
      'Can not load stick structure from file "'
      + aFileName
      + '": no such file exists'
    );
    result := nil;
  end;
end;

function CreateStickStructureFromJsonStream(const aStream: TStream): TStickStructure;
var
  parser: TJSONParser;
  data: TJSONData;
begin
  parser := TJSONParser.Create(aStream);
  data := parser.Parse;
  result := CreateStickStructure(data);
  data.Free;
  parser.Free;
end;

const
  Id_Circle = 'Circle';
  Id_Pipe = 'Pipe';

procedure LoadStickStructureCircles(const aStructure: TStickStructure; const aObject: TJSONObject);
var
  i: Cardinal;
  currentName: string;
  currentData: TJSONData;
  currentCircle: TStickCircle;
begin
  aStructure.Circles.Reserve(aObject.Count);
  for i := 0 to aObject.Count - 1 do
  begin
    currentName := aObject.Names[i];
    currentData := aObject.Items[i];
    currentCircle.Name := currentName;
    currentCircle.defaultRadius := currentData.AsInteger;
    aStructure.Circles.PushBack(currentCircle);
  end;
end;

procedure LoadStickStructurePipes(const aStructure: TStickStructure; const aObject: TJSONObject);
begin

end;

function CreateStickStructure(const aData: TJSONData): TStickStructure;
var
  data: TJSONObject;
begin
  if
    aData.JSONType <> jtObject
  then
    raise EStickStructureLoaderJson.Create(
        'Can not load stick structure from json file: root object is not an object'
      );
  result := TStickStructure.Create;
  data := TJsonObject(aData);
  LoadStickStructureCircles(
    result,
    data.Get(Id_Circle, TJSONObject(nil))
  );
  LoadStickStructurePipes(
    result,
    data.Get(Id_Pipe, TJSONObject(nil))
  );
end;

end.

