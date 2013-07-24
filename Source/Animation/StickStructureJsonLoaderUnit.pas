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

const
  CNLSSFJDM = 'Can not load stick structure from json document';

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
  CircleJsonID = 'Circle';
  PipeJsonID = 'Pipe';

procedure LoadStickStructureCircles(const aStructure: TStickStructure; const aObject: TJSONObject);
var
  i: Cardinal;
  currentName: string;
  currentData: TJSONData;
  currentCircle: PStickCircle;
begin
  aStructure.Circles.Reserve(aObject.Count);
  for i := 0 to aObject.Count - 1 do
  begin
    currentName := aObject.Names[i];
    currentData := aObject.Items[i];
    New(currentCircle);
    currentCircle^.Name := currentName;
    currentCircle^.defaultRadius := currentData.AsInteger;
    aStructure.Circles.PushBack(currentCircle);
  end;
end;

procedure LoadStickStructurePipePoints(
  const aStructure: TStickStructure; var aPipe: TStickPipe; const aArray: TJSONArray);
begin
  if
    aArray.Count <> 2
  then
    raise EStickStructureLoaderJson.Create('');
  aPipe.A := aStructure.FindCircle(aArray[0].AsString);
  aPipe.B := aStructure.FindCircle(aArray[1].AsString);
end;

procedure LoadStickStructurePipes(const aStructure: TStickStructure; const aObject: TJSONObject);
var
  i: Cardinal;
  currentName: string;
  currentData: TJsonData;
  currentPipe: TStickPipe;
begin
  aStructure.Pipes.Reserve(aObject.Count);
  for i := 0 to aObject.Count - 1 do
  begin
    currentName := aObject.Names[i];
    currentData := aObject.Items[i];
    currentPipe.Name := currentName;
    if
      currentData.JSONType <> jtArray
    then
      raise EStickStructureLoaderJson.Create(
          'Can not load stick structure from json document: pipe value is not an array: '
          + '"' + currentName + '"'
        );
    LoadStickStructurePipePoints(aStructure, currentPipe, currentData as TJSONArray);
    aStructure.Pipes.PushBack(currentPipe);
  end;
end;

function CreateStickStructure(const aData: TJSONData): TStickStructure;
var
  data: TJSONObject;
begin
  if
    aData.JSONType <> jtObject
  then
    raise EStickStructureLoaderJson.Create(
        'Can not load stick structure from json document: root object is not an object'
      );
  result := TStickStructure.Create;
  data := TJsonObject(aData);
  LoadStickStructureCircles(
    result,
    data.Get(CircleJsonID, TJSONObject(nil))
  );
  LoadStickStructurePipes(
    result,
    data.Get(PipeJsonID, TJSONObject(nil))
  );
end;

end.

