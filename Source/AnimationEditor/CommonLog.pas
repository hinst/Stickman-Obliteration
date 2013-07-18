unit CommonLog;

interface

uses
  LogManagerUnit,
  RootLoggerUnit,
  LoggerUnit,
  DefaultOutputLogWriterUnit,
  DefaultTextLogMessageFormatterUnit;

procedure StartupGlobalLog;

function GetGlobalLog: TLogger;
function GetGlobalRootLog: TRootLogger;

procedure ShutdownGlobalLog;

implementation

procedure WriteLine(const aText: string);
begin
  if IsConsole then
    WriteLN(aText);
end;

var
  GlobalLogManager: TLogManager;
  GlobalRootLog: TRootLogger;
  GlobalLog: TLogger;

procedure StartupLogWriters;
begin
  GlobalLogManager.Add(
    TDefaultOutputLogWriter.Create(
      TDefaultTextLogMessageFormatter.Create
    )
  );
end;

procedure StartupGlobalLog;
begin
  WriteLine('Now starting up Global Log...');
  GlobalLogManager := TLogManager.Create;
  StartupLogWriters;
  GlobalRootLog := TRootLogger.Create(GlobalLogManager);
  GlobalLog := TLogger.Create(GlobalRootLog);
  GlobalLog.Name := 'GlobalLog';
  GlobalLog.Write('Log is now running...');
end;

function GetGlobalLog: TLogger;
begin
  result := GlobalLog;
end;

function GetGlobalRootLog: TRootLogger;
begin
  result := GlobalRootLog;
end;

procedure ShutdownGlobalLog;
begin
  WriteLine('Now shutting down Global Log...');
  GlobalLog.Free;
  GlobalRootLog.Free;
  GlobalLogManager.Free;
end;

end.

