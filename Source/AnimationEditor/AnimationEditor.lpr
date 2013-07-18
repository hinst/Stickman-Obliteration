program AnimationEditor;

uses
  Interfaces,
  Forms,
  CommonLog,
  MainWindowUnit, StickStructureUnit, StickStructureJsonLoaderUnit;

procedure WidgetsetApplicationRoutine;
begin
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.Run;
end;

begin
  StartupGlobalLog;
  WidgetsetApplicationRoutine;
  ShutdownGlobalLog;
end.

